"""Utilities for packaging tackle2 analysis outputs for external delivery.

The packaging workflow is intended to run *after* the main analysis pipeline
completes. It gathers the curated result artefacts (tables, plots, logs, and
optionally caches) from the configured `savedir`, writes an export manifest,
and bundles everything into a single ZIP archive that can be shared with
customers.

Typical usage (via the CLI integration):

```
$ tackle2 package --config path/to/config.toml
```

or, if the output directory is known:

```
$ tackle2 package --savedir /path/to/results
```

To avoid Windows path-length issues you can ask the packager to emit a
directory containing per-folder ZIP files instead of a single archive:

```
$ tackle2 package --config run_config.toml --split-components
```

The module exposes `package_results` so it can be reused programmatically or
tested without invoking the CLI.
"""

from __future__ import annotations

import json
import logging
import os
import re
import shutil
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable, List, Optional, Sequence
import zipfile
import hashlib

try:  # Python >=3.11
    import tomllib  # type: ignore[attr-defined]
except ModuleNotFoundError:  # pragma: no cover - fallback for older runtimes
    import tomli as tomllib  # type: ignore[no-redef]

try:  # Python >=3.8
    from importlib import metadata as importlib_metadata
except ModuleNotFoundError:  # pragma: no cover
    import importlib_metadata  # type: ignore[no-redef]


logger = logging.getLogger(__name__)


class PackagingError(RuntimeError):
    """Custom exception raised when packaging prerequisites are not met."""


DEFAULT_EXCLUDES = {"cache", "ranks", "tmp", "run_cache"}


@dataclass
class FileRecord:
    """Represents a single file destined for the export archive."""

    absolute_path: Path
    relative_path: Path
    size_bytes: int
    mtime_iso: str
    sha256: Optional[str] = None

    def to_manifest_entry(self) -> dict:
        entry = {
            "path": self.relative_path.as_posix(),
            "size_bytes": self.size_bytes,
            "modified_at": self.mtime_iso,
        }
        if self.sha256:
            entry["sha256"] = self.sha256
        return entry


def _read_config(config_path: Path) -> dict:
    logger.debug("Loading configuration from %s", config_path)
    with config_path.open("rb") as fh:
        return tomllib.load(fh)


def _find_savedir(
    savedir: Optional[Path],
    config_path: Optional[Path],
) -> tuple[Path, Optional[Path], Optional[dict]]:
    """Resolve the results directory using the explicit path or a config file."""

    if savedir:
        resolved = savedir.expanduser().resolve()
        if not resolved.exists() or not resolved.is_dir():
            raise PackagingError(f"Results directory not found: {resolved}")
        return resolved, None, None

    if not config_path:
        raise PackagingError("Either --savedir or --config must be supplied")

    config_path = config_path.expanduser().resolve()
    if not config_path.exists():
        raise PackagingError(f"Config file not found: {config_path}")

    config_data = _read_config(config_path)
    params = config_data.get("params") or {}
    savedir_value = params.get("savedir")

    if not savedir_value:
        raise PackagingError(
            f"Config {config_path} does not define params.savedir; specify --savedir"
        )

    candidate = Path(savedir_value)
    if not candidate.is_absolute():
        candidate = (config_path.parent / candidate).resolve()

    if not candidate.exists() or not candidate.is_dir():
        raise PackagingError(
            f"Resolved results directory does not exist: {candidate} (from {config_path})"
        )

    return candidate, config_path, config_data


def _iter_files(savedir: Path, excludes: Sequence[str]) -> Iterable[Path]:
    """Yield files under savedir while respecting excluded directory names."""

    exclude_set = set(excludes)
    for root, dirnames, filenames in os.walk(savedir):
        root_path = Path(root)
        # mutate dirnames in-place so os.walk skips excluded subdirectories
        dirnames[:] = [d for d in dirnames if d not in exclude_set]
        for filename in filenames:
            file_path = root_path / filename
            yield file_path


def _format_mtime(mtime: float) -> str:
    return (
        datetime.fromtimestamp(mtime, tz=timezone.utc)
        .isoformat(timespec="seconds")
        .replace("+00:00", "Z")
    )


def _compute_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as fh:
        for chunk in iter(lambda: fh.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _collect_file_records(
    savedir: Path,
    excludes: Sequence[str],
    include_hashes: bool,
) -> List[FileRecord]:
    records: List[FileRecord] = []
    for file_path in _iter_files(savedir, excludes):
        if not file_path.is_file():
            continue
        relative = file_path.relative_to(savedir)
        stat = file_path.stat()
        hash_value = _compute_sha256(file_path) if include_hashes else None
        records.append(
            FileRecord(
                absolute_path=file_path,
                relative_path=relative,
                size_bytes=stat.st_size,
                mtime_iso=_format_mtime(stat.st_mtime),
                sha256=hash_value,
            )
        )
    return records


def _sanitize_token(token: str) -> str:
    token = token.strip()
    token = re.sub(r"[^A-Za-z0-9._-]+", "_", token)
    token = re.sub(r"_+", "_", token)
    token = token.strip("_-")
    return token


def _compose_export_stem(
    savedir: Path,
    base_token: Optional[str],
    timestamp_token: str,
    file_count: int,
) -> str:
    tokens: List[str] = []
    if base_token:
        tokens.append(base_token)
    else:
        tokens.append(savedir.name or "tackle2_results")

    parent_name = savedir.parent.name
    if parent_name and parent_name not in ("", tokens[0]):
        tokens.append(parent_name)

    tokens.append(f"{file_count}files")
    tokens.append(timestamp_token)

    sanitized = [_sanitize_token(token) for token in tokens if token]
    sanitized = [token for token in sanitized if token]
    if not sanitized:
        return "tackle2_export"
    return "_".join(sanitized)


def _resolve_output_path(
    output: Optional[Path],
    savedir: Path,
    timestamp_token: str,
    file_count: int,
) -> Path:
    if output:
        output = output.expanduser().resolve()
        if output.exists() and output.is_dir():
            raise PackagingError(
                f"Output path {output} is a directory; supply the archive filename instead"
            )

        suffix = output.suffix.lower()
        if suffix == ".zip":
            parent = output.parent
            if not parent.exists():
                raise PackagingError(f"Output directory does not exist: {parent}")
            return output

        base_token = output.stem if suffix else output.name
        archive_stem = _compose_export_stem(savedir, base_token, timestamp_token, file_count)
        final_path = output.parent / f"{archive_stem}.zip"
        parent = final_path.parent
        if not parent.exists():
            raise PackagingError(f"Output directory does not exist: {parent}")
        return final_path

    archive_stem = _compose_export_stem(savedir, None, timestamp_token, file_count)
    final_path = (savedir.parent / f"{archive_stem}.zip").resolve()
    parent = final_path.parent
    if not parent.exists():
        raise PackagingError(f"Output directory does not exist: {parent}")
    return final_path


def _resolve_export_root(
    savedir: Path,
    label: Optional[str],
    timestamp_token: str,
    file_count: int,
) -> str:
    base = label.strip().replace(os.sep, "_") if label else None
    return _compose_export_stem(savedir, base, timestamp_token, file_count)


def _resolve_output_directory(
    output: Optional[Path],
    savedir: Path,
    timestamp_token: str,
    file_count: int,
) -> Path:
    if output:
        output = output.expanduser().resolve()
        if output.suffix.lower() == ".zip":
            output = output.with_suffix("")
        if output.exists():
            if not output.is_dir():
                raise PackagingError(
                    f"Output path {output} exists and is not a directory"
                )
        else:
            output.parent.mkdir(parents=True, exist_ok=True)
            output.mkdir(parents=True, exist_ok=True)
        return output

    dir_name = _compose_export_stem(savedir, None, timestamp_token, file_count)
    destination = (savedir.parent / dir_name).resolve()
    destination.mkdir(parents=True, exist_ok=True)
    return destination


def _group_records_by_top_level(records: Sequence[FileRecord]) -> dict[str, List[FileRecord]]:
    grouped: dict[str, List[FileRecord]] = defaultdict(list)
    for rec in records:
        parts = rec.relative_path.parts
        if len(parts) > 1:
            key = parts[0]
        else:
            key = "__root__"
        grouped[key].append(rec)
    return grouped


def _unique_component_name(name: str, seen: set[str]) -> str:
    base = _sanitize_token(name) or "component"
    candidate = base
    index = 1
    while candidate in seen:
        candidate = f"{base}_{index}"
        index += 1
    seen.add(candidate)
    return candidate


def _write_split_archives(
    base_dir: Path,
    records: Sequence[FileRecord],
    resolved_config: Optional[Path],
) -> List[dict]:
    for existing_zip in base_dir.glob("*.zip"):
        if existing_zip.is_file():
            existing_zip.unlink()

    config_dir = base_dir / "config"
    if config_dir.exists() and config_dir.is_dir():
        shutil.rmtree(config_dir)

    grouped = _group_records_by_top_level(records)
    seen_names: set[str] = set()
    components: List[dict] = []

    for key, recs in grouped.items():
        display_name = "root_files" if key == "__root__" else key
        archive_basename = _unique_component_name(display_name, seen_names)
        archive_path = base_dir / f"{archive_basename}.zip"

        with zipfile.ZipFile(archive_path, mode="w", compression=zipfile.ZIP_DEFLATED) as zf:
            for rec in recs:
                arcname = rec.relative_path.as_posix()
                zf.write(rec.absolute_path, arcname)

        components.append(
            {
                "component": display_name,
                "archive": archive_path.name,
                "file_count": len(recs),
                "total_size_bytes": sum(r.size_bytes for r in recs),
            }
        )

    if resolved_config:
        config_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(resolved_config, config_dir / resolved_config.name)

    return components


def package_results(
    savedir: Optional[Path] = None,
    config_path: Optional[Path] = None,
    output_path: Optional[Path] = None,
    include_cache: bool = False,
    include_ranks: bool = False,
    include_hashes: bool = True,
    export_label: Optional[str] = None,
    extra_excludes: Optional[Sequence[str]] = None,
    split_components: bool = False,
) -> tuple[Path, dict]:
    """Bundle analysis results into a distributable ZIP archive.

    Parameters
    ----------
    savedir : Path, optional
        Directory containing pipeline outputs. If omitted, `config_path` must
        be supplied so `params.savedir` can be read from the TOML config.
    config_path : Path, optional
        Path to the TOML configuration that produced the results. When
        provided it will be copied into the export archive for provenance.
    output_path : Path, optional
        Destination path for the ZIP archive. If omitted a timestamped name is
        generated next to `savedir`.
    include_cache : bool, default False
        Whether to include cached RDS files in the export bundle.
    include_ranks : bool, default False
        Whether to include intermediate rank files (often large).
    include_hashes : bool, default True
        Compute SHA-256 checksums for each exported file and record them in the
        manifest. Disable to speed up packaging for very large exports.
    export_label : str, optional
        Custom root directory name to use inside the archive.
    extra_excludes : Sequence[str], optional
        Additional directory names to omit from the export.
    split_components : bool, default False
        Emit a directory containing per-top-level ZIP bundles instead of a
        single archive. Useful when downstream environments have restrictive
        path-length limits (e.g. Windows extraction).

    Returns
    -------
    tuple[Path, dict]
        The resolved archive path and the manifest dictionary.
    """

    results_dir, resolved_config, config_data = _find_savedir(savedir, config_path)

    excludes = set(DEFAULT_EXCLUDES)
    if include_cache:
        excludes.discard("cache")
    if include_ranks:
        excludes.discard("ranks")
    if extra_excludes:
        excludes.update(extra_excludes)

    records = _collect_file_records(results_dir, sorted(excludes), include_hashes)
    if not records:
        raise PackagingError(f"No files found under {results_dir}; nothing to package")

    file_count = len(records)
    now = datetime.now(timezone.utc)
    timestamp_token = now.strftime("%Y%m%dT%H%M%SZ")

    version = "unknown"
    try:
        version = importlib_metadata.version("tackle2")
    except importlib_metadata.PackageNotFoundError:  # pragma: no cover
        logger.debug("tackle2 package metadata not found; using 'unknown'")

    total_size = sum(rec.size_bytes for rec in records)

    if split_components:
        destination = _resolve_output_directory(output_path, results_dir, timestamp_token, file_count)
        export_root = destination.name if not export_label else _compose_export_stem(
            results_dir, export_label, timestamp_token, file_count
        )

        manifest = {
            "packaged_at": now.isoformat(timespec="seconds").replace("+00:00", "Z"),
            "tool_version": version,
            "savedir": str(results_dir),
            "export_root": export_root,
            "file_count": file_count,
            "total_size_bytes": total_size,
            "excluded_directories": sorted(excludes),
            "source_config": str(resolved_config) if resolved_config else None,
            "package_mode": "split_components",
        }
        if resolved_config:
            manifest["config_copy"] = str(Path("config") / resolved_config.name)
        else:
            manifest["config_copy"] = None
        if config_data:
            manifest["config_snapshot"] = {
                "params": config_data.get("params", {}),
            }

        manifest["files"] = [rec.to_manifest_entry() for rec in records]

        components = _write_split_archives(destination, records, resolved_config)
        manifest["component_archives"] = components

        manifest_path = destination / "manifest.json"
        manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=True))

        logger.info("Wrote split-package directory to %s", destination)
        return destination, manifest

    archive_path = _resolve_output_path(
        output_path, results_dir, timestamp_token, file_count
    )
    export_root = _resolve_export_root(
        results_dir, export_label, timestamp_token, file_count
    )

    manifest = {
        "packaged_at": now.isoformat(timespec="seconds").replace("+00:00", "Z"),
        "tool_version": version,
        "savedir": str(results_dir),
        "export_root": export_root,
        "results_subdir": "results",
        "file_count": file_count,
        "total_size_bytes": total_size,
        "excluded_directories": sorted(excludes),
        "source_config": str(resolved_config) if resolved_config else None,
        "package_mode": "single_archive",
    }
    if resolved_config:
        manifest["config_copy"] = str(Path(export_root) / "config" / resolved_config.name)
    else:
        manifest["config_copy"] = None
    if config_data:
        manifest["config_snapshot"] = {
            "params": config_data.get("params", {}),
        }

    manifest["files"] = [rec.to_manifest_entry() for rec in records]

    logger.info("Writing export archive to %s", archive_path)
    with zipfile.ZipFile(archive_path, mode="w", compression=zipfile.ZIP_DEFLATED) as zf:
        results_prefix = Path(export_root) / "results"
        for rec in records:
            arcname = (results_prefix / rec.relative_path).as_posix()
            zf.write(rec.absolute_path, arcname)

        manifest_path = Path(export_root) / "manifest.json"
        zf.writestr(manifest_path.as_posix(), json.dumps(manifest, indent=2, sort_keys=True))

        if resolved_config:
            config_arcname = Path(export_root) / "config" / resolved_config.name
            zf.write(resolved_config, config_arcname.as_posix())

    return archive_path, manifest


__all__ = ["PackagingError", "package_results"]
