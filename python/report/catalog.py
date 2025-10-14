"""Filesystem inventory helpers for tackle2 report generation."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Set


PLOT_EXTENSIONS = {".pdf", ".png", ".svg"}
TABLE_EXTENSIONS = {".tsv"}
LOG_EXTENSIONS = {".log", ".txt"}


@dataclass(frozen=True)
class SavedirArtefacts:
    savedir: Path
    gsea_tables: List[Path]
    plot_files: List[Path]
    log_files: List[Path]
    html_files: List[Path]
    other_files: List[Path]


def _scan_tsvs(base: Path) -> List[Path]:
    results = sorted(path for path in base.glob("gsea_tables/**/*.tsv") if path.is_file())
    return results


def _walk_files(base: Path) -> Iterable[Path]:
    for path in base.rglob("*"):
        if path.is_file():
            yield path


def _is_within(path: Path, candidates: Set[Path]) -> bool:
    return any(path == candidate for candidate in candidates)


def scan_savedir(savedir: Path) -> SavedirArtefacts:
    """Collect artefact paths of interest for subsequent summarisation."""

    base = Path(savedir).expanduser().resolve()
    if not base.exists() or not base.is_dir():
        raise FileNotFoundError(f"Results directory not found: {base}")

    gsea_tables = _scan_tsvs(base)
    tracked: Set[Path] = set(gsea_tables)

    plot_files: List[Path] = []
    log_files: List[Path] = []
    html_files: List[Path] = []
    other_files: List[Path] = []

    for file_path in _walk_files(base):
        if _is_within(file_path, tracked):
            continue

        suffix = file_path.suffix.lower()
        if suffix in PLOT_EXTENSIONS:
            plot_files.append(file_path)
            continue
        if suffix in LOG_EXTENSIONS:
            log_files.append(file_path)
            continue
        if suffix in {".html"}:
            html_files.append(file_path)
            continue

        other_files.append(file_path)

    plot_files.sort()
    log_files.sort()
    html_files.sort()
    other_files.sort()

    return SavedirArtefacts(
        savedir=base,
        gsea_tables=gsea_tables,
        plot_files=plot_files,
        log_files=log_files,
        html_files=html_files,
        other_files=other_files,
    )


__all__ = ["SavedirArtefacts", "scan_savedir", "PLOT_EXTENSIONS"]
