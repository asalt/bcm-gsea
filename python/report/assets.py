"""Utilities for preparing static assets (plot previews, etc.) for reports."""

from __future__ import annotations

import hashlib
import shutil
import subprocess
from pathlib import Path
from typing import Iterable


def _hash_path(path: Path) -> str:
    digest = hashlib.sha1(str(path).encode("utf-8")).hexdigest()
    return digest[:16]


def prepare_plot_previews(
    plot_files: Iterable[Path],
    savedir: Path,
    output_dir: Path,
    limit: int = 12,
) -> dict[Path, Path]:
    """Generate PNG previews for the first N PDF plots using pdftoppm.

    Returns a mapping of original relative paths (relative to the savedir)
    to preview paths (relative to the report output directory).
    """

    converter = shutil.which("pdftoppm")
    if not converter:
        return {}

    preview_root = output_dir / "static" / "previews"
    preview_root.mkdir(parents=True, exist_ok=True)

    previews: dict[Path, Path] = {}
    for path in list(plot_files)[:limit]:
        if path.suffix.lower() != ".pdf":
            continue

        token = _hash_path(path)
        output_prefix = preview_root / token
        try:
            subprocess.run(
                [converter, "-singlefile", "-png", str(path), str(output_prefix)],
                check=True,
                capture_output=True,
            )
        except (subprocess.CalledProcessError, FileNotFoundError):
            continue

        preview_path = output_prefix.with_suffix(".png")
        if not preview_path.exists():
            continue

        try:
            rel = path.relative_to(savedir)
        except ValueError:
            rel = path
        previews[rel] = preview_path.relative_to(output_dir)

    return previews


__all__ = ["prepare_plot_previews"]
