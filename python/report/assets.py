"""Utilities for preparing static assets (plot previews, etc.) for reports."""

from __future__ import annotations

import hashlib
import shutil
import subprocess
import logging
from pathlib import Path
from typing import Iterable

logger = logging.getLogger(__name__)


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
    logger.debug("pdftoppm converter resolved to %s", converter)
    if not converter:
        logger.info("pdftoppm converter not found; skipping preview generation.")
        return {}

    preview_root = output_dir / "static" / "previews"
    preview_root.mkdir(parents=True, exist_ok=True)
    logger.debug("Preview output directory initialised at %s", preview_root)

    previews: dict[Path, Path] = {}
    converted = 0
    for path in plot_files:
        logger.debug("Inspecting plot candidate %s", path)
        if path.suffix.lower() != ".pdf":
            logger.debug("Skipping non-PDF plot %s", path)
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
            logger.warning("pdftoppm failed to convert %s; skipping preview.", path)
            continue

        preview_path = output_prefix.with_suffix(".png")
        if not preview_path.exists():
            logger.warning(
                "pdftoppm reported success but %s was not created; skipping preview.",
                preview_path,
            )
            continue

        try:
            rel = path.relative_to(savedir)
        except ValueError:
            rel = path
        previews[rel] = preview_path.relative_to(output_dir)
        converted += 1
        logger.debug(
            "Created preview for %s at %s (converted %d)",
            rel,
            previews[rel],
            converted,
        )
        if converted >= limit:
            logger.debug("Preview conversion limit (%d) reached; stopping early.", limit)
            break

    if not previews:
        logger.info("No PDF previews were generated (processed %d candidates).", converted)
    else:
        logger.info("Generated %d PDF previews.", len(previews))

    return previews


__all__ = ["prepare_plot_previews"]
