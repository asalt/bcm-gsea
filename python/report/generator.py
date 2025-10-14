"""High-level entry point for building HTML reports from tackle2 outputs."""

from __future__ import annotations

import http.server
import os
import socket
import socketserver
import webbrowser
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import click

from .. import export_packager
from . import assets, catalog, summary, templating


class ReportGenerationError(RuntimeError):
    """Raised when report prerequisites are not satisfied."""


@dataclass(frozen=True)
class ReportPaths:
    savedir: Path
    output_dir: Path
    config_path: Optional[Path]


def _resolve_paths(
    savedir: Optional[Path],
    config: Optional[Path],
    output: Optional[Path],
) -> ReportPaths:
    resolved_savedir, resolved_config, _ = export_packager._find_savedir(  # type: ignore[attr-defined]
        Path(savedir) if savedir else None,
        Path(config) if config else None,
    )

    if output:
        output_dir = Path(output).expanduser().resolve()
        if output_dir.exists() and output_dir.is_file():
            raise ReportGenerationError(f"Output path {output_dir} must be a directory")
        output_dir.mkdir(parents=True, exist_ok=True)
    else:
        output_dir = resolved_savedir / "report"
        output_dir.mkdir(parents=True, exist_ok=True)

    return ReportPaths(
        savedir=resolved_savedir,
        output_dir=output_dir,
        config_path=resolved_config,
    )


def generate_report(
    savedir: Optional[Path] = None,
    config: Optional[Path] = None,
    output: Optional[Path] = None,
) -> Path:
    """Create the static HTML report and return the generated index path."""

    paths = _resolve_paths(savedir=savedir, config=config, output=output)

    artefacts = catalog.scan_savedir(paths.savedir)
    context = summary.build_context(paths.savedir, artefacts, config_path=paths.config_path)

    previews = assets.prepare_plot_previews(artefacts.plot_files, paths.savedir, paths.output_dir)
    if previews:
        for plot in context.get("plots", []):
            original_path = plot["path"]
            if original_path in previews:
                plot["preview"] = previews[original_path].as_posix()

    index_html = templating.render_report(context)
    output_path = paths.output_dir / "index.html"
    output_path.write_text(index_html, encoding="utf-8")

    templating.install_static_assets(paths.output_dir)

    return output_path


def _find_free_port(preferred_port: int = 8000) -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        try:
            sock.bind(("", preferred_port))
            return preferred_port
        except OSError:
            sock.bind(("", 0))
            return sock.getsockname()[1]


def serve_directory(path: Path, port: int = 8000, open_browser: bool = True) -> None:
    """Host the generated report via a simple HTTP server."""

    directory = Path(path).resolve()
    if not directory.is_dir():
        raise ReportGenerationError(f"{directory} is not a directory; cannot serve")

    port = _find_free_port(port)
    handler = http.server.SimpleHTTPRequestHandler

    class _Handler(handler):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, directory=str(directory), **kwargs)

    with socketserver.ThreadingTCPServer(("", port), _Handler) as httpd:
        click.echo(f"Serving report at http://localhost:{port}/ (Ctrl+C to stop)")
        if open_browser:
            try:
                webbrowser.open_new_tab(f"http://localhost:{port}/")
            except Exception:
                pass
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            click.echo("Stopping server…")


__all__ = ["generate_report", "serve_directory", "ReportGenerationError"]
