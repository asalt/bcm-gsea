"""High-level entry point for building HTML reports from tackle2 outputs."""

from __future__ import annotations

import http.server
import os
import socket
import socketserver
import webbrowser
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List, Dict, Any, Iterable

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
        output_dir = resolved_savedir

    return ReportPaths(
        savedir=resolved_savedir,
        output_dir=output_dir,
        config_path=resolved_config,
    )


PLOT_TITLES = {
    "bar": "Bar Plots",
    "bubble": "Bubble Plots",
    "enrichplots": "Enrichment Plots",
    "heatmaps_gene": "Gene Heatmaps",
    "heatmaps_gsea": "GSEA Heatmaps",
    "pca": "PCA Plots",
    "umap_gene": "Gene UMAP Plots",
}

MAX_PREVIEWS_PER_GROUP = 12


def _slugify_token(token: Optional[str], fallback: str = "item") -> str:
    if not token:
        token = fallback
    token = token.strip().lower()
    token = re.sub(r"[^a-z0-9]+", "-", token)
    token = re.sub(r"-{2,}", "-", token).strip("-")
    return token or fallback


def _token_set(*values: str) -> set[str]:
    tokens: set[str] = set()
    for value in values:
        if not value:
            continue
        base = value.lower()
        tokens.add(base)
        tokens.add(_slugify_token(base))
        parts = [part for part in re.split(r"[^a-z0-9]+", base) if part]
        tokens.update(parts)
    return tokens


def _collect_plot_entries(
    plot_files: Iterable[Path],
    savedir: Path,
    previews: Dict[Path, Path],
) -> List[Dict[str, Any]]:
    entries: List[Dict[str, Any]] = []
    for path in plot_files:
        try:
            rel = path.relative_to(savedir)
        except ValueError:
            rel = path
        parts = rel.parts
        plot_type = parts[1] if len(parts) > 1 else "other"
        preview_path = previews.get(rel)
        entries.append(
            {
                "path": rel,
                "name": rel.name,
                "type": plot_type,
                "parts": tuple(part.lower() for part in parts),
                "path_str": rel.as_posix().lower(),
                "preview": preview_path.as_posix() if preview_path else None,
            }
        )
    return entries


def _collect_log_entries(log_files: Iterable[Path], savedir: Path) -> List[Dict[str, Any]]:
    entries: List[Dict[str, Any]] = []
    for path in log_files:
        try:
            rel = path.relative_to(savedir)
        except ValueError:
            rel = path
        entries.append(
            {
                "path": rel,
                "name": rel.name,
                "parts": tuple(part.lower() for part in rel.parts),
                "path_str": rel.as_posix().lower(),
            }
        )
    return entries


def _filter_collection_plots(collection_id: str, entries: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    target = collection_id.lower()
    results = [entry for entry in entries if entry["parts"] and entry["parts"][0] == target]
    return results


def _group_plots_by_type(entries: List[Dict[str, Any]], limit: int = MAX_PREVIEWS_PER_GROUP) -> List[Dict[str, Any]]:
    grouped: Dict[str, List[Dict[str, Any]]] = {}
    for entry in entries:
        grouped.setdefault(entry["type"], []).append(entry)

    groups: List[Dict[str, Any]] = []
    for plot_type, items in grouped.items():
        title = PLOT_TITLES.get(plot_type, plot_type.replace("_", " ").title())
        groups.append(
            {
                "type": plot_type,
                "title": title,
                "plots": [
                    {
                        "name": item["name"],
                        "path": item["path"].as_posix(),
                        "preview": item["preview"],
                    }
                    for item in items[:limit]
                ],
            }
        )

    groups.sort(key=lambda g: g["title"].lower())
    return groups


def _filter_entries_by_tokens(
    entries: List[Dict[str, Any]],
    collection_id: str,
    tokens: set[str],
) -> List[Dict[str, Any]]:
    collection_lower = collection_id.lower()
    results: List[Dict[str, Any]] = []
    for entry in entries:
        if not entry["parts"] or entry["parts"][0] != collection_lower:
            continue
        if tokens and not any(token in entry["path_str"] for token in tokens):
            continue
        results.append(entry)
    return results


def generate_report(
    savedir: Optional[Path] = None,
    config: Optional[Path] = None,
    output: Optional[Path] = None,
) -> Path:
    """Create the static HTML report and return the generated index path."""

    paths = _resolve_paths(savedir=savedir, config=config, output=output)

    artefacts = catalog.scan_savedir(paths.savedir)
    context = summary.build_context(paths.savedir, artefacts, config_path=paths.config_path)

    templating.install_static_assets(paths.output_dir)

    previews = assets.prepare_plot_previews(artefacts.plot_files, paths.savedir, paths.output_dir, limit=120)
    if previews:
        for plot in context.get("plots", []):
            original_path = plot["path"]
            if original_path in previews:
                plot["preview"] = previews[original_path].as_posix()

    plot_entries = _collect_plot_entries(artefacts.plot_files, paths.savedir, previews)
    log_entries = _collect_log_entries(artefacts.log_files, paths.savedir)

    collection_pages: List[Dict[str, Any]] = []
    collections_dir = paths.output_dir / "collections"

    for collection in context.get("collections", []):
        collection_slug = _slugify_token(collection.identifier, fallback="collection")
        collection_filename = Path("collections") / f"{collection_slug}.html"

        collection_plot_entries = _filter_collection_plots(collection.identifier, plot_entries)
        collection_plot_groups = _group_plots_by_type(collection_plot_entries)

        collection_log_entries = _filter_collection_plots(collection.identifier, log_entries)
        collection_logs = [
            {
                "name": entry["name"],
                "path": entry["path"].as_posix(),
            }
            for entry in collection_log_entries
        ]

        total_pathways = sum(c.total_pathways for c in collection.comparisons)
        significant_pathways = sum(c.significant_pathways for c in collection.comparisons)

        comparison_links: List[Dict[str, Any]] = []

        comparison_detail_map: Dict[str, str] = {}

        for comparison in collection.comparisons:
            comparison_slug = _slugify_token(comparison.identifier, fallback="comparison")
            comparison_filename = Path("collections") / collection_slug / f"{comparison_slug}.html"

            tokens = _token_set(comparison.identifier, comparison.display_name)
            comparison_plots = _filter_entries_by_tokens(plot_entries, collection.identifier, tokens)
            comparison_plot_groups = _group_plots_by_type(comparison_plots)

            comparison_logs = _filter_entries_by_tokens(log_entries, collection.identifier, tokens)
            comparison_logs_payload = [
                {
                    "name": entry["name"],
                    "path": entry["path"].as_posix(),
                }
                for entry in comparison_logs
            ]

            comparison_context = {
                "collection": collection,
                "comparison": comparison,
                "plot_groups": comparison_plot_groups,
                "logs": comparison_logs_payload,
                "savedir_name": context["savedir_name"],
                "savedir_path": context["savedir_path"],
                "generated_at": context["generated_at"],
                "savedir_relprefix": "../..",
                "preview_relprefix": "../..",
                "static_href": "../../static/report.css",
                "back_href": f"../{collection_slug}.html",
            }

            comparison_destination = paths.output_dir / comparison_filename
            comparison_destination.parent.mkdir(parents=True, exist_ok=True)
            comparison_html = templating.render_comparison(comparison_context)
            comparison_destination.write_text(comparison_html, encoding="utf-8")

            comparison_href = f"{collection_slug}/{comparison_slug}.html"

            comparison_links.append(
                {
                    "name": comparison.display_name,
                    "href": comparison_href,
                    "identifier": comparison.identifier,
                    "significant": comparison.significant_pathways,
                    "total": comparison.total_pathways,
                }
            )
            comparison_detail_map[comparison.identifier] = comparison_href

        detail_context = {
            "collection": collection,
            "plot_groups": collection_plot_groups,
            "logs": collection_logs,
            "comparison_pages": comparison_links,
            "comparison_detail_map": comparison_detail_map,
            "stats": {
                "total_comparisons": len(collection.comparisons),
                "total_pathways": total_pathways,
                "significant_pathways": significant_pathways,
            },
            "savedir_name": context["savedir_name"],
            "savedir_path": context["savedir_path"],
            "generated_at": context["generated_at"],
            "savedir_relprefix": "..",
            "preview_relprefix": "..",
            "static_href": "../static/report.css",
            "back_href": "../index.html",
        }

        detail_html = templating.render_collection(detail_context)
        destination = paths.output_dir / collection_filename
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_text(detail_html, encoding="utf-8")

        collection_pages.append(
            {
                "name": collection.display_name,
                "href": collection_filename.as_posix(),
            }
        )

    context["collection_pages"] = collection_pages
    context["savedir_relprefix"] = "."
    context["preview_relprefix"] = ""

    index_html = templating.render_report(context)
    output_path = paths.output_dir / "index.html"
    output_path.write_text(index_html, encoding="utf-8")

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
