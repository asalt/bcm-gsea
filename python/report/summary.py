"""Build the data context consumed by the HTML report template."""

from __future__ import annotations

import datetime as dt
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd

from .catalog import SavedirArtefacts


@dataclass
class GeneratedSummary:
    """Container for LLM-authored narrative snippets."""

    text: str
    bullets: Tuple[str, ...] = field(default_factory=tuple)
    model: Optional[str] = None


@dataclass
class TopPathway:
    pathway: str
    nes: Optional[float]
    padj: Optional[float]
    leading_genes: str


@dataclass
class ComparisonSummary:
    identifier: str
    display_name: str
    table_path: Path
    relative_path: Path
    total_pathways: int
    significant_pathways: int
    top_pathways: List[TopPathway] = field(default_factory=list)
    llm_summary: Optional[GeneratedSummary] = None


@dataclass
class CollectionSummary:
    identifier: str
    display_name: str
    comparisons: List[ComparisonSummary] = field(default_factory=list)
    llm_summary: Optional[GeneratedSummary] = None


def _split_table_name(table_path: Path) -> tuple[str, str]:
    stem = table_path.stem
    if "__" in stem:
        collection, comparison = stem.split("__", 1)
    elif "_" in stem:
        collection, comparison = stem.split("_", 1)
    else:
        collection, comparison = stem, "comparison"
    return collection, comparison


def _format_display(token: str) -> str:
    return token.replace("_", " ").strip() or token


def _summarise_table(table_path: Path, savedir: Path) -> ComparisonSummary:
    df = pd.read_csv(table_path, sep="\t")
    total = len(df.index)
    padj_series = df.get("padj")

    significant = 0
    if padj_series is not None:
        significant = int(pd.to_numeric(padj_series, errors="coerce").lt(0.05).sum())

    padj_sorted = pd.to_numeric(df.get("padj"), errors="coerce")
    nes_sorted = pd.to_numeric(df.get("NES"), errors="coerce")
    df = df.assign(_padj_sorted=padj_sorted, _nes_sorted=nes_sorted)

    top_rows = df.sort_values(by=["_padj_sorted", "_nes_sorted"], ascending=[True, False], na_position="last").head(5)

    pathways: List[TopPathway] = []
    for _, row in top_rows.iterrows():
        leading = row.get("leadingEdge_genesymbol")
        if pd.isna(leading) or not isinstance(leading, str):
            leading = row.get("leadingEdge")
        if isinstance(leading, str):
            genes = leading.split("/")
            leading = ", ".join(genes[:6])
            if len(genes) > 6:
                leading += " …"
        else:
            leading = ""

        nes_value = row.get("NES")
        padj_value = row.get("padj")

        if pd.isna(nes_value):
            nes_value = None
        else:
            nes_value = float(nes_value)

        if pd.isna(padj_value):
            padj_value = None
        else:
            padj_value = float(padj_value)

        pathways.append(
            TopPathway(
                pathway=str(row.get("pathway", "")),
                nes=nes_value,
                padj=padj_value,
                leading_genes=leading,
            )
        )

    collection_token, comparison_token = _split_table_name(table_path)

    return ComparisonSummary(
        identifier=comparison_token,
        display_name=_format_display(comparison_token),
        table_path=table_path,
        relative_path=table_path.relative_to(savedir),
        total_pathways=total,
        significant_pathways=significant,
        top_pathways=pathways,
    )


def build_context(
    savedir: Path,
    artefacts: SavedirArtefacts,
    config_path: Optional[Path] = None,
) -> Dict:
    """Assemble a JSON-serialisable context for the report template."""

    savedir = Path(savedir)
    collections: Dict[str, CollectionSummary] = {}

    for table_path in artefacts.gsea_tables:
        comparison_summary = _summarise_table(table_path, artefacts.savedir)
        collection_token, _ = _split_table_name(table_path)
        collection_summary = collections.setdefault(
            collection_token,
            CollectionSummary(
                identifier=collection_token,
                display_name=_format_display(collection_token),
            ),
        )
        collection_summary.comparisons.append(comparison_summary)

    for collection in collections.values():
        collection.comparisons.sort(key=lambda cs: cs.display_name.lower())

    global_significant = sum(c.significant_pathways for coll in collections.values() for c in coll.comparisons)
    total_pathways = sum(c.total_pathways for coll in collections.values() for c in coll.comparisons)
    total_comparisons = sum(len(coll.comparisons) for coll in collections.values())

    plots = [
        {
            "path": path.relative_to(savedir),
            "name": path.relative_to(savedir).as_posix(),
        }
        for path in artefacts.plot_files
    ][:24]

    logs = [
        {
            "path": path.relative_to(savedir),
            "name": path.name,
        }
        for path in artefacts.log_files
    ]

    now_utc = dt.datetime.now(dt.timezone.utc)

    context = {
        "generated_at": now_utc.strftime("%Y-%m-%d %H:%M UTC"),
        "savedir_name": savedir.name,
        "savedir_path": str(savedir),
        "stats": {
            "collections": len(collections),
            "comparisons": total_comparisons,
            "tables": len(artefacts.gsea_tables),
            "pathways": total_pathways,
            "significant_pathways": global_significant,
        },
        "collections": list(collections.values()),
        "plots": plots,
        "logs": logs,
        "has_tables": len(artefacts.gsea_tables) > 0,
        "config_path": str(config_path) if config_path else None,
        "llm_summary": None,
    }

    return context


__all__ = ["build_context", "CollectionSummary", "ComparisonSummary", "TopPathway", "GeneratedSummary"]
