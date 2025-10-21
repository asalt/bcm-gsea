"""LLM-backed summarisation helpers for tackle2 reports."""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from typing import Any, Dict, Iterable, Mapping, Optional, Protocol, Sequence

import ollama
from ollama import Client, RequestError, ResponseError

from .summary import CollectionSummary, ComparisonSummary, GeneratedSummary, TopPathway

logger = logging.getLogger(__name__)


class SummarisationError(RuntimeError):
    """Raised when the configured LLM fails in an unexpected way."""


class ReportSummarizer(Protocol):
    """Interface implemented by helpers capable of generating report prose."""

    def summarise_run(self, collections: Sequence[CollectionSummary]) -> Optional[GeneratedSummary]:
        ...

    def summarise_collections(self, collections: Sequence[CollectionSummary]) -> Dict[str, GeneratedSummary]:
        ...

    def summarise_comparisons(self, collection: CollectionSummary) -> Dict[str, GeneratedSummary]:
        ...


@dataclass(frozen=True)
class OllamaConfig:
    """Configuration for :class:`OllamaSummarizer`."""

    model: str = "llama3.2:3b"
    host: Optional[str] = None
    temperature: float = 0.2
    max_tokens: int = 512
    run_collection_limit: int = 6
    comparisons_per_collection: int = 3
    pathways_per_comparison: int = 3
    enable_collection_summaries: bool = True
    collection_summary_limit: int = 3
    enable_comparison_summaries: bool = False
    comparison_summary_limit: int = 0


class OllamaSummarizer:
    """Generate narrative summaries using a locally hosted Ollama model."""

    _run_system_prompt = (
        "You are assisting with summarising gene set enrichment analysis (GSEA) results. "
        "Use only the provided data to produce concise, factual summaries. "
        "Highlight comparisons with the most significant pathways (padj < 0.05)."
    )

    _collection_system_prompt = (
        "You are writing short summaries for a single gene set enrichment analysis collection. "
        "Summaries must stay grounded in the provided JSON data and emphasise key pathways."
    )

    _comparison_system_prompt = (
        "You are summarising findings for a single gene set enrichment comparison. "
        "Base your statements strictly on the supplied JSON data."
    )

    def __init__(self, config: OllamaConfig | None = None) -> None:
        self._config = config or OllamaConfig()
        self._client: Any
        if self._config.host:
            self._client = Client(host=self._config.host)
        else:
            self._client = ollama

    # ------------------------------------------------------------------
    @property
    def config(self) -> OllamaConfig:
        return self._config

    # ------------------------------------------------------------------
    def summarise_run(self, collections: Sequence[CollectionSummary]) -> Optional[GeneratedSummary]:
        selected = self._select_collections(collections, limit=self._config.run_collection_limit)
        if not selected:
            return None
        payload = {
            "collections": [self._collection_payload(collection) for collection in selected],
            "total_collections": len(collections),
        }
        prompt = self._render_prompt(
            "Create a 2-3 sentence overview and 3-5 bullet highlights describing the overall GSEA findings. "
            "Return ONLY a JSON object with keys 'summary' and 'bullets'.",
            payload,
        )
        data = self._invoke_llm(prompt, system=self._run_system_prompt)
        return self._parse_summary(data)

    # ------------------------------------------------------------------
    def summarise_collections(self, collections: Sequence[CollectionSummary]) -> Dict[str, GeneratedSummary]:
        if not self._config.enable_collection_summaries:
            return {}

        summaries: Dict[str, GeneratedSummary] = {}
        selected = self._select_collections(collections, limit=self._config.collection_summary_limit)
        for collection in selected:
            payload = self._collection_payload(collection)
            prompt = self._render_prompt(
                "Summarise this collection in 2 short sentences and provide up to 3 bullet highlights. "
                "Return ONLY a JSON object with keys 'summary' and 'bullets'.",
                payload,
            )
            data = self._invoke_llm(prompt, system=self._collection_system_prompt)
            summary = self._parse_summary(data)
            if summary:
                summaries[collection.identifier] = summary
        return summaries

    # ------------------------------------------------------------------
    def summarise_comparisons(self, collection: CollectionSummary) -> Dict[str, GeneratedSummary]:
        if not self._config.enable_comparison_summaries:
            return {}

        limit = self._config.comparison_summary_limit
        if limit > 0:
            comparisons = collection.comparisons[:limit]
        else:
            comparisons = collection.comparisons

        summaries: Dict[str, GeneratedSummary] = {}
        for comparison in comparisons:
            payload = self._comparison_payload(collection, comparison)
            prompt = self._render_prompt(
                "Write a single-sentence summary and up to 3 bullet highlights for this comparison. "
                "Return ONLY a JSON object with keys 'summary' and 'bullets'.",
                payload,
            )
            data = self._invoke_llm(prompt, system=self._comparison_system_prompt)
            summary = self._parse_summary(data)
            if summary:
                summaries[comparison.identifier] = summary
        return summaries

    # ------------------------------------------------------------------
    def _select_collections(
        self,
        collections: Sequence[CollectionSummary],
        *,
        limit: int,
    ) -> Sequence[CollectionSummary]:
        if limit <= 0:
            return collections
        ordered = sorted(collections, key=self._collection_priority, reverse=True)
        return ordered[:limit]

    @staticmethod
    def _collection_priority(collection: CollectionSummary) -> tuple[int, int]:
        significant = sum(comp.significant_pathways for comp in collection.comparisons)
        total = sum(comp.total_pathways for comp in collection.comparisons)
        return significant, total

    def _render_prompt(self, instruction: str, payload: Mapping[str, object]) -> str:
        data = json.dumps(payload, indent=2, ensure_ascii=False)
        return f"{instruction}\n\nDATA:\n{data}"

    def _invoke_llm(self, prompt: str, *, system: str) -> Optional[Mapping[str, object]]:
        options: Dict[str, Any] = {}
        if self._config.temperature is not None:
            options["temperature"] = self._config.temperature
        if self._config.max_tokens > 0:
            options["num_predict"] = self._config.max_tokens
        try:
            response = self._client.generate(  # type: ignore[attr-defined]
                model=self._config.model,
                prompt=prompt,
                system=system,
                format="json",
                options=options or None,
            )
        except (RequestError, ResponseError, ConnectionError, TimeoutError, OSError) as exc:
            logger.warning("Failed to obtain summary from model %s: %s", self._config.model, exc)
            return None

        text = getattr(response, "response", None)
        if text is None and isinstance(response, Mapping):
            text = response.get("response")
        if not text:
            logger.info("Model %s returned an empty summary response.", self._config.model)
            return None

        try:
            return json.loads(text)
        except json.JSONDecodeError:
            logger.warning(
                "Model %s returned invalid JSON payload: %s",
                self._config.model,
                text,
            )
            return None

    def _parse_summary(self, payload: Optional[Mapping[str, object]]) -> Optional[GeneratedSummary]:
        if not payload:
            return None

        summary_text = str(payload.get("summary", "")).strip()
        bullets_raw = payload.get("bullets", [])

        bullets: list[str] = []
        if isinstance(bullets_raw, str):
            bullets = [line.strip(" •-\t") for line in bullets_raw.splitlines() if line.strip()]
        elif isinstance(bullets_raw, Iterable):
            for entry in bullets_raw:
                text = str(entry).strip()
                if text:
                    bullets.append(text.lstrip("•- ").strip())

        if not summary_text and bullets:
            summary_text = bullets[0]

        if not summary_text:
            return None

        return GeneratedSummary(text=summary_text, bullets=tuple(bullets), model=self._config.model)

    def _collection_payload(self, collection: CollectionSummary) -> Dict[str, object]:
        comparisons = self._comparison_entries(collection.comparisons)
        return {
            "collection": collection.display_name,
            "identifier": collection.identifier,
            "comparisons": comparisons,
            "comparisons_total": len(collection.comparisons),
            "significant_pathways": sum(c["significant_pathways"] for c in comparisons),
        }

    def _comparison_entries(self, comparisons: Sequence[ComparisonSummary]) -> list[Dict[str, object]]:
        limit = self._config.comparisons_per_collection
        if limit > 0:
            candidates = list(comparisons)[:limit]
        else:
            candidates = list(comparisons)

        entries: list[Dict[str, object]] = []
        for comparison in candidates:
            entries.append(
                {
                    "comparison": comparison.display_name,
                    "identifier": comparison.identifier,
                    "significant_pathways": comparison.significant_pathways,
                    "total_pathways": comparison.total_pathways,
                    "top_pathways": self._pathway_entries(comparison.top_pathways),
                }
            )
        return entries

    def _pathway_entries(self, pathways: Sequence[TopPathway]) -> list[Dict[str, object]]:
        limit = self._config.pathways_per_comparison
        if limit > 0:
            candidates = list(pathways)[:limit]
        else:
            candidates = list(pathways)

        entries: list[Dict[str, object]] = []
        for pathway in candidates:
            entries.append(
                {
                    "name": pathway.pathway,
                    "nes": pathway.nes,
                    "padj": pathway.padj,
                    "leading_genes": pathway.leading_genes,
                }
            )
        return entries

    def _comparison_payload(self, collection: CollectionSummary, comparison: ComparisonSummary) -> Dict[str, object]:
        return {
            "collection": {
                "name": collection.display_name,
                "identifier": collection.identifier,
            },
            "comparison": {
                "name": comparison.display_name,
                "identifier": comparison.identifier,
                "significant_pathways": comparison.significant_pathways,
                "total_pathways": comparison.total_pathways,
                "top_pathways": self._pathway_entries(comparison.top_pathways),
            },
        }


__all__ = [
    "OllamaConfig",
    "OllamaSummarizer",
    "ReportSummarizer",
    "SummarisationError",
]
