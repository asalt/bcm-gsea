"""Summarisation helpers built on the unified context data model."""

from __future__ import annotations

from collections import deque
from textwrap import shorten
from typing import Callable, Iterable, List, Optional, Sequence

from .context_base import ContextItem, ContextRegistry, ContextView
from .context_pack import ContextPack, Pack
from .context_graph import ContextGraph

__all__ = ["SummaryEngine", "RollingSummary"]


class SummaryEngine:
    """Derive textual summaries from :class:`ContextRegistry` data."""

    def __init__(
        self,
        registry: ContextRegistry,
        *,
        reducer: Optional[Callable[[Sequence[str], int], str]] = None,
    ) -> None:
        self._registry = registry
        self._reducer = reducer or self._default_reducer

    # ------------------------------------------------------------------
    @property
    def registry(self) -> ContextRegistry:  # pragma: no cover - trivial accessor
        return self._registry

    # ------------------------------------------------------------------
    @staticmethod
    def _normalise_text(text: str) -> str:
        return " ".join(text.split())

    @staticmethod
    def _default_reducer(texts: Sequence[str], max_chars: int) -> str:
        cleaned = [SummaryEngine._normalise_text(text) for text in texts if text and text.strip()]
        if not cleaned:
            return ""
        if max_chars <= 0:
            max_chars = 1
        combined = " ".join(cleaned)
        return shorten(combined, width=max_chars, placeholder="…")

    def _ordered_items(
        self,
        item_ids: Optional[Sequence[str]],
        *,
        prefer_score: bool,
    ) -> List[ContextItem]:
        if item_ids is None:
            items = list(self._registry.iter_items(reverse=True))
        else:
            items = [self._registry[item_id] for item_id in item_ids if item_id in self._registry]
        key = (
            (lambda item: (item.score, item.created_at))
            if prefer_score
            else (lambda item: item.created_at)
        )
        return sorted(items, key=key, reverse=True)

    def summarize_items(
        self,
        item_ids: Optional[Sequence[str]] = None,
        *,
        max_items: int = 5,
        prefer_score: bool = True,
        max_chars: int = 480,
    ) -> str:
        ordered = self._ordered_items(item_ids, prefer_score=prefer_score)
        texts = [item.text for item in ordered[:max_items]]
        return self._reducer(texts, max_chars)

    def summarize_view(
        self,
        view: ContextView,
        *,
        max_items: Optional[int] = None,
        prefer_score: bool = True,
        max_chars: int = 480,
    ) -> str:
        item_ids = view.item_ids
        if max_items is None:
            max_items = len(item_ids)
        return self.summarize_items(item_ids, max_items=max_items, prefer_score=prefer_score, max_chars=max_chars)

    def summarize_pack(
        self,
        pack: Pack,
        *,
        max_items: Optional[int] = None,
        prefer_score: bool = True,
        max_chars: int = 480,
    ) -> str:
        max_items = max_items or len(pack.item_ids)
        return self.summarize_items(pack.item_ids, max_items=max_items, prefer_score=prefer_score, max_chars=max_chars)

    def summarize_graph(
        self,
        graph: ContextGraph,
        root_id: str,
        *,
        depth: Optional[int] = None,
        direction: str = "forward",
        include_root: bool = True,
        max_items: int = 8,
        prefer_score: bool = True,
        max_chars: int = 480,
    ) -> str:
        item_ids = [item.id for item in graph.walk(root_id, depth=depth, direction=direction, include_start=include_root)]
        return self.summarize_items(item_ids, max_items=max_items, prefer_score=prefer_score, max_chars=max_chars)

    # Convenience wrappers -------------------------------------------------
    def summarize_recent(self, *, max_items: int = 5, max_chars: int = 480) -> str:
        return self.summarize_items(None, max_items=max_items, max_chars=max_chars)

    def summarize_pack_id(
        self,
        pack_registry: ContextPack,
        pack_id: str,
        *,
        max_items: Optional[int] = None,
        prefer_score: bool = True,
        max_chars: int = 480,
    ) -> str:
        pack = pack_registry.get_pack(pack_id)
        return self.summarize_pack(pack, max_items=max_items, prefer_score=prefer_score, max_chars=max_chars)


class RollingSummary:
    """Maintain a rolling summary over a moving window of context items."""

    def __init__(
        self,
        engine: SummaryEngine,
        *,
        history_size: int = 20,
        max_items: int = 5,
        max_chars: int = 480,
    ) -> None:
        self._engine = engine
        self._history: deque[str] = deque(maxlen=history_size)
        self._max_items = max_items
        self._max_chars = max_chars
        self._current: str = ""

    @property
    def summary(self) -> str:  # pragma: no cover - trivial accessor
        return self._current

    def push(self, item_id: str) -> str:
        if item_id not in self._engine.registry:
            raise KeyError(f"Unknown context item '{item_id}'")
        self._history.append(item_id)
        return self._recompute()

    def extend(self, item_ids: Iterable[str]) -> str:
        for item_id in item_ids:
            self.push(item_id)
        return self._current

    def _recompute(self) -> str:
        relevant = list(self._history)[-self._max_items :]
        self._current = self._engine.summarize_items(
            relevant,
            max_items=self._max_items,
            max_chars=self._max_chars,
        )
        return self._current

