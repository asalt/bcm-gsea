"""Core data structures used across the AGI engine context utilities.

Historically the context graph, packing utilities and summariser all kept
slightly different versions of the same bookkeeping logic.  The classes
below consolidate that shared behaviour so higher level modules can focus
on their specific tasks (graph relationships, logical packing or
summarisation).
"""

from __future__ import annotations

from dataclasses import dataclass, replace, field
from datetime import datetime, timezone
from typing import Any, Callable, Dict, Iterable, Iterator, List, Mapping, Optional, Sequence, Tuple
from types import MappingProxyType

__all__ = ["ContextItem", "ContextRegistry", "ContextView"]


def _freeze_mapping(value: Optional[Mapping[str, Any]]) -> Mapping[str, Any]:
    """Return an immutable mapping, normalising ``None`` to an empty dict."""

    if value is None:
        return MappingProxyType({})
    if isinstance(value, MappingProxyType):
        return value
    return MappingProxyType(dict(value))


def _normalise_tags(tags: Optional[Sequence[str]]) -> Tuple[str, ...]:
    if not tags:
        return tuple()
    return tuple(dict.fromkeys(tag.strip() for tag in tags if tag and tag.strip()))


def _normalise_parents(parent_ids: Optional[Sequence[str]]) -> Tuple[str, ...]:
    if not parent_ids:
        return tuple()
    return tuple(dict.fromkeys(pid for pid in parent_ids if pid))


def _clean_text(text: str) -> str:
    return " ".join(text.split())


@dataclass(frozen=True)
class ContextItem:
    """A single unit of contextual information."""

    id: str
    text: str
    score: float = 0.0
    metadata: Mapping[str, Any] = field(default_factory=lambda: MappingProxyType({}))
    tags: Tuple[str, ...] = field(default_factory=tuple)
    parent_ids: Tuple[str, ...] = field(default_factory=tuple)
    created_at: datetime = field(default_factory=lambda: datetime.now(timezone.utc))

    def clone(
        self,
        *,
        id: Optional[str] = None,
        text: Optional[str] = None,
        score: Optional[float] = None,
        metadata: Optional[Mapping[str, Any]] = None,
        tags: Optional[Sequence[str]] = None,
        parent_ids: Optional[Sequence[str]] = None,
        created_at: Optional[datetime] = None,
    ) -> "ContextItem":
        """Return a copy with any provided fields updated."""

        updates: Dict[str, Any] = {}
        if id is not None:
            updates["id"] = id
        if text is not None:
            updates["text"] = _clean_text(text)
        if score is not None:
            updates["score"] = score
        if metadata is not None:
            updates["metadata"] = _freeze_mapping(metadata)
        if tags is not None:
            updates["tags"] = _normalise_tags(tags)
        if parent_ids is not None:
            updates["parent_ids"] = _normalise_parents(parent_ids)
        if created_at is not None:
            updates["created_at"] = created_at
        return replace(self, **updates)

    def merged_metadata(self, extra: Mapping[str, Any]) -> "ContextItem":
        """Return a copy with ``extra`` merged into the metadata."""

        new_metadata: Dict[str, Any] = dict(self.metadata)
        new_metadata.update(extra)
        return self.clone(metadata=new_metadata)


@dataclass(frozen=True)
class ContextView:
    """A lightweight handle describing a subset of context items."""

    item_ids: Tuple[str, ...]
    label: Optional[str] = None
    metadata: Mapping[str, Any] = field(default_factory=lambda: MappingProxyType({}))

    def items(self, registry: "ContextRegistry") -> Iterator[ContextItem]:
        for item_id in self.item_ids:
            yield registry[item_id]


class ContextRegistry:
    """Central store for :class:`ContextItem` instances.

    The registry provides consistent lifecycle management (creation,
    updates, pruning and serialisation) which is reused by the graph,
    packing and summarisation helpers.  Callers can subclass it to bolt on
    additional behaviour without re-implementing the persistence layer.
    """

    def __init__(self, *, id_prefix: str = "ctx", items: Optional[Iterable[ContextItem]] = None) -> None:
        self._id_prefix = id_prefix
        self._next_index: int = 1
        self._items: Dict[str, ContextItem] = {}
        self._order: List[str] = []
        if items:
            for item in items:
                self._register_item(item)

    # ------------------------------------------------------------------
    # basic container protocol helpers
    def __contains__(self, item_id: object) -> bool:  # pragma: no cover - trivial
        return isinstance(item_id, str) and item_id in self._items

    def __len__(self) -> int:  # pragma: no cover - trivial
        return len(self._items)

    def __iter__(self) -> Iterator[ContextItem]:  # pragma: no cover - convenience
        return self.iter_items()

    def __getitem__(self, item_id: str) -> ContextItem:
        return self._items[item_id]

    # ------------------------------------------------------------------
    def _generate_id(self) -> str:
        generated = f"{self._id_prefix}-{self._next_index}"
        self._next_index += 1
        return generated

    def _adjust_index(self, item_id: str) -> None:
        prefix = f"{self._id_prefix}-"
        if item_id.startswith(prefix):
            suffix = item_id[len(prefix) :]
            if suffix.isdigit():
                self._next_index = max(self._next_index, int(suffix) + 1)

    def _register_item(self, item: ContextItem) -> None:
        self._items[item.id] = item
        self._order.append(item.id)
        self._adjust_index(item.id)

    # ------------------------------------------------------------------
    def add_item(
        self,
        text: str,
        *,
        score: float = 0.0,
        metadata: Optional[Mapping[str, Any]] = None,
        tags: Optional[Sequence[str]] = None,
        parent_ids: Optional[Sequence[str]] = None,
        item_id: Optional[str] = None,
        created_at: Optional[datetime] = None,
    ) -> ContextItem:
        """Create and store a new :class:`ContextItem`."""

        if item_id is None:
            item_id = self._generate_id()
        elif item_id in self._items:
            raise ValueError(f"Context item '{item_id}' already exists")

        item = ContextItem(
            id=item_id,
            text=_clean_text(text),
            score=score,
            metadata=_freeze_mapping(metadata),
            tags=_normalise_tags(tags),
            parent_ids=_normalise_parents(parent_ids),
            created_at=created_at or datetime.now(timezone.utc),
        )
        self._register_item(item)
        return item

    def update_item(
        self,
        item_id: str,
        *,
        text: Optional[str] = None,
        score: Optional[float] = None,
        metadata: Optional[Mapping[str, Any]] = None,
        tags: Optional[Sequence[str]] = None,
        parent_ids: Optional[Sequence[str]] = None,
    ) -> ContextItem:
        """Update an existing context item, returning the new version."""

        item = self._items[item_id]
        updates: Dict[str, Any] = {}
        if text is not None:
            updates["text"] = _clean_text(text)
        if score is not None:
            updates["score"] = score
        if metadata is not None:
            merged = dict(item.metadata)
            merged.update(metadata)
            updates["metadata"] = _freeze_mapping(merged)
        if tags is not None:
            updates["tags"] = _normalise_tags(tags)
        if parent_ids is not None:
            updates["parent_ids"] = _normalise_parents(parent_ids)
        updated = replace(item, **updates)
        self._items[item_id] = updated
        return updated

    def merge_metadata(self, item_id: str, extra: Mapping[str, Any]) -> ContextItem:
        updated = self._items[item_id].merged_metadata(extra)
        self._items[item_id] = updated
        return updated

    def iter_items(self, *, reverse: bool = False) -> Iterator[ContextItem]:
        order = reversed(self._order) if reverse else self._order
        for item_id in order:
            yield self._items[item_id]

    def get_recent(self, count: int) -> List[ContextItem]:
        return list(self.iter_items(reverse=True))[:count]

    def filter_items(self, predicate: Callable[[ContextItem], bool]) -> Iterator[ContextItem]:
        for item in self.iter_items():
            if predicate(item):
                yield item

    def remove_item(self, item_id: str) -> ContextItem:
        removed = self._items.pop(item_id)
        self._order.remove(item_id)
        return removed

    def prune(self, *, max_items: int) -> None:
        """Drop the oldest items until ``max_items`` remain."""

        if max_items < 0:
            raise ValueError("max_items must be non-negative")
        while len(self._order) > max_items:
            oldest_id = self._order.pop(0)
            self._items.pop(oldest_id, None)

    # ------------------------------------------------------------------
    def combine(self, other: "ContextRegistry") -> Dict[str, str]:
        """Merge another registry into this one.

        Returns a mapping of ``other``'s ids to the ids that ended up in the
        combined registry which allows subclasses to remap secondary
        structures (e.g. graph edges or pack membership).
        """

        id_map: Dict[str, str] = {}
        for item in other.iter_items():
            new_id = item.id
            if new_id in self._items:
                new_id = self._generate_id()
            self._register_item(item.clone(id=new_id))
            id_map[item.id] = new_id
        return id_map

    def to_snapshot(self) -> Dict[str, Any]:
        """Return a serialisable representation of the registry."""

        return {
            "items": [
                {
                    "id": item.id,
                    "text": item.text,
                    "score": item.score,
                    "metadata": dict(item.metadata),
                    "tags": list(item.tags),
                    "parent_ids": list(item.parent_ids),
                    "created_at": item.created_at.isoformat(),
                }
                for item in self.iter_items()
            ]
        }

    # ------------------------------------------------------------------
    def copy(self) -> "ContextRegistry":
        """Return a shallow copy preserving insertion order."""

        return ContextRegistry(id_prefix=self._id_prefix, items=list(self.iter_items()))


