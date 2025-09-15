"""Context packing utilities built on the shared :mod:`agi.engine` core."""

from __future__ import annotations

from dataclasses import dataclass, replace, field
from typing import Dict, Iterable, Iterator, Mapping, Optional, Sequence, Tuple
from types import MappingProxyType

from .context_base import ContextItem, ContextRegistry

__all__ = ["ContextPack", "Pack"]


def _freeze_metadata(metadata: Optional[Mapping[str, object]]) -> Mapping[str, object]:
    if metadata is None:
        return MappingProxyType({})
    if isinstance(metadata, MappingProxyType):
        return metadata
    return MappingProxyType(dict(metadata))


def _normalise_ids(item_ids: Sequence[str]) -> Tuple[str, ...]:
    return tuple(dict.fromkeys(item_ids))


@dataclass(frozen=True)
class Pack:
    """Metadata describing a logical grouping of context items."""

    id: str
    item_ids: Tuple[str, ...]
    label: Optional[str] = None
    metadata: Mapping[str, object] = field(default_factory=lambda: MappingProxyType({}))
    summary: Optional[str] = None

    def update(
        self,
        *,
        item_ids: Optional[Sequence[str]] = None,
        label: Optional[str] = None,
        metadata: Optional[Mapping[str, object]] = None,
        summary: Optional[str] = None,
        id: Optional[str] = None,
    ) -> "Pack":
        updates: Dict[str, object] = {}
        if id is not None:
            updates["id"] = id
        if item_ids is not None:
            updates["item_ids"] = _normalise_ids(item_ids)
        if label is not None:
            updates["label"] = label
        if metadata is not None:
            merged = dict(self.metadata)
            merged.update(metadata)
            updates["metadata"] = _freeze_metadata(merged)
        if summary is not None:
            updates["summary"] = summary
        return replace(self, **updates)


class ContextPack(ContextRegistry):
    """Context registry supporting logical pack groupings."""

    def __init__(
        self,
        *,
        id_prefix: str = "ctx",
        pack_prefix: str = "pack",
        items: Optional[Iterable[ContextItem]] = None,
        packs: Optional[Iterable[Pack]] = None,
    ) -> None:
        super().__init__(id_prefix=id_prefix, items=items)
        self._pack_prefix = pack_prefix
        self._packs: Dict[str, Pack] = {}
        self._next_pack_index = 1
        if packs:
            for pack in packs:
                self._register_pack(pack)

    def _register_pack(self, pack: Pack) -> None:
        self._packs[pack.id] = pack
        prefix = f"{self._pack_prefix}-"
        if pack.id.startswith(prefix):
            suffix = pack.id[len(prefix) :]
            if suffix.isdigit():
                self._next_pack_index = max(self._next_pack_index, int(suffix) + 1)

    def _generate_pack_id(self) -> str:
        generated = f"{self._pack_prefix}-{self._next_pack_index}"
        self._next_pack_index += 1
        return generated

    # ------------------------------------------------------------------
    def create_pack(
        self,
        item_ids: Sequence[str],
        *,
        label: Optional[str] = None,
        metadata: Optional[Mapping[str, object]] = None,
        summary: Optional[str] = None,
        pack_id: Optional[str] = None,
    ) -> Pack:
        if pack_id is None:
            pack_id = self._generate_pack_id()
        elif pack_id in self._packs:
            raise ValueError(f"Pack '{pack_id}' already exists")
        normalised_ids = _normalise_ids(item_ids)
        for item_id in normalised_ids:
            if item_id not in self:
                raise KeyError(f"Unknown context item '{item_id}'")
        pack = Pack(
            id=pack_id,
            item_ids=normalised_ids,
            label=label,
            metadata=_freeze_metadata(metadata),
            summary=summary,
        )
        self._register_pack(pack)
        return pack

    def remove_pack(self, pack_id: str) -> Pack:
        return self._packs.pop(pack_id)

    def packs_for_item(self, item_id: str) -> Tuple[Pack, ...]:
        return tuple(pack for pack in self._packs.values() if item_id in pack.item_ids)

    def get_pack(self, pack_id: str) -> Pack:
        return self._packs[pack_id]

    def iter_packs(self) -> Iterator[Pack]:
        return iter(self._packs.values())

    def extend_pack(self, pack_id: str, new_items: Sequence[str]) -> Pack:
        pack = self.get_pack(pack_id)
        updated_ids = _normalise_ids((*pack.item_ids, *new_items))
        for item_id in updated_ids:
            if item_id not in self:
                raise KeyError(f"Unknown context item '{item_id}'")
        updated = pack.update(item_ids=updated_ids)
        self._packs[pack_id] = updated
        return updated

    def attach_summary(self, pack_id: str, summary: str) -> Pack:
        updated = self.get_pack(pack_id).update(summary=summary)
        self._packs[pack_id] = updated
        return updated

    def combine(self, other: ContextRegistry) -> None:
        id_map = super().combine(other)
        if not isinstance(other, ContextPack):
            return
        for pack in other.iter_packs():
            new_id = pack.id
            if new_id in self._packs:
                new_id = self._generate_pack_id()
            remapped_ids = [id_map.get(item_id, item_id) for item_id in pack.item_ids]
            self._register_pack(pack.update(id=new_id, item_ids=remapped_ids))

