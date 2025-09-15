"""Graph utilities for contextual information."""

from __future__ import annotations

from collections import deque, defaultdict
from typing import Dict, Iterable, Iterator, Optional, Sequence, Set, Tuple

from .context_base import ContextItem, ContextRegistry

__all__ = ["ContextGraph"]


class ContextGraph(ContextRegistry):
    """Context registry with parent/child relationships between items."""

    def __init__(
        self,
        *,
        id_prefix: str = "ctx",
        items: Optional[Iterable[ContextItem]] = None,
        edges: Optional[Iterable[Tuple[str, str]]] = None,
    ) -> None:
        super().__init__(id_prefix=id_prefix, items=items)
        self._children: Dict[str, Set[str]] = defaultdict(set)
        self._parents: Dict[str, Set[str]] = defaultdict(set)
        if edges:
            for parent_id, child_id in edges:
                if parent_id not in self or child_id not in self:
                    raise KeyError(f"Edge references unknown item: {parent_id!r}->{child_id!r}")
                self._children[parent_id].add(child_id)
                self._parents[child_id].add(parent_id)

    # ------------------------------------------------------------------
    def add_edge(self, parent_id: str, child_id: str) -> None:
        """Link two existing context items."""

        if parent_id not in self:
            raise KeyError(f"Unknown parent '{parent_id}'")
        if child_id not in self:
            raise KeyError(f"Unknown child '{child_id}'")
        if child_id in self._children[parent_id]:
            return
        self._children[parent_id].add(child_id)
        self._parents[child_id].add(parent_id)
        child = self[child_id]
        parent_ids = tuple(dict.fromkeys((*child.parent_ids, parent_id)))
        super().update_item(child_id, parent_ids=parent_ids)

    def remove_edge(self, parent_id: str, child_id: str) -> None:
        if child_id in self._children.get(parent_id, set()):
            self._children[parent_id].remove(child_id)
            if not self._children[parent_id]:
                self._children.pop(parent_id, None)
            parents = set(self._parents.get(child_id, set()))
            parents.discard(parent_id)
            if parents:
                self._parents[child_id] = parents
            else:
                self._parents.pop(child_id, None)
            super().update_item(child_id, parent_ids=tuple(sorted(parents)))

    def add_item(
        self,
        text: str,
        *,
        parent_ids: Optional[Sequence[str]] = None,
        **kwargs,
    ) -> ContextItem:
        item = super().add_item(text, parent_ids=parent_ids, **kwargs)
        for parent_id in item.parent_ids:
            self.add_edge(parent_id, item.id)
        return item

    def remove_item(self, item_id: str) -> ContextItem:
        for parent_id in list(self._parents.get(item_id, set())):
            self.remove_edge(parent_id, item_id)
        for child_id in list(self._children.get(item_id, set())):
            self.remove_edge(item_id, child_id)
        return super().remove_item(item_id)

    # ------------------------------------------------------------------
    def parents(self, item_id: str) -> Tuple[str, ...]:
        return tuple(sorted(self._parents.get(item_id, set())))

    def children(self, item_id: str) -> Tuple[str, ...]:
        return tuple(sorted(self._children.get(item_id, set())))

    def ancestors(self, item_id: str) -> Tuple[str, ...]:
        seen: Set[str] = set()
        queue: deque[str] = deque(self._parents.get(item_id, set()))
        while queue:
            current = queue.popleft()
            if current in seen:
                continue
            seen.add(current)
            queue.extend(self._parents.get(current, set()))
        return tuple(sorted(seen))

    def descendants(self, item_id: str) -> Tuple[str, ...]:
        seen: Set[str] = set()
        queue: deque[str] = deque(self._children.get(item_id, set()))
        while queue:
            current = queue.popleft()
            if current in seen:
                continue
            seen.add(current)
            queue.extend(self._children.get(current, set()))
        return tuple(sorted(seen))

    def walk(
        self,
        start_id: str,
        *,
        direction: str = "forward",
        depth: Optional[int] = None,
        include_start: bool = True,
    ) -> Iterator[ContextItem]:
        """Yield items reachable from ``start_id`` using breadth-first search."""

        if start_id not in self:
            raise KeyError(f"Unknown item '{start_id}'")
        if direction not in {"forward", "backward", "both"}:
            raise ValueError("direction must be 'forward', 'backward' or 'both'")

        visited: Set[str] = set()
        queue: deque[Tuple[str, int]] = deque([(start_id, 0)])
        while queue:
            current, current_depth = queue.popleft()
            if current in visited:
                continue
            visited.add(current)
            if current != start_id or include_start:
                yield self[current]
            if depth is not None and current_depth >= depth:
                continue
            next_depth = current_depth + 1
            if direction in {"forward", "both"}:
                for child in self._children.get(current, set()):
                    queue.append((child, next_depth))
            if direction in {"backward", "both"}:
                for parent in self._parents.get(current, set()):
                    queue.append((parent, next_depth))

    def subgraph(self, root_id: str, *, depth: Optional[int] = None) -> "ContextGraph":
        """Return a new :class:`ContextGraph` limited to ``root_id``'s neighbourhood."""

        items = {root_id}
        edges: Set[Tuple[str, str]] = set()
        for node in self.walk(root_id, depth=depth):
            items.add(node.id)
        for parent_id in list(items):
            for child_id in self._children.get(parent_id, set()):
                if child_id in items:
                    edges.add((parent_id, child_id))
        return ContextGraph(
            id_prefix=self._id_prefix,
            items=[self[item_id] for item_id in items],
            edges=edges,
        )

    # ------------------------------------------------------------------
    def combine(self, other: ContextRegistry) -> None:
        """Extend :meth:`ContextRegistry.combine` to include edges."""

        if not isinstance(other, ContextGraph):
            super().combine(other)
            return
        id_map = super().combine(other)
        for parent_id, children in other._children.items():
            new_parent = id_map.get(parent_id, parent_id)
            for child_id in children:
                new_child = id_map.get(child_id, child_id)
                if new_parent in self and new_child in self:
                    self.add_edge(new_parent, new_child)

