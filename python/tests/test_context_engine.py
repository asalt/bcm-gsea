import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

from agi.engine import (
    ContextGraph,
    ContextPack,
    ContextRegistry,
    ContextView,
    RollingSummary,
    SummaryEngine,
)


def test_registry_add_update_and_prune():
    registry = ContextRegistry()
    first = registry.add_item(" first message  ", score=0.1, metadata={"source": "alpha"}, tags=["tag", "tag"])
    second = registry.add_item("second message", score=0.3)

    updated_first = registry.update_item(first.id, score=0.5, metadata={"priority": "high"}, tags=["tag", "new"])
    assert updated_first.score == 0.5
    assert updated_first.metadata["source"] == "alpha"
    assert updated_first.metadata["priority"] == "high"
    assert updated_first.tags == ("tag", "new")

    registry.prune(max_items=1)
    assert len(registry) == 1
    assert second.id in registry
    assert first.id not in registry


def test_registry_combine_returns_mapping():
    base = ContextRegistry()
    existing = base.add_item("existing", item_id="ctx-1")

    other = ContextRegistry()
    duplicate = other.add_item("duplicate", item_id=existing.id)

    mapping = base.combine(other)
    assert len(base) == 2
    assert duplicate.id in mapping
    assert mapping[duplicate.id] != duplicate.id


def test_context_graph_relationships():
    graph = ContextGraph()
    root = graph.add_item("root event")
    child = graph.add_item("child", parent_ids=[root.id])
    grandchild = graph.add_item("grandchild", parent_ids=[child.id])

    assert graph.parents(child.id) == (root.id,)
    assert graph.children(root.id) == (child.id,)
    assert set(graph.descendants(root.id)) == {child.id, grandchild.id}

    walked = [item.id for item in graph.walk(root.id)]
    assert walked == [root.id, child.id, grandchild.id]

    sub = graph.subgraph(root.id)
    assert isinstance(sub, ContextGraph)
    assert set(item.id for item in sub.iter_items()) == {root.id, child.id, grandchild.id}


def test_context_pack_grouping_and_combine():
    pack_registry = ContextPack()
    first = pack_registry.add_item("alpha entry", score=0.1)
    second = pack_registry.add_item("bravo entry", score=0.9)
    pack = pack_registry.create_pack([first.id, second.id], label="session", metadata={"kind": "demo"})

    pack_registry.extend_pack(pack.id, [first.id])
    assert pack_registry.get_pack(pack.id).item_ids == (first.id, second.id)

    other = ContextPack()
    other_item = other.add_item("charlie", item_id=first.id)
    other_pack = other.create_pack([other_item.id], pack_id="pack-1")

    pack_registry.combine(other)
    combined_packs = list(pack_registry.iter_packs())
    assert len(combined_packs) == 2
    combined_ids = {p.id for p in combined_packs}
    assert pack.id in combined_ids
    assert any(pack_registry[p.item_ids[0]].text == "charlie" for p in combined_packs if other_pack.id != p.id)


def test_summary_engine_behaviour():
    registry = ContextRegistry()
    low = registry.add_item("low priority", score=0.1)
    high = registry.add_item("critical update", score=0.9)

    engine = SummaryEngine(registry)
    summary = engine.summarize_items(max_items=1)
    assert "critical update" in summary

    view = ContextView(item_ids=(low.id, high.id), label="both")
    view_summary = engine.summarize_view(view, max_items=2)
    assert "low priority" in view_summary

    pack_registry = ContextPack(items=registry.get_recent(2))
    pack = pack_registry.create_pack([low.id, high.id])
    pack_summary = engine.summarize_pack(pack)
    assert "critical update" in pack_summary

    graph = ContextGraph(items=registry.get_recent(2))
    graph.add_edge(low.id, high.id)
    graph_summary = engine.summarize_graph(graph, low.id)
    assert "low priority" in graph_summary


def test_rolling_summary_tracks_recent_items():
    registry = ContextRegistry()
    first = registry.add_item("Message A")
    second = registry.add_item("Message B")
    third = registry.add_item("Message C")

    engine = SummaryEngine(registry)
    rolling = RollingSummary(engine, history_size=2, max_items=2, max_chars=120)

    rolling.push(first.id)
    assert "Message A" in rolling.summary

    rolling.push(second.id)
    assert "Message B" in rolling.summary

    rolling.push(third.id)
    assert "Message C" in rolling.summary
    assert "Message A" not in rolling.summary

    rolling.extend([first.id, second.id])
    assert "Message B" in rolling.summary
    assert "Message C" not in rolling.summary
