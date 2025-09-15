"""Unifies context management utilities for the AGI engine.

This module exposes a cohesive API that was previously spread across
``context_graph``, ``context_pack`` and ``summarizer``.  The
implementation now revolves around :class:`~agi.engine.context_base.ContextRegistry`
which provides the shared data model for all high-level helpers.
"""

from .context_base import ContextItem, ContextRegistry, ContextView
from .context_graph import ContextGraph
from .context_pack import ContextPack, Pack
from .summarizer import SummaryEngine, RollingSummary

__all__ = [
    "ContextItem",
    "ContextRegistry",
    "ContextView",
    "ContextGraph",
    "ContextPack",
    "Pack",
    "SummaryEngine",
    "RollingSummary",
]
