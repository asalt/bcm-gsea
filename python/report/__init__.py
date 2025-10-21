"""HTML report generation utilities for tackle2 results directories."""

from .generator import ReportGenerationError, generate_report  # noqa: F401
from .llm import OllamaConfig, OllamaSummarizer  # noqa: F401

__all__ = ["generate_report", "ReportGenerationError", "OllamaSummarizer", "OllamaConfig"]
