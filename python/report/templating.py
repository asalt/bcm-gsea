"""Rendering helpers for tackle2 HTML reports."""

from __future__ import annotations

import shutil
from functools import lru_cache
from importlib import resources
from pathlib import Path

from jinja2 import Environment, FileSystemLoader, select_autoescape


TEMPLATE_NAME = "index.html.j2"
TEMPLATE_PACKAGE = __package__


@lru_cache(maxsize=1)
def _environment() -> Environment:
    template_dir = resources.files(TEMPLATE_PACKAGE) / "templates"
    return Environment(
        loader=FileSystemLoader(str(template_dir)),
        autoescape=select_autoescape(["html", "xml"]),
        trim_blocks=True,
        lstrip_blocks=True,
    )


def render_report(context: dict) -> str:
    env = _environment()
    template = env.get_template(TEMPLATE_NAME)
    return template.render(**context)


def install_static_assets(destination: Path) -> None:
    base = resources.files(TEMPLATE_PACKAGE)
    static_dir = base / "static"
    target = Path(destination) / "static"
    if target.exists():
        shutil.rmtree(target)
    shutil.copytree(static_dir, target)


__all__ = ["render_report", "install_static_assets"]
