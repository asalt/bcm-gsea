import json
from pathlib import Path

import tomllib

from python import config_schema


def test_default_config_matches_base_toml(tmp_path):
    generated = config_schema.build_default_config()
    base = tomllib.loads(Path("config/base.toml").read_text())
    assert generated["params"]["genesets"] == base["params"]["genesets"]
    generated["params"]["genesets"] = []
    base["params"]["genesets"] = []
    assert generated == base


def test_describe_section_root_and_nested():
    root = config_schema.describe_section()
    assert root.path == "params"
    ranks_field = next(field for field in root.fields if field.name == "ranks_from")
    assert "volcano" in (ranks_field.choices or [])

    bubble = config_schema.describe_section("params.bubbleplot")
    bubble_fields = {field.name for field in bubble.fields}
    assert {"limit", "do_individual", "do_combined", "glyph"}.issubset(bubble_fields)
    assert any(sub.path == "params.bubbleplot.advanced" for sub in bubble.subsections)

    advanced = config_schema.describe_section("params.bubbleplot.advanced")
    assert any(field.name == "stroke_width" for field in advanced.fields)
