import os
import shutil
import subprocess
from pathlib import Path

import pytest
from click.testing import CliRunner

from python.cli import main
from python.report.generator import generate_report
from python.report import assets


ROOT = Path(__file__).resolve().parents[2]
GENERATE_SCRIPT = ROOT / "scripts" / "generate_demo_savedir.R"


def _require_rscript() -> str:
    rscript = shutil.which("Rscript")
    if not rscript:
        pytest.skip("Rscript not available; skipping report integration test")
    if not GENERATE_SCRIPT.exists():
        pytest.skip("Demo savedir script missing")
    return rscript


def _build_savedir(tmp_path: Path) -> Path:
    rscript = _require_rscript()
    destination = tmp_path / "demo"
    result = subprocess.run(
        [rscript, str(GENERATE_SCRIPT), str(destination)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        fixture_savedir = ROOT / "R" / "tests" / "integration_tests" / "test_output"
        if fixture_savedir.exists():
            fallback = destination / "fixture_savedir"
            shutil.copytree(fixture_savedir, fallback)
            return fallback
        pytest.skip(f"Failed to generate demo savedir: {result.stderr}")

    candidates = [line.strip() for line in result.stdout.splitlines() if line.strip()]
    if not candidates:
        raise AssertionError("generate_demo_savedir produced no output path")

    savedir = Path(candidates[-1])
    if not savedir.exists():
        raise AssertionError(f"Reported savedir does not exist: {savedir}")

    return savedir


@pytest.mark.integration
def test_generate_report_end_to_end(tmp_path):
    savedir = _build_savedir(tmp_path)
    output_dir = tmp_path / "report"
    index_path = generate_report(savedir=savedir, output=output_dir)

    assert index_path.exists()
    css_path = index_path.parent / "static" / "report.css"
    assert css_path.exists()

    html = index_path.read_text(encoding="utf-8")
    assert "tackle2 Summary" in html
    assert "HALLMARK" in html


@pytest.mark.integration
def test_cli_report_command(tmp_path):
    savedir = _build_savedir(tmp_path)
    runner = CliRunner()
    result = runner.invoke(
        main,
        ["report", "--savedir", str(savedir), "--output", str(tmp_path / "web")],
        catch_exceptions=False,
    )
    assert result.exit_code == 0, result.output
    assert "Report written to" in result.output
    index_path = tmp_path / "web" / "index.html"
    assert index_path.exists()


def test_prepare_plot_previews_handles_errors(tmp_path):
    dummy_pdf = tmp_path / "plot.pdf"
    previews = assets.prepare_plot_previews([dummy_pdf], tmp_path, tmp_path)
    assert previews == {}


def test_prepare_plot_previews_prioritises_pdfs(tmp_path, monkeypatch):
    pdf_path = tmp_path / "plot.pdf"
    png_path = tmp_path / "plot.png"
    pdf_path.touch()
    png_path.touch()

    calls = []

    def fake_run(cmd, check, capture_output):
        from pathlib import Path
        calls.append(cmd)
        prefix = Path(cmd[-1])
        prefix.with_suffix(".png").write_bytes(b"")
        import subprocess

        return subprocess.CompletedProcess(cmd, 0)

    monkeypatch.setattr(assets.subprocess, "run", fake_run)
    monkeypatch.setattr(assets.shutil, "which", lambda name: "/usr/bin/pdftoppm")

    previews = assets.prepare_plot_previews(
        [png_path, pdf_path],
        savedir=tmp_path,
        output_dir=tmp_path,
        limit=1,
    )

    assert len(previews) == 1
    key = next(iter(previews))
    assert key == pdf_path.relative_to(tmp_path)
    assert (tmp_path / previews[key]).exists()
    assert len(calls) == 1
