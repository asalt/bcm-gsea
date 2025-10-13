from pathlib import Path

from click.testing import CliRunner

from python.cli import main


def test_get_config_creates_default_files():
    runner = CliRunner()
    with runner.isolated_filesystem():
        result = runner.invoke(main, ["get-config"])
        assert result.exit_code == 0
        assert "Writing" in result.output
        assert "done" in result.output
        assert (Path("bcm-gsea.toml")).exists()


def test_get_config_with_colormap():
    runner = CliRunner()
    with runner.isolated_filesystem():
        result = runner.invoke(
            main,
            [
                "get-config",
                "--include-colormap",
                "--name",
                "custom.toml",
                "--colormap-name",
                "custom-map.json",
            ],
        )
        assert result.exit_code == 0
        assert "custom.toml" in result.output
        assert "custom-map.json" in result.output
        assert Path("custom.toml").exists()
        assert Path("custom-map.json").exists()
