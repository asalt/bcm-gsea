import zipfile
from pathlib import Path

import pytest

from python import export_packager


def _write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def test_package_results_excludes_cache_and_ranks(tmp_path):
    savedir = tmp_path / "results"
    _write(savedir / "run.log", "log line\n")
    _write(savedir / "gsea_tables" / "H__group_A.tsv", "col1\tcol2\nval1\tval2\n")
    _write(savedir / "cache" / "should_skip.txt", "cached\n")
    _write(savedir / "ranks" / "rankfile.rnk", "rank\n")

    config_dir = tmp_path / "cfg"
    config_dir.mkdir()
    config_path = config_dir / "run.toml"
    config_path.write_text("""[params]\nsavedir = "../results"\n""")

    archive_path, manifest = export_packager.package_results(
        config_path=config_path,
        output_path=tmp_path / "export.zip",
        include_cache=False,
        include_ranks=False,
        include_hashes=False,
    )

    assert archive_path.exists()
    assert manifest["file_count"] == 2
    assert manifest["package_mode"] == "single_archive"
    assert manifest["config_copy"].endswith(f"config/{config_path.name}")

    export_root = manifest["export_root"]
    with zipfile.ZipFile(archive_path) as zf:
        entries = set(zf.namelist())
        assert f"{export_root}/results/run.log" in entries
        assert f"{export_root}/results/gsea_tables/H__group_A.tsv" in entries
        assert f"{export_root}/config/{config_path.name}" in entries
        assert all("cache" not in entry for entry in entries)
        assert all("ranks" not in entry for entry in entries)


def test_package_requires_source(tmp_path):
    with pytest.raises(export_packager.PackagingError):
        export_packager.package_results()


def test_output_path_without_suffix_is_augmented(tmp_path):
    savedir = tmp_path / "analysis"
    _write(savedir / "gsea_tables" / "res.tsv", "a\tb\n1\t2\n")

    archive_path, manifest = export_packager.package_results(
        savedir=savedir,
        output_path=tmp_path / "project1",
        include_hashes=False,
    )

    assert archive_path.suffix == ".zip"
    assert archive_path.name.startswith("project1_")
    assert f"{manifest['file_count']}files" in archive_path.stem


def test_default_output_includes_savedir_context(tmp_path):
    savedir = tmp_path / "projectA" / "results"
    _write(savedir / "run.log", "log\n")

    archive_path, manifest = export_packager.package_results(
        savedir=savedir,
        include_hashes=False,
    )

    assert archive_path.suffix == ".zip"
    stem = archive_path.stem
    assert "results" in stem
    assert f"{manifest['file_count']}files" in stem


def test_split_components_creates_component_archives(tmp_path):
    savedir = tmp_path / "proj" / "results"
    _write(savedir / "gsea_tables" / "collection" / "res.tsv", "a\tb\n1\t2\n")
    _write(savedir / "plots" / "plot1.png", "binary")
    _write(savedir / "run.log", "root file\n")

    package_dir, manifest = export_packager.package_results(
        savedir=savedir,
        split_components=True,
        include_hashes=False,
    )

    assert package_dir.is_dir()
    component_zips = sorted(p.name for p in package_dir.glob("*.zip"))
    assert "gsea_tables.zip" in component_zips
    assert "plots.zip" in component_zips
    assert "root_files.zip" in component_zips

    plots_zip = package_dir / "plots.zip"
    with zipfile.ZipFile(plots_zip) as zf:
        assert "plots/plot1.png" in zf.namelist()

    assert manifest["package_mode"] == "split_components"
    assert any(c["component"] == "plots" for c in manifest["component_archives"])
