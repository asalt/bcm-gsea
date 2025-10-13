import sys
import os
import re
import glob
import shutil
import subprocess
import functools
import logging

import pathlib
from pathlib import Path
import click
from collections import defaultdict

APP_NAME = "tackle2"

# from concurrent.futures import ProcessPoolExecutor, as_completed
# from tqdm import tqdm
import pandas as pd

from . import export_packager
from . import config_schema

# from concurrent.futures import ThreadPoolExecutor, as_completed

# import pyfaidx
# from pyfaidx import Fasta

# import janitor

# from . import log
# from . import io
# from . import io_external
# from . import modisite
# from .utils import data_generator
# from .constants import VALID_MODI_COLS, get_all_columns
# from .runner import run_pipeline
# from . import mapper
# from . import reduce
logger = logging.getLogger(name=__file__)


# CLI_R = os.path.abspath((os.path.join(os.path.dirname(__file__), "cli.R")))
CLI_R = pathlib.Path(__file__).parent.parent / "R" / "cli.R"
if not CLI_R.exists():
    raise FileNotFoundError(f"{CLI_R} does not exist")


@click.group(chain=True)
def main():
    pass


def _resolve_example(path_components: tuple[str, ...]) -> pathlib.Path:
    base = pathlib.Path(__file__).resolve().parent.parent
    candidate = base.joinpath(*path_components)
    if candidate.exists():
        return candidate
    raise FileNotFoundError(f"{candidate} does not exist")


def _copy_example(source: pathlib.Path, destination: pathlib.Path) -> bool:
    if destination.exists():
        click.echo(f"File {destination} already exists, not overwriting")
        return False
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy(source, destination)
    click.echo(f"Writing {destination}")
    return True


@main.command()
@click.option("-n", "--name", default=f"{APP_NAME}.toml", help="Output filename for the example TOML config.")
@click.option(
    "--include-colormap/--skip-colormap",
    default=False,
    show_default=True,
    help="Copy the example colormap JSON alongside the TOML config.",
)
@click.option(
    "--colormap-name",
    default="colormap.example.json",
    help="Output filename for the optional colormap example.",
)
def get_config(name, include_colormap, colormap_name):
    """Copy example configuration files into the current directory."""

    config_file = _resolve_example(("config", "base.toml"))
    if not name.endswith(".toml"):
        name = name + ".toml"
    config_dest = pathlib.Path.cwd() / name

    copied_any = _copy_example(config_file, config_dest)

    if include_colormap:
        colormap_source = _resolve_example(("config", "colormap.example.json"))
        colormap_dest = pathlib.Path.cwd() / colormap_name
        copied = _copy_example(colormap_source, colormap_dest)
        copied_any = copied_any or copied

    if copied_any:
        click.echo("done")


@main.command()
@click.option(
    "-c",
    "--config",
    type=click.Path(exists=True, dir_okay=False),
    help=".toml file with additional parameters for report",
)
@click.option(
    "-i",
    "--interactive",
    type=bool,
    is_flag=True,
    default=True,
    show_default=True,
    help="run in interactive session within python rpy2. button has no effect. always on",
)
@click.option("-v", "--verbose", type=bool, is_flag=True, help="verbose output")
@click.option(
    "-l",
    "--log_level",
    type=click.Choice(["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]),
    help="log level",
)
def run(config, interactive, verbose, log_level):


    pwd = pathlib.Path('.').absolute()
    logging.info(f"{pwd}")
    config = pathlib.Path(config).absolute()
    if not config.exists():
        raise ValueError(f"Config file {str(config)} does not exist")
    import rpy2.robjects as robjects


    robjects.r(
        f"""
        message("sourcing run file")
        setwd('{str(CLI_R.parent)}')
        """
    )

    robjects.r(
        f"""
        source('{str(CLI_R)}')
        source('utils.R')  # for clean_args
        """
    )
    robjects.r.assign("pwd", str(pwd))
    logger.info(f"Assignning pwd to {pwd} in R environ")
    logger.info(f"trying to load {str(config)} with RcppTOML")
    try:
        robjects.r(
            f"""
            message("trying to load {str(config)}")
            params <- RcppTOML::parseTOML("{str(config)}")
            message("cleaning params")
            cleaned_params <- clean_args(params$params, root_dir = pwd)
            message("running")
            run(cleaned_params)
            """
        )
    except Exception as e:
        print(robjects.r("rlang::last_trace()"))



@main.command()
@click.option(
    "-p",
    "--port",
    type=int,
    default=8765,
    help="port to run the server on",
    show_default=True,
)
def launch_assistant(port):
    from .persistent_llm_server import start_server, run_server

    run_server(port=port)


@main.command()
@click.option(
    "-s",
    "--savedir",
    type=click.Path(exists=True, file_okay=False),
    help=f"Directory containing {APP_NAME} pipeline outputs",
)
@click.option(
    "-c",
    "--config",
    type=click.Path(exists=True, dir_okay=False),
    help="TOML configuration file used for the run",
)
@click.option(
    "-o",
    "--output",
    type=click.Path(dir_okay=False),
    help="Target ZIP file path for the packaged export",
)
@click.option("--include-cache/--exclude-cache", default=False, show_default=True)
@click.option("--include-ranks/--exclude-ranks", default=False, show_default=True)
@click.option("--include-hashes/--skip-hashes", default=True, show_default=True)
@click.option(
    "--label",
    type=str,
    help="Custom root directory name inside the archive",
)
@click.option(
    "--split-components/--single-archive",
    default=False,
    show_default=True,
    help="Package each top-level result folder into its own zip inside an output directory",
)
def package(
    savedir,
    config,
    output,
    include_cache,
    include_ranks,
    include_hashes,
    label,
    split_components,
):
    """Package analysis outputs into a distributable archive."""

    if not savedir and not config:
        raise click.UsageError("Provide either --savedir or --config")

    try:
        archive_path, manifest = export_packager.package_results(
            savedir=Path(savedir) if savedir else None,
            config_path=Path(config) if config else None,
            output_path=Path(output) if output else None,
            include_cache=include_cache,
            include_ranks=include_ranks,
            include_hashes=include_hashes,
            export_label=label,
            split_components=split_components,
        )
    except export_packager.PackagingError as exc:
        raise click.ClickException(str(exc))

    descriptor = "package directory" if split_components else "archive"
    click.echo(f"Created {descriptor}: {archive_path}")
    click.echo(f"Files packaged: {manifest['file_count']}")
    click.echo(f"Total size: {manifest['total_size_bytes']:,} bytes")


def _serialize_default(value):
    if isinstance(value, (dict, list)):
        import json

        return json.dumps(value, indent=2, sort_keys=True)
    return value


@main.command()
@click.argument("section", required=False)
@click.option("--json", "as_json", is_flag=True, help="Emit JSON instead of text tables")
def describe(section, as_json):
    """Describe configuration sections and defaults."""

    try:
        descriptor = config_schema.describe_section(section)
    except KeyError as exc:
        raise click.ClickException(str(exc)) from exc

    if as_json:
        import json

        click.echo(json.dumps(config_schema.section_to_dict(descriptor), indent=2, sort_keys=True))
        return

    header = descriptor.path
    click.echo(f"[{header}]")
    click.echo(descriptor.description or "")
    click.echo("")

    if descriptor.fields:
        key_width = max(len(field.name) for field in descriptor.fields)
        type_width = max(len(field.type) for field in descriptor.fields)
        for field_info in descriptor.fields:
            default_value = _serialize_default(field_info.default)
            click.echo(
                f"{field_info.name.ljust(key_width)}  {field_info.type.ljust(type_width)}  default={default_value}"
            )
            if field_info.description:
                click.echo(f"  {field_info.description}")
            if field_info.choices:
                click.echo(f"  choices: {', '.join(map(str, field_info.choices))}")
            click.echo("")
    else:
        click.echo("(no options)")
        click.echo("")

    if descriptor.subsections:
        click.echo("Subsections:")
        for subsection in descriptor.subsections:
            suffix = " (array)" if subsection.is_array else ""
            click.echo(f"  - {subsection.path}{suffix}: {subsection.description}")
