import sys
import os
import re
import glob
import shutil
import subprocess
import functools
import logging

import pathlib
import click
from collections import defaultdict

# from concurrent.futures import ProcessPoolExecutor, as_completed
# from tqdm import tqdm
import pandas as pd

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


@main.command()
@click.option("-n", "--name", default="bcm-gsea.toml")
def get_config(name):
    config_file = (
        pathlib.Path(__file__).absolute().parent.parent / "config" / "base.toml"
    )  #  . exists()
    if not config_file.exists():
        raise FileNotFoundError(f"{config_file} does not exist")
    if not name.endswith(".toml"):
        name = name + ".toml"
    new_file = pathlib.Path(".").absolute() / name
    if new_file.exists():
        print(f"File {new_file} already exists, not overwriting")
        return
    print(f"Writing {new_file} ")
    shutil.copy(config_file, new_file)

    print(f"done")


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
