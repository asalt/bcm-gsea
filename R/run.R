suppressPackageStartupMessages(library(purrr)) # for map()
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(fs)) # for dir_ls()
suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(reactable))
suppressPackageStartupMessages(library(ggalt))
suppressPackageStartupMessages(library(here))
# suppressPackageStartupMessages(library(rmdformats))



run <- function(params) {
  # == local
  # putting all this here to reduce load time when not needed (e.g. testing, checking help)
  io_tools <- new.env()
  source(file.path(here("R"), "io.R"), local = io_tools)

  util_tools <- new.env()
  source(file.path(here("R"), "utils.R"), local = util_tools)

  geneset_tools <- new.env()
  source(file.path(here("R"), "geneset_utils.R"), local = geneset_tools)

  fgsea_tools <- new.env()
  source(file.path(here("R"), "fgsea.R"), local = fgsea_tools)

  plot_tools <- new.env()
  source(file.path(here("R"), "plot.R"), local = plot_tools)

  plot_utils <- new.env()
  source(file.path(here("R"), "plot_utils.R"), local = plot_utils)

  pca_tools <- new.env()
  source(file.path(here("R"), "pca.R"), local = pca_tools)
  # ==


  savedir <- params$savedir
  if (is.null(savedir)) {
    savedir <- file.path("./plots")
  }

  save_func <- util_tools$make_partial(
    plot_utils$plot_and_save,
    path = savedir
  )

  log_msg <- util_tools$make_partial(util_tools$log_msg)
  # log_message <- make_partial(util_tools$log_message)
  log_msg(msg = paste0("===\n*starting bcm gsea*\n==="))

  # =======
  species <- params$species
  genesets_array <- params$genesets

  genesets_of_interest <- geneset_tools$geneset_array_to_df(genesets_array)

  rankfiledir <- params$rankfiledir
  volcanodir <- params$volcanodir
  gct_path <- params$gct_path
  ranks_from <- params$ranks_from

  log_msg(paste0("rankfiledir: ", rankfiledir))
  log_msg(paste0("volcanodir: ", volcanodir))
  log_msg(paste0("gct_path: ", gct_path))
  log_msg(paste0("ranks from: ", ranks_from))

  # ==
  ranks_list <- io_tools$load_and_process_ranks(params)
  # more here..
}
