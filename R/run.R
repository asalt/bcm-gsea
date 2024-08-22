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
suppressPackageStartupMessages(library(rlang))
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
  # ===========================


  savedir <- params$savedir
  if (is.null(savedir)) {
    savedir <- file.path("./plots")
  }

  cachedir <- params$advanced$cachedir
  if (!is.null(cachedir)) {
    if (cachedir == "savedir") {
      cachedir <- file.path(savedir, "cache")
    } else {
      cachedir <- NULL # leave it empty
    }
  }

  rankfiledir <- params$rankfiledir %||% file.path(savedir, "ranks")
  if (!is.null(rankfiledir)) {
    if (rankfiledir == "savedir") {
      rankfiledir <- file.path(savedir, "ranks")
    }
  }
  params$rankfiledir <- rankfiledir

  save_func <- util_tools$make_partial(
    plot_utils$plot_and_save,
    path = savedir,
    replace = T
  )

  if (!is.null(params$rankname_order)) {
    if (length(params$rankname_order) == 1 && params$rankname_order == "sample_order") {
      params$rankname_order <- params$sample_order
    }
  }

  if (!is.null(params$sample_order)) {
    if (length(params$sample_order) == 1 && params$sample_order == "rankname_order") {
      params$sample_order <- params$rankname_order
    }
  }

  logfile <- params$logfile %>% ifelse(!is.null(.), ., "run.log")
  options("bcm_gsea_log_msg_filename" = logfile)
  log_msg <- util_tools$make_partial(util_tools$log_msg, filename = logfile)
  # log_message <- make_partial(util_tools$log_message)
  log_msg(msg = paste0("===\n*starting bcm gsea*\n==="))

  # =======
  species <- params$species

  # == genesets

  if (is.null(params$genesets)) {
    genesets_array <- list(list(category = "H", subcategory = "", collapse = FALSE))
  } else {
    genesets_array <- params$genesets
  }
  .msg <- paste0("genesets:\n", paste(sapply(genesets_array, function(gs) paste(gs$category, gs$subcategory, sep = ": ")), collapse = ", "))
  log_msg(msg = .msg)


  genesets_of_interest <- geneset_tools$geneset_array_to_df(genesets_array)
  list_of_geneset_dfs <- geneset_tools$get_collections(genesets_of_interest, species = species)
  genesets_list_of_lists <- list_of_geneset_dfs %>% purrr::map(geneset_tools$genesets_df_to_list)

  # =======

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
  # =======

  # == run fgsea

  parallel <- params$advanced$parallel %>% ifelse(!is.null(.), ., FALSE)
  if (parallel) {
    workers <- future::availableCores() - 1
    future::plan(future::multicore, workers = workers)
  }
  cache <- params$advanced$cache %>% ifelse(!is.null(.), ., TRUE)
  log_msg(msg = paste0("parallel set to: ", parallel))
  log_msg(msg = paste0("cache set to: ", cache))
  log_msg(msg = paste0("cachedir set to: ", cachedir))

  # dump.frames(to.file = TRUE)
  # # Quit R with error status
  # q(status = 1)


  # this part is challenging to pass everything in the right format
  results_list <- fgsea_tools$run_all_pathways(genesets_list_of_lists,
    ranks_list,
    parallel = parallel,
    genesets_additional_info = genesets_of_interest,
    cache = TRUE,
    cache_dir = cachedir,
    logger = log_msg
  )

  log_msg(msg = "names gsea results list: ")
  log_msg(msg = str_c(names(results_list), sep = "\n"))

  log_msg(msg = "comparison  names gsea results list: ")
  log_msg(msg = str_c(names(results_list[[1]]), sep = "\n"))



  # =======
  # save
  ._ <- results_list %>%
    io_tools$save_gsea_results(savedir = file.path(savedir, "gsea_tables"))


  # =======  load gct

  if (!is.null(gct_path) && file.exists(gct_path)) {
    log_msg(msg = paste0("reading gct file: ", gct_path))
    gct <- cmapR::parse_gctx(gct_path)
  } else {
    gct <- NULL
  }


  # =======  barplots
  do_individual_barplots <- params$do_individual_barplots %>% ifelse(!is.null(.), ., TRUE)
  if (do_individual_barplots) {
    log_msg(msg = "plotting individual barplots")
    plts <- results_list %>% plot_tools$all_barplots_with_numbers(
      sample_order = params$rankname_order %||% params$sample_order,
      save_func = save_func
    )
  }



  log_msg(msg = "plotting faceted barplots")
  do_combined_barplots <- params$do_combined_barplots %>% ifelse(!is.null(.), ., TRUE)
  if (do_combined_barplots) {
    plts <- results_list %>%
      plot_tools$do_combined_barplots(
        sample_order = params$rankname_order %||% params$sample_order,
        save_func = save_func
      )
  }


  # =======
  log_msg(msg = "combining gsea marices")
  all_gsea_results <- fgsea_tools$concat_results_all_collections(results_list)


  log_msg(msg = "drawing all heatmaps. ")
  # here is where changes to the save func are made (e.g. width, height, file format)

  if (exists("gct") && !is.null(gct) && (params$ranks_from == "gct")) {
    metadata <- gct@cdesc
  } else {
    metadata <- NULL
  }

  hts <- all_gsea_results %>% plot_tools$plot_results_all_collections(
    # limit=20,
    metadata = metadata,
    pstat_cutoff = 1,
    limit = 40,
    cut_by = params$cut_by,
    save_func = save_func,
    sample_order = params$rankname_order
  )

  # =============

  log_msg(msg = paste0("maybe plot edges"))
  # gct@cdesc %<>% mutate(group = factor(group, levels = c("Pre_V", "Pre_A", "Control", "PCL", "PCL_Bi", "PCL_Bi_MSCs"), ordered = T))

  if (is.null(params$edgeplot_limit)) {
    edgeplot_limit <- 10
  } else {
    edgeplot_limit <- params$edgeplot_limit
  }

  if (!is.null(gct)) {
    ht_edge_plots <- plot_tools$plot_heatmap_of_edges(gct, results_list,
      save_func = save_func,
      limit = edgeplot_limit,
      sample_order = params$sample_order
      #  cut_by = params$cut_by,
    )
  }
  # ===== plot es

  if (is.null(params$es_limit)) {
    es_limit <- 10
  } else {
    es_limit <- params$edgeplot_limit
  }

  ._ <- plot_tools$plot_top_ES_across(all_gsea_results,
    ranks_list = ranks_list,
    genesets_list_of_lists,
    limit = es_limit,
    save_func = save_func
  )



  if (exists("gct") && !is.null(gct) && (params$ranks_from == "gct")) {
    metadata <- gct@cdesc
  } else {
    metadata <- NULL
  }

  # this is the pca chunk
  # =============

  do_pca <- params$pca %>% ifelse(!is.null(.), ., TRUE)
  if (do_pca) {
    pca_objects <- all_gsea_results %>% pca_tools$do_all(
      metadata = metadata
    )

    log_msg(msg = paste0("plot pca all biplots"))
    pca_objects %>% pca_tools$plot_all_biplots(
      save_func = save_func,
      top_pc = 3,
      showLoadings = T,
      labSize = 2,
      pointSize = 3,
      sizeLoadingsNames = 2,
      fig_width = 11,
      fig_height = 9,
      # colby = NULL,
    )
  }
}
