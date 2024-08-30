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

  voice_tools <- new.env()
  source(file.path(here("R"), "voice.R"), local = voice_tools)
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
    replace = params$advanced$replace %||% TRUE
  )

  #
  if (!is.null(params$extra$rankname_order)) {
    if (length(params$extra$rankname_order) == 1 && params$extra$rankname_order == "sample_order") {
      params$extra$rankname_order <- params$extra$sample_order
    }
  } else {
    params$extra$rankname_order <- params$extra$sample_order
  }

  if (!is.null(params$extra$sample_order)) {
    if (length(params$extra$sample_order) == 1 && params$extra$sample_order == "rankname_order") {
      params$extra$sample_order <- params$extra$rankname_order
    }
  } else {
    params$extra$sample_order <- params$extra$rankname_order
  }

  logfile <- params$logfile %>% ifelse(!is.null(.), ., "run.log")
  options("bcm_gsea_log_msg_filename" = logfile)
  log_msg <- util_tools$make_partial(util_tools$log_msg, filename = logfile)
  # log_message <- make_partial(util_tools$log_message)
  log_msg(msg = paste0("===\n*starting bcm gsea*\n==="))
  if (is.null(params$quiet) || params$quiet == FALSE) {
    voice_tools$speak_text("starting bcm g s e a")
  }

  # =======
  species <- params$species %||% "Homo sapiens"

  # == genesets

  if (is.null(params$genesets)) {
    genesets_array <- list(list(category = "H", subcategory = "", collapse = FALSE))
  } else {
    genesets_array <- params$genesets
  }
  .msg <- paste0("genesets:\n", paste(sapply(genesets_array, function(gs) paste(gs$category, gs$subcategory, sep = ": ")), collapse = ", "))
  log_msg(msg = .msg[[1]])


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


  # =======  load gct

  if (!is.null(gct_path) && file.exists(gct_path)) {
    log_msg(msg = paste0("reading gct file: ", gct_path))
    gct <- cmapR::parse_gctx(gct_path)
  } else {
    gct <- NULL
  }

  # this part is challenging to pass everything in the right format

  genesets_for_iteration <- names(genesets_list_of_lists)

  # we then iterate over the genesets one by one
  # so all results for one get generated before moving to the next

  genesets_for_iteration %>% purrr::walk(
    ~{
    .msg <- paste0("running gsea for: ", .x)
    # if (is.null(params$quiet) || params$quiet == FALSE) {
    #   voice_tools$speak_text(text = paste0('running g s e a for ', .x))
    # }


    log_msg(msg = .msg)
    voice_tools$speak_text(.x)

    genesets_list_of_lists <- genesets_list_of_lists[.x]
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

    # ======= save
    ._ <- results_list %>%
      io_tools$save_gsea_results(savedir = file.path(savedir, "gsea_tables"))

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

    if (exists("gct") && !is.null(gct) && (params$ranks_from == "gct")) {
      metadata <- gct@cdesc
    } else {
      metadata <- NULL
    }

    # ======= gsea level heatmap
    if (params$heatmap_gsea$do %||% TRUE){
      hts <- all_gsea_results %>% plot_tools$plot_results_all_collections(
        # limit=20,
        metadata = metadata,
        pstat_cutoff = .25,
        limit = params$heatmap_gsea$limit %||% 20,
        cut_by = params$heatmap_gsea$cut_by %||% params$cut_by %||% NULL,
        save_func = save_func,
        sample_order = params$extra$rankname_order
      )
    }

    # =============

    log_msg(msg = paste0("maybe plot edges"))

    if (!is.null(gct) && params$heatmap_gene$do %||% TRUE) {
      ht_edge_plots <- plot_tools$plot_heatmap_of_edges(
        gct,
        results_list,
        save_func = save_func,
        limit = params$heatmap_gene$limit %||% 10,
        sample_order = params$extra$sample_order,
        cut_by = params$heatmap_gene$cut_by %||% params$cut_by %||% NULL,
        pstat_cutoff = params$heatmap_gene$pstat_cutoff %||% 1
      )
    }

    # ===== plot es
    # this part is messy
    # first is metadata organization to determine if and how to aggregate curves
    combine_by <- params$enplot$combine_by %||% NULL
    combine_by_df <- NULL
    if (!is.null(combine_by) && !is.null(metadata)) {
      if (!combine_by %in% colnames(metadata)) {
        combine_by_df <- NULL
      } else {
        combine_by_df <- metadata %>%
          select(!!sym(combine_by), id) %>%
          rename(
            facet = !!sym(combine_by),
            rankname = id
          )
        if (!is.null(params$extra$facet_order)) {
          combine_by_df <- combine_by_df %>%
            mutate(facet = factor(facet, levels = params$extra$facet_order, ordered = T)) %>%
            arrange(facet)
        }
      }
    }

    ._ <- plot_tools$plot_top_ES_across(all_gsea_results,
      ranks_list = ranks_list,
      genesets_list_of_lists,
      save_func = save_func,
      limit = params$enplot$limit %||% 10,
      do_individual = params$enplot$do_individual %||% TRUE,
      do_combined = params$enplot$do_combined %||% TRUE,
      combine_by = combine_by_df, # this is metadata table rankname and facet if exists
      width = params$enplot$width %||% 5.4,
      height = params$enplot$height %||% 4.0,
      combined_show_ticks = params$enplot$combined_show_ticks %||% FALSE,
    )

    # pca
    # =============

    do_pca <- params$pca$do %>% ifelse(!is.null(.), ., TRUE)
    if (do_pca) {
      pca_objects <- all_gsea_results %>% pca_tools$do_all(
        metadata = metadata
      )

      log_msg(msg = paste0("plot pca all biplots"))
      pca_objects %>% pca_tools$plot_all_biplots(
        save_func = save_func,
        top_pc = params$pca$top_pc %||% 4,
        showLoadings = T,
        labSize = params$pca$labSize %||% 1.8,
        pointSize = params$pca$pointSize %||% 4,
        sizeLoadingsNames = params$pca$sizeLoadingsNames %||% 1.4,
        colby = params$col_by,
        fig_width = params$pca$width %||% 8.4,
        fig_height = params$pca$height %||% 7.6,
      )
    }
    # todo plot more edge plots and es plots based on the pca results
  }) # end of purrr::map loop for individual genesets

  # end
  if (is.null(params$quiet) || params$quiet == FALSE) {
    voice_tools$speak_text("bcm g s e a is finished")
  }
}
