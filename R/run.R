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

  logfile <- params$logfile %>% ifelse(!is.null(.), ., "run.log")
  log_msg <- util_tools$make_partial(util_tools$log_msg, filename=logfile)
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
  log_msg(msg = paste0("genesets: ", as.character(genesets_array)))

  genesets_of_interest <- geneset_tools$geneset_array_to_df(genesets_array)
  list_of_geneset_dfs <- geneset_tools$get_collections(genesets_of_interest)
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

  parallel <- params$parallel %>% ifelse(!is.null(.), ., FALSE)
  log_msg(paste0("parallel set to: ", parallel))

  # this part is challenging to pass everything in the right format
  results_list <- fgsea_tools$run_all_pathways(genesets_list_of_lists,
    ranks_list,
    parallel = parallel,
    genesets_additional_info = genesets_of_interest
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
  log_msg(msg = "plotting individual barplots")
  plts <- results_list %>% plot_tools$all_barplots_with_numbers(save_func = util_tools$make_partial(save_func, width = 12, height = 12))



  log_msg(msg = "plotting faceted barplots")
  plts <- results_list %>%
    plot_tools$process_results_across_rnks(
      save_func = util_tools$make_partial(save_func, width = 14, height = 12)
    )


  # =======
  log_msg(msg = "combining gsea marices")
  all_gsea_results <- fgsea_tools$concat_results_all_collections(results_list)


  log_msg(msg = "drawing all heatmap. save_func is not null")
  # here is where changes to the save func are made (e.g. width, height, file format)

  hts <- all_gsea_results %>% plot_tools$plot_results_all_collections(
    # limit=20,
    pstat_cutoff = 1,
    limit = 40,
    # save_func = .gsea_heatmap_save_func,
    save_func = save_func
  )

  for (ht in hts) {
    draw(ht,
      heatmap_legend_side = "bottom",
      padding = unit(c(2, 24, 2, 24), "mm"), # top, left, bottom, right
    )
  }


  # =============

  log_msg(msg = paste0("maybe plot edges"))
  # gct@cdesc %<>% mutate(group = factor(group, levels = c("Pre_V", "Pre_A", "Control", "PCL", "PCL_Bi", "PCL_Bi_MSCs"), ordered = T))

  if (is.null(params$genesets)) {
    edgeplot_limit <- 10
  } else {
    edgeplot_limit <- params$edgeplot_limit
  }

  if (!is.null(gct)) {
    if (!"group" %in% colnames(gct@cdesc) && "treatment" %in% colnames(gct@cdesc)){
        gct@cdesc[['group']] <- gct@cdesc[['treatment']]
}
    # gct@cdesc %>% count(group)
    # here can check if gct is loaded properly and in proper format, properly alligned metadata
    ht_edge_plots <- plot_tools$plot_heatmap_of_edges(gct, results_list,
        save_func = save_func,
        limit = edgeplot_limit
    )
  }
  # ===== plot es

 ._ <- plot_tools$plot_top_ES_across(all_gsea_results,
                        ranks_list = ranks_list,
                        genesets_list_of_lists,
                        limit=30,
                        save_func = save_func
                            )




  # this part
  if (exists("gct") && !is.null(gct) && (params$ranks_from=="gct")) {
    metadata <- gct@cdesc
  } else {
    metadata <- NULL
  }

  # this is the pca chunk
  # =============
  # browser()

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
    # colby = NULL,
  )









}
