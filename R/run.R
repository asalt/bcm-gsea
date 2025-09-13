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
prepare_metadata <- function(gct, params) {
  if (is.null(gct) || params$ranks_from != "gct") {
    return(NULL)
  }
  metadata <- gct@cdesc
  possible_group_order <- params$extra$rankname_order %||% NULL
  if (!is.null(possible_group_order)) {
    if ("group" %in% colnames(metadata) &&
        all(possible_group_order %in% metadata$group)) {
      metadata <- metadata %>%
        mutate(group = factor(group, levels = possible_group_order, ordered = TRUE)) %>%
        arrange(group)
    } else {
      warning("rankname_order provided but group column missing or incomplete; ignoring ordering")
    }
  }
  metadata <- metadata[gct@cid, , drop = FALSE]
  metadata
}




run <- function(params) {
  # == local
  # putting all this here to reduce load time when not needed (e.g. testing, checking help)

  source(file.path(here("R", "lazyloader.R")))
  io_tools <- get_tool_env("io")

  util_tools <- get_tool_env("utils")
  params <- util_tools$clean_args(params)

  # geneset_tools <- get_tool_env("geneset_utils")

  # fgsea_tools <- get_tool_env("fgsea")
  plot_tools <- get_tool_env("plot")
  plot_utils <- get_tool_env("plot_utils")
  pca_tools <- get_tool_env("pca")
  voice_tools <- get_tool_env("voice")

  # io_tools <- new.env()
  # source(file.path(here("R"), "io.R"), local = io_tools)

  # util_tools <- new.env()
  # source(file.path(here("R"), "utils.R"), local = util_tools)

  get_args <- util_tools$get_args
  get_arg <- util_tools$get_arg
  make_partial <- util_tools$make_partial

  # geneset_tools <- new.env()
  # source(file.path(here("R"), "geneset_utils.R"), local = geneset_tools)
  #
  # fgsea_tools <- new.env()
  # source(file.path(here("R"), "fgsea.R"), local = fgsea_tools)
  #
  # plot_tools <- new.env()
  # source(file.path(here("R"), "plot.R"), local = plot_tools)
  #
  # plot_utils <- new.env()
  # source(file.path(here("R"), "plot_utils.R"), local = plot_utils)
  #
  # pca_tools <- new.env()
  # source(file.path(here("R"), "pca.R"), local = pca_tools)
  #
  # voice_tools <- new.env()
  # source(file.path(here("R"), "voice.R"), local = voice_tools)
  # ===========================

  save_func <- util_tools$make_partial(
    plot_utils$plot_and_save,
    path = params$savedir,
    replace = params$advanced$replace %||% TRUE
  )

  # ===========================

  savedir <- params$savedir
  cachedir <- params$advanced$cachedir
  print(cachedir)
  rankfiledir <- params$rankfiledir
  species <- params$species %||% "Homo sapiens"

  logfile <- params$advanced$logfile %>% ifelse(!is.null(.), ., "run.log")
  options("bcm_gsea_log_msg_filename" = logfile)

  log_msg <- util_tools$make_partial(util_tools$log_msg, filename = logfile)
  log_msg(msg = paste0("===\n*starting bcm gsea*\n==="))
  if ((params$advanced$quiet %||% FALSE) == FALSE) {
    voice_tools$speak_text("starting bcm g s e a")
  }

  # =======

  # == genesets

  geneset_tools <- get_tool_env("geneset_utils") # load module
  genesets_of_interest <- geneset_tools$geneset_array_to_df(params$genesets)
  list_of_geneset_dfs <- geneset_tools$get_collections(genesets_of_interest, species = species)
  genesets_list_of_lists <- list_of_geneset_dfs %>% purrr::map(geneset_tools$genesets_df_to_list)

  # =======

  # log_msg(paste0("rankfiledir: ", params$rankfiledir))
  # log_msg(paste0("volcanodir: ", params$volcanodir))
  # log_msg(paste0("gct_path: ", params$gct_path))
  # log_msg(paste0("ranks from: ", params$ranks_from))

  # ==
  ranks_list <- io_tools$load_and_process_ranks(params)
  # =======

  # == run fgsea

  parallel <- params$advanced$parallel
  if (parallel) {
    workers <- future::availableCores() - 1
    future::plan(future::multicore, workers = workers)
    # walk_func <- future::future_map
  }
  cache <- params$advanced$cache %||% TRUE
  log_msg(msg = paste0("parallel set to: ", parallel))
  log_msg(msg = paste0("cache set to: ", cache))
  log_msg(msg = paste0("cachedir set to: ", cachedir))

  # dump.frames(to.file = TRUE)
  # # Quit R with error status
  # q(status = 1)


  # =======  load gct

  log_msg(msg = params$gct_path)
  sample_exclude <- params$sample_exclude %||% NULL
  if (!is.null(params$gct_path) && file.exists(params$gct_path)) {
    log_msg(msg = paste0("reading gct file: ", params$gct_path))
    gct <- cmapR::parse_gctx(params$gct_path)
    to_exclude <- rownames(gct@cdesc[sample_exclude, ])
    to_keep <- setdiff(gct@cid, to_exclude)
    if (length(to_exclude) > 0){
        gct <- cmapR::subset_gct(gct, cid=to_keep)
    }
  } else {
    gct <- NULL
  }
  metadata <- prepare_metadata(gct, params)


  # this part is challenging to pass everything in the right format
  fgsea_tools <- get_tool_env("fgsea") # load module
  genesets_for_iteration <- names(genesets_list_of_lists)
  # we then iterate over the genesets one by one
  # so all results for one get generated before moving to the next

  # extra pathways to ensure we plot
  pathways_of_interest = params$pathways_of_interest


  genesets_for_iteration %>% purrr::walk(
    ~ {
      .msg <- paste0("running gsea for: ", .x)
      # if (is.null(params$quiet) || params$quiet == FALSE) {
      #   voice_tools$speak_text(text = paste0('running g s e a for ', .x))
      # }

      log_msg(msg = .msg)
      if (params$advanced$quiet != TRUE) voice_tools$speak_text(.x)

      genesets_list_of_lists <- genesets_list_of_lists[.x]
      results_list <- fgsea_tools$run_all_pathways(genesets_list_of_lists,
        ranks_list,
        parallel = parallel,
        genesets_additional_info = genesets_of_interest,
        cache = params$advanced$cache %||% TRUE,
        cache_dir = cachedir,
        logger = log_msg,
        species = species
      )

      log_msg(msg = "names gsea results list: ")
      log_msg(msg = str_c(names(results_list), sep = "\n"))

      log_msg(msg = "comparison  names gsea results list: ")
      log_msg(msg = str_c(names(results_list[[1]]), sep = "\n"))


      # =======
      log_msg(msg = "combining gsea marices")
      all_gsea_results <- fgsea_tools$concat_results_all_collections(results_list)
      # all_gsea_results %>% saveRDS(file = file.path(savedir, 'allgsearesults.RDS'))


      # ======= save

      ._ <- results_list %>%
        io_tools$save_individual_gsea_results(
          savedir = file.path(savedir, "gsea_tables"),
          replace = params$advanced$replace,
          species = species
        )

      # maybe save pivoted file. gets extremely big extremely quickly
      if (params$advanced$pivot_gsea_results == TRUE) {
        log_msg(msg = "pivoting gsea results")
        all_gsea_results %>% io_tools$save_pivoted_gsea_results(
          savedir = file.path(savedir, "gsea_tables"),
          replace = params$advanced$replace,
          species = species
        )
      }


      # now we plot results

      # pca
      # =============
      if (params$pca$do == TRUE) {
        if (is.null(metadata)) {
          warning("PCA skipped: metadata not available")
        } else if (!all(names(ranks_list) %in% rownames(metadata))) {
          warning("PCA skipped: metadata missing required ranknames")
        } else {
          pca_objects <- all_gsea_results %>% pca_tools$do_all(
            metadata = metadata
          )

          log_msg(msg = paste0("plot pca all biplots"))

          pca_objects %>% pca_tools$plot_all_biplots(
            save_func = save_func,
            top_pc = params$pca$top_pc %||% params$pca$max_pc %||% 3,
            showLoadings = TRUE,
            labSize = params$pca$labSize %||% 1.8,
            pointSize = params$pca$pointSize %||% 4,
            sizeLoadingsNames = params$pca$sizeLoadingsNames %||% 1.4,
            colby = params$pca$col_by %||% params$col_by %||% NULL,
            shape = params$pca$mark_by %||% params$mark_by %||% NULL,
            fig_width = params$pca$width %||% 8.4,
            fig_height = params$pca$height %||% 7.6,
            rankname_order = params$extra$rankname_order
          )

          all_gsea_results %>% purrr::imap(~ {
            collection_name <- .y
            pca_object <- pca_objects[[.y]]
            .savedir <- file.path(get_arg(save_func, "path"), make.names(collection_name), "pca")
            .filename <- paste0(make.names(collection_name), "_top_loadings")
            .save_func <- make_partial(save_func, path = .savedir, filename = .filename)
            pca_tools$make_heatmap_from_loadings(
              gsea_object = .x,
              pca_object = pca_object,
              save_func = .save_func,
              rankname_order = params$extra$rankname_order,
              cluster_rows = params$pca$cluster_rows %||% TRUE,
              cluster_columns = params$pca$cluster_columns %||% c(FALSE, TRUE),
              cut_by = params$pca$cut_by %||% params$heatmap_gene$cut_by %||% params$cut_by %||% NULL,
              meta_to_include = params$pca$legend_include %||% params$legend_include,
              meta_to_exclude = params$pca$legend_exclude %||% params$legend_exclude
            )
          })
        }
      } # todo plot more edge plots and es plots based on the pca results


      # now plot
      plot_tools <- get_tool_env("plot")

      # =======  barplots
      if (params$barplot$do_individual == TRUE) {
        log_msg(msg = "plotting individual barplots")
        plts <- results_list %>% plot_tools$all_barplots_with_numbers(
          # sample_order = params$rankname_order %||% params$sample_order, # no t necessary to pass this here
          limit = params$barplot$limit %||% c(10, 20, 30, 50),
          save_func = save_func
        )
      }

      if (params$barplot$do_combined == TRUE) {
        log_msg(msg = "plotting faceted barplots")
        plts <-  tryCatch({
          plot_tools$do_combined_barplots(
            results_list,
            facet_order = NULL, # this is n't working properly
            save_func = save_func,
            limit = params$barplot$limit %||% c(10, 20, 30, 50)
          )},
          error = function(e) { return(NULL) }
            )
      }


      # ======= gsea level heatmap
      if (params$heatmap_gsea$do == TRUE) {
        log_msg(msg = "drawing all heatmaps. ")
        hts <- all_gsea_results %>% plot_tools$plot_results_all_collections(
          # limit=20,
          metadata = metadata,
          # pstat_cutoff = .25,
          pstat_cutoff = 1,
          limit = params$heatmap_gsea$limit %||% 20,
          cut_by = params$heatmap_gsea$cut_by %||% params$cut_by %||% NA,
          save_func = save_func,
          cluster_rows = params$heatmap_gsea$cluster_rows %||% TRUE,
          cluster_columns = params$heatmap_gsea$cluster_columns %||% c(FALSE, TRUE),
          # sample_order = params$extra$rankname_order, # get rid of this one, pretty sure
          rankname_order = params$extra$rankname_order,
          meta_to_include = params$heatmap_gsea$legend_include %||% params$legend_include,
          meta_to_exclude = params$heatmap_gsea$legend_exclude %||% params$legend_exclude
        )
      }

      # =============

      log_msg(msg = paste0("maybe plot edges"))

      # ===== plot edgse and es
      # this part is messy
      # first is metadata organization to determine if and how to aggregate curves
      # this is nto used right now for heatmap_gene
      # it is used for plot_top_ES_across
      combine_by <- params$enplot$combine_by %||% params$combine_by %||% NULL
      combine_by_df <- NULL
      if (!is.null(combine_by) && !is.null(metadata)) {
        combine_by_grid <- expand.grid(combine_by_val=combine_by, stringsAsFactors = FALSE)
        combine_by_grid %>% purrr::pmap(
        ~{
            params = list(...)
            combine_by_val <- params$combine_by_val
            splits <- util_tools$process_cut_by(combine_by_val, metadata)
            #if (!combine_by_val %in% colnames(metadata)) {
            if (is.null(splits)){
              combine_by_df <- NULL
            } else {
              combine_by_df <- data.frame(id=rownames(metadata), facet=splits)
              rownames(combine_by_df) <- combine_by_df$id
              if (!is.null(params$extra$rankname_order)) { # this will fail if not match exactly
                combine_by_df <- combine_by_df %>%
                  mutate(facet = factor(facet, levels = params$extra$rankname_order, ordered = T)) %>%
                  arrange(facet)
              }
          }
        })
      }



      if (is.null(gct) && params$heatmap_gene$do == TRUE) {
          log_msg(msg = "heatmap_gene$do set to true but gct is NULL")
      }
      if (!is.null(gct) && params$heatmap_gene$do == TRUE) {
        ht_edge_plots <- plot_tools$plot_heatmap_of_edges(
          gct,
          results_list,
          scale = params$zscore_emat %||% params$heatmap_gene$scale %||% TRUE,
          scale_by = params$zscore_emat_groupby %||% params$heatmap_gene$scale_by %||% NULL,
          limit = params$heatmap_gene$limit %||% 10,
          sample_order = params$extra$sample_order,
          cut_by = params$heatmap_gene$cut_by %||% params$cut_by %||% NA,
          combine_by = params$heatmap_gene$combine_by %||% params$combine_by %||% NULL,
          cluster_rows = params$heatmap_gene$cluster_rows %||% c(FALSE, TRUE),
          cluster_columns = params$heatmap_gene$cluster_columns %||% c(FALSE, TRUE),
          pstat_cutoff = params$heatmap_gene$pstat_cutoff %||% 1,
          save_func = save_func,
          replace = params$advanced$replace %||% TRUE,
          #combine_by = combine_by_df, # this is metadata table rankname and facet if exists
          #combine_by_name = combine_by,
          sample_exclude = sample_exclude,
          pathways_of_interest = pathways_of_interest,
          meta_to_include = params$heatmap_gene$legend_include %||% params$legend_include %||% NULL,
          meta_to_exclude = params$heatmap_gene$legend_exclude %||% params$legend_exclude %||% NULL,
          parallel = params$heatmap_gene$parallel %||% FALSE   #params$advanced$parallel %||% FALSE, # setting this to true is unstable

        )
      }

      # print(all_gsea_results)
      # print(ranks_list)
      # print(genesets_list_of_lists)
      # print(save_func)

      # ===== plot edgse and es
      # this part is messy
      # first is metadata organization to determine if and how to aggregate curves
      # this is nto used right now for heatmap_gene
      # it is used for plot_top_ES_across
      combine_by <- params$enplot$combine_by %||% params$combine_by %||% NULL
      combine_by_df <- NULL
      #if (!is.null(combine_by) && !is.null(metadata)) {
      if (params$enplot$do_combined == TRUE) {
        combine_by_grid <- expand.grid(combine_by_val=combine_by, stringsAsFactors = FALSE)
        .enplot_limit <- params$enplot$limit %||% 20
        ._ <- combine_by_grid %>% purrr::pmap(
        ~{
            params = list(...)
            combine_by_val <- params$combine_by_val
            splits <- util_tools$process_cut_by(combine_by_val, metadata)
            if (is.null(splits)){
              combine_by_df <- NULL
            } else {
              combine_by_df <- data.frame(id=rownames(metadata), facet=splits)
              rownames(combine_by_df) <- combine_by_df$id
              combine_by_df$rankname <- combine_by_df$id
            }
            if ((!is.null(params$extra$rankname_order)) && (!is.null(combine_by_df))) {
              combine_by_df <- combine_by_df %>%
                mutate(facet = factor(facet, levels = params$extra$rankname_order, ordered = T)) %>%
                arrange(facet)
            }

            print(paste0('limit is: ', .enplot_limit))
            ._ <- plot_tools$plot_top_ES_across(all_gsea_results,
              ranks_list = ranks_list,
              genesets_list_of_lists,
              save_func = save_func,
              limit = .enplot_limit,
              #do_individual = params$enplot$do_individual %||% TRUE,
              do_individual = FALSE, # becase we do it down below
              do_combined = params$enplot$do_combined %||% TRUE,
              combine_by = combine_by_df, # this is metadata table rankname and facet if exists
              combine_by_name = combine_by_val,
              width = params$enplot$width %||% 5.4,
              height = params$enplot$height %||% 4.0,
              pathways_of_interest = pathways_of_interest,
              combined_show_ticks = params$enplot$combined_show_ticks %||% FALSE,
              combined_label_size = params$enplot$combined_label_size %||% 1.85
            )


        })
      }


      if (!is.null(params$enplot$do_individual) && params$enplot$do_individual == TRUE ) {
        ._ <- plot_tools$plot_top_ES_across(all_gsea_results,
          ranks_list = ranks_list,
          genesets_list_of_lists,
          save_func = save_func,
          limit = params$enplot$limit %||% 10,
          do_individual = params$enplot$do_individual,
          do_combined = FALSE, # because it is perfromed above in a grid of parameters
          # combine_by = combine_by_df, # this is metadata table rankname and facet if exists
          # combine_by_name = combine_by,
          width = params$enplot$width %||% 5.4,
          height = params$enplot$height %||% 4.0,
          pathways_of_interest = pathways_of_interest,
          # combined_show_ticks = params$enplot$combined_show_ticks %||% FALSE,
          # combined_label_size = params$enplot$combined_label_size %||% 2.05
        )
    }

    }
  ) # end of purrr::map loop for individual genesets

  # end
  if (is.null(params$quiet) || params$quiet == FALSE) {
    if (params$advanced$quiet != TRUE) voice_tools$speak_text("bcm g s e a is finished")
  }
}
