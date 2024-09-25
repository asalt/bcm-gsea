suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))
suppressPackageStartupMessages(library(colorspace))

suppressPackageStartupMessages(library(here))

basedir <- file.path(here())
src_dir <- file.path(here("R"))

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)

fgsea_tools <- new.env()
source(file.path(src_dir, "./fgsea.R"), local = fgsea_tools)


plot_utils <- new.env()
source(file.path(src_dir, "./plot_utils.R"), local = plot_utils)
make_partial <- plot_utils$make_partial
get_args <- plot_utils$get_args
get_arg <- plot_utils$get_arg

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)
log_msg <- util_tools$make_partial(util_tools$log_msg)

handle_save_func <- function(save_func, path, filename, width = NULL, height = NULL) {
  if (!is.null(save_func)) {
    if (!is.null(path)) {
      save_func <- make_partial(save_func, path = path)
    }
    if (!is.null(filename)) {
      save_func <- make_partial(save_func, filename = filename)
    }
    if (!is.null(width) && !is.null(height)) {
      save_func <- make_partial(save_func, width = width, height = height)
    }
  }
  return(save_func)
}


make_heatmap_fromgct <- function(
    gct,
    row_title = "",
    column_title = "",
    save_func = NULL,
    cluster_rows = T,
    cluster_columns = F,
    color_mapper = NULL,
    # sample_order = NULL,
    cut_by = NULL,
    # scale = T
    ...) {
  # gct <- subgct
  # gct@cdesc$treat <-
  #   factor(.gct@cdesc$treat , levels = c("untreated", "carboplatin", "IMT", "carboplatin_IMT"), ordered = T)

  if (!is.null(sample_order)) {
    gct <- cmapR::subset_gct(gct, cid = sample_order)
  }

  # cat("make_heatmap\n")
  ca <- NULL
  .colors <- plot_utils$create_named_color_list(gct@cdesc, c("group"))
  if ("group" %in% colnames(gct@cdesc)) {
    ca <- ComplexHeatmap::columnAnnotation(
      group = gct@cdesc$group, # this needs to be dynamically set
      col = .colors
      # col = list(
      #   group = c(
      #     `168EC` = "blue",
      #     `Ox_hTERT` = "red",
      #     `No_Ox` = "yellow"
      #   )
      # )
    )
  }

  .legend_width <- unit(6, "cm")
  .cbar_title <- "zscore"
  heatmap_legend_param <- list(
    title = .cbar_title,
    direction = "horizontal",
    just = "bottom",
    legend_width = .legend_width
  )


  # .note <- paste0(description, '\nNES ', sprintf("%.2f", NES), '  pvalue: ', pval)

  if ("rdesc" %in% colnames(gct@rdesc)) {
    row_labels <- gct@rdesc$rdesc
  } else if ("genesymbol" %in% colnames(gct@rdesc)) {
    row_labels <- gct@rdesc$genesymbol
  } else {
    row_labels <- gct@rid
  }


  .mat <- gct@mat
  # gct@mat %>% apply( 1, function(x) scale(x, center = T, scale = scale)) %>% t() %>% as.matrix()),

  heatmap_matrix_width <- unit(ncol(.mat) * .2, "in")
  heatmap_matrix_height <- unit(nrow(.mat) * .2, "in")

  if (cut_by %in% colnames(gct@cdesc)) {
    cut_by <- gct@cdesc[[cut_by]]
    cut_by <- factor(cut_by, levels = unique(cut_by))
  } else {
    cut_by <- NULL
  }

  ht <- ComplexHeatmap::Heatmap(
    .mat,
    width = heatmap_matrix_width,
    height = heatmap_matrix_height,
    # TODO use z_score_withna or other custom func for handling nas when scaling

    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    #

    row_title = row_title,
    column_title = column_title,

    row_labels = row_labels,
    column_labels = gct@cdesc$id, # id should always be here

    column_split = cut_by,

    top_annotation = ca,
    heatmap_legend_param = heatmap_legend_param,
    row_names_gp = grid::gpar(fontsize = 7),
    column_names_gp = grid::gpar(fontsize = 7),
    cluster_column_slices = FALSE,
    column_names_side = "top",
  )



  # log_msg(debug = paste0("defining draw func"))
  do_draw <- function() {
    draw(ht,
      heatmap_legend_side = "bottom",
      padding = unit(c(2, 24, 2, 24), "mm"), # top, left, bottom, right
    )
  }

  # log_msg(debug = paste0("save func: ", class(save_func) %>% as.character()))
  # log_msg(debug = paste0("is null save func: ", is.null(save_func)))

  height <- 4 + (nrow(.mat) * .20)
  width <- 8 + (ncol(.mat) * .26)

  if (!is.null(save_func)) {

    # log_msg(debug = paste0("save func attrs before: "))
    # log_msg(debug = paste0(names(get_args(save_func)), "-", get_args(save_func)))

    save_func <- make_partial(save_func, height = height, width = width)

    # log_msg(debug = paste0("save func attrs after: "))
    # log_msg(debug = paste0(names(get_args(save_func)), "-", get_args(save_func)))

    save_func(plot_code = do_draw)
  }


  return(ht)
}

plot_heatmap_of_edges <- function(
    gct,
    results_list,
    scale = T,
    scale_subset = NULL,
    save_func = NULL,
    limit = 20,
    sample_order = NULL,
    to_include = NULL,
    cut_by = NULL,
    pstat_cutoff = 1,
    ...) {

  .gct <- gct
  if (scale == T) .gct <- util_tools$scale_gct(gct, group_by = scale_subset)
  hts <- names(results_list) %>%
    purrr::map(
      ~ {
        collection_name <- .x
        hts <- names(results_list[[collection_name]]) %>%
          purrr::map(
            ~ {
              comparison_name <- .x
              result <- results_list[[collection_name]][[comparison_name]]
              # print(collection_name)
              # print(comparison_name)
              forplot <- result %>% fgsea_tools$select_topn(
                limit = limit,
                to_include = to_include, # extra pathways to expilcitly include
                pstat_cutoff = pstat_cutoff
              )
              log_msg(debug = paste0("plotting heatmaps for ", collection_name, " ", comparison_name))
              log_msg(debug = paste0("n forplot: ", forplot %>% nrow()))

              # result %>%
              # forplot <- result %>%
              #   arrange(pval) %>%
              #   head(10) %>%
              #   mutate(leadingedgelist = stringr::str_split(leadingEdge, ","))
              # for (i in seq_len(nrow(forplot))) {
              ht <- seq_len(nrow(forplot)) %>% purrr::map(~{
                row <- forplot[.x, ]
                .id <- row$pathway
                .row_title <- row$pval
                # .leading_edge <- unlist(lapply(row$leadingedgelist, function(x) gsub('[c\\(\\)" ]', "", x)))
                subgct <- .gct %>% cmapR::subset_gct(row$leadingEdge[[1]])

                # print(.id)
                .row_title <- paste(
                  paste0("pval: ", row$pval %>% round(4) %>% as.character()),
                  paste0("padj: ", row$padj %>% round(4) %>% as.character()),
                  paste0("NES: ", row$NES %>% round(4) %>% as.character()),
                  sep = "\n")
                log_msg(debug = paste0("plotting gene heatmap for ", .id, " ", comparison_name))


                ht <- NULL
                ht <- tryCatch(
                  {
                    #
                    path <- get_arg(save_func, "path")
                    newpath <- file.path(path, paste0(make.names(collection_name)), make.names("heatmaps_gene"))
                    filename <- paste0(get_arg(save_func, "filename"), make.names(row$pathway), make.names(comparison_name), nrow(subgct@mat))
                    save_func <- make_partial(save_func, filename = filename, path = newpath)
                    # save_func <- make_partial(save_func, filename = filename)
                    ht <- make_heatmap_fromgct(subgct,
                      save_func = save_func,
                      cluster_rows = F,
                      cluster_columns = F,
                      sample_order = sample_order,
                      row_title = .row_title,
                      column_title = paste0(.id, "\n", comparison_name),
                      cut_by = cut_by,
                      ...
                    )
                    # print(ht)
                  },
                  error = function(e) print(sprintf("failed to produce heatmap, %s", e$message))
                )
                return(ht)
              }) # end of one heatmap
            }) # end of all heatmaps for one collection
        return(hts)
      }
    )
  return(hts)
}



# do_plot_edge_heatmaps_onecollection <- function(
#     df,
#     cut_by = NULL,
#     limit = 120,
#     pstat_cutoff = 1,
#     pstat_usetype = "padj",
#     main_pathway_ratio = 0.1,
#     cluster_rows = F,
#     cluster_columns = F,
#     ...) {
#   kwargs <- list(...)
#   sampleresults_names <- names(sampleresults_list)
#   if (!"rankname" %in% colnames(df)) {
#     warning("rankname not in colnames df, returning")
#     return(NULL)
#   }
#
#   df <- fgsea_tools$filter_on_mainpathway(df, main_pathway_ratio = main_pathway_ratio)
#   # Limit the number of pathways if necessary
#   top_pathways <- df %>%
#     arrange(-abs(NES)) %>%
#     filter(!!as.symbol(pstat_usetype) < pstat_cutoff) %>%
#     slice_head(n = limit) %>%
#     pull(pathway)
#   df <- df %>% filter(pathway %in% top_pathways)
# }
#
# do_plot_edge_heatmaps_allcollections <- function(results_list, ...) {
#   # input is named list
#   # names are collection names
#   # values are gsea dataframe results, long form
#   # with comparison/sample names inside
#   log_msg(msg = "starting all edge plots")
#   kwargs <- list(...)
#   collections <- names(results_list)
#   # if is null then exit
#   purrr::map(~ {
#     collection <- .x
#     do_plot_edge_heatmaps_onecollection(results_list[[collection]], ...)
#   })
# }









# Function: prepare_data_for_barplot
# Description: This function prepares the data for a barplot by taking in a dataframe as input.
# Parameters:
#   - df: The dataframe containing the data for the barplot.
# Returns: None
prepare_data_for_barplot <- function(df) {
  df_renamed <- df %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "REACTOME_")) %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "GOBP_")) %>%
    dplyr::mutate(pathway = stringr::str_remove(pathway, "GOCC_"))
  df <- df_renamed

  # is across necessary?
  #   df_renamed <- df %>%
  #   mutate(across(starts_with("pathway"), ~str_remove(., "HALLMARK_"))) %>%
  #   mutate(across(starts_with("pathway"), ~str_remove(., "KEGG_"))) %>%
  #   mutate(across(starts_with("pathway"), ~str_remove(., "GOMF_"))) %>%
  #   mutate(across(starts_with("pathway"), ~str_remove(., "REACTOME_")))
  # df <- df_renamed


  sel <- df %>%
    arrange(-abs(NES)) %>%
    arrange(-NES) %>%
    mutate(pathway = str_replace_all(pathway, "_", " ") %>% str_wrap(width = 40)) %>%
    mutate(pathway = factor(pathway, levels = unique(pathway), ordered = T)) %>%
    arrange(pathway) # %>%

  sel <- sel %>%
    rowwise() %>%
    mutate(leadingEdgeNum = length(leadingEdge)) %>%
    mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size)) %>%
    ungroup() %>%
    mutate(outline_val = dplyr::if_else(padj < .05, "black", NA))

  return(sel)
}


#' Barplot with Numbers
#'
#' This function creates a barplot with numbers using the provided dataframe.
#'
#' @param df dataframe with columns pathway, NES, padj, leadingEdge, size
#' @param title title of the plot
#' @param save_func function to save the plot
#' @param ... additional arguments to be passed to the function
#'
#' @return a ggplot object representing the barplot with numbers
#' automatic wrapping of pathway names
#' facet wrap by rankname if present
#' @examples
#' df <- data.frame(
#'   pathway = c("Pathway 1", "Pathway 2", "Pathway 3"),
#'   NES = c(1.5, 2.0, 1.8),
#'   padj = c(1.05, 01, 0.001),
#'   leadingEdge = c("A", "B", "C"),
#'   size = c(10, 15, 20)
#' )
#' barplot_with_numbers(df, title = "Pathway Analysis")
#'
#' @export
barplot_with_numbers <- function(
    df,
    title = "",
    save_func = NULL,
    facet_order = NULL,
    ...) {
  sel <- prepare_data_for_barplot(df)

  custom_labeller <- function(value) {
    wrapped_labels <- sapply(value, function(label) {
      label %>%
        str_replace_all("_", " ") %>%
        str_wrap(width = 30)
    })
    return(wrapped_labels)
  }
  labeller_func <- custom_labeller


  if (!is.null(facet_order)) {
    sel <- sel %>%
      mutate(rankname = factor(rankname, levels = facet_order, ordered = T)) %>%
      arrange(rankname)
  }

  get_size <- function(x){
    x <- as.character(x)
   ifelse(nchar(x) < 27, 7.6, ifelse(nchar(x) < 64, 6.6, ifelse(nchar(x) < 84, 6.2, 5.2)))
  }

  p <- sel %>%
    ggplot2::ggplot(
      aes(
        y = pathway,
        x = NES,
        # size = leadingEdgeNum,
        fill = padj,
        # fill = rankname
        # color = outline_val,
        # col = id
      )
    ) +
    # scale_color_gradient(low = "#0000ffee", high = "#ff0000ee") +  # Adjust colors to represent p-values
    # geom_point() +
    scale_fill_gradient2(high = "grey", mid = "#ba2020", low = "#c92020", midpoint = .25, limits = c(0, 1)) +
    geom_col(linewidth = 0.8, aes(color = outline_val)) +
    scale_color_identity() +
    labs(title = title %>% labeller_func()) +
    # scale_color_manual(values=c("black", 'blue'))+
    # scale_size(range = c(4, 12)) +  # Adjust point sizes
    geom_text(
      aes(
        label = leadingEdgeFrac,
        x = sign(NES) * .46,
      ),
      color = "white",
      fontface = "bold",
      size = 2.2,
      # position = position_dodge(width = 0.8),
      vjust = 0.5, hjust = 0.5
    ) +
    theme_bw() #+ theme(axis.text.y = element_text(size = map(sel$pathway, get_size)))
  if ("rankname" %in% colnames(df) && (length(unique(df$rankname)) > 1)) {
    log_msg(info = "facet wrapping by rankname")
    p <- p + facet_wrap(~rankname, labeller = as_labeller(labeller_func))
    num_panels <- length(unique(df$rankname))
    ncol <- ceiling(sqrt(num_panels)) # ggplot2 default behavior if ncol is not specified
    nrow <- ceiling(num_panels / ncol) # Calculate rows
  } else {
    ncol <- 1
    nrow <- 1
  }
  panel_width_in <- 3.8
  panel_height_in <- 4

  # Calculate total figure size
  total_width_in <- 2 + panel_width_in * ncol
  total_height_in <- panel_height_in * nrow + (length(unique(sel$pathway)) / 8)
  log_msg(msg = paste0("total width: ", total_width_in, " total height: ", total_height_in))


  if (!is.null(save_func)) {
    save_func(
      plot_code = function() {
        print(p)
      },
      width = total_width_in,
      height = total_height_in
    )
  }
  return(p)
}

all_barplots_with_numbers <- function(
    results_list,
    save_func = NULL,
    facet_order = NULL,
    limit = 20,
    ...) {
  if (!is.null(save_func)) {
    filename <- paste0(get_arg(save_func, "filename"), "barplot_")
    save_func <- make_partial(save_func, filename = filename)
  }

  plts <- results_list %>% purrr::imap(
    ~ {
      collection_name <- .y
      list_of_comparisons <- .x
      plts <- list_of_comparisons %>% imap(
        ~ {
          dataframe <- .x
          comparison_name <- .y
          .plts <- limit %>% purrr::map(
            ~{
              .limit <- .x
              sel <- fgsea_tools$select_topn(dataframe, limit = .limit)
              .title <- comparison_name # %>% fs::path_file() %>% fs::path_ext_remove() #%>% gsub(pattern="_", replacement=" ", x=.)

              if (!is.null(save_func)) {
                filename <- paste0(
                  get_arg(save_func, "filename"),
                  make.names(collection_name), "_", make.names(comparison_name),
                  "_", nrow(sel)
                )
                path <- file.path(get_arg(save_func, "path"), make.names(collection_name), "barplots")
                save_func <- make_partial(save_func, filename = filename, path = path)
              }
              log_msg(msg = paste0("plotting barplot target ", path, " ", collection_name))

              p <- barplot_with_numbers(sel,
                title = .title,
                save_func = save_func,
                facet_order = facet_order,
                ...
              )

            return(p)
            }
          )
          return(.plts)
        }
      )
    }
  )
  return(plts)
}


do_combined_barplots <- function(
    results_list,
    save_func = NULL,
    facet_order = NULL,
    limit = 20,
    ...) {
  genesets <- names(results_list)

  # args <- list(...)
  # if ("save_func" %in% names(args)) {
  #   save_func <- args$save_func
  # } else {
  #   save_func <- NULL
  # }

  purrr::map(genesets, ~ {
    geneset_name <- .x
    fgsea_res_list <- results_list[[geneset_name]]
    # geneset <- genesets_list[[geneset_name]]

    plts <- limit %>% purrr::map(~{
      .limit <- .x
      res <- fgsea_res_list %>% bind_rows(.id = "rankname") # all comparisons 1 gene set
      # res <- res %>% mutate(rankname = rankname %>% fs::path_file() %>% fs::path_ext_remove())
      res <- fgsea_tools$select_topn(res, limit = .limit)
      n_sel <- res %>% distinct(pathway) %>% nrow()
      # pathway_df <- get_pathway_info(geneset_name)
      # .merge <- left_join(res , pathway_df, by= )
      if (!is.null(save_func)) {
        filename <- paste0("barplot_", make.names(geneset_name), "_", n_sel, "_all")
        path <- file.path(get_arg(save_func, "path"), make.names(geneset_name), "barplots")
        log_msg(msg = paste0("plotting barplot target ", path, " ", geneset_name, " "))
        save_func <- make_partial(save_func, filename = filename, path = path)
      }
      p <- res %>% barplot_with_numbers(
        title = geneset_name,
        save_func = save_func,
        facet_order = facet_order,
        ...
      )
      return(p)
    })
    return(plts)
  })
}



plot_results_all_collections <- function(
    list_of_lists,
    metadata = NULL,
    cut_by = NULL,
    limit = 120,
    pstat_cutoff = 1,
    pstat_usetype = "padj",
    cluster_rows = TRUE,
    cluster_columns = FALSE,
    main_pathway_ratio = 0.1,
    save_func = NULL,
    rankname_order = NULL,
    # sample_order = NULL,
    ...) {
  xtra_args <- list(...) # dunno what to do with these
  if (length(limit) == 1) {
    limit <- rep(limit, length(list_of_lists))
  }
  res <- list_of_lists %>% purrr::imap(
    ~ {

      if (!is.null(save_func)) {
        filename <- paste0(get_arg(save_func, "filename"), "gsea_heatmap_", make.names(.y))
        path <- file.path(get_arg(save_func, "path"), make.names(.y), "heatmaps_gsea")
        save_func <- make_partial(save_func, filename = filename, path = path)
      }

       .data <- .x
       .title <- .y
      plts <- limit %>% purrr::map(
        ~ plot_results_one_collection(.data,
          title = .title,
          metadata = metadata,
          cut_by = cut_by,
          limit = .x,
          pstat_cutoff = pstat_cutoff,
          pstat_usetype = pstat_usetype,
          cluster_rows = cluster_rows,
          cluster_columns = cluster_columns,
          save_func = save_func,
          rankname_order = rankname_order,
          ...
          )
        )
      return(plts)

    }


  )
  # res <- purrr::map(list_of_lists, function(item) {
  #   do.call("plot_results_one_collection", c(list(df = item), args))
  # })
  return(res)
}

plot_results_one_collection <- function(
    df,
    metadata = NULL,
    pathway_metadata = NULL,
    title = "",
    cut_by = NULL,
    limit = 120,
    pstat_cutoff = 1,
    pstat_usetype = "padj",
    cluster_rows = TRUE,
    cluster_columns = FALSE,
    sample_order = NULL,
    rankname_order = NULL,
    meta_exclude = c("recno", "runno", "searchno", "label"),
    meta_exclude_extra = NULL,
    row_annotation = NULL, # can be passed if made somewhere else
    save_func = NULL,
    ...) {
  log_msg(msg = paste0("calling plot results one collection"))

  meta_exclude <- c(meta_exclude, meta_exclude_extra)
  log_msg(msg = paste0('rankname order is ', rankname_order))

  # Ensure necessary columns are present
  required_cols <- c("pathway", "NES")
  if (!all(required_cols %in% colnames(df))) {
    stop("Required columns not found in dataframe")
  }

  # if (is.null(sample_order)) {
  #   sample_order <- unique(df$rankname)
  # } else {
  #   sample_order <- union(sample_order, unique(df$rankname))
  # }

  if (is.null(rankname_order)) {
    rankname_order <- unique(df$rankname)
  } else {
    rankname_order <- intersect(rankname_order, unique(df$rankname))
    if (length(rankname_order) == 0) {
      warning("rankname_order is empty, using default")
      rankname_order <- unique(df$rankname) # default backup
    }
  }


  # Align metadata with dataframe
  if (!is.null(metadata) && length(metadata) > 0) {
    if (!all(rownames(metadata) %in% df$rankname)) {
      cat("Metadata not aligned with df")
      log_msg(msg = "Metadata not aligned with df")
      metadata <- NULL
    }
  }

  if (is.null(metadata)) {
    metadata <- data.frame(id = unique(df$rankname), dummy = "X") # necessary for some reason
    rownames(metadata) <- metadata$id
  }
  metadata <- metadata[rankname_order, ] # order metadata by rankname_order


  browser()
  metadata %<>% dplyr::select(-any_of(meta_exclude)) # remove columns


  # Handling cut_by parameter
  if (!is.null(cut_by)){
    if (!is.null(cut_by) && cut_by %in% colnames(metadata)) {
      cut_by <- metadata[, cut_by]
      cut_by <- factor(cut_by, levels = unique(cut_by))
    } else {
      warning("cut_by not found in metadata")
      cut_by <- NULL
    }
  }

  df <- fgsea_tools$select_topn(
    df,
    pstat_cutoff = pstat_cutoff,
    pstat_usetype = pstat_usetype,
    limit = limit
  )

  if (nrow(df) == 0) {
    log_msg(msg = "No pathways to plot")
    return(NULL)
  }

  if (!is.null(row_annotation)) {
    if (!"HeatmapAnnotation" %in% class(row_annotation)) {
      warning("row_annotation is not a HeatmapAnnotation object")
      row_annotation <- NULL
    }
  }


  # Prepare data for heatmap
  dfp <- df %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = rankname) %>%
    dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOBP_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOCC_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "REACTOME_")) %>%
    as.data.frame()
  # dfp %<>% as.data.frame() # because we will be assigning row names


  rownames(dfp) <- dfp$pathway
  dfp <- dfp[, metadata$id] # select ranknames as ordered inside metadata dataframe

  # dfp["pathway"] <- NULL # now remove this column to exclude from heatmap



  dfp_padj <- df %>%
    pivot_wider(id_cols = pathway, values_from = padj, names_from = rankname) %>%
    as.data.frame()
  rownames(dfp_padj) <- dfp_padj$pathway
  dfp_padj["pathway"] <- NULL
  dfp_padj <- dfp_padj[, metadata$id]
  # %>% select(-pathway, all_of(metadata$id))
  logical_matrix <- dfp_padj < 0.25
  star_matrix <- ifelse(logical_matrix, "*", "")
  star_matrix <- star_matrix[, metadata$id] # shouldn't be necessary as comes from dfp_padj
  star_matrix[is.na(star_matrix)] <- ""

  # Set up color scale
  q01 <- quantile(abs(df$NES), 0.99, na.rm = TRUE)
  num_colors <- 5
  my_colors <- colorRampPalette(c(
    "#0000ff",
    "#8888ffbb",
    "#ddddff77",
    "#dddddd",
    "#ffdddd77",
    "#ff8888bb",
    "#ff0000"
  ))(num_colors)
  break_points <- seq(-q01, q01, length.out = num_colors)
  col <- colorRamp2(breaks = break_points, colors = my_colors)


  # Prepare column annotation
  log_msg(msg = paste0("metadata: ", head(metadata)))

  # it isn't strictly necessary to exclude these colum s here,
  # as they will be excluded upon creation of the column_annotation object
  metadata_for_colors <- metadata %>% select(-any_of(c("id", "dummy")))
  if (ncol(metadata_for_colors) > 0) {
    colors_list <- metadata_for_colors %>% colnames() %>%
      map(~{
        # Extract unique values for the current column
        .unique_vals <- unique(metadata[[.x]])

        # Generate a qualitative color palette
        .colors <- colorspace::qualitative_hcl(n = length(.unique_vals))

        # Assign unique values as names to the colors
        names(.colors) <- .unique_vals

        # Return the named color vector
        .colors
      }) %>% set_names(colnames(metadata_for_colors))

    log_msg(msg = paste0("color list ", as.character(colors_list)))

    column_annotation <- ComplexHeatmap::columnAnnotation(
        df = metadata %>% dplyr::select(-any_of(c("id", "dummy"))),
        col = colors_list
      )
  } else {
    column_annotation <- NULL
  }

  # Construct heatmap

  heatmap_legend_param <- list(
    title = "NES",
    direction = "horizontal",
    just = "bottom",
    legend_width = unit(6, "cm"),
    at = break_points %>% round(1)
  )

  # cell_fun <- function(j, i, x, y, width, height, fill) {
  #   # Retrieve the value that indicates whether to draw an asterisk
  #   value <- star_matrix[i, j]
  #   if (value == "*") {
  #     # Draw asterisk
  #     grid::grid.text(value, x, y, gp = grid::gpar(fontsize = 12, col = "black"))
  #     # Draw border around the cell
  #     grid::grid.rect(x, y,
  #       width = width, height = height,
  #       gp = grid::gpar(col = "black", fill = NA, lwd = 1)
  #     )
  #   }
  # }

  cell_fun <- function(j, i, x, y, width, height, fill) {
    # Ensure value is not NA before comparison
    # value <- star_matrix[i, j]
    value <- dfp_padj[i, j]
    # print(paste("Processing cell:", i, j, "Value:", value))
    # cat(sprintf("NES Cell [%d, %d] with value '%s'\n", i, j, dfp[i, j]))
    # cat(sprintf("star_matrix Cell [%d, %d] with value '%s'\n", i, j, star_matrix    [i, j]))

    if (!is.na(value) && value < 0.05) {
      # Draw asterisk
      grid::grid.text(
        "\u2B51",
        # "*",
        x, y,
        gp = grid::gpar(fontsize = 12, col = "black")
      )
    }
    if (!is.na(value) && value < 0.25) {
      # Draw border around the cell
      grid::grid.rect(x, y,
        width = width, height = height,
        gp = grid::gpar(col = "black", fill = NA, lwd = 1)
      )
    }
  }

  # height <- 6 + (nrows(dfp) * .16)
  .ncol <- ncol(dfp)
  heatmap_matrix_width <- unit(ncol(dfp) * .2, "in")
  heatmap_matrix_height <- unit(nrow(dfp) * .2, "in")

  # .row_fontsizes <- ifelse(nchar(rownames(dfp) < 20), 9.8, 5.8)
  # make the below better, later
  .column_fontsizes <- lapply(colnames(dfp), function(x) ifelse(nchar(x) < 22, 9.6, ifelse(nchar(x) < 28, 7.6, ifelse(nchar(x) < 54, 6.2, 5.2))))
  .row_fontsizes <- lapply(rownames(dfp), function(x) ifelse(nchar(x) < 36, 7.6, ifelse(nchar(x) < 64, 6.6, ifelse(nchar(x) < 84, 6.2, 5.2))))
  # rownames_gp <- ifelse(nchar(rownames(dfp) < 20), c(grid::gpar(fontsize=9.8, lineheight=.8)), c(grid::gpar(fontsize=5.8, lineheight=.8)))

  ht <- ComplexHeatmap::Heatmap(
    dfp %>% as.matrix(),
    name = "mat",
    col = col,
    top_annotation = column_annotation,
    left_annotation = row_annotation,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    heatmap_legend_param = heatmap_legend_param,
    column_gap = unit(1, "mm"),
    width = heatmap_matrix_width,
    height = heatmap_matrix_height,
    # width = ncol(dfp) * 9,
    # height = nrow(dfp) * 5,
    border = T,
    column_split = cut_by,
    row_labels = rownames(dfp) %>%
      str_replace_all("_", " ") %>%
      str_wrap(width = 42),
    # row_names_gp = grid::gpar(fontsize = 6.8, lineheight=.8),
    row_names_gp = grid::gpar(fontsize = .row_fontsizes, lineheight = .8),
    column_names_gp = grid::gpar(fontsize = .column_fontsizes, lineheight = .8),
    column_labels = colnames(dfp) %>%
      str_replace_all("_", " ") %>%
      str_wrap(width = 36),
    row_names_side = "right",
    # row_names_rot=(pi/24)*180,
    column_title_gp = grid::gpar(fontsize = 12, hjust = 2),
    clustering_distance_rows = util_tools$dist_no_na,
    clustering_distance_columns = util_tools$dist_no_na,
    clustering_method_rows = "ward.D2",
    clustering_method_columns = "ward.D2",
    column_names_side = "top",
    column_title = title,
    cell_fun = cell_fun
  )


  log_msg(msg = paste0("defining draw func"))
  do_draw <- function() {
    ht <- draw(ht,
      heatmap_legend_side = "bottom",
      padding = unit(c(2, 24, 2, 24), "mm"), # top, left, bottom, right
    )
    xunit <- ifelse(cluster_rows == TRUE, 1, 2.4)
    decorate_heatmap_body("mat", {
      grid.text(
        paste0(
          "\u25A0  padj < .25",
          "\n",
          "\u2605  padj < .05"
        ),
        unit(xunit, "cm"), unit(-2, "cm"),
        gp = gpar(fontsize = 8)
      )
    })
    return(ht)
  }

  ht <- do_draw()



  log_msg(debug = paste0("save func: ", class(save_func) %>% as.character()))
  log_msg(debug = paste0("is null save func: ", is.null(save_func)))

  height <- 4 + (nrow(dfp) * .20)
  width <- 8 + (ncol(dfp) * .26)

  if (!is.null(save_func)) {
    log_msg(debug = paste0("save func attrs before: "))
    log_msg(debug = paste0(names(get_args(save_func)), "-", get_args(save_func)))
    filename <- paste0(get_arg(save_func, "filename"), nrow(dfp), "x", ncol(dfp))

    save_func <- make_partial(save_func, height = height, width = width, filename = filename)

    log_msg(debug = paste0("save func attrs after: "))
    log_msg(debug = paste0(names(get_args(save_func)), "-", get_args(save_func)))

    save_func(plot_code = do_draw)
  }

  return(ht)
}

make_selection <- function(x) {
  .first <- which(x$ES == es_min)
  .second <- which(x$ES == es_max)
  .top <- x %>%
    arrange(rank) %>%
    head(.first)
  .bot <- x %>%
    arrange(rank) %>%
    tail(dim(x)[1] - .second)
  return(bind_rows(.top, .bot))
}


edgeplot1 <- function(rankorder_object, ...) {
  posES <- rankorder_object$posES
  negES <- rankorder_object$negES
  rankorder_edge <- rankorder_object$edge
  # rankorder_edge %>%
  #   filter(!is.na(stat)) %>%
  #   dim()
  # ggplot(aes(x = rank, y = ES)) +
  #   geom_point()

  p <- rankorder_edge %>%
    # filter(!is.na(stat_tick)) %>%
    ggplot(aes(x = stat_tick, y = ES, col = rank)) +
    geom_point() +
    scale_color_continuous(type = "viridis", option = "H") +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed")

  p
}


# across all pathways
plot_top_ES_across <- function(
    gsea_results,
    ranks_list,
    geneset_collections,
    limit = 30,
    do_individual = TRUE,
    do_combined = TRUE,
    combine_by = NULL,
    save_func = NULL,
    combined_show_ticks = FALSE,
    width=3.4,
    height=4,
    ...) {
  if (!"list" %in% class(gsea_results)) {
    stop(cat("gsea_results should be a list of data frames"))
  }

  if (!"list" %in% class(geneset_collections)) {
    stop(cat("geneset_collections should be a list of data frames"))
  }

  missing <- setdiff(
    names(gsea_results),
    names(geneset_collections)
  )
  if (length(missing) > 0) {
    cat(paste0("missing some genesets..."))
  }

  list_of_plts <- gsea_results %>%
    purrr::imap(~ {
      df <- .x
      collection_name <- .y
      geneset_collection <- geneset_collections[[collection_name]]
      if (!is.null(save_func)) {
        path <- get_arg(save_func, "path")
        newpath <- file.path(path, make.names(collection_name), make.names("enrichplots"))
        filename <- make.names(collection_name)
        save_func <- make_partial(save_func, path = newpath, filename = filename)
      }
      plts <- plot_top_ES(df, ranks_list, geneset_collection,
        limit = limit,
        do_individual = do_individual,
        do_combined = do_combined,
        combine_by = combine_by,
        save_func = save_func,
        panel_width = width,
        panel_height = height,
        combined_show_ticks = combined_show_ticks,
      )
      return(plts)
    })
  return(list_of_plts)
}

# across all comparisons in one pathway
plot_top_ES <- function(
    df,
    ranks_list,
    geneset_collection,
    limit = 30,
    do_individual = T,
    do_combined = T,
    combine_by = NULL,
    save_func = NULL,
    panel_width = 4.6,
    panel_height = 3.4,
    combined_show_ticks = FALSE,
    combined_label_size = 1.75,
    filter_on_mainpathway = TRUE,
    ...) {
  #

  if (filter_on_mainpathway == TRUE){
    df <- fgsea_tools$filter_on_mainpathway(df)
  }
    df %<>% fgsea_tools$select_topn(limit = limit)
  if (nrow(df) == 0) {
    return(NULL)
  }
  pathways <- df$pathway %>% unique()
  geneset_lists <- geneset_collection[pathways]

  rankorder_by_pw <- fgsea_tools$get_rankorder_across(
    df,
    ranks_list,
    geneset_lists
  ) # this yields a named list
  # names are pathways
  # values are named lists of rankorder for each sample


  plts <- list()

  if (do_combined) {
    if (is.null(combine_by)) {
      combine_by <- list() ##?
    }
    # everything needed for plotting gets assigned here
    rankorder_samplepathway <- fgsea_tools$combine_rankorders_on_sample(
      rankorder_by_pw,
      metadata = combine_by
    )


    .plts <- rankorder_samplepathway %>% purrr::imap(~ {

      rankorder <- .x # rankorder is a list of dataframes, each df has the facet info
      pathway_name <- .y

      log_msg(msg = paste0("plotting combined ", pathway_name))
      log_msg(msg = paste0("show ticks: ", combined_show_ticks))

      .plt <- plotES_combined(rankorder,
       title = pathway_name %>% str_replace_all("_", " ") %>% str_wrap(width = 40),
        show_ticks = combined_show_ticks,
         label_size = combined_label_size,
      ) # will be faceted if "facet" in .x


      if (!is.null(save_func)) {
        filename <- paste(get_arg(save_func, "filename"),
          make.names(pathway_name),
          make.names("combined"),
          sep = "_"
        )

        if (!"facet" %in% names(rankorder$edge)) { # this should be true if do_combined is true
          rankorder_samplepathway$facet <- 'x'
        }
        num_panels <- max(length(unique(rankorder$edge$facet)), 1)
        if (num_panels > 1) {
          panel_width <- panel_width * .68
          panel_height <- panel_height * .75
        }
        .ncol <- ceiling(sqrt(num_panels)) # ggplot2 default behavior if ncol is not specified
        .nrow <- ceiling(num_panels / .ncol) # Calculate rows
        .width <- panel_width * .ncol
        .height <- panel_height * .8 * .nrow

        save_func <- make_partial(save_func, filename = filename, width = .width, height = .height)
        # and now call it
        save_func(plot_code = function() print(.plt))
      }
      return(.plt)
    })
    plts <- c(plts, .plts)
  }


  # invidual plots
  if (!do_individual) {
    return(plts)
  }


  .plts <- rankorder_by_pw %>%
    purrr::imap(~ {
      rankorders <- .x
      pathway_name <- .y

      rankorders %>% purrr::imap(~ {
        rankorder <- .x
        comparison <- .y
        .stats <- df %>% filter(pathway == !!pathway_name, rankname == !!comparison)
        .title <- paste0(comparison, "\n", pathway_name)
        .subtitle <- ""
        if (nrow(.stats) == 1){
          .subtitle <- paste0(
          "ES: ", .stats[["ES"]] %>% round(2) %>% as.character(),
          "\tNES: ", .stats[["NES"]] %>% round(2) %>% as.character(),
          "\tpval: ", .stats[["pval"]] %>% round(2) %>% as.character(),
          "\tpadj: ", .stats[["padj"]] %>% round(2) %>% as.character()
          )
        }
        plt <- plotES(rankorder, title = .title, subtitle = .subtitle)


        if (!is.null(save_func)) {
          filename <- paste(get_arg(save_func, "filename"),
            make.names(pathway_name),
            make.names(comparison),
            sep = "_"
          )
          save_func <- make_partial(save_func, filename = filename)
          # and now call it
          save_func(
            plot_code = function() print(plt),
            width = panel_width,
            height = panel_height
          )
        }
        return(plt)
      })
    })
  plts <- c(plts, .plts)
  return(plts)
}

plotES_combined <- function(enplot_data, label_size=1.85, title = "", show_ticks = F, ...) {
  # plot ES for multiple samples/comparisons.
  spreadES <- max(enplot_data$curve$ES, na.rm = T) - min(enplot_data$curve$ES, na.rm = T)
  # print(spreadES)
  # look at add_color_mappings and similar called from combine_rankorders_on_sample for color assignments
  curve <- enplot_data$curve
  ticks <- enplot_data$ticks
  p <- ggplot(data = curve) +
    geom_line(aes(x = rank, y = ES, color=rankname), alpha = .6, show.legend = F) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_text_repel(
      data = curve %>% group_by(rankname) %>% arrange(-abs(ES)) %>% slice_head(n=1) %>% ungroup(),
      aes(label = rankname, x = rank, y = ES, color = rankname),
      # nudge_x = 0.5,
      # nudge_y = 0.5,
      size = label_size,
      show.legend = F,
      max.overlaps = Inf
    )
    if (show_ticks == TRUE){
      p <- p + geom_segment(
          data = ticks,
          mapping = aes(
            x = rank, y = -spreadES / 32,
            xend = rank, yend = spreadES / 32,
            color = rankname
          ),
          alpha = .4,
          show.legend = F
        )
    }
    p <- p + theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      title = element_text(size = 8),
      subtitle = element_text(size = 5)
    ) +
    labs(x = "rank", y = "enrichment score", title = title )
  if ("facet" %in% names(enplot_data$curve)) {
    p <- p + facet_wrap(~facet)
  }
  p
}

plotES <- function(enplot_data, ticksSize = 4, title = "", subtitle = "") {
  # this is directly from the example in fgsea plotEnrichmentData

  expected_names <- c("curve", "stats", "ticks", "maxAbsStat", "spreadES", "posES", "negES")
  for (name in expected_names) {
    if (!name %in% names(enplot_data)) {
      stop(paste0("missing ", name, " in enplot_data"))
    }
  }

  maxAbsStat <- enplot_data$maxAbsStat
  spreadES <- enplot_data$spreadES
  posES <- enplot_data$posES
  negES <- enplot_data$negES

  p <- with(
    enplot_data,
    ggplot(data = curve) +
      geom_line(aes(x = rank, y = ES), color = "green") +
      geom_ribbon(
        data = stats,
        mapping = aes(
          x = rank, ymin = 0,
          ymax = stat / maxAbsStat * (spreadES / 4)
        ),
        fill = "grey"
      ) +
      geom_segment(
        data = ticks,
        mapping = aes(
          x = rank, y = -spreadES / 16,
          xend = rank, yend = spreadES / 16
        ),
        size = 0.2
      ) +
      geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
      geom_hline(yintercept = negES, colour = "red", linetype = "dashed") +
      geom_hline(yintercept = 0, colour = "black") +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey92")
      ) +
      labs(x = "rank", y = "enrichment score")
  )
  p <- p + labs(title = title, subtitle = subtitle) +
    theme(
      title = element_text(size = 8),
      subtitle = element_text(size = 5)
      )
  return(p)
}



plot_table <- function(fgsea_res,
                       ranks,
                       pathways,
                       gsea_param = 1.0,
                       savefunc = NULL,
                       ...) {
  # top_pathways_u <- fgsea_res[ES > 0][head(order(pval), n=10), pathway]
  # top_pathways_d <- fgsea_res[ES < 0][head(order(pval), n=10), pathway]
  top_pathways_u <- fgsea_res %>%
    filter(ES > 0) %>%
    arrange(pval) %>%
    head(10) %>%
    pull(pathway)
  top_pathways_d <- fgsea_res %>%
    filter(ES < 0) %>%
    arrange(pval) %>%
    head(10) %>%
    pull(pathway)
  top_pathways <- c(top_pathways_u, rev(top_pathways_d)) #  rev reverse order
  tableobject <- pathways[top_pathways] %>%
    plotGseaTable(ranks,
      fgsea_res,
      gseaParam = gsea_param
      # gseaParam=1.0
    )
  return(tableobject)
}


plot_tables <- function(
    results_list,
    ranks_list,
    pathways_list) {
  # Ensure names are aligned and iterate over them
  pathway_names <- names(results_list)

  map(pathway_names, ~ {
    geneset_name <- .x
    fgsea_res_list <- results_list[[geneset_name]]
    genesets <- pathways_list[[geneset_name]]

    # Now, iterate over each rank file within this pathway category
    map(names(ranks_list), function(rank_name) {
      rankobj <- ranks_list[[rank_name]]
      # Generate a unique identifier for this plot/table, e.g., combining pathway name and rank file name
      rank_name_nice <- rank_name %>%
        fs::path_file() %>%
        fs::path_ext_remove()
      plot_id <- paste(geneset_name, rank_name_nice, sep = "_")


      .fgsea_res <- fgsea_res_list[[rank_name]]
      p <- plot_table(
        fgsea_res = .fgsea_res,
        ranks = rankobj,
        pathways = genesets,
        gsea_param = 0.5,
      )

      # outpath <- file.path(ENPLOT_BASEDIR, make.names(geneset_name), make.names(rank_name_nice))
      # if (!fs::dir_exists(outpath)) fs::dir_create(outpath)

      # print(p)
      return(p)

      # c(".png", ".pdf") %>% purrr::walk(
      # ~ggsave(filename = paste0(make.names(geneset_name), "_toptable", "_gseaparam.5", .x),
      #        path = outpath,
      #        dpi = 300,
      #        width = 19,
      #        height=12,
      #        )
      # )
    })
  })
}

plot_tables_faster <- function(
    results_list,
    ranks_list,
    pathways_list) {
  # Ensure names are aligned and iterate over them
  pathway_names <- names(results_list)

  # furrr::future_map(pathway_names, ~{
  purrr::map(pathway_names, ~ {
    geneset_name <- .x
    fgsea_res_list <- results_list[[geneset_name]]
    genesets <- pathways_list[[geneset_name]]

    # Now, iterate over each rank file within this pathway category
    # map(names(ranks_list), function(rank_name) {
    # furrr::future_map(names(ranks_list), function(rank_name) {
    furrr::future_imap(ranks_list, function(rankobj, rank_name) {
      # rankobj <- ranks_list[[rank_name]]
      # Generate a unique identifier for this plot/table, e.g., combining pathway name and rank file name
      rank_name_nice <- rank_name %>%
        fs::path_file() %>%
        fs::path_ext_remove()
      plot_id <- paste(geneset_name, rank_name_nice, sep = "_")


      .fgsea_res <- fgsea_res_list[[rank_name]]
      p <- plot_table(
        fgsea_res = .fgsea_res,
        ranks = rankobj,
        pathways = genesets,
        gsea_param = 0.5,
      )

      outpath <- file.path(ENPLOT_BASEDIR, make.names(geneset_name), make.names(rank_name_nice))
      if (!fs::dir_exists(outpath)) fs::dir_create(outpath)

      # print(p)
      return(p)
      # c(".png", ".pdf") %>% purrr::walk(
      # ~ggsave(filename = paste0(make.names(geneset_name), "_toptable", "_gseaparam.5", .x),
      #        path = outpath,
      #        dpi = 300,
      #        width = 19,
      #        height=12,
      #        )
      # )
    })
  })
}
