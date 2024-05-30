suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))

suppressPackageStartupMessages(library(here))

src_dir <- file.path(here("R"))

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)

# DEBUG


make_heatmap <- function(.gct, row_note = "", scale = T) {
  # .gct <- subgct
  # .gct@cdesc$treat <-
  #   factor(.gct@cdesc$treat , levels = c("untreated", "carboplatin", "IMT", "carboplatin_IMT"), ordered = T)

  ca <- ComplexHeatmap::columnAnnotation(
    group = .gct@cdesc$group, # this needs to be dynamically set
    col = list(
      group = c(
        `168EC` = "blue",
        `Ox_hTERT` = "red",
        `No_Ox` = "yellow"
      )
    )
  )


  .legend_width <- unit(2.5, "cm")
  .cbar_title <- "zscore"
  heatmap_legend_param <- list(
    title = .cbar_title,
    direction = "vertical",
    just = "bottom",
    legend_width = .legend_width
  )


  # .note <- paste0(description, '\nNES ', sprintf("%.2f", NES), '  pvalue: ', pval)


  ht <- ComplexHeatmap::Heatmap(
    .gct@mat %>% apply(1, function(x) scale(x, center = T, scale = scale)) %>% t() %>% as.matrix(),
    # TODO use z_score_withna or other custom func for handling nas when scaling
    row_labels = .gct@rdesc$rdesc,
    column_labels = .gct@cdesc$id, # id should always be here
    column_split = .gct@cdesc$group, # treat is not guaranteed to be here, this needs to be dynamically set
    row_labels = .gct@rdesc$rdesc, # TODO: more robust check
    top_annotation = ca,
    heatmap_legend_param = heatmap_legend_param,
    row_names_gp = grid::gpar(fontsize = 7),
    column_names_gp = grid::gpar(fontsize = 7),
    cluster_column_slices = FALSE,
    column_names_side = "top",
    cluster_columns = F,
  )

  ht
}


custom_labeller <- function(value) {
  wrapped_labels <- sapply(value, function(label) {
    label %>%
      str_replace_all("_", " ") %>%
      str_wrap(width = 30)
  })
  return(wrapped_labels)
}



prepare_data_for_barplot <- function(df) {
  df_renamed <- df %>%
    dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "REACTOME_"))
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
    mutate(leadingEdgeNum = str_count(leadingEdge, ",") + 1) %>%
    dplyr::mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size)) %>%
    dplyr::mutate(outline_val = dplyr::if_else(padj < .05, "black", NA))

  # Ensure leadingEdge is treated as a character vector
  sel <- sel %>%
    mutate(leadingEdge = as.character(leadingEdge)) %>%
    mutate(leadingEdgeNum = str_count(leadingEdge, ",") + 1)

  sel <- sel %>%
    mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size)) %>%
    mutate(outline_val = dplyr::if_else(padj < .05, "black", NA))

  # sel %<>% mutate(leadingEdgeNum = str_count(leadingEdge, ",") + 1)
  # sel %<>% mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size))
  # sel %<>% mutate(outline_val = if_else(padj < .05, "black", NA))

  sel
}



#' @param df dataframe with columns pathway, NES, padj, leadingEdge, size
#' @param title title of the plot
barplot_with_numbers <- function(df, title = "", use_custom_labeller = T, ...) {
  sel <- prepare_data_for_barplot(df)

  if (use_custom_labeller == T) {
    labeller_func <- custom_labeller
  } else if (use_custom_labeller == F) labeller_func <- function(x) x
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
    scale_fill_gradient2(high = "grey", mid = "#ba2020", low = "#c92020", midpoint = .25) +
    geom_col(aes(color = sel$outline_val)) +
    scale_color_identity() +
    labs(title = title %>% labeller_func()) +
    # scale_color_manual(values=c("black", 'blue'))+
    # scale_size(range = c(4, 12)) +  # Adjust point sizes
    geom_text(
      aes(
        label = leadingEdgeFrac,
        x = sign(NES) * .4,
      ),
      color = "white",
      fontface = "bold",
      size = 2.4,
      # position = position_dodge(width = 0.8),
      vjust = 0.5, hjust = 0.5
    ) +
    theme_bw()
  if ("rankname" %in% colnames(df) && (length(unique(df$rankname)) > 1)) {
    p <- p + facet_wrap(~rankname, labeller = as_labeller(labeller_func))
  }
  p
}


process_results_across_rnks <- function(results_list, genesets = NULL, ...) {
  # Ensure names are aligned and iterate over them
  # geneset_names <- names(results_list)
  if (is.null(genesets)) {
    genesets <- names(results_list)
  }

  purrr::map(genesets, ~ {
    geneset_name <- .x
    fgsea_res_list <- results_list[[geneset_name]]
    # geneset <- genesets_list[[geneset_name]]

    res <- fgsea_res_list %>% bind_rows(.id = "rankname") # all comparisons 1 gene set
    res <- res %>% mutate(rankname = rankname %>% fs::path_file() %>% fs::path_ext_remove())
    # take topn
    .pathways_for_plot <- res %>%
      arrange(pval) %>%
      distinct(pathway) %>%
      head(20) %>%
      pull(pathway)
    res <- res %>% filter(pathway %in% .pathways_for_plot)


    # pathway_df <- get_pathway_info(geneset_name)
    # .merge <- left_join(res , pathway_df, by= )
    p <- res %>% barplot_with_numbers(title = geneset_name, use_custom_labeller = T)

    return(p)


    # maxylab <- res$pathway %>% nchar() %>% max()
    # res$pathway %>%
    # width <- maxylab-
    npathways <- p$data %>%
      distinct(pathway) %>%
      dim() %>%
      .[1]
    height <- max(6, npathways * .5)

    nfacets <- p$data %>%
      distinct(rankname) %>%
      dim() %>%
      .[1]
    nfacetrows <- nfacets %/% 3
    height <- height * nfacetrows
    height <- min(height, 34)
    height <- max(height, 6)

    max_facet_len <- res$rankname %>%
      nchar() %>%
      max()
    # width = 10
    # if (max_facet_len > 100){
    #   width <- width + 4
    # }
    # if (max_facet_len > 200){
    #   width <- width + 4
    # }

    # number plotted
    # height =
    # outpath <- file.path(ENPLOT_BASEDIR, make.names(geneset_name))
    # if (!fs::dir_exists(outpath)) fs::dir_create(outpath)

    # c(".png", ".pdf") %>% purrr::walk(
    #   ~ ggsave(
    #     filename = paste0(make.names(geneset_name), make.names(.x)),
    #     path = outpath,
    #     dpi = 300,
    #     width = 19,
    #     height = height,
    #   )
    # )
  })
}


# maybe these should be somewhere else

concat_results_one_collection <- function(list_of_dfs) {
  res <- list_of_dfs %>%
    purrr::imap(
      .f = ~ {
        .x %>% dplyr::mutate(var = .y)
      }
    ) %>%
    dplyr::bind_rows()

  return(res)
}

concat_results_all_collections <- function(list_of_lists) {
  res <- list_of_lists %>%
    purrr::map(
      ~ concat_results_one_collection(.x)
    ) %>%
    # purrr::reduce(rbind)
    return(res)
}


plot_results_all_collections <- function(list_of_lists, ...) {
  args <- list(...)
  # res <- list_of_lists %>%
  #   purrr::map(
  #     ~ plot_results_one_collection(.x, ...)
  #   )

  res <- purrr::map(list_of_lists, function(item) {
    do.call("plot_results_one_collection", c(list(df = item), args))
  })
  return(res)
}

xx_plot_results_one_collection <- function(df, metadata = NULL, cut_by = NULL, limit = 120) {
  if (!"pathway" %in% colnames(df)) {
    stop("pathway column not found")
  }

  if (!"NES" %in% colnames(df)) {
    stop("NES column not found")
  }

  if (!is.null(metadata) && !all(rownames(metadata) %in% df$var)) {
    stop("metadata not aligned with df")
  }

  if (is.null(metadata)) {
    metadata <- data.frame(id = unique(df$var))
  }

  if (!is.null(cut_by) && !cut_by %in% colnames(metadata)) {
    warning("cut_by not found in metadata")
    cut_by <- NULL
  } else if (is.null(cut_by)) {
    cut_by <- NULL
  } else if (cut_by %in% colnames(metadata)) {
    cut_by <- metadata[[cut_by]]
  }




  if (dim(df)[1] > limit) {
    pathways_for_plot <- df %>%
      arrange(-abs(NES)) %>%
      distinct(pathway, .keep_all = T) %>%
      head(limit) %>%
      pull(pathway)

    df <- df %>% dplyr::filter(pathway %in% pathways_for_plot)
  }

  dfp <- df %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = var) %>%
    as.data.frame()
  rownames(dfp) <- dfp$pathway
  dfp["pathway"] <- NULL
  dfp <- dfp[, metadata$id]

  dfp_padj <- df %>%
    pivot_wider(id_cols = pathway, values_from = padj, names_from = var) %>%
    dplyr::select(-pathway) %>%
    dplyr::select(all_of(metadata$id))
  logical_matrix <- dfp_padj < 0.05
  star_matrix <- ifelse(logical_matrix, "*", "")
  star_matrix <- star_matrix[, metadata$id]


  # maxval <- df %>%
  #   pull(NES) %>%
  #   abs() %>%
  #   max(na.rm = T)
  q1 <- quantile(abs(df$NES), 0.99, na.rm = TRUE)

  num_colors <- 11
  my_colors <- colorRampPalette(c("#0000ff", "#8888ffbb", "#ddddff77", "#dddddd", "#ffdddd77", "#ff8888bb", "#ff0000"))(num_colors)
  break_points <- seq(-q1, q1, length.out = num_colors)
  col <- colorRamp2(breaks = break_points, colors = my_colors)


  heatmap_legend_param <- list(
    title = "NES",
    direction = "horizontal",
    just = "bottom",
    legend_width = unit(8, "cm"),
    at = break_points %>% round(1)
  )

  ht <- ComplexHeatmap::Heatmap(
    dfp %>% dplyr::select(!pathway) %>% dplyr::select(all_of(metadata$id)) %>% as.matrix(),
    show_row_names = T,
    col = col,
    heatmap_legend_param = heatmap_legend_param,
    column_split = cut_by,
    row_labels = dfp$pathway,
    show_row_names = T,
    row_names_gp = grid::gpar(fontsize = 8),
    clustering_distance_rows = util_tools$dist_no_na,
    clustering_distance_columns = util_tools$dist_no_na,
    cell_fun = function(j, i, x, y, width, height, fill) {
      row <- i
      col <- j
      value <- star_matrix[row, col]
      grid.text(value, x, y, gp = gpar(fontsize = 12))
    }
  )

  ht <- ComplexHeatmap::draw(ht, heatmap_legend_side = "bottom", padding = unit(c(10, 2, 2, 2), "mm"))
  return(ht)
}


plot_results_one_collection <- function(
    df, metadata = NULL, cut_by = NULL, limit = 120,
    title = "", ...) {
  # Ensure necessary columns are present
  required_cols <- c("pathway", "NES")
  if (!all(required_cols %in% colnames(df))) {
    stop("Required columns not found in dataframe")
  }

  # Align metadata with dataframe
  if (!is.null(metadata)) {
    if (!all(rownames(metadata) %in% df$var)) {
      stop("Metadata not aligned with df")
    }
  } else {
    metadata <- data.frame(id = unique(df$var))
  }

  # Handling cut_by parameter
  if (!is.null(cut_by) && cut_by %in% colnames(metadata)) {
    cut_by <- metadata[[cut_by]]
  } else {
    warning("cut_by not found in metadata, using default")
    cut_by <- NULL
  }

  # Limit the number of pathways if necessary
  if (nrow(df) > limit) {
    top_pathways <- df %>%
      arrange(-abs(NES)) %>%
      slice_head(n = limit) %>%
      pull(pathway)
    df <- df %>% filter(pathway %in% top_pathways)
  }

  # Prepare data for heatmap
  dfp <- df %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = var) %>%
    as.data.frame()

  .df_renamed <- dfp %>%
    dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "REACTOME_"))
  dfp <- .df_renamed
  rownames(dfp) <- dfp$pathway
  dfp <- dfp[, metadata$id]
  dfp["pathway"] <- NULL

  dfp_padj <- df %>%
    pivot_wider(id_cols = pathway, values_from = padj, names_from = var) %>%
    as.data.frame()
  rownames(dfp_padj) <- dfp_padj$pathway
  dfp_padj["pathway"] <- NULL
  dfp_padj <- dfp_padj[, metadata$id]
  # %>% select(-pathway, all_of(metadata$id))
  logical_matrix <- dfp_padj < 0.05
  star_matrix <- ifelse(logical_matrix, "*", "")
  star_matrix <- star_matrix[, metadata$id] # shouldn't be necessary as comes from dfp_padj
  star_matrix[is.na(star_matrix)] <- ""

  # Set up color scale
  q01 <- quantile(abs(df$NES), 0.99, na.rm = TRUE)
  num_colors <- 11
  my_colors <- colorRampPalette(c("#0000ff", "#8888ffbb", "#ddddff77", "#dddddd", "#ffdddd77", "#ff8888bb", "#ff0000"))(num_colors)
  break_points <- seq(-q01, q01, length.out = num_colors)
  col <- colorRamp2(breaks = break_points, colors = my_colors)

  # Construct heatmap

  heatmap_legend_param <- list(
    title = "NES",
    direction = "horizontal",
    just = "bottom",
    legend_width = unit(8, "cm"),
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
    value <- star_matrix[i, j]
    # print(paste("Processing cell:", i, j, "Value:", value))
    # cat(sprintf("NES Cell [%d, %d] with value '%s'\n", i, j, dfp[i, j]))
    # cat(sprintf("star_matrix Cell [%d, %d] with value '%s'\n", i, j, star_matrix    [i, j]))

    if (!is.na(value) && value == "*") {
      # Draw asterisk
      grid::grid.text(value, x, y, gp = grid::gpar(fontsize = 12, col = "black"))
      # Draw border around the cell
      grid::grid.rect(x, y,
        width = width, height = height,
        gp = grid::gpar(col = "black", fill = NA, lwd = 1)
      )
    }
  }

  ht <- ComplexHeatmap::Heatmap(
    dfp %>% as.matrix(),
    col = col,
    cluster_rows = F,
    cluster_columns = F,
    heatmap_legend_param = heatmap_legend_param,
    column_split = cut_by,
    row_labels = rownames(dfp) %>% str_replace_all("_", " ") %>% str_wrap(width = 28),
    row_names_gp = grid::gpar(fontsize = 12),
    column_title_gp = grid::gpar(fontsize = 14),
    clustering_distance_rows = util_tools$dist_no_na,
    clustering_distance_columns = util_tools$dist_no_na,
    clustering_method_rows = "ward.D2",
    clustering_method_columns = "ward.D2",
    column_names_side = "top",
    column_title = title,
    cell_fun = cell_fun # Use the updated cell_fun here
  )

  draw(ht,
    heatmap_legend_side = "bottom",
    padding = unit(c(2, 24, 2, 24), "mm"), # top, left, bottom, right
  )

  return(ht)
}




edgeplot1 <- function(rankorder_edge, ...) {
  rankorder_edge %>%
    filter(!is.na(stat_stat)) %>%
    dim()
  # ggplot(aes(x = rank, y = ES)) +
  #   geom_point()

  posES <- enplot_data$posES
  negES <- enplot_data$negES
  p <- rankorder_edge %>%
    ggplot(aes(x = stat_tick, y = ES, col = rank)) +
    geom_point() +
    scale_color_continuous(type = "viridis", option = "H") +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed")

  p
}


other2 <- function() {
  posES <- enplot_data$posES
  negES <- enplot_data$negES
  rankorder_edge %>%
    ggplot(aes(x = stat_tick, y = ES, col = rank)) +
    geom_point() +
    # scale_color_viridis_c(option = "magma") +
    scale_color_continuous(type = "viridis", option = "H") +
    # scale_color_continuous(type="viridis")+
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed")
}


other3 <- function() {
  with(enplot_data, ggplot(data = stats) +
    geom_line(aes(x = rank, y = stat),
      color = "green"
    ) +
    geom_segment(data = ticks, mapping = aes(
      x = rank,
      y = -spreadES / 16, xend = rank, yend = spreadES / 16
    ), linewidth = ticksSize) +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = maxAbsStat, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = -maxAbsStat, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "black") +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey92")
    ) +
    labs(x = "rank", y = "enrichment score"))
}


plot_table <- function(fgsea_res,
                       ranks,
                       pathways,
                       gsea_param = 1.0,
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
  pathways[top_pathways] %>%
    plotGseaTable(ranks,
      fgsea_res,
      gseaParam = gsea_param
      # gseaParam=1.0
    )
}



plot_pcs <- function(...) {
  # this is non functional at the moment
  library(PCAtools)

  .df <- all_gsea_results$`C5_GO:MF` %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = var) %>%
    as.data.frame()
  rownames(.df) <- .df$pathway %>% str_remove("GOMF_")
  .df$pathway <- NULL
  .metadata <- data.frame(
    row.names = colnames(.df),
    id = colnames(.df),
    group = c(rep("A", 3), rep("B", 3), rep("C", 3))
  )
  .pca_res <- PCAtools::pca(.df, metadata = .metadata)

  (.pcs <- .pca_res$rotated %>% colnames() %>% combn(2) %>% as.data.frame() %>% as.list())
  # could be alot, too many
  .vec <- c("PC1", "PC2", "PC3") # "PC4")
  .pcs <- combn(.vec, 2) %>%
    as.data.frame() %>%
    as.list()
  #
  .pcs %>%
    purrr::map(
      ~ {
        COLBY <- "group"
        # stopifnot(~COLBY%in%colnames(.metadata))
        .x1 <- .x[[1]]
        .x2 <- .x[[2]]

        plt <- PCAtools::biplot(
          .pca_res,
          x = .x1,
          y = .x2,
          showLoadings = T,
          labSize = 2,
          pointSize = 3,
          sizeLoadingsNames = 2,
          colby = COLBY,
          # shape="source",
          legendPosition = "right",
          encircle = T,
        ) #+      coord_equal()

        print(plt)

        # name <- paste(.x1, .x2, sep='_', "withnas")
        # outp <- file.path("./figures", "pca", "biplots_withloadings")
        # if (!fs::dir_exists(outp)) fs::dir_create(outp)
        # outf <- file.path(outp, paste0(name, '.pdf'))
        # pdf(outf, width=9, height=9)
        # print(plt)
        # dev.off()
      }
    )
}
