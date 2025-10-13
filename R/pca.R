# pca.R
suppressPackageStartupMessages(library(PCAtools))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(colorspace))

src_dir <- file.path(here("R"))

fgsea_tools <- new.env()
source(file.path(src_dir, "./fgsea.R"), local = fgsea_tools)


plot_tools <- new.env()
source(file.path(src_dir, "./plot.R"), local = plot_tools)


plot_utils <- new.env()
source(file.path(src_dir, "./plot_utils.R"), local = plot_utils)

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)

make_partial <- util_tools$make_partial
get_args <- util_tools$get_args
get_arg <- util_tools$get_arg
log_msg <- util_tools$make_partial(util_tools$log_msg)

name_cleaner <- function(df){
  df %>%
    dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "REACTOME_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOBP_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOCC_")) %>%
    dplyr::mutate(pathway = str_replace_all(pathway, "_", " ")) %>%
    dplyr::mutate(pathway = str_wrap(pathway, 20))
}

# ==

#' handle 1 long form gsea result table
#'
#' This function runs PCA on a single GSEA result table
#' with columns "pathway", "pval", "padj", "ES", "NES", size, leadingEdge, mainpathway (logical), and rankname
#' pivot on rankname, using NES as the value
#'
#' @param gsea_object
#' @param metadata The metadata to use for the PCA
#' @return The mean of the numeric vector. If the input vector has zero length, the result is NA.
#' @examples
#' # Basic usage
#' gsea_object <- data.frame()
#'
#' # Handling NA values
#' mean_numeric(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#' @export
do_one <- function(
    gsea_object,
    metadata = NULL,
    main_pathway_ratio = 0.1,
    ...) {
  # if ("mainpathway" %in% colnames(gsea_object)) {
  #   gsea_object <- gsea_object %>% filter(mainpathway == TRUE)
  # }

  log_msg(msg = "running pca")

  required_cols <- c("pathway", "NES", "rankname")
  for (col in required_cols) {
    if (!(col %in% colnames(gsea_object))) {
      stop(paste0(col, " column not found in the input data"))
    }
  }

  if (is.null(metadata)) {
    log_msg(msg = "metadata is null, making standin")
    metadata <- data.frame(id = unique(gsea_object$rankname))
    rownames(metadata) <- metadata$id
    metadata$dummy <- "a" ## ?
    log_msg(msg = paste0("metadata is \n", metadata))
  }

  gsea_object <- fgsea_tools$filter_on_mainpathway(gsea_object,
    main_pathway_ratio = main_pathway_ratio
  )
  if (nrow(gsea_object) == 0){return()}

  # clean names
  gsea_object <- gsea_object %>% name_cleaner()
    # dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    # dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    # dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    # dplyr::mutate(pathway = str_remove(pathway, "REACTOME_")) %>%
    # dplyr::mutate(pathway = str_remove(pathway, "GOBP_")) %>%
    # dplyr::mutate(pathway = str_remove(pathway, "GOCC_")) %>%
    # dplyr::mutate(pathway = str_replace_all(pathway, "_", " ")) %>%
    # dplyr::mutate(pathway = str_wrap(pathway, 20))

  wide_df <- gsea_object %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = rankname) %>%
    as.data.frame()
  # set pathway as rowname, remove from columns
  rownames(wide_df) <- wide_df$pathway
  wide_df$pathway <- NULL
  wide_df[is.na(wide_df)] <- 0 #min(wide_df, na.rm = TRUE) # zero because its an NES of zero, not down

  print(colnames(wide_df))
  print(rownames(metadata))
  pca_res <- wide_df %>% PCAtools::pca(metadata = metadata[colnames(wide_df), ])

  return(pca_res)
}

do_all <- function(
    gsea_objects,
    metadata = NULL) {
  pca_objects <- gsea_objects %>%
    purrr::map(~ do_one(.x, metadata = metadata))
  names(pca_objects) <- names(gsea_objects)
  return(pca_objects)
}

plot_biplot <- function(
    pca_object,
    top_pc = 3,
    showLoadings = T,
    labSize = 1.8,
    pointSize = 3,
    sizeLoadingsNames = 2,
    colby = NULL, # or a string like 'group'
    shape = NULL, # or a string like 'group'
    encircle = !is.null(colby),
    title = "",
    ...) {
  args <- list(...)
  if ("save_func" %in% names(args)) {
    save_func <- args$save_func
  } else {
    save_func <- NULL
  }
  if (!is.logical(encircle) || length(encircle) != 1 || is.na(encircle)) {
    stop("`encircle` must be a single logical value (TRUE or FALSE). Received: ", encircle)
  }

  vec <- paste0("PC", 1:top_pc)
  # vec <- c("PC1", "PC2", "PC3") # "PC4")
  pcs <- combn(vec, 2) %>%
    as.data.frame() %>%
    as.list()
  #

    log_msg(info=paste0('colby is: ', colby))
    log_msg(info=paste0('metadata is : ', pca_object$metadata))
  if (!is.null(colby) &&
    !is.null(pca_object$metadata) &&
    !colby %in% colnames(pca_object$metadata)) {
    warning(paste0(colby, " not found in metadata"))
    colby <- NULL
    encircle <- F
  }

  if (!is.null(shape) &&
    !is.null(pca_object$metadata) &&
    !shape %in% colnames(pca_object$metadata)) {
    warning(paste0(shape, " not found in metadata"))
    shape <- NULL
  }

  n_features <- nrow(pca_object$loadings)
  footnote <- paste0("n = ", as.character(n_features))


  plts <- pcs %>%
    purrr::map(
      ~ {
        # stopifnot(~COLBY%in%colnames(.metadata))
        .x1 <- .x[[1]]
        .x2 <- .x[[2]]

        if (!.x1 %in% names(pca_object$rotated)) {
          warning("not enough PCs")
          return()
        }

        if (!.x2 %in% names(pca_object$rotated)) {
          warning("not enough PCs")
          return()
        }

        .max_x <- max(
          pca_object$rotated[[.x1]] %>% abs(),
          pca_object$rotated[[.x2]] %>% abs()
        )
        .max_y <- .max_x

        plt <- PCAtools::biplot(
          pca_object,
          x = .x1,
          y = .x2,
          showLoadings = showLoadings,
          labSize = labSize,
          pointSize = pointSize,
          alphaLoadingsArrow = 0.5,
          sizeLoadingsNames = sizeLoadingsNames,
          colby = colby,
          shape = shape,
          drawConnectorsLoadings = TRUE,
          boxedLoadingsNames = TRUE,
          fillBoxedLoadings = "#ededed99",
          lengthLoadingsArrowsFactor = 1.1,
          # shape="source",
          legendPosition = "right",
          encircle = encircle,
          title = title,
          max.overlaps = Inf,
          maxoverlapsConnectors = Inf,
          ntopLoadings = 5,
        ) +
          coord_fixed(ratio = 1) +
          xlim(-.max_x, .max_x) +
          ylim(-.max_y, .max_y) +
          labs(caption = footnote) +
          geom_hline(yintercept = 0, color = "grey50", show.legend = NA) +
          geom_vline(xintercept = 0, color = "grey50", show.legend = NA) +
          colorspace::scale_color_discrete_qualitative(palette = "Dynamic") +
          colorspace::scale_fill_discrete_qualitative(palette = "Dynamic") +
          scale_shape_manual(values = c(16, 17, 15, 7, 9, 12, 13, 14)) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          )

        if (!is.null(save_func)) {
          current_args <- get_args(save_func)
          filename <- current_args$filename
          if (is.null(filename)) {
            filename <- paste0("pca_biplot_")
          }
          filename <- paste0(filename, "_", .x1, "_", .x2)
          save_func(plot_code = function() print(plt), filename = filename)
        } else {
          print(plt)
        }
        return(plt)
      } # exit inner
    ) # exit outer
  return(plts)
}

plot_all_biplots <- function(
    pca_objects,
    top_pc = 3,
    showLoadings = T,
    labSize = 3,
    pointSize = 4,
    sizeLoadingsNames = 1.75,
    colby = "group",
    shape = NULL,
    fig_width = 8.4,
    fig_height = 7.6,
    save_func = NULL,
    ...) {
  log_msg(debug=paste0('colby equal to : ', colby))
  pca_objects %>%
    purrr::imap(
      ~ {
        pca_object <- .x
        title <- .y
        collection_name <- .y

        plts <- colby %>% purrr::map(
            ~{
              .colby <- .x

              # Additional validation
              if (!is.character(.colby) || length(.colby) != 1) {
                stop(paste0("Invalid .colby value: ", .colby))
              }

              log_msg(msg=paste0('.colby equal to : ', .colby))
              if (!is.null(save_func)) {
              collection_dir <- util_tools$safe_path_component(collection_name)
              save_func <- make_partial(save_func,
                filename = util_tools$safe_filename(
                  "pca_biplot",
                  title,
                  paste0("col", .colby),
                  fallback = "pca_biplot"
                ),
                path = util_tools$safe_subdir(get_arg(save_func, "path"), collection_dir, "pca"),
                width = fig_width, height = fig_height
              )
              }
              p <- tryCatch(
              {
                  plot_biplot(
                    pca_object,
                    top_pc = top_pc,
                    showLoadings = showLoadings,
                    labSize = labSize,
                    pointSize = pointSize,
                    sizeLoadingsNames = sizeLoadingsNames,
                    colby = .colby,
                    shape = shape,
                    title = title,
                    save_func = save_func
                    # ...
                )
              }, error = function(e) {
                log_msg(msg = paste0("error in plot_all_biplots: ", e))
              })
              return(p)
          })
          return(plts)
        }
    )
}

get_top_loadings <- function(pcaobj,
                             components = c("PC1", "PC2", "PC3", "PC4"),
                             rangeRetain = 0.05,
                             limit = Inf
                             ) { # directly lifted from https://github.com/kevinblighe/PCAtools/blob/50f240ba76fe07e2a546998e76dc8aa1c3570693/R/plotloadings.R#L7 and edited
  components <- intersect(components, pcaobj$components)
  x <- pcaobj$loadings[, components, drop = FALSE]
  membership <- list()
  retain <- c()
  # could this be rewritten with tidy group_by and arrange and slice top largest diff?
  for (i in seq_along(components)) {
    # for each PC, based on the loadings range, calculate the rangeRetain
    # fraction of the range
    offset <- (max(x[, i]) - min(x[, i])) * rangeRetain

    # to obtain upper and lower cut-offs, offset max and min by the offset
    uppercutoff <- max(x[, i]) - offset
    lowercutoff <- min(x[, i]) + offset

    # build a consensus list of variables that will be included
    retain_vals <- c(
      which(x[, i] >= uppercutoff),
      which(x[, i] <= lowercutoff)
    ) %>% unique()
    if (limit < Inf) {
      retain_vals <- order(abs(x[retain_vals, i]), decreasing=T) %>% head(n=limit)
    }

    retain <- unique(c(
      retain,
      retain_vals
    ))

    membership[[paste0("PC", i, "_member")]] <- retain_vals
  }
  membership_df <- stack(membership)
  membership_df[["var"]] <- rownames(x)[membership_df$values]
  membership_df[["boolvalue"]] <- !is.na(membership_df$var)
  membership_dfw <- membership_df %>% pivot_wider(id_cols = c("values", "var"), names_from = "ind", values_from = "boolvalue")
  membership_dfw[is.na(membership_dfw)] <- FALSE

  message("-- variables retained:")
  message(paste0(rownames(x)[retain], collapse = ", "))
  x_final <- x[retain, , drop = FALSE]
  # create a plot object (2-col df) of PC names and
  # explained variance of each
  x_final <- data.frame(rownames(x_final), x_final[, components, drop = FALSE])
  colnames(x_final)[1] <- "var"

  x_final <- x_final %>% left_join(membership_dfw)
  x_final$values <- NULL
  x_final %<>% as.data.frame
  rownames(x_final) <- x_final$var
  return(x_final)
}


make_enplots_from_loadings <- function(){
  stop("not implemented")
}

make_heatmap_from_loadings <- function(
    gsea_object,
    pca_object,
    components = c("PC1", "PC2", "PC3", "PC4"),
    save_func = NULL,
    cut_by = NULL,
    cluster_rows = TRUE,
    cluster_columns = FALSE,
    meta_to_include = NULL,
    meta_to_exclude = NULL,
    ...){


  components <- intersect(components, pca_object$components) # there may be less than 4 components
  top_loadings <- get_top_loadings(pca_object, components = components, rangeRetain = 0.05, limit = Inf)
  submat <- gsea_object %>% name_cleaner() %>% dplyr::filter(pathway %in% rownames(top_loadings))

  ra <- ComplexHeatmap::rowAnnotation(
    PC1 = top_loadings$PC1_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC2 = top_loadings$PC2_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC3 = top_loadings$PC3_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC4 = top_loadings$PC4_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white"))
  )


  maybe_metadata <- pca_object$metadata
  if ("dummy" %in% colnames(maybe_metadata)) {
    maybe_metadata <- NULL
  }

  param_grid <- expand.grid(
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    cut_by = unique(c(cut_by, NA)),
    stringsAsFactors = FALSE
  )

  param_grid %>% purrr::pmap(~{

    params = list(...)
    .cut_by <- params$cut_by
    .cut_by_val <- plot_utils$process_cut_by(.cut_by, pca_object$metadata)
    cut_by_label <- if (!is.null(.cut_by_val)) {
      paste0("cut_", util_tools$safe_path_component(.cut_by, max_chars = 32))
    } else {
      ""
    }
    cluster_rows <- params$cluster_rows
    cluster_columns <- params$cluster_columns

    .save_func <- NULL
    if (!is.null(save_func)) {
      filename <- util_tools$safe_filename(
        get_arg(save_func, "filename"),
        "gsea_heatmap_5pct",
        util_tools$cluster_flag_token(cluster_rows, "rc"),
        util_tools$cluster_flag_token(cluster_columns, "cc"),
        cut_by_label,
        fallback = "gsea_heatmap"
      )
        .save_func <- make_partial(save_func, filename = filename)
      }

    tryCatch({
      ht <- plot_tools$plot_results_one_collection(
        df = submat,
        metadata = pca_object$metadata, # guaranteed to match the colnames of df which was used originally to make the pca obj
        # pathway_metadata = loadings,
        row_annotation = ra,
        limit = Inf,
        pstat_cutoff = 1, # we want no filtering
        title = "Top 5% Loadings",
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        cut_by = .cut_by,
        save_func = .save_func,
      )}, error = function(msg) { log_msg(error=msg) }
    ) # end of tryCatch
  }) # end of pmap


  top_loadings <- get_top_loadings(pca_object, components = components, rangeRetain = 1, limit = 5)
  submat <- gsea_object %>% name_cleaner() %>% dplyr::filter(pathway %in% rownames(top_loadings))
  submat %<>% mutate(pathway = factor(pathway, levels = rownames(top_loadings), ordered=TRUE)) %>% arrange(pathway)

  ra <- ComplexHeatmap::rowAnnotation(
    PC1 = top_loadings$PC1_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC2 = top_loadings$PC2_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC3 = top_loadings$PC3_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white")),
    PC4 = top_loadings$PC4_member %>% as.character() %>% anno_simple(col = c("TRUE" = "black", "FALSE" = "white"))
  )


  param_grid %>% purrr::pmap(~{

    params = list(...)
    .cut_by <- params$cut_by
    .cut_by_val <- plot_utils$process_cut_by(.cut_by, pca_object$metadata)
    cut_by_label <- if (!is.null(.cut_by_val)) {
      paste0("cut_", util_tools$safe_path_component(.cut_by, max_chars = 32))
    } else {
      ""
    }
    cluster_rows <- params$cluster_rows
    cluster_columns <- params$cluster_columns

    .save_func <- NULL
    if (!is.null(save_func)) {
      filename <- util_tools$safe_filename(
        get_arg(save_func, "filename"),
        "gsea_heatmap_top5",
        util_tools$cluster_flag_token(cluster_rows, "rc"),
        util_tools$cluster_flag_token(cluster_columns, "cc"),
        cut_by_label,
        fallback = "gsea_heatmap"
      )
        .save_func <- make_partial(save_func, filename = filename)
      }


    tryCatch({
      ht <- plot_tools$plot_results_one_collection(
        df = submat,
        metadata = pca_object$metadata, # guaranteed to match the colnames of df which was used originally to make the pca obj
        # pathway_metadata = loadings,
        row_annotation = ra,
        limit = Inf,
        pstat_cutoff = 1, # we want no filtering
        title = "Top 5 Loadings Per PC",
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        cut_by = .cut_by,
        save_func = .save_func,
      )}, error = function(msg) { log_msg(error=msg) }
    ) # end of tryCatch
  }) # end of pmap



  # param_grid %>% purrr:pmap(~{
  #   params <- list(...)
  #   tryCatch({
  #     ht <- plot_tools$plot_results_one_collection(
  #       df = submat,
  #       metadata = maybe_metadata, # guaranteed to match the colnames of df which was used originally to make the pca obj
  #       # pathway_metadata = loadings,
  #       row_annotation = ra,
  #       limit = Inf,
  #       pstat_cutoff = 1, # we want no filtering
  #       save_func = save_func,
  #       title = "Top 5 Loadings Per PC",
  #       ...
  #       )}, error = function(msg){ log_msg(error=msg) }
  #   ) # end of tryCatch
  # })

}
