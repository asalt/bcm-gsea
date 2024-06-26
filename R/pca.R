# pca.R
library(PCAtools)
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)

src_dir <- file.path(here("R"))

fgsea_tools <- new.env()
source(file.path(src_dir, "./fgsea.R"), local = fgsea_tools)

# plot_tools <- new.env()
# source(file.path(src_dir, "./plot.R"), local = plot_tools)


plot_utils <- new.env()
source(file.path(src_dir, "./plot_utils.R"), local = plot_utils)
make_partial <- plot_utils$make_partial
get_args <- plot_utils$get_args


#' handle 1 long form gsea result table
#'
#' This function runs PCA on a single GSEA result table
#' with columns "pathway", "pval", "padj", "ES", "NES", size, leadingEdge, mainpathway (logical), and var
#' pivot on var, using NES as the value
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

  required_cols <- c("pathway", "NES", "var")
  for (col in required_cols) {
    if (!(col %in% colnames(gsea_object))) {
      stop(paste0(col, " column not found in the input data"))
    }
  }

  if (is.null(metadata)) {
    metadata <- data.frame(id = unique(gsea_object$var))
    rownames(metadata) <- metadata$id
  }

  gsea_object <- fgsea_tools$filter_on_mainpathway(gsea_object,
    main_pathway_ratio = main_pathway_ratio
  )

  # clean names
  gsea_object <- gsea_object %>%
    dplyr::mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    dplyr::mutate(pathway = str_remove(pathway, "REACTOME_"))

  wide_df <- gsea_object %>%
    pivot_wider(id_cols = pathway, values_from = NES, names_from = var) %>%
    as.data.frame()
  # set pathway as rowname, remove from columns
  rownames(wide_df) <- wide_df$pathway
  wide_df$pathway <- NULL
  wide_df[is.na(wide_df)] <- 0

  pca_res <- wide_df %>% PCAtools::pca(metadata = metadata)

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
    labSize = 2,
    pointSize = 3,
    sizeLoadingsNames = 2,
    colby = NULL, # or a string like 'group'
    shape = NULL, # or a string like 'group'
    encircle = ifelse(!is.null(colby), T, F),
    title = "",
    ...) {
  args <- list(...)
  if ("save_func" %in% names(args)) {
    save_func <- args$save_func
  } else {
    save_func <- NULL
  }

  vec <- paste0("PC", 1:top_pc)
  # vec <- c("PC1", "PC2", "PC3") # "PC4")
  pcs <- combn(vec, 2) %>%
    as.data.frame() %>%
    as.list()
  #
  if (!is.null(colby) &&
    !is.null(pca_object$metadata) &&
    !colby %in% colnames(pca_object$metadata)) {
    warning(paste0(colby, " not found in metadata"))
    colby <- NULL
    encircle <- F
  }
  plts <- pcs %>%
    purrr::map(~ {
      # stopifnot(~COLBY%in%colnames(.metadata))
      .x1 <- .x[[1]]
      .x2 <- .x[[2]]

      if (!.x1 %in% names(pca_object$rotated)){
        warning("not enough PCs")
        return()
    }

      if (!.x2 %in% names(pca_object$rotated)){
        warning("not enough PCs")
        return()
    }

    plt <- PCAtools::biplot(
      pca_object,
      x = .x1,
      y = .x2,
      showLoadings = showLoadings,
      labSize = labSize,
      pointSize = pointSize,
      sizeLoadingsNames = sizeLoadingsNames,
      colby = colby,
      shape=shape,
      # shape="source",
      legendPosition = "right",
      encircle = encircle,
      title = title,
      max.overlaps = Inf,
      maxoverlapsConnectors = Inf,
      ntopLoadings=5,
    ) #+      coord_equal()

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
  })
  return(plts)
}

plot_all_biplots <- function(
    pca_objects,
    top_pc = 3,
    showLoadings = T,
    labSize = 2,
    pointSize = 3,
    sizeLoadingsNames = 2,
    colby = "group",
    save_func = NULL,
    ...) {
  pca_objects %>%
    purrr::imap(
      ~ {
        title <- .y

        if (!is.null(save_func)) {
          save_func <- make_partial(save_func,
            filename = paste0("pca_biplot_", make.names(title))
          )
        }

        plot_biplot(.x,
          top_pc = top_pc,
          showLoadings = showLoadings,
          labSize = labSize,
          pointSize = pointSize,
          sizeLoadingsNames = sizeLoadingsNames,
          colby = colby,
          title = title,
          save_func = save_func,
          # ...
        )
      }
    )
}
