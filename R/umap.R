suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(uwot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(readr))

source(file.path(here("R"), "lazyloader.R"))

plot_utils <- get_tool_env("plot_utils")
util_tools <- get_tool_env("utils")
log_msg <- util_tools$make_partial(util_tools$log_msg)
make_partial <- util_tools$make_partial
get_args <- util_tools$get_args

.prepare_umap_metadata <- function(metadata, requested) {
  requested <- requested[requested %in% colnames(metadata)]
  missing <- setdiff(requested, colnames(metadata))
  if (length(missing) > 0) {
    log_msg(warning = paste0("UMAP: metadata_color entries not found; ignoring: ", paste(missing, collapse = ", ")))
  }
  if (length(requested) == 0) {
    requested <- NA_character_
  }
  requested
}

.standardise_expression <- function(expr, center = TRUE, scale = TRUE) {
  scaled <- t(scale(t(expr), center = center, scale = scale))
  scaled[!is.finite(scaled)] <- 0
  scaled
}

.plot_umap_scatter <- function(df, color_col, shape_col, title, save_func) {
  aes_args <- list(x = df$UMAP1, y = df$UMAP2)
  df_plot <- df

  colour_label <- if (is.null(color_col)) "none" else color_col
  shape_label <- if (is.null(shape_col)) "none" else shape_col

  if (!is.null(color_col)) {
    if (is.numeric(df_plot[[color_col]])) {
      aes_args$colour <- df_plot[[color_col]]
    } else {
      df_plot[[color_col]] <- as.factor(df_plot[[color_col]])
      aes_args$colour <- df_plot[[color_col]]
    }
  }

  if (!is.null(shape_col)) {
    df_plot[[shape_col]] <- as.factor(df_plot[[shape_col]])
    aes_args$shape <- df_plot[[shape_col]]
  }

  plt <- ggplot(df_plot, do.call(aes, aes_args)) +
    geom_point(size = 2.4, alpha = 0.85) +
    theme_minimal(base_size = 12) +
    labs(
      x = "UMAP1",
      y = "UMAP2",
      title = title,
      colour = if (!is.null(color_col)) color_col else NULL,
      shape = if (!is.null(shape_col)) shape_col else NULL
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  save_func(
    plot_code = function() print(plt),
    filename = util_tools$safe_filename("gene_umap", colour_label, shape_label),
    width = get_args(save_func)$width,
    height = get_args(save_func)$height
  )
}

run_gene_umap_pipeline <- function(gct, params, savedir, replace = TRUE) {
  if (is.null(gct) || ncol(gct@mat) < 2) {
    log_msg(warning = "Gene UMAP skipped: GCT is NULL or has fewer than 2 samples.")
    return(NULL)
  }

  expr <- gct@mat
  if (!is.matrix(expr)) {
    expr <- as.matrix(expr)
  }

  variance <- apply(expr, 1, stats::var, na.rm = TRUE)
  keep_genes <- which(!is.na(variance) & variance > 0)
  if (length(keep_genes) < 2) {
    log_msg(warning = "Gene UMAP skipped: fewer than two genes with non-zero variance.")
    return(NULL)
  }
  expr <- expr[keep_genes, , drop = FALSE]

  if (isTRUE(params$scale)) {
    expr <- .standardise_expression(expr)
  }

  expr_t <- t(expr)
  sample_ids <- rownames(expr_t)
  metadata <- as.data.frame(gct@cdesc, stringsAsFactors = FALSE)
  metadata <- metadata[sample_ids, , drop = FALSE]
  rownames(metadata) <- sample_ids

  colour_columns <- .prepare_umap_metadata(metadata, params$metadata_color)

  metadata_shape <- params$metadata_shape
  if (!is.null(metadata_shape) && (!nzchar(metadata_shape) || !metadata_shape %in% colnames(metadata))) {
    if (nzchar(metadata_shape) && !metadata_shape %in% colnames(metadata)) {
      log_msg(warning = paste0("UMAP: metadata_shape '", metadata_shape, "' not found; ignoring."))
    }
    metadata_shape <- NULL
  }

  set.seed(params$seed)
  embedding <- uwot::umap(
    expr_t,
    n_neighbors = params$n_neighbors,
    min_dist = params$min_dist,
    metric = params$metric,
    verbose = FALSE,
    ret_model = FALSE
  )

  embedding_df <- data.frame(
    sample = sample_ids,
    UMAP1 = embedding[, 1],
    UMAP2 = embedding[, 2],
    metadata[, , drop = FALSE],
    row.names = NULL,
    check.names = FALSE
  )

  base_dir <- util_tools$safe_subdir(savedir, "umap_gene")
  plot_base <- util_tools$make_partial(
    plot_utils$plot_and_save,
    path = util_tools$safe_subdir(base_dir, "plots"),
    replace = replace,
    width = params$width,
    height = params$height
  )

  for (color_col in colour_columns) {
    colour_title <- if (is.na(color_col)) "none" else color_col
    plot_title <- paste0("Gene Expression UMAP", if (is.na(color_col)) "" else paste0(" - ", color_col))
    plot_fun <- make_partial(
      plot_base,
      filename = util_tools$safe_filename("gene_umap", colour_title)
    )
    .plot_umap_scatter(
      embedding_df,
      color_col = if (is.na(color_col)) NULL else color_col,
      shape_col = metadata_shape,
      title = plot_title,
      save_func = plot_fun
    )
  }

  tables_dir <- util_tools$safe_subdir(base_dir, "tables")
  fs::dir_create(tables_dir, recurse = TRUE)
  readr::write_tsv(
    embedding_df,
    fs::path(tables_dir, "gene_umap_embedding.tsv")
  )

  log_msg(info = paste0("Gene UMAP outputs written to ", base_dir))

  invisible(embedding_df)
}
