suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(uwot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(digest))

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

resolve_umap_variants <- function(base_params, variants) {
  normalized <- list()

  base <- base_params
  base$variants <- NULL
  base$variant_name <- base$name %||% "default"
  normalized[[1]] <- base

  if (length(variants) == 0) {
    return(normalized)
  }

  for (idx in seq_along(variants)) {
    override <- variants[[idx]]
    if (!is.list(override)) {
      next
    }
    variant <- modifyList(base, override)
    variant$variant_name <- variant$name %||% paste0("variant", idx)
    variant$variants <- NULL
    normalized[[length(normalized) + 1]] <- variant
  }
  normalized
}

variant_label <- function(params) {
  if (!is.null(params$variant_name)) {
    return(util_tools$safe_path_component(params$variant_name))
  }
  util_tools$safe_path_component(
    paste0(
      "nn", params$n_neighbors,
      "_min", format(params$min_dist, digits = 2, trim = TRUE),
      "_", params$metric,
      "_sc", if (isTRUE(params$scale)) "T" else "F",
      "_seed", params$seed
    )
  )
}

hash_umap <- function(expr_matrix, params, color_cols, shape_col) {
  digest::digest(list(
    type = "umap",
    rows = dim(expr_matrix)[1],
    cols = dim(expr_matrix)[2],
    expr_hash = digest::digest(expr_matrix),
    params = list(
      n_neighbors = params$n_neighbors,
      min_dist = params$min_dist,
      metric = params$metric,
      seed = params$seed,
      scale = params$scale
    ),
    color_cols = color_cols,
    shape = shape_col
  ))
}

load_cached_umap <- function(cache_dir, key) {
  if (is.null(cache_dir)) {
    return(NULL)
  }
  target <- file.path(cache_dir, paste0("umap_", key, ".rds"))
  if (!fs::file_exists(target)) {
    return(NULL)
  }
  readRDS(target)
}

write_cached_umap <- function(cache_dir, key, object) {
  if (is.null(cache_dir)) {
    return(invisible(NULL))
  }
  if (!fs::dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  }
  target <- file.path(cache_dir, paste0("umap_", key, ".rds"))
  saveRDS(object, target)
}

run_gene_umap_pipeline <- function(gct, params, savedir, replace = TRUE, cachedir = NULL) {
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

  base_dir <- util_tools$safe_subdir(savedir, "umap_gene")
  variants <- resolve_umap_variants(params, params$variants %||% list())
  embeddings <- list()

  for (variant in variants) {
    variant_dir <- util_tools$safe_subdir(base_dir, variant_label(variant))
    plot_base <- util_tools$make_partial(
      plot_utils$plot_and_save,
      path = util_tools$safe_subdir(variant_dir, "plots"),
      replace = replace,
      width = variant$width,
      height = variant$height
    )

    variant_colors <- .prepare_umap_metadata(metadata, variant$metadata_color)
    variant_shape <- variant$metadata_shape
    if (!is.null(variant_shape) && (!nzchar(variant_shape) || !variant_shape %in% colnames(metadata))) {
      if (nzchar(variant_shape) && !variant_shape %in% colnames(metadata)) {
        log_msg(warning = paste0("UMAP: metadata_shape '", variant_shape, "' not found for variant; ignoring."))
      }
      variant_shape <- NULL
    }

    cache_key <- hash_umap(expr_t, variant, variant_colors, variant_shape)
    cached <- load_cached_umap(cachedir, cache_key)
    if (!is.null(cached)) {
      embedding_df_variant <- cached
    } else {
      set.seed(variant$seed)
      embedding_variant <- uwot::umap(
        expr_t,
        n_neighbors = variant$n_neighbors,
        min_dist = variant$min_dist,
        metric = variant$metric,
        verbose = FALSE,
        ret_model = FALSE
      )
      embedding_df_variant <- data.frame(
        sample = sample_ids,
        UMAP1 = embedding_variant[, 1],
        UMAP2 = embedding_variant[, 2],
        metadata[, , drop = FALSE],
        row.names = NULL,
        check.names = FALSE
      )
      write_cached_umap(cachedir, cache_key, embedding_df_variant)
    }

    for (color_col in variant_colors) {
      colour_title <- if (is.na(color_col)) "none" else color_col
      plot_title <- paste0(
        "Gene Expression UMAP",
        if (variant$variant_name != "default") paste0(" (", variant$variant_name, ")") else "",
        if (is.na(color_col)) "" else paste0(" - ", color_col)
      )
      plot_fun <- make_partial(
        plot_base,
        filename = util_tools$safe_filename("gene_umap", variant$variant_name, colour_title)
      )
      .plot_umap_scatter(
        embedding_df_variant,
        color_col = if (is.na(color_col)) NULL else color_col,
        shape_col = variant_shape,
        title = plot_title,
        save_func = plot_fun
      )
    }

    tables_dir <- util_tools$safe_subdir(variant_dir, "tables")
    fs::dir_create(tables_dir, recurse = TRUE)
    readr::write_tsv(
      embedding_df_variant,
      fs::path(tables_dir, "gene_umap_embedding.tsv")
    )
    log_msg(info = paste0("Gene UMAP outputs written to ", variant_dir))

    embeddings[[variant_label(variant)]] <- embedding_df_variant
  }

  invisible(embeddings)
}
