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

run_gene_umap_pipeline <- function(gct, params, savedir, replace = TRUE, cachedir = NULL, ranks = NULL) {
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
  rownames(expr) <- gct@rid[keep_genes]
  colnames(expr) <- colnames(gct@mat)

  if (isTRUE(params$scale)) {
    expr <- .standardise_expression(expr)
  }

  sample_ids <- colnames(expr)
  sample_metadata <- as.data.frame(gct@cdesc, stringsAsFactors = FALSE)
  sample_metadata <- sample_metadata[sample_ids, , drop = FALSE]
  if (ncol(sample_metadata) == 0) {
    sample_metadata <- data.frame(id = sample_ids, stringsAsFactors = FALSE)
  }
  rownames(sample_metadata) <- sample_ids
  if (!"id" %in% colnames(sample_metadata)) {
    sample_metadata$id <- sample_ids
  }

  gene_ids <- rownames(expr)
  gene_metadata <- as.data.frame(gct@rdesc, stringsAsFactors = FALSE)
  gene_metadata <- gene_metadata[gene_ids, , drop = FALSE]
  if (ncol(gene_metadata) == 0) {
    gene_metadata <- data.frame(id = gene_ids, stringsAsFactors = FALSE)
  }
  rownames(gene_metadata) <- gene_ids
  if (!"id" %in% colnames(gene_metadata)) {
    gene_metadata$id <- gene_ids
  }

  contexts <- list(
    gene = list(
      matrix = expr,
      base_metadata = gene_metadata,
      id_label = "gene"
    ),
    sample = list(
      matrix = t(expr),
      base_metadata = sample_metadata,
      id_label = "sample"
    )
  )

  base_dir <- util_tools$safe_subdir(savedir, "umap_gene")
  variants <- resolve_umap_variants(params, params$variants %||% list())
  embeddings <- list()

  for (variant in variants) {
    orientation <- tolower(variant$point_type %||% params$point_type %||% "gene")
    if (!orientation %in% names(contexts)) {
      log_msg(warning = paste0("UMAP: point_type '", orientation, "' is not supported; skipping variant."))
      next
    }

    ctx <- contexts[[orientation]]
    embedding_input <- ctx$matrix
    point_ids <- rownames(embedding_input)
    metadata <- ctx$base_metadata[point_ids, , drop = FALSE]
    rownames(metadata) <- point_ids
    if (!"id" %in% colnames(metadata)) {
      metadata$id <- point_ids
    }

    rank_column_name <- NULL
    if (orientation == "gene") {
      rank_name_variant <- variant$rank_name %||% params$rank_name %||% ""
      rank_name_variant <- trimws(rank_name_variant)
      if (length(rank_name_variant) > 0 && nzchar(rank_name_variant)) {
        rank_column_name <- paste0("rank_", util_tools$safe_path_component(rank_name_variant, max_chars = 40))
        rank_values_col <- rep(NA_real_, length(point_ids))
        if (is.null(ranks) || length(ranks) == 0) {
          log_msg(warning = paste0("UMAP: ranks not available; cannot colour by '", rank_name_variant, "'."))
        } else {
          available_ranks <- names(ranks)
          match_idx <- which(available_ranks == rank_name_variant)
          if (length(match_idx) == 0) {
            match_idx <- which(tolower(available_ranks) == tolower(rank_name_variant))
          }
          if (length(match_idx) == 0) {
            log_msg(warning = paste0(
              "UMAP: rank_name '", rank_name_variant, "' not found; available: ",
              paste(available_ranks, collapse = ", "), "."
            ))
          } else {
            rank_values <- ranks[[match_idx[[1]]]]
            rank_names <- names(rank_values)
            if (is.null(rank_names)) {
              log_msg(warning = paste0(
                "UMAP: rank '", rank_name_variant, "' lacks gene identifiers; colouring skipped."
              ))
            } else {
              if (!is.numeric(rank_values)) {
                rank_numeric <- suppressWarnings(as.numeric(rank_values))
                names(rank_numeric) <- rank_names
              } else {
                rank_numeric <- rank_values
              }
              mapping <- rank_numeric[match(point_ids, names(rank_numeric))]
              if (all(is.na(mapping))) {
                alt_columns <- intersect(
                  c("GeneSymbol", "gene_symbol", "Symbol", "symbol", "Gene", "gene", "id"),
                  colnames(metadata)
                )
                for (alt_col in alt_columns) {
                  alt_ids <- as.character(metadata[[alt_col]])
                  alt_map <- rank_numeric[match(alt_ids, names(rank_numeric))]
                  if (!all(is.na(alt_map))) {
                    mapping <- alt_map
                    log_msg(info = paste0(
                      "UMAP: used metadata column '", alt_col, "' to map ranks for '", rank_name_variant, "'."
                    ))
                    break
                  }
                }
              }
              if (all(is.na(mapping))) {
                log_msg(warning = paste0(
                  "UMAP: no overlap between rank '", rank_name_variant, "' and gene identifiers; colouring skipped."
                ))
              } else {
                rank_values_col <- mapping
              }
            }
          }
        }
        metadata[[rank_column_name]] <- rank_values_col
      } else {
        rank_column_name <- NULL
      }
    }

    requested_colors <- variant$metadata_color
    if (orientation == "gene" && !is.null(rank_column_name) && nzchar(rank_column_name)) {
      requested_colors <- unique(c(requested_colors, rank_column_name))
    }
    variant_colors <- .prepare_umap_metadata(metadata, requested_colors)

    variant_shape <- variant$metadata_shape
    if (!is.null(variant_shape) && (!nzchar(variant_shape) || !variant_shape %in% colnames(metadata))) {
      if (nzchar(variant_shape) && !variant_shape %in% colnames(metadata)) {
        log_msg(warning = paste0("UMAP: metadata_shape '", variant_shape, "' not found for variant; ignoring."))
      }
      variant_shape <- NULL
    }

    cache_key <- hash_umap(embedding_input, variant, variant_colors, variant_shape)
    cached <- load_cached_umap(cachedir, cache_key)
    if (!is.null(cached)) {
      embedding_df_variant <- cached
      if (!identical(colnames(embedding_df_variant)[1], ctx$id_label)) {
        colnames(embedding_df_variant)[1] <- ctx$id_label
      }
    } else {
      set.seed(variant$seed)
      embedding_variant <- uwot::umap(
        embedding_input,
        n_neighbors = variant$n_neighbors,
        min_dist = variant$min_dist,
        metric = variant$metric,
        verbose = FALSE,
        ret_model = FALSE
      )
      embedding_df_variant <- cbind(
        data.frame(
          point_id = point_ids,
          UMAP1 = embedding_variant[, 1],
          UMAP2 = embedding_variant[, 2],
          stringsAsFactors = FALSE
        ),
        metadata[point_ids, , drop = FALSE]
      )
      colnames(embedding_df_variant)[1] <- ctx$id_label
      write_cached_umap(cachedir, cache_key, embedding_df_variant)
    }

    variant_dir_name <- variant_label(variant)
    if (!grepl(orientation, variant_dir_name, fixed = TRUE)) {
      variant_dir_name <- util_tools$safe_path_component(paste0(variant_dir_name, "_", orientation))
    }
    variant_dir <- util_tools$safe_subdir(base_dir, variant_dir_name)
    plot_base <- util_tools$make_partial(
      plot_utils$plot_and_save,
      path = util_tools$safe_subdir(variant_dir, "plots"),
      replace = replace,
      width = variant$width,
      height = variant$height
    )

    base_title <- if (orientation == "gene") "Gene Feature UMAP" else "Sample Expression UMAP"
    variant_name <- variant$variant_name %||% "default"
    for (color_col in variant_colors) {
      colour_title <- if (is.na(color_col)) "none" else color_col
      plot_title <- paste0(
        base_title,
        if (!identical(variant_name, "default")) paste0(" (", variant_name, ")") else "",
        if (is.na(color_col)) "" else paste0(" - ", color_col)
      )
      plot_fun <- make_partial(
        plot_base,
        filename = util_tools$safe_filename("gene_umap", orientation, variant_name, colour_title)
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
    table_filename <- paste0(orientation, "_umap_embedding.tsv")
    readr::write_tsv(
      embedding_df_variant,
      fs::path(tables_dir, table_filename)
    )
    log_msg(info = paste0("Gene UMAP outputs written to ", variant_dir))

    embeddings[[variant_dir_name]] <- embedding_df_variant
  }

  invisible(embeddings)
}
