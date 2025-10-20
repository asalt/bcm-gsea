suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(limma))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(splines))
suppressPackageStartupMessages(library(ggrepel))

source(file.path(here("R"), "lazyloader.R"))

util_tools <- get_tool_env("utils")
log_msg <- util_tools$make_partial(util_tools$log_msg)

compute_model_cache_key <- function(
    gct_path,
    design,
    contrasts,
    sample_exclude,
    exclude_samples_from_data,
    model_name,
    model_index,
    model_file = NULL,
    volcano_cutoff = 0.05,
    volcano_top_n = 35) {
  info <- tryCatch(fs::file_info(gct_path), error = function(e) NULL)
  sample_exclude <- sort(unique(as.character(sample_exclude %||% character(0))))
  digest::digest(
    list(
      version = 1L,
      gct_path = normalizePath(gct_path, winslash = "/", mustWork = FALSE),
      gct_size = if (!is.null(info)) as.numeric(info$size) else NA_real_,
      gct_mtime = if (!is.null(info)) as.numeric(info$modification_time) else NA_real_,
      design = design,
      contrasts = unname(unlist(contrasts)),
      sample_exclude = sample_exclude,
      exclude_samples_from_data = isTRUE(exclude_samples_from_data),
      model_name = model_name,
      model_index = model_index,
      model_file = model_file %||% "",
      volcano_cutoff = volcano_cutoff,
      volcano_top_n = volcano_top_n
    ),
    algo = "xxhash64"
  )
}

load_cached_model_results <- function(cache_dir, cache_key, logger = NULL) {
  if (is.null(cache_dir) || !nzchar(cache_dir) || is.null(cache_key)) {
    return(NULL)
  }
  target <- fs::path(cache_dir, paste0("model_", cache_key, ".rds"))
  if (!fs::file_exists(target)) {
    if (!is.null(logger)) {
      logger(debug = paste0("Model cache miss: ", target))
    }
    return(NULL)
  }
  if (!is.null(logger)) {
    logger(info = paste0("Model cache hit: ", target))
  }
  readRDS(target)
}

write_cached_model_results <- function(cache_dir, cache_key, results, logger = NULL) {
  if (is.null(cache_dir) || !nzchar(cache_dir) || is.null(cache_key)) {
    return(invisible(NULL))
  }
  if (!fs::dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  }
  target <- fs::path(cache_dir, paste0("model_", cache_key, ".rds"))
  saveRDS(results, target)
  if (!is.null(logger)) {
    logger(info = paste0("Saved model cache: ", target))
  }
}

prettify_term_label <- function(original_term, sanitized_term) {
  sanitized_term <- sanitized_term %||% ""
  candidate <- original_term %||% ""
  candidate <- gsub("`", "", candidate)
  candidate <- trimws(candidate)
  if (!nzchar(candidate)) {
    candidate <- sanitized_term
  }

  hi_pattern <- "^factor\\(I\\(([A-Za-z0-9._-]+)\\s*>\\s*median\\(([^)]+)\\)\\)\\)(TRUE|1)$"
  hi_match <- regmatches(candidate, regexec(hi_pattern, candidate))[[1]]
  if (length(hi_match) >= 2) {
    gene <- hi_match[2]
    return(paste0(gene, "_hi_indicator"))
  }

  lo_pattern <- "^factor\\(I\\(([A-Za-z0-9._-]+)\\s*>\\s*median\\(([^)]+)\\)\\)\\)(FALSE|0)$"
  lo_match <- regmatches(candidate, regexec(lo_pattern, candidate))[[1]]
  if (length(lo_match) >= 2) {
    gene <- lo_match[2]
    return(paste0(gene, "_lo_indicator"))
  }

  factor_pattern <- "^factor\\(([^)]+)\\)([A-Za-z0-9._-]+)$"
  factor_match <- regmatches(candidate, regexec(factor_pattern, candidate))[[1]]
  if (length(factor_match) >= 3) {
    base <- gsub("\\s+", "_", factor_match[2])
    level <- gsub("\\s+", "_", factor_match[3])
    return(paste(base, level, sep = "_"))
  }

  label <- candidate
  replacements <- c(
    "factor\\(" = "",
    "I\\(" = "",
    "\\)" = "",
    ":" = "_by_",
    "~" = "_tilde_",
    "," = "_",
    "\\+" = "_plus_",
    "-" = "_minus_",
    "\\*" = "_times_",
    ">" = "_gt_",
    "<" = "_lt_",
    "=" = "_eq_"
  )
  for (pattern in names(replacements)) {
    label <- gsub(pattern, replacements[[pattern]], label)
  }
  label <- gsub("\\s+", "_", label)
  label <- gsub("__+", "_", label)
  label <- gsub("^_", "", label)
  label <- gsub("_$", "", label)
  if (!nzchar(label)) {
    label <- sanitized_term
  }
  label
}

append_metadata_to_gct <- function(gct, metadata_values, output_dir, model_label, replace = FALSE, logger = NULL) {
  if (!length(metadata_values) || is.null(output_dir) || !nzchar(output_dir)) {
    return(invisible(NULL))
  }

  sample_ids <- gct@cid
  augmented <- gct
  for (name in names(metadata_values)) {
    values <- metadata_values[[name]]
    aligned <- rep(NA_real_, length(sample_ids))
    names(aligned) <- sample_ids
    if (!is.null(names(values))) {
      overlap <- intersect(names(values), sample_ids)
      aligned[overlap] <- as.numeric(values[overlap])
    } else if (length(values) == length(sample_ids)) {
      aligned <- as.numeric(values)
    } else {
      aligned[] <- as.numeric(values)
    }
    augmented@cdesc[[name]] <- aligned
  }

  metadata_dir <- fs::path(output_dir, "metadata")
  fs::dir_create(metadata_dir, recurse = TRUE)
  filename <- paste0(util_tools$safe_filename(model_label, fallback = "model"), "_annotated.gct")
  out_path <- fs::path(metadata_dir, filename)
  if (!replace && fs::file_exists(out_path)) {
    if (!is.null(logger)) {
      logger(info = paste0("Metadata GCT exists, skipping write: ", out_path))
    }
    return(invisible(out_path))
  }
  cmapR::write_gct(augmented, out_path, appenddim = FALSE)
  if (!is.null(logger)) {
    logger(info = paste0("Wrote metadata-augmented GCT: ", out_path))
  }
  invisible(out_path)
}

persist_model_outputs <- function(
    results,
    output_dir,
    replace,
    model_label,
    logger = NULL,
    sig_cutoff = 0.05,
    label_top_n = 35) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    return(invisible(NULL))
  }

  tables <- attr(results, "tables") %||% list()
  if (!length(tables)) {
    return(invisible(NULL))
  }

  volcano <- attr(results, "volcano") %||% vector("list", length(tables))
  file_stubs <- attr(results, "file_stubs") %||% setNames(names(tables), names(tables))
  aliases <- attr(results, "aliases") %||% setNames(names(tables), names(tables))
  sig_cutoff <- attr(results, "volcano_sig_cutoff") %||% sig_cutoff
  label_top_n <- attr(results, "volcano_top_n") %||% label_top_n

  tables_dir <- fs::path(output_dir, "tables")
  volcano_plot_dir <- fs::path(output_dir, "volcano_plots")

  fs::dir_create(tables_dir, recurse = TRUE)
  fs::dir_create(volcano_plot_dir, recurse = TRUE)

  for (name in names(tables)) {
    stub <- file_stubs[[name]] %||% util_tools$safe_filename(name, fallback = name)
    alias <- aliases[[name]] %||% name
    table <- tables[[name]]
    volcano_df <- volcano[[name]]

    if (!is.data.frame(table)) {
      next
    }
    if (!is.data.frame(volcano_df)) {
      next
    }

    table_path <- fs::path(tables_dir, paste0(stub, ".tsv"))
    if (replace || !fs::file_exists(table_path)) {
      readr::write_tsv(table, table_path)
      if (!is.null(logger)) {
        logger(info = paste0("Wrote limma table: ", table_path))
      }
    }

    volcano_plot <- volcano_df
    sig_vec <- volcano_plot$`adj.P.Val`
    volcano_plot$significant <- !is.na(sig_vec) & sig_vec < sig_cutoff
    volcano_plot$direction <- ifelse(
      volcano_plot$significant,
      ifelse(volcano_plot$logFC >= 0, "up", "down"),
      "ns"
    )
    plot_df <- volcano_plot
    plot_df$.label <- if ("GeneSymbol" %in% names(plot_df)) {
      labs <- as.character(plot_df$GeneSymbol)
      fallback <- as.character(plot_df$GeneID)
      labs[is.na(labs) | labs == ""] <- fallback[is.na(labs) | labs == ""]
      labs
    } else {
      as.character(plot_df$GeneID)
    }

    volcano_pdf <- fs::path(volcano_plot_dir, paste0(stub, ".pdf"))
    if (replace || !fs::file_exists(volcano_pdf)) {
      label_top_n_val <- max(label_top_n, 0)
      label_df <- NULL
      if (label_top_n_val > 0) {
        ord <- order(plot_df$P.Value, na.last = NA)
        ord <- ord[seq_len(min(label_top_n_val, length(ord)))]
        if (length(ord) > 0) {
          label_df <- plot_df[ord, , drop = FALSE]
        }
      }

      plot_title <- paste(model_label, alias, sep = " - ")
      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = logFC, y = neg_log10_p, color = direction)
      ) +
        ggplot2::geom_point(
          alpha = 0.7,
          size = 1.3,
          na.rm = TRUE
        ) +
        ggplot2::geom_vline(
          xintercept = 0,
          linetype = "dashed",
          colour = "#bbbbbb"
        ) +
        ggplot2::geom_hline(
          yintercept = -log10(0.05),
          linetype = "dashed",
          colour = "#bbbbbb"
        ) +
        ggplot2::labs(
          title = plot_title,
          x = "log2 Fold Change",
          y = "-log10(P value)"
        ) +
        ggplot2::theme_minimal(base_size = 11)

      p <- p +
        ggplot2::scale_color_manual(
          values = c("up" = "#d73027", "down" = "#1f77b4", "ns" = "#bdbdbd"),
          breaks = c("up", "down", "ns"),
          guide = "none"
        )

      if (!is.null(label_df) && nrow(label_df) > 0) {
        p <- p +
          ggrepel::geom_text_repel(
            data = label_df,
            ggplot2::aes(label = .label),
            size = 3.0,
            max.overlaps = Inf,
            min.segment.length = 0,
            box.padding = 0.25,
            point.padding = 0.2,
            na.rm = TRUE
          )
      }

      sig_count <- sum(plot_df$significant, na.rm = TRUE)
      sig_total <- nrow(plot_df)
      sig_label <- sprintf(
        "%d / %d genes padj < %.3g",
        sig_count,
        sig_total,
        sig_cutoff
      )
      x_pos <- max(plot_df$logFC, na.rm = TRUE)
      y_pos <- min(plot_df$neg_log10_p, na.rm = TRUE)
      if (is.finite(x_pos) && is.finite(y_pos)) {
        p <- p +
          ggplot2::annotate(
            "text",
            x = x_pos,
            y = y_pos,
            label = sig_label,
            hjust = 1,
            vjust = -0.3,
            size = 3.2
          )
      }

      ggplot2::ggsave(filename = volcano_pdf, plot = p, width = 7.2, height = 5.0)
    }
  }
  invisible(NULL)
}

sanitize_model_terms <- function(terms) {
  if (length(terms) == 0) {
    return(character(0))
  }

  sanitized <- gsub("[^A-Za-z0-9]+", "", terms)
  empty_idx <- which(!nzchar(sanitized))
  if (length(empty_idx) > 0) {
    sanitized[empty_idx] <- paste0("coef", empty_idx)
  }
  make.unique(sanitized, sep = "_")
}

sanitize_predictor_column_name <- function(token, existing_names = character(0)) {
  base <- util_tools$safe_filename("expr", token, fallback = "expr")
  if (!(base %in% existing_names)) {
    return(base)
  }
  candidate <- base
  counter <- 2
  while (candidate %in% existing_names) {
    candidate <- paste0(base, "_", counter)
    counter <- counter + 1
  }
  candidate
}

parse_contrast_specs <- function(contrast_strings) {
  if (length(contrast_strings) == 0) {
    return(list())
  }

  specs <- vector("list", length(contrast_strings))
  for (idx in seq_along(contrast_strings)) {
    spec <- contrast_strings[[idx]]
    parts <- strsplit(spec, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      label <- trimws(parts[1])
      expression <- trimws(paste(parts[-1], collapse = "="))
    } else {
      label <- ""
      expression <- trimws(spec)
    }

    if (!nzchar(expression)) {
      stop("Contrast definition is empty after parsing: '", spec, "'")
    }

    display <- if (nzchar(label)) label else expression
    alias_candidate <- if (nzchar(label)) label else expression
    file_candidate <- if (nzchar(display)) display else expression

    specs[[idx]] <- list(
      label = display,
      expression = expression,
      alias_candidate = alias_candidate,
      fallback = paste0("contrast_", idx),
      file_candidate = file_candidate
    )
  }

  alias_raw <- vapply(
    specs,
    function(item) {
      candidate <- item$alias_candidate
      if (!nzchar(candidate)) {
        item$fallback
      } else {
        candidate
      }
    },
    character(1)
  )
  alias_unique <- make.unique(make.names(alias_raw), sep = "_")

  stub_candidates <- vapply(
    specs,
    function(item) {
      util_tools$safe_filename(
        if (nzchar(item$file_candidate)) item$file_candidate else item$fallback,
        fallback = item$fallback
      )
    },
    character(1)
  )
  stub_unique <- make.unique(stub_candidates, sep = "_")

  for (idx in seq_along(specs)) {
    specs[[idx]]$alias <- alias_unique[[idx]]
    specs[[idx]]$file_stub <- stub_unique[[idx]]
  }

  specs
}

resolve_expression_predictor <- function(token, gct) {
  rid_matches <- which(gct@rid == token)
  if (length(rid_matches) == 1) {
    values <- gct@mat[rid_matches, , drop = TRUE]
    return(setNames(as.numeric(values), gct@cid))
  }

  for (column_name in colnames(gct@rdesc)) {
    column <- gct@rdesc[[column_name]]
    if (is.null(column)) next
    column <- as.character(column)
    matches <- which(!is.na(column) & column == token)
    if (length(matches) == 1) {
      values <- gct@mat[matches, , drop = TRUE]
      return(setNames(as.numeric(values), gct@cid))
    }
    if (length(matches) > 1) {
      stop("Multiple rows matched predictor '", token, "' in rdesc column '", column_name, "'. Please disambiguate.")
    }
  }

  NULL
}

create_rnkfiles_from_model <- function(
    gct_path,
    model_spec,
    sample_exclude = NULL,
    exclude_samples_from_data = FALSE,
    output_dir = NULL,
    replace = FALSE,
    model_index = 1,
    cache = TRUE,
    cache_dir = NULL) {
  if (is.null(gct_path) || !nzchar(gct_path)) {
    stop("gct_path must be provided when ranks_from='model'")
  }
  if (!file.exists(gct_path)) {
    stop("gct_path '", gct_path, "' does not exist")
  }

  spec <- model_spec %||% list()
  model_name <- spec$name %||% paste0("model", model_index)
  model_label <- model_name
  model_type <- tolower(spec$type %||% "limma")
  if (!identical(model_type, "limma")) {
    stop("Unsupported model type '", spec$type, "'. Currently only 'limma' is supported.")
  }

  design_str <- spec$design %||% ""
  if (!nzchar(design_str)) {
    stop("Model design formula must be provided when ranks_from='model'")
  }

  contrasts <- spec$contrasts %||% list()
  if (is.character(contrasts)) {
    contrasts <- as.list(contrasts)
  }

  sig_cutoff <- spec$volcano_padj_cutoff %||% 0.05
  label_top_n <- spec$volcano_top_n %||% 35

  cache_enabled <- isTRUE(cache) && !is.null(cache_dir) && nzchar(cache_dir)
  cache_key <- NULL

  design_formula <- tryCatch(
    stats::as.formula(design_str),
    error = function(e) {
      stop("Failed to parse model formula '", design_str, "': ", e$message)
    }
  )

  gct <- cmapR::parse_gctx(gct_path)
  metadata <- as.data.frame(gct@cdesc, stringsAsFactors = FALSE)
  metadata$.sample_id <- gct@cid
  rownames(metadata) <- gct@cid

  symbol_map <- NULL
  symbol_candidates <- c(
    "pr_gene_symbol",
    "GeneSymbol",
    "gene_symbol",
    "Symbol",
    "symbol"
  )
  for (candidate in symbol_candidates) {
    if (candidate %in% colnames(gct@rdesc)) {
      symbol_map <- gct@rdesc[[candidate]]
      break
    }
  }
  if (!is.null(symbol_map)) {
    symbol_map <- as.character(symbol_map)
    names(symbol_map) <- as.character(gct@rid)
  }

  normalized_exclude <- util_tools$normalize_sample_exclude(sample_exclude, metadata)
  if (length(normalized_exclude) > 0) {
    keep <- setdiff(rownames(metadata), normalized_exclude)
    if (length(keep) == 0) {
      stop("All samples were removed by sample_exclude; cannot fit model.")
    }
    removed <- setdiff(rownames(metadata), keep)
    if (length(removed) > 0) {
      log_msg(info = paste0("Excluding ", length(removed), " samples from model fitting: ", paste(removed, collapse = ", ")))
    }
    metadata <- metadata[keep, , drop = FALSE]
  }

  if (cache_enabled) {
    cache_key <- compute_model_cache_key(
      gct_path = gct_path,
      design = design_str,
      contrasts = contrasts,
      sample_exclude = normalized_exclude,
      exclude_samples_from_data = exclude_samples_from_data,
      model_name = model_name,
      model_index = model_index,
      model_file = spec$model_file %||% NULL,
      volcano_cutoff = sig_cutoff,
      volcano_top_n = label_top_n
    )
    cached <- load_cached_model_results(cache_dir, cache_key, logger = log_msg)
    if (!is.null(cached) && !isTRUE(replace)) {
      cached_metadata <- attr(cached, "metadata_values") %||% list()
      if (length(cached_metadata) > 0) {
        annotated_path <- append_metadata_to_gct(
          gct = gct,
          metadata_values = cached_metadata,
          output_dir = output_dir,
          model_label = model_label,
          replace = FALSE,
          logger = log_msg
        )
        if (!is.null(annotated_path)) {
          attr(cached, "annotated_gct_path") <- annotated_path
        }
      }
      persist_model_outputs(
        cached,
        output_dir,
        replace = FALSE,
        model_label = attr(cached, "model_name") %||% model_label,
        logger = log_msg
      )
      return(cached)
    }
  }

  expression_mat <- gct@mat[, rownames(metadata), drop = FALSE]

  predictor_columns <- list()

  vars <- all.vars(design_formula)
  missing_vars <- setdiff(vars, colnames(metadata))
  if (length(missing_vars) > 0) {
    for (token in missing_vars) {
      predictor <- resolve_expression_predictor(token, gct)
      if (is.null(predictor)) {
        stop("Formula variable '", token, "' not found in metadata or expression matrix.")
      }
      metadata[[token]] <- as.numeric(predictor[rownames(metadata)])
      safe_name <- sanitize_predictor_column_name(
        token,
        existing_names = names(predictor_columns)
      )
      predictor_columns[[safe_name]] <- list(values = predictor, token = token)
    }
  }

  model_frame <- stats::model.frame(
    design_formula,
    metadata,
    na.action = stats::na.omit,
    drop.unused.levels = TRUE
  )

  sample_ids <- rownames(model_frame)
  metadata <- metadata[sample_ids, , drop = FALSE]
  if (length(sample_ids) == 0) {
    stop("No samples remain after constructing the model frame (check missing values).")
  }

  expression_mat <- expression_mat[, sample_ids, drop = FALSE]
  if (!is.numeric(expression_mat)) {
    storage.mode(expression_mat) <- "double"
  }

  design_matrix <- stats::model.matrix(design_formula, model_frame)
  original_terms <- colnames(design_matrix)
  sanitized_terms <- sanitize_model_terms(original_terms)
  colnames(design_matrix) <- sanitized_terms
  term_lookup <- stats::setNames(original_terms, sanitized_terms)

  if (ncol(design_matrix) == 0) {
    stop("Design matrix contains no columns after sanitisation.")
  }

  keep_cols <- vapply(
    seq_len(ncol(design_matrix)),
    function(i) any(abs(design_matrix[, i]) > .Machine$double.eps),
    logical(1)
  )
  if (!all(keep_cols)) {
    dropped <- sanitized_terms[!keep_cols]
    log_msg(warning = paste0("Dropping zero-variance design columns: ", paste(dropped, collapse = ", ")))
    design_matrix <- design_matrix[, keep_cols, drop = FALSE]
    sanitized_terms <- colnames(design_matrix)
    original_terms <- term_lookup[sanitized_terms]
    term_lookup <- stats::setNames(original_terms, sanitized_terms)
  }

  if (isTRUE(spec$print_model_matrix)) {
    preview <- utils::capture.output(print(head(as.data.frame(design_matrix), n = 10)))
    log_msg(debug = paste0("Limma model matrix preview (first 10 rows):\n", paste(preview, collapse = "\n")))
  }

  if (ncol(design_matrix) == 0) {
    stop("All design columns were dropped; check model specification.")
  }

  apply_contrasts <- length(contrasts) > 0
  pretty_alias_map <- stats::setNames(
    vapply(sanitized_terms, function(term) prettify_term_label(term_lookup[[term]], term), character(1)),
    sanitized_terms
  )
  alias_to_term <- stats::setNames(names(pretty_alias_map), pretty_alias_map)

  normalize_contrast_expression <- function(expr) {
    updated <- expr
    for (alias_label in names(alias_to_term)) {
      target <- alias_to_term[[alias_label]]
      pattern <- sprintf("(?<![A-Za-z0-9_.])%s(?![A-Za-z0-9_.])", alias_label)
      updated <- gsub(pattern, target, updated, perl = TRUE)
    }
    updated
  }

  if (apply_contrasts) {
    contrast_defs <- parse_contrast_specs(contrasts)
    for (idx in seq_along(contrast_defs)) {
      raw_expr <- contrast_defs[[idx]]$expression
      converted <- normalize_contrast_expression(raw_expr)
      contrast_defs[[idx]]$expression <- converted
      if (converted %in% sanitized_terms) {
        contrast_defs[[idx]]$coef_name <- converted
      } else if (raw_expr %in% alias_to_term) {
        contrast_defs[[idx]]$coef_name <- alias_to_term[[raw_expr]]
      }
      if (is.null(contrast_defs[[idx]]$coef_name)) {
        contrast_defs[[idx]]$coef_name <- converted
      }
    }
    contrast_args <- setNames(
      lapply(contrast_defs, function(def) def$expression),
      vapply(contrast_defs, function(def) def$alias, character(1))
    )
    contrast_matrix <- do.call(
      limma::makeContrasts,
      c(contrast_args, list(levels = sanitized_terms))
    )
    if (is.null(dim(contrast_matrix))) {
      contrast_matrix <- matrix(
        contrast_matrix,
        ncol = 1,
        dimnames = list(sanitized_terms, names(contrast_args))
      )
    }
  } else {
    is_intercept <- sanitized_terms %in% c("(Intercept)", "Intercept", "XIntercept", "X.Intercept.")
    base_terms <- sanitized_terms[!is_intercept]
    if (length(base_terms) == 0) {
      base_terms <- sanitized_terms
    }
    if (length(base_terms) == 0) {
      stop("Design matrix contains no estimable terms after removing intercept.")
    }
    contrast_defs <- lapply(seq_along(base_terms), function(idx) {
      term <- base_terms[[idx]]
      original_term <- term_lookup[[term]] %||% term
      pretty_alias <- prettify_term_label(original_term, term)
      list(
        alias = pretty_alias,
        expression = term,
        file_stub = util_tools$safe_filename(
          pretty_alias,
          fallback = paste0("coef_", idx)
        ),
        original_term = original_term,
        coef_name = term
      )
    })
    alias_unique <- make.unique(vapply(contrast_defs, function(def) def$alias, character(1)), sep = "_")
    stub_unique <- make.unique(vapply(contrast_defs, function(def) def$file_stub, character(1)), sep = "_")
    for (idx in seq_along(contrast_defs)) {
      contrast_defs[[idx]]$alias <- alias_unique[[idx]]
      contrast_defs[[idx]]$file_stub <- stub_unique[[idx]]
    }
  }

  # TODO: add better logging here of the design matrix
  print("Design matrix is :")
  print(design_matrix)
  fit <- limma::lmFit(expression_mat, design_matrix)
  if (apply_contrasts) {
    fit <- limma::contrasts.fit(fit, contrast_matrix)
  }
  fit <- limma::eBayes(fit)

  # TODO: add better logging here of the fitted model

  t_stats <- as.matrix(fit$t)
  if (is.null(colnames(t_stats))) {
    colnames(t_stats) <- vapply(contrast_defs, function(def) def$alias, character(1))
  }

  prefix <- util_tools$safe_filename(model_name, fallback = paste0("model", model_index))

  output <- vector("list", length(contrast_defs))
  result_tables <- vector("list", length(contrast_defs))
  volcano_tables <- vector("list", length(contrast_defs))

  names(output) <- vapply(
    contrast_defs,
    function(def) util_tools$safe_filename(prefix, def$file_stub),
    character(1)
  )

  contrast_aliases <- vapply(contrast_defs, function(def) def$alias, character(1))
  contrast_file_stubs <- vapply(contrast_defs, function(def) def$file_stub, character(1))
  names(contrast_aliases) <- names(output)
  names(contrast_file_stubs) <- names(output)

  lookup_term <- function(key) {
    if (!is.character(key) || length(key) == 0) {
      return(NULL)
    }
    candidate <- key[[1]]
    if (!nzchar(candidate) || is.null(names(term_lookup))) {
      return(NULL)
    }
    if (!(candidate %in% names(term_lookup))) {
      return(NULL)
    }
    term_lookup[[candidate]]
  }

  for (idx in seq_along(contrast_defs)) {
    alias <- contrast_defs[[idx]]$alias
    candidates <- c(
      contrast_defs[[idx]]$coef_name,
      contrast_defs[[idx]]$expression,
      alias,
      lookup_term(contrast_defs[[idx]]$coef_name),
      lookup_term(contrast_defs[[idx]]$expression)
    )
    candidates <- unique(Filter(function(x) is.character(x) && nzchar(x), candidates))
    available_cols <- colnames(t_stats)
    hit <- candidates[candidates %in% available_cols]
    if (length(hit) == 0) {
      stop("Contrast '", alias, "' was not found in the fitted model output.")
    }
    coef_name <- hit[[1]]
    output_name <- names(output)[[idx]]
    values <- t_stats[, coef_name, drop = TRUE]
    df <- data.frame(
      id = rownames(t_stats),
      value = as.numeric(values),
      stringsAsFactors = FALSE
    )
    df <- df[is.finite(df$value), , drop = FALSE]
    output[[output_name]] <- df

    top_table <- limma::topTable(
      fit,
      coef = coef_name,
      number = Inf,
      sort.by = "none"
    )
    top_table$GeneID <- rownames(top_table)
    if (!is.null(symbol_map)) {
      gene_symbols <- symbol_map[as.character(top_table$GeneID)]
      top_table$GeneSymbol <- ifelse(
        is.na(gene_symbols) | gene_symbols == "",
        NA_character_,
        gene_symbols
      )
      top_table <- top_table[, c("GeneID", "GeneSymbol", setdiff(colnames(top_table), c("GeneID", "GeneSymbol")))]
    } else {
      top_table <- top_table[, c("GeneID", setdiff(colnames(top_table), "GeneID"))]
    }
    result_tables[[idx]] <- top_table

    volcano_plot <- top_table
    volcano_plot$signedlogP <- sign(volcano_plot$logFC) * -log10(pmax(volcano_plot$P.Value, .Machine$double.eps))
    volcano_plot$neg_log10_p <- -log10(pmax(volcano_plot$P.Value, .Machine$double.eps))
    volcano_plot$significant <- !is.na(volcano_plot$`adj.P.Val`) & volcano_plot$`adj.P.Val` < sig_cutoff
    volcano_plot$direction <- ifelse(
      volcano_plot$significant,
      ifelse(volcano_plot$logFC >= 0, "up", "down"),
      "ns"
    )
    volcano_tables[[idx]] <- volcano_plot
  }

  updated_metadata <- list()
  annotated_gct_path <- NULL
  if (length(predictor_columns) > 0) {
    for (safe_name in names(predictor_columns)) {
      predictor <- predictor_columns[[safe_name]]$values
      aligned <- rep(NA_real_, length(metadata$.sample_id))
      names(aligned) <- metadata$.sample_id
      matches <- intersect(names(predictor), names(aligned))
      aligned[matches] <- as.numeric(predictor[matches])
      metadata[[safe_name]] <- aligned
      updated_metadata[[safe_name]] <- aligned
    }
    attr(output, "metadata_columns") <- names(updated_metadata)
    attr(output, "metadata_values") <- updated_metadata
    log_msg(info = paste0(
      "Captured predictor metadata columns for model ",
      model_name, ": ",
      paste(names(updated_metadata), collapse = ", ")
    ))
    annotated_gct_path <- append_metadata_to_gct(
      gct = gct,
      metadata_values = updated_metadata,
      output_dir = output_dir,
      model_label = model_label,
      replace = replace,
      logger = log_msg
    )
  }

  attr(output, "tables") <- setNames(result_tables, names(output))
  attr(output, "volcano") <- setNames(volcano_tables, names(output))
  attr(output, "model_name") <- model_name
  attr(output, "model_type") <- model_type
  attr(output, "aliases") <- contrast_aliases
  attr(output, "file_stubs") <- contrast_file_stubs
  attr(output, "volcano_sig_cutoff") <- sig_cutoff
  attr(output, "volcano_top_n") <- label_top_n
  if (!is.null(annotated_gct_path)) {
    attr(output, "annotated_gct_path") <- annotated_gct_path
  }

  persist_model_outputs(
    output,
    output_dir,
    replace = replace,
    model_label = model_label,
    logger = log_msg,
    sig_cutoff = sig_cutoff,
    label_top_n = label_top_n
  )

  if (cache_enabled && !is.null(cache_key)) {
    write_cached_model_results(cache_dir, cache_key, output, logger = log_msg)
  }

  output
}
