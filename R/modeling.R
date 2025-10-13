suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(limma))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(splines))

source(file.path(here("R"), "lazyloader.R"))

util_tools <- get_tool_env("utils")
log_msg <- util_tools$make_partial(util_tools$log_msg)

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
    exclude_samples_from_data = FALSE) {
  if (is.null(gct_path) || !nzchar(gct_path)) {
    stop("gct_path must be provided when ranks_from='model'")
  }
  if (!file.exists(gct_path)) {
    stop("gct_path '", gct_path, "' does not exist")
  }

  spec <- model_spec %||% list()
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

  expression_mat <- gct@mat[, rownames(metadata), drop = FALSE]

  vars <- all.vars(design_formula)
  missing_vars <- setdiff(vars, colnames(metadata))
  if (length(missing_vars) > 0) {
    for (token in missing_vars) {
      predictor <- resolve_expression_predictor(token, gct)
      if (is.null(predictor)) {
        stop("Formula variable '", token, "' not found in metadata or expression matrix.")
      }
      metadata[[token]] <- as.numeric(predictor[rownames(metadata)])
    }
  }

  model_frame <- stats::model.frame(
    design_formula,
    metadata,
    na.action = stats::na.omit,
    drop.unused.levels = TRUE
  )

  sample_ids <- rownames(model_frame)
  if (length(sample_ids) == 0) {
    stop("No samples remain after constructing the model frame (check missing values).")
  }

  expression_mat <- expression_mat[, sample_ids, drop = FALSE]
  if (!is.numeric(expression_mat)) {
    storage.mode(expression_mat) <- "double"
  }

  design_matrix <- stats::model.matrix(design_formula, model_frame)
  sanitized_terms <- sanitize_model_terms(colnames(design_matrix))
  colnames(design_matrix) <- sanitized_terms

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
  }

  if (ncol(design_matrix) == 0) {
    stop("All design columns were dropped; check model specification.")
  }

  apply_contrasts <- length(contrasts) > 0
  if (apply_contrasts) {
    contrast_defs <- parse_contrast_specs(contrasts)
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
    contrast_defs <- lapply(seq_along(sanitized_terms), function(idx) {
      list(
        alias = sanitized_terms[[idx]],
        expression = sanitized_terms[[idx]],
        file_stub = util_tools$safe_filename(
          sanitized_terms[[idx]],
          fallback = paste0("coef_", idx)
        )
      )
    })
    stub_unique <- make.unique(vapply(contrast_defs, function(def) def$file_stub, character(1)), sep = "_")
    for (idx in seq_along(contrast_defs)) {
      contrast_defs[[idx]]$file_stub <- stub_unique[[idx]]
    }
  }

  fit <- limma::lmFit(expression_mat, design_matrix)
  if (apply_contrasts) {
    fit <- limma::contrasts.fit(fit, contrast_matrix)
  }
  fit <- limma::eBayes(fit)

  t_stats <- as.matrix(fit$t)
  if (is.null(colnames(t_stats))) {
    colnames(t_stats) <- vapply(contrast_defs, function(def) def$alias, character(1))
  }

  output <- vector("list", length(contrast_defs))
  names(output) <- vapply(contrast_defs, function(def) def$file_stub, character(1))

  for (idx in seq_along(contrast_defs)) {
    alias <- contrast_defs[[idx]]$alias
    stub <- contrast_defs[[idx]]$file_stub
    if (!alias %in% colnames(t_stats)) {
      stop("Contrast '", alias, "' was not found in the fitted model output.")
    }
    values <- t_stats[, alias, drop = TRUE]
    df <- data.frame(
      id = rownames(t_stats),
      value = as.numeric(values),
      stringsAsFactors = FALSE
    )
    df <- df[is.finite(df$value), , drop = FALSE]
    output[[stub]] <- df
  }

  output
}
