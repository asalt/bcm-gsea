suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(digest))
# suppressPackageStartupMessages(library(dplyr))

source(file.path(here("R"), "lazyloader.R"))
# listen_tools <- get_tool_env("listen.R")
# util_tools <- get_tool_env("utils")



#
# Helper function to prepend root_dir only to valid paths
prepend_root_dir <- function(params, root_dir) {
  # Function to prepend root_dir if the constructed path exists
  modify_path <- function(x) {
    if (is.character(x)) { # Check if it's a character string
      potential_path <- file.path(root_dir, x)
      if (file.exists(potential_path)) { # Check if the constructed path exists
        return(potential_path) # Use the constructed path
      }
    }
    return(x) # Return the value as is if it's not a valid path
  }

  # Apply the modify_path function to all elements in params
  params <- lapply(params, function(param) {
    if (is.list(param)) {
      return(lapply(param, modify_path)) # Recursively apply for nested lists
    } else {
      return(modify_path(param)) # Apply to non-list items
    }
  })

  return(params)
}


is_absolute_path <- function(path) {
  grepl(paste0("^", normalizePath(root_dir)), normalizePath(path))
}

clean_args <- function(params, root_dir = "/") {
  # would be best for root_dir to be explicitly specified, which it is elsewhere

  # this doesn't work-
  # params <- prepend_root_dir(params, root_dir)

  if (is.null(params$savedir)) {
    params$savedir <- file.path("./plots")
  }
  params$savedir <- file.path(root_dir, (params$savedir %||% file.path("./plots")))

  # all top level params
  params$advanced <- params$advanced %||% list()
  params$barplot <- params$barplot %||% list()
  params$bubbleplot <- params$bubbleplot %||% list()
  params$heatmap_gsea <- params$heatmap_gsea %||% list()
  params$heatmap_gene <- params$heatmap_gene %||% list()
  params$enplot <- params$enplot %||% list()

  params$barplot$do_individual <- params$barplot$do_individual %||% TRUE
  params$barplot$do_combined <- params$barplot$do_combined %||% TRUE
  params$bubbleplot$do_individual <- params$bubbleplot$do_individual %||% TRUE
  params$bubbleplot$do_combined <- params$bubbleplot$do_combined %||% TRUE
  params$barplot$advanced <- params$barplot$advanced %||% list()
  params$bubbleplot$advanced <- params$bubbleplot$advanced %||% list()

  default_limit_values <- c(10, 20, 30, 50)
  params$barplot$limit <- normalize_limit_vector(params$barplot$limit, default_limit_values)
  params$bubbleplot$limit <- normalize_limit_vector(params$bubbleplot$limit, params$barplot$limit)
  params$bubbleplot$glyph <- params$bubbleplot$glyph %||% "⁕"

  params$heatmap_gsea$do <- params$heatmap_gsea$do %||% TRUE
  params$heatmap_gene$do <- params$heatmap_gene$do %||% TRUE
  params$pca$do <- params$pca$do %||% TRUE


  if (!is.null(params$volcanodir)) {
    params$volcanodir <- file.path(root_dir, params$volcanodir)
    if (!file.exists(params$volcanodir)) stop(paste0("volcanodir does not exist: ", params$volcanodir))
  }

  if (!is.null(params$gct_path) && params$gct_path != "") {
    params$gct_path <- file.path(root_dir, params$gct_path)
  } else {
    params$gct_path <- NULL
  }

  if (!is.null(params$gct_path) && !file.exists(params$gct_path)) {
    stop(paste0(params$gct_path, " does not exist, exiting.."))
  }

  params$advanced$cache <- params$advanced$cache %||% TRUE

  # print(params$enplot$combine_by)
  params$enplot$combine_by <- params$enplot$combine_by %||% NULL
  # print(params$enplot$combine_by)

  cachedir <- params$advanced$cachedir %||% file.path(params$savedir, "cache")
  if (!is.null(cachedir)) {
    if (cachedir == "savedir") {
      cachedir <- file.path(params$savedir, "cache")
    }
  }
  params$advanced$cachedir <- cachedir


  # this block could be cleaned up
  if (!is.null(params$rankfiledir)) {
    if (params$rankfiledir == "savedir" || params$rankfiledir == "") {
      params$rankfiledir <- file.path(params$savedir, "ranks")
    } else {
      params$rankfiledir <- file.path(root_dir, params$rankfiledir)
    }
  } else {
      params$rankfiledir <- file.path(params$savedir, "ranks")
  }

  params$advanced$pivot_gsea_results <- params$advanced$pivot_gsea_results %||% FALSE
  #
  if (!is.null(params$extra$rankname_order)) {
    params$extra$rankname_order <- as.character(params$extra$rankname_order)
    if (length(params$extra$rankname_order) == 1 && params$extra$rankname_order == "sample_order") {
      log_msg(info = "params.extra.rankname_order references 'sample_order'; using params.extra.sample_order instead")
      params$extra$rankname_order <- params$extra$sample_order
    }
  } else {
    params$extra$rankname_order <- params$extra$sample_order
  }

  if (!is.null(params$extra$sample_order)) {
    params$extra$sample_order <- as.character(params$extra$sample_order)
    if (length(params$extra$sample_order) == 1 && params$extra$sample_order == "rankname_order") {
      log_msg(info = "params.extra.sample_order references 'rankname_order'; using params.extra.rankname_order instead")
      params$extra$sample_order <- params$extra$rankname_order
    }
  } else {
    params$extra$sample_order <- params$extra$rankname_order
  }

  if (!is.null(params$extra$rankname_order) && !is.null(params$extra$sample_order)) {
    if (!identical(params$extra$rankname_order, params$extra$sample_order)) {
      log_msg(warning = "params.extra.rankname_order and params.extra.sample_order differ; continuing with rankname_order as canonical list")
    }
  }

  params$species <- params$species %||% "Homo sapiens"

  params$genesets <- params$genesets %||% list(list(category = "H", subcategory = "", collapse = FALSE))

  params$advanced$quiet <- params$advanced$quiet %||% FALSE

  params$advanced$parallel <- params$advanced$parallel %||% FALSE

  logfile <- params$advanced$logfile %||% file.path(params$savedir, "run.log")
  # browser()
  loglevel <- params$advanced$loglevel
  options("bcm_gsea_log_msg_filename" = logfile)
  options("bcm_gsea_loglevel" = loglevel)
  params$advanced$logfile <- logfile

  print(str(params))

  return(params)
}


# Helpers for safe filesystem naming ------------------------------------

.sanitize_component <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  candidate <- as.character(x)[1]
  if (is.na(candidate)) {
    return(NA_character_)
  }
  candidate <- trimws(candidate)
  if (!nzchar(candidate)) {
    return(NA_character_)
  }

  # transliterate to ASCII where possible, fall back to underscore substitution
  candidate_ascii <- suppressWarnings(iconv(candidate,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = "_"
  ))
  if (!is.na(candidate_ascii) && nzchar(candidate_ascii)) {
    candidate <- candidate_ascii
  }

  candidate <- gsub("[^A-Za-z0-9._-]+", "_", candidate)
  candidate <- gsub("_+", "_", candidate)
  candidate <- gsub("^_+|_+$", "", candidate)

  if (!nzchar(candidate)) {
    return(NA_character_)
  }

  return(candidate)
}

safe_path_component <- function(x, fallback = "item", max_chars = 60, allow_empty = FALSE) {
  candidate <- .sanitize_component(x)

  if (is.na(candidate) || (!nzchar(candidate) && !allow_empty)) {
    candidate <- fallback
  } else if (allow_empty && (is.na(candidate) || !nzchar(candidate))) {
    candidate <- ""
  }

  if (nzchar(candidate) && nchar(candidate) > max_chars) {
    hash <- substr(digest::digest(candidate, algo = "crc32"), 1, 8)
    keep <- max(1, max_chars - nchar(hash) - 1)
    candidate <- paste0(substr(candidate, 1, keep), "_", hash)
  }

  if (!allow_empty && !nzchar(candidate)) {
    candidate <- fallback
  }

  return(candidate)
}

safe_filename <- function(..., fallback = "file", max_chars = 80, delim = "_") {
  parts <- list(...)
  if (length(parts) == 0) {
    return(fallback)
  }

  sanitized <- vapply(parts, function(part) {
    safe_path_component(part, fallback = "", max_chars = max_chars, allow_empty = TRUE)
  }, character(1), USE.NAMES = FALSE)

  sanitized <- sanitized[nzchar(sanitized)]
  if (length(sanitized) == 0) {
    candidate <- fallback
  } else {
    candidate <- paste(sanitized, collapse = delim)
  }

  if (!nzchar(candidate)) {
    candidate <- fallback
  }

  if (nchar(candidate) > max_chars) {
    hash <- substr(digest::digest(candidate, algo = "crc32"), 1, 8)
    keep <- max(1, max_chars - nchar(hash) - nchar(delim))
    candidate <- paste0(substr(candidate, 1, keep), delim, hash)
  }

  if (!nzchar(candidate)) {
    candidate <- fallback
  }

  return(candidate)
}

safe_subdir <- function(base, ..., max_chars = 60) {
  components <- list(...)
  if (length(components) == 0) {
    return(base)
  }

  sanitized <- vapply(components, function(part) {
    safe_path_component(part, max_chars = max_chars)
  }, character(1), USE.NAMES = FALSE)

  do.call(file.path, c(list(base), as.list(sanitized)))
}



normalize_limit_vector <- function(limit, fallback = c(10, 20, 30, 50)) {
  if (is.null(limit)) {
    return(fallback)
  }

  if (is.list(limit)) {
    limit <- unlist(limit, use.names = FALSE)
  }

  if (is.character(limit)) {
    if (length(limit) == 1L) {
      limit_candidate <- tolower(limit)
      if (limit_candidate %in% c("auto", "default", "all", "")) {
        return(fallback)
      }
    }
    limit <- suppressWarnings(as.numeric(limit))
  }

  limit <- suppressWarnings(as.numeric(limit))
  limit <- limit[is.finite(limit) & !is.na(limit) & limit > 0]

  if (length(limit) == 0) {
    return(fallback)
  }

  sort(unique(limit))
}



infer_ordered_factor <- function(column) { # this probably doesn't do what you want it to do
  # Function to infer ordering for a given column vector

  # Extract numeric values from strings
  numeric_values <- as.numeric(gsub("[^0-9.-]+", "", column))

  # Find the minimum numeric value
  min_num <- min(numeric_values, na.rm = TRUE)
  if (is.infinite(min_num)) {
    min_num <- 0 # Default to 0 if no numeric values are found
  }

  # Initialize order values with numeric values
  order_values <- numeric_values

  # Indices of non-numeric values
  non_numeric_indices <- which(is.na(order_values))

  if (length(non_numeric_indices) > 0) {
    non_numeric_values <- tolower(column[non_numeric_indices])

    # Assign special order value for "veh" or "vehicle"
    is_vehicle <- grepl("^veh$|^vehicle|^ctrl|^dmso$", non_numeric_values, ignore.case = TRUE)
    order_values[non_numeric_indices[is_vehicle]] <- min_num - 2 # Highest priority

    # Assign next priority to other non-numeric values
    order_values[non_numeric_indices[!is_vehicle]] <- min_num - 1
  }

  # Create a data frame for sorting
  df <- data.frame(
    original_value = column,
    order_value = order_values,
    stringsAsFactors = FALSE
  )

  # Remove duplicates while preserving order
  df_unique <- df[!duplicated(df$original_value), ]

  # Sort the data frame by order_value
  df_ordered <- df_unique[order(df_unique$order_value), ]

  # Create an ordered factor with levels in the sorted order
  ordered_factor <- factor(
    column,
    levels = df_ordered$original_value,
    ordered = TRUE
  )

  return(ordered_factor)
}


myzscore <- function(value, minval = NA, remask = TRUE) {
  mask <- is.na(value)
  if (is.na(minval)) minval <- min(value, na.rm = TRUE)

  if (minval == Inf) {
    minval <- 0
  }

  value[is.na(value)] <- minval
  # todo make smaller than min val
  out <- scale(value)

  # if all NA:
  if (sum(!is.finite(out)) == length(out)) {
    out[, 1] <- 0
  }

  if (remask == TRUE) {
    out[mask] <- NA
  }
  # coerce to vector
  out <- as.vector(out)

  return(out)
}

dist_no_na <- function(mat) {
  mat[is.na(mat)] <- min(mat, na.rm = TRUE)
  edist <- dist(mat)
  return(edist)
}

scale_gct <- function(gct, group_by = NULL) {
  log_msg(msg = "zscoring gct file by row")
  log_msg(msg = paste0("group_by is set to: ", group_by))
  if (!is.null(group_by) && length(group_by) == 1 && group_by == FALSE) group_by <- NULL
  group_cols <- group_by # this is a hack to get around the fact that group_by is a dplyr function
  res <- gct %>%
    melt_gct() # %>%
  # Conditionally add group_by
  if (!is.null(group_cols)) {
    if (length(group_cols) == 1) {
      res <- group_by(res, id.x, !!sym(group_cols))
    }
    if (length(group_cols) > 1) {
      res <- group_by(res, id.x, across(all_of(group_cols)))
    }
  } else {
    res <- group_by(res, id.x)
  }

  res <- res %>%
    dplyr::mutate(zscore = myzscore(value)) %>%
    dplyr::ungroup()

  # make a new gct and return
  res <- res %>%
    dplyr::select(id.x, id.y, zscore) %>%
    tidyr::pivot_wider(names_from = id.y, values_from = zscore) %>%
    as.data.frame()
  rownames(res) <- res$id.x
  res$id.x <- NULL
  res <- as.matrix(res)
  rdesc <- gct@rdesc
  cdesc <- gct@cdesc

  if (length(colnames(gct@rdesc)) == 1) {
    rdesc["dummy"] <- "X"
  }

  if (length(colnames(res)) == 1) {
    rdesc["dummy"] <- "X"
  }

  newgct <- new("GCT",
    mat = res,
    rid = rownames(res),
    cid = colnames(res),
    rdesc = rdesc[rownames(res), ],
    cdesc = cdesc[colnames(res), ]
  )

  return(newgct)
}


# # this is exploratory rewrite of plot_utils::make_partial
# get_arg <- function(f, arg, default = "") {
#   args <- if (!is.null(attr(f, "preset_args"))) attr(f, "preset_args") else list()
#   if (arg %in% names(args)) {
#     return(args[[arg]])
#   }
#   return(default)
# }

# another version, maybe easier to read
# rewrite of plot_utils::make_partial
get_arg <- function(f, arg, default = "") {
  args <- attr(f, "preset_args") # will return NULL if no attr
  if (!is.null(args) && !is.null(args[[arg]])) {
    return(args[[arg]])
  }
  return(default)
}

# another version, maybe easier to read
# rewrite of plot_utils::make_partial
get_args <- function(f, ...) {
  args <- attr(f, "preset_args") # will return NULL if no attr
  if (!is.null(args)) {
    return(args)
  }
  return(list())
}

# Revised make_partial using an environment for cleaner argument handling
make_partial <- function(.f, ...) {
  # Ensure the function is correctly resolved
  if (is.character(.f)) {
    .f <- get(.f, envir = parent.frame())
  }

  # print(.f)
  # print(is.function(.f)) # Should be TRUE if .f is correctly resolved
  if (!is.function(.f)) {
    stop("The first argument must be a function")
  }

  # Environment to store arguments

  env <- new.env()
  env$preset_args <- if (!is.null(attr(.f, "preset_args"))) attr(.f, "preset_args") else list()

  # New fixed arguments
  args_fixed <- list(...)

  # Combine old and new arguments
  if (!is.null(names(args_fixed))) {
    env$preset_args[names(args_fixed)] <- args_fixed
  }

  # Inner function using environment
  inner <- function(...) {
    new_args <- list(...)
    # combined_args <- c(env$preset_args, new_args)
    for (arg in names(new_args)) {
      env$preset_args[[arg]] <- new_args[[arg]]
    }
    combined_args <- env$preset_args
    # combined_args <- modifyList(env$preset_args, list(...))
    do.call(.f, combined_args)
  }

  # Attach environment as an attribute (optional but can be helpful for debugging)
  attr(inner, "preset_args") <- env$preset_args

  return(inner)
}

# logging.getLevelNamesMapping() # py
# {'CRITICAL': 50,
#  'FATAL': 50,
#  'ERROR': 40,
#  'WARN': 30,
#  'WARNING': 30,
#  'INFO': 20,
#  'DEBUG': 10,
#  'NOTSET': 0}

.log_levels <- list(
  "CRITICAL" = 50,
  "ERROR" = 40,
  "WARN" = 30,
  "WARNING" = 30,
  "INFO" = 20,
  "DEBUG" = 10,
  "NOTSET" = 0
)



globalloglevel <- options("bcm_gsea_loglevel")[[1]] %||% "INFO"

log_msg <- function(msg = NULL, info = NULL, debug = NULL, warning = NULL, warn = NULL, error = NULL, filename = NULL, end = "\n", shell = T, loglevel = loglevel, send_over_socket = TRUE, socket_port = 8765, ...) {
  level <- case_when(
    !is.null(msg) ~ "INFO",
    !is.null(info) ~ "INFO",
    !is.null(warning) ~ "WARNING",
    !is.null(warn) ~ "WARNING",
    !is.null(debug) ~ "DEBUG",
    !is.null(error) ~ "ERROR",
    TRUE ~ "INFO"
  )

  is_lvl_too_low <- .log_levels[[level]] < .log_levels[[globalloglevel]]
  if (is_lvl_too_low == TRUE) {
    return()
  }

  msg <- Filter(Negate(is.null), list(msg, info, warning, warn, debug, error))
  if (length(msg) > 0){
      msg <- msg[[1]]
  } else {
      msg <- "??"
  }

  prefix <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), level, ": ")

  maybe_filename <- options("bcm_gsea_log_msg_filename")[[1]]
  if (!is.null(maybe_filename)) {
    filename <- maybe_filename[[1]]
  }
  if (is.null(filename)) {
    filename <- "bcm_gsea.log"
  }

  dir_path <- fs::path_dir(filename)
  if (!dir_exists(dir_path)) {
    dir_create(dir_path, recurse = TRUE)
  }
  cat(paste0(prefix, msg, end), file = filename, append = TRUE)
  if (shell) {
    cat(paste0(prefix, msg, end))
  }

  # if (send_to_websocket){
  #   listen_tools$send_to_websocket(msg, port=socket_port)
  # }
}


process_cut_by <- function(cut_by, cdesc) {
  #print("***")
  #print(cut_by)
  # Return NULL immediately if cut_by is NULL
  if (is.null(cut_by)) {
    return(NULL)
  }

  # If cut_by is a single string containing ':', split it into a vector
  if (is.character(cut_by) && length(cut_by) == 1 && grepl(":", cut_by)) {
    cut_by <- strsplit(cut_by, ":")[[1]]
  }

  # Ensure cut_by is now a character vector
  if (!is.character(cut_by)) {
    # warning("cut_by should be a character string or vector.")
    # return(NULL)
    # this is fine
    cut_by <- as.character(cut_by)
  }

  # Check if all elements in cut_by are valid column names
  invalid_cols <- setdiff(cut_by, colnames(cdesc))
  if (length(invalid_cols) > 0) {
    warning(
      "The following cut_by elements are not column names in cdesc: ",
      paste(invalid_cols, collapse = ", ")
    )
    return(NULL)
  }

  # Subset the relevant columns and create the interaction factor
  cut_by_factor <- interaction(cdesc[, cut_by, drop = FALSE], drop = TRUE)

  print("***")
  print(cut_by_factor)

  return(cut_by_factor)
}
