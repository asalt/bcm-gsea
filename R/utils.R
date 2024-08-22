suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))


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

scale_gct <- function(gct, subset = NULL) {
  # Start the pipe with the initial gct object
  res <- gct %>%
    melt_gct() %>%
    {
      # Conditionally add group_by
      if (!is.null(subset)) {
        group_by(., id.x, !!sym(subset))
      } else {
        group_by(., id.x)
      }
    } %>%
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



make_random_gct <- function(nrow = 10, ncol = 4) {
  set.seed(369)
  nrow <- max(nrow, 1)
  ncol <- max(ncol, 1)
  .mat <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  .rids <- seq(1, dim(.mat)[1]) %>% as.character()
  .cids <- seq(1, dim(.mat)[2]) %>% as.character()
  .cids <- paste0("X", .cids)
  .cdesc <- data.frame(
    metavar1 = sample(letters[1:5], ncol, replace = T),
    metavar2 = sample(letters[1:5], ncol, replace = T)
  )
  .rdesc <- data.frame(
    rdesc = paste0("gene", seq(1, nrow))
  )
  gct <- cmapR::GCT(mat = .mat, rid = .rids, cid = .cids, cdesc = .cdesc, rdesc = .rdesc)
  gct
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



log_msg <- function(msg = NULL, info = NULL, debug = NULL, filename = "run.log", end = "\n") {
  level <- case_when(
    !is.null(msg) ~ "INFO",
    !is.null(info) ~ "INFO",
    !is.null(debug) ~ "DEBUG",
    TRUE ~ "INFO"
  )
  if (is.null(msg) && (!is.null(debug))) msg <- debug
  prefix <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), level, ": ")
  cat(paste0(prefix, msg, end), file = filename, append = TRUE)
}
