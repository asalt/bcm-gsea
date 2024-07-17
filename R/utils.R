library(cmapR)
library(magrittr)


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
  return(out)
}

dist_no_na <- function(mat) {
  mat[is.na(mat)] <- min(mat, na.rm = TRUE)
  edist <- dist(mat)
  return(edist)
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


# this is exploratory rewrite of plot_utils::make_partial
get_arg <- function(f, arg, default = "") {
  args <- if (!is.null(attr(f, "preset_args"))) attr(f, "preset_args") else list()
  if (arg %in% names(args)) {
    return(args[[arg]])
  }
  return(default)
}

# Revised make_partial using an environment for cleaner argument handling
make_partial <- function(.f, ...) {
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
    combined_args <- modifyList(env$preset_args, list(...))
    do.call(.f, combined_args)
  }
  
  # Attach environment as an attribute (optional but can be helpful for debugging)
  attr(inner, "preset_args", env$preset_args)
  
  return(inner)
}
