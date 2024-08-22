suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))

suppressPackageStartupMessages(library(here))

basedir <- file.path(here())
src_dir <- file.path(here("R"))

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)
make_partial <- util_tools$make_partial
get_args <- util_tools$get_args
get_arg <- util_tools$get_arg
log_msg <- util_tools$make_partial(util_tools$log_msg)

fgsea_tools <- new.env()
source(file.path(src_dir, "./fgsea.R"), local = fgsea_tools)

# moved to utils.R
# # Helper function to get current preset arguments or an empty list if none are set
# get_args <- function(f) {
#   if (!is.null(attr(f, "preset_args"))) {
#     return(attr(f, "preset_args"))
#   } else {
#     return(list()) # Return an empty list for easier handling
#   }
# }
# get_arg <- function(f, arg) {
#   args <- get_args(f)
#   val <- args[[arg]]
#   if (is.null(val)) {
#     return("")
#   }
#   return(val)
# }

# # Custom partial function with dynamic argument handling
# make_partial <- function(.f, ...) {
#   # Retrieve current preset arguments, if any
#   current_args <- get_args(.f)

#   # New fixed arguments
#   args_fixed <- list(...)

#   # Ensure that named arguments are handled properly
#   if (!is.null(names(args_fixed))) {
#     # Overwrite or add new arguments
#     current_args[names(args_fixed)] <- args_fixed
#   }

#   # Inner function to call .f with the correct arguments
#   inner <- function(...) {
#     # Combine fixed arguments with any new ones provided at call time
#     args <- modifyList(current_args, list(...))
#     do.call(.f, args)
#   }

#   # Attach updated preset arguments to the inner function for later inspection
#   attr(inner, "preset_args") <- current_args

#   return(inner)
# }


plot_and_save <- function(
    plot_code,
    filename,
    path = file.path(
      basedir,
      "plots/"
    ),
    type = "pdf",
    width = 8,
    height = 6,
    replace = T,
    ...) {
  # Setup: Open the appropriate graphics device

  log_msg(msg = "plot_and_save")

  full_path <- file.path(path, paste0(filename, ".", type))

  log_msg(msg = paste0("full_path: ", full_path))

  if (!fs::dir_exists(path)) fs::dir_create(path)

  if (file.exists(full_path) && replace == FALSE) {
    return()
  }

  if (type == "pdf") {
    pdf(full_path, width = width, height = height)
  } else if (type == "png") {
    png(full_path, width = width, height = height, units = "in", res = 300)
  } else {
    stop("Unsupported file type")
  }

  # Execute the plot code (passed as a function)
  h <- plot_code()

  # Teardown: Close the graphics device
  dev.off()
  log_msg(msg = paste0("done"))

  return(h)
}
