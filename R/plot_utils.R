suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ComplexHeatmap))
suppressPackageStartupMessages(library(circlize))
library(colorspace)

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


plot_and_save_unsafe <- function(
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
  if (is.null(filename)){
      log_msg(warning = "filename is null")
  }

  full_path <- file.path(path, paste0(filename, ".", type))

  log_msg(msg = paste0("full_path: ", full_path))

  if (!fs::dir_exists(path)) fs::dir_create(path)

  if (file.exists(full_path) && replace == FALSE) {
    graphics.off() # turn off anything that opened
    return()
  }
  # ??
  on.exit(dev.off(), add = TRUE)

  if (type == "pdf") {
    # pdf(full_path, width = width, height = height)
    cairo_pdf(full_path, width = width, height = height)
  } else if (type == "png") {
    png(full_path, width = width, height = height, units = "in", res = 300)
  } else {
    stop("Unsupported file type")
  }


  # Execute the plot code (passed as a function)
  h <- plot_code()
  # if (inherits(h, "ggplot")) {  # this may be included already in the plot_code closure and unnecessary and not worth pytting here
  #   print(h)  # Required for ggplot rendering inside functions
  # }

  # Teardown: Close the graphics device
  # dev.off()
  # Ensure device closes even if an error occurs by putting the command here
  # is this the proper way to do this to ensure avoiding error of having too many open devices?
  # check all open devices with dev.list()
  # on.exit(dev.off(), add = TRUE)  moved to above

  log_msg(msg = paste0("done"))

  return(h)
}

safe_plot_and_save <- function(...) {
  tryCatch(
    plot_and_save_unsafe(...),
    error = function(e) {
      message("\n❗️ Error caught: ", conditionMessage(e))
      message("\nLast traceback:")
      print(rlang::last_trace(drop = FALSE))
      
      message("\nOpen graphics devices:")
      print(dev.list())

      # Optional: Force close them
      if (length(dev.list()) > 0) {
        message("\nClosing all devices...")
        graphics.off()
      }
      
      # Optional: Write to log file
      log_msg(warning = paste("Plotting failed:", conditionMessage(e)))

      # Optionally re-throw if you want upstream failure
      # stop(e)
    }
  )
}
plot_and_save <- safe_plot_and_save # 


# Matplotlib default colors
#matplotlib_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
#                       "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
#                       "#bcbd22", "#17becf")

create_named_color_list <- function(df, columns, c=80) {

  # Initialize an empty list to store the result
  color_list <- list()

  # Iterate over each column
  for (col_name in columns) {
    df[[col_name]][is.na(df[[col_name]])] <- "NA"
    unique_vals <- sort(unique(df[[col_name]]))
    n_vals <- length(unique_vals)

    # Assign colors based on the number of unique values, recycling matplotlib colors if needed
    # colors_assigned <- rep(matplotlib_colors, length.out = n_vals)
    colors_assigned <- colorspace::qualitative_hcl(palette='Dynamic', n=n_vals, c=c)


    # Create a named vector for the unique values with corresponding colors
    color_list[[col_name]] <- setNames(colors_assigned, unique_vals)
  }

  return(color_list)
}



process_cut_by <- function(cut_by, cdesc) {
  print("***")
  print(cut_by)
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

