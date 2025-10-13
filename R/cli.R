suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(argparser))
suppressPackageStartupMessages(library(RcppTOML))
suppressPackageStartupMessages(library(here))
options(error = rlang::entrace)


source(file.path(here("R"), "run.R")) #
source(file.path(here("R"), "lazyloader.R")) #
util_tools <- get_tool_env("utils")

setClass("filetype")
setMethod(
  "coerce", c(from = "ANY", to = "filetype"),
  function(from, to) {
    if (!file.exists(from)) {
      stop(paste0(from, " does not exist, exiting.."))
    }
    return(from)
  }
)

get_parser <- function() {
  parser <- arg_parser("tackle2")
  parser <- add_argument(parser, "config", help = "toml config file", type = "filetype")
  return(parser)
}



main <- function() {
  parser <- get_parser()
  argv <- parse_args(parser)

  params <- RcppTOML::parseTOML(argv$config)

  cleaned_params <- util_tools$clean_args(params$params)
  run(cleaned_params) # named list with first order [params] and nested subsections
}

if (sys.nframe() == 0) { # if ran directly, not sourced, equivalent to python if __name__ == "__main__"
  main()
}
