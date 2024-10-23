suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(argparser))
suppressPackageStartupMessages(library(RcppTOML))
suppressPackageStartupMessages(library(here))

source(file.path(here("R"), "run.R")) #

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
  parser <- arg_parser("bcm-gsea")
  parser <- add_argument(parser, "config", help = "toml config file", type = "filetype")
  return(parser)
}

clean_args <- function(params) {
  if (is.null(params$savedir)) {
    params$savedir <- file.path("./plots")
  }

  params$barplot$do_individual <- params$barplot$do_individual %||% TRUE
  params$barplot$do_combined <- params$barplot$do_combined %||% TRUE

  params$heatmap_gsea$do <- params$heatmap_gsea$do %||% TRUE
  params$heatmap_gene$do <- params$heatmap_gene$do %||% TRUE
  params$pca$do <- params$pca$do %||% TRUE

  if (!is.null(params$gct_path) && !file.exists(params$gct_path)) {
    stop(paste0(params$gct_path, " does not exist, exiting.."))
  }


  # print(params$enplot$combine_by)
  params$enplot$combine_by <- params$enplot$combine_by %||% NULL
  # print(params$enplot$combine_by)


  cachedir <- params$advanced$cachedir
  if (!is.null(cachedir)) {
    if (cachedir == "savedir") {
      cachedir <- file.path(params$savedir, "cache")
    } else {
      cachedir <- params$advanced$cachedir
    }
  } else {
    cachedir <- NULL
  }
  params$advanced$cachedir <- cachedir


  rankfiledir <- params$rankfiledir %||% file.path(params$savedir, "ranks")
  if (!is.null(rankfiledir)) {
    if (rankfiledir == "savedir") {
      rankfiledir <- file.path(params$savedir, "ranks")
    }
  }
  params$rankfiledir <- rankfiledir

  params$advanced$pivot_gsea_results <- params$advanced$pivot_gsea_results %||% FALSE
  #
  if (!is.null(params$extra$rankname_order)) {
    if (length(params$extra$rankname_order) == 1 && params$extra$rankname_order == "sample_order") {
      params$extra$rankname_order <- params$extra$sample_order
    }
  } else {
    params$extra$rankname_order <- params$extra$sample_order
  }

  if (!is.null(params$extra$sample_order)) {
    if (length(params$extra$sample_order) == 1 && params$extra$sample_order == "rankname_order") {
      params$extra$sample_order <- params$extra$rankname_order
    }
  } else {
    params$extra$sample_order <- params$extra$rankname_order
  }

  params$species <- params$species %||% "Homo sapiens"

  params$genesets <- params$genesets %||% list(list(category = "H", subcategory = "", collapse = FALSE))

  params$advanced$quiet <- params$advanced$quiet %||% FALSE

  params$advanced$parallel <- params$advanced$parallel %||% FALSE

  logfile <- params$advanced$logfile %||% "run.log" %>% ifelse(. == "savedir", params$savedir, .)
  loglevel <- params$advanced$loglevel
  options("bcm_gsea_log_msg_filename" = logfile)
  options("bcm_gsea_loglevel" = loglevel)
  params$advanced$logfile <- logfile

  print(str(params))

  return(params)
}


main <- function() {
  parser <- get_parser()
  argv <- parse_args(parser)

  params <- RcppTOML::parseTOML(argv$config)

  cleaned_params <- clean_args(params$params)
  run(cleaned_params) # named list with first order [params] and nested subsections
}

if (sys.nframe() == 0) { # if ran directly, not sourced, equivalent to python if __name__ == "__main__"
  main()
}
