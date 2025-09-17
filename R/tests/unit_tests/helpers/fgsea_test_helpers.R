suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))

fgsea_test_fake_fgsea <- function(pathways, stats, ...) {
  if (length(pathways) == 0) {
    return(tibble())
  }

  sizes <- purrr::map_int(pathways, length)
  es_values <- purrr::map_dbl(pathways, ~ {
    genes <- intersect(.x, names(stats))
    sum(stats[genes], na.rm = TRUE)
  })

  nes_values <- ifelse(sizes == 0, 0, es_values / sizes)

  tibble(
    pathway = names(pathways),
    pval = rep(0.01, length(pathways)),
    padj = rep(0.02, length(pathways)),
    log2err = rep(0, length(pathways)),
    ES = es_values,
    NES = nes_values,
    size = sizes,
    leadingEdge = purrr::map(pathways, ~ head(.x, n = min(3, length(.x))))
  )
}

fgsea_test_leadingedge_stub <- function() {
  function(fgsea_res_list, species = NULL) {
    purrr::map(fgsea_res_list, ~ {
      if (is.null(.x) || !"leadingEdge" %in% names(.x)) {
        return(.x)
      }
      edge_strings <- purrr::map_chr(.x$leadingEdge, ~ paste(.x, collapse = "/"))
      .x$leadingEdge_entrezid <- edge_strings
      .x$leadingEdge_genesymbol <- edge_strings
      .x
    })
  }
}

fgsea_test_install_leadingedge_stub <- function(map_env) {
  original <- NULL
  if (exists("add_leadingedges_to_results_list", envir = map_env, inherits = FALSE)) {
    original <- get("add_leadingedges_to_results_list", envir = map_env)
  }
  assign("add_leadingedges_to_results_list", fgsea_test_leadingedge_stub(), envir = map_env)
  original
}

fgsea_test_restore_leadingedge_stub <- function(map_env, original) {
  if (!is.null(original)) {
    assign("add_leadingedges_to_results_list", original, envir = map_env)
  } else {
    rm("add_leadingedges_to_results_list", envir = map_env)
  }
}
