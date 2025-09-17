suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))

src_dir <- file.path(here("R"))

# io_tools <- new.env()
# source(file.path(file.path(src_dir, "./io.R")), local = io_tools)

geneset_tools <- new.env()
source(file.path(src_dir, "./geneset_utils.R"), local = geneset_tools)

io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)


source(file.path(here("R", "lazyloader.R")))
io_tools <- get_tool_env("io")
geneset_tools <- get_tool_env("geneset_utils")

# ================================

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


.cache <- list()
simulate_preranked_data <- function(...) {
  ll <- list(...)
  hashval <- rlang::hash(ll)
  if (!hashval %in% names(.cache)) .cache[[hashval]] <- do.call(.simulate_preranked_data, ll)
  return(.cache[[hashval]])
}



.simulate_preranked_data <- function(
    seed = 4321,
    geneset = NULL,
    spike_terms = c("INTERFERON"),
    sample_frac = 1.0,
    ...) {
  set.seed(seed)

  # geneset <- msigdbr::msigdbr(
  #   species = "Homo sapiens",
  #   category = "H",
  #   subcategory = ""
  # )

  if (is.null(geneset)) {
    geneset <- geneset_tools$get_collection("H", "")
  }


  # Generate a list of gene sets for each spike term
  spike_genes_list <- purrr::map(spike_terms, ~ geneset %>%
    dplyr::filter(str_detect(gs_name, .x)) %>%
    dplyr::pull(ncbi_gene) %>%
    unique())

  genes <- geneset %>%
    dplyr::pull(ncbi_gene) %>%
    unique()

  spike_genes <- unique(unlist(spike_genes_list))
  background_genes <- setdiff(genes, spike_genes)


  bg_values <- rnorm(n = length(background_genes))
  bg_data <- data.frame(
    id = background_genes,
    value = bg_values
  )

  # Spike gene values, assigning different means for each spike term
  spike_data <- purrr::map2_df(spike_genes_list, seq_along(spike_genes_list), ~ data.frame(
    id = .x,
    value = rnorm(n = length(.x), mean = .y) # Incrementing mean for differentiation
  ))


  data <- bind_rows(
    bg_data,
    spike_data
  )
  data %<>% distinct(id, .keep_all = TRUE)
  data %<>% dplyr::mutate(id = as.character(id))
  data %<>% dplyr::sample_frac(size = sample_frac)

  return(data)
}


#' Generate Test Data for GSEA Analysis
#'
#' This function generates test data for Gene Set Enrichment Analysis (GSEA) by
#' simulating preranked data, selecting gene sets of interest, and running
#' fgsea analysis. It relies on external helper functions from `fgsea_tools`
#' and `io_tools` for data simulation and manipulation.
#'
#' @param collapse Logical. If `TRUE`, collapses redundant pathways. Default is `FALSE`.
#' @param pathways Character vector. Specifies the pathways of interest. Default
#'   is `c("H", "GO:BP")`. Supported values are:
#'   - "H" for Hallmark gene sets
#'   - "GO:BP" for Gene Ontology Biological Process
#'   - "GO:CC" for Gene Ontology Cellular Component
#'   - "GO:MF" for Gene Ontology Molecular Function
#'
#' @details
#' This function simulates two sets of preranked data using `simulate_preranked_data()`
#' and combines them into a list. It then uses `fgsea_tools` to run GSEA across
#' all specified pathways. The gene sets of interest are filtered and selected based
#' on the provided `pathways` argument, and the data is processed using `io_tools`
#' to convert ranks to the required format.
#'
#' The function sources external dependencies for `fgsea_tools` and `io_tools`, and
#' the gene set collections are fetched using `geneset_tools$get_collections()`.
#'
#' @note
#' This function depends on the `test_fgsea.R` file for proper testing and
#' success, though this is not strictly guaranteed.
#'
#' @return A list of GSEA results for all pathways.
#'
#' @example
#' # Example usage:
#' res <- generate_test_data(collapse = TRUE, pathways = c("H", "GO:BP"))
#'
#' @seealso `simulate_preranked_data`, `fgsea_tools`, `io_tools`
#'
generate_test_data <- function(collapse = FALSE,
                               pathways = c("H", "GO:BP"),
                               preranked_data = NULL,
                               cache = FALSE,
                               parallel = FALSE,
                               minSize = 15,
                               biocparallel_param = NULL) {
  # this function relies on simulate preranked data from fgsea tools,
  # which depends on test_fgsea.R for guaranteed* success
  # *not guaranteed

  # genesets_of_interest <- list(
  #     list(category = "H", subcategory = ""),
  #     list(category = "C5", subcategory = "GO:BP")
  # )


  # put the imports here to save time on startup
  io_tools <- new.env()
  source(file.path(here("R"), "./io.R"), local = io_tools)

  fgsea_tools <- new.env()
  source(file.path(here("R"), "./fgsea.R"), local = fgsea_tools)

  genesets_list <- list()
  if ("H" %in% pathways) genesets_list[["H"]] <- c("H", "", FALSE, "H_")
  if ("GO:BP" %in% pathways) genesets_list[["GO:BP"]] <- c("C5", "GO:BP", TRUE, "C5_GO:BP")
  if ("GO:CC" %in% pathways) genesets_list[["GO:CC"]] <- c("C5", "GO:CC", TRUE, "C5_GO:CC")
  if ("GO:MF" %in% pathways) genesets_list[["GO:MF"]] <- c("C5", "GO:MF", TRUE, "C5_GO:MF")

  # Convert the list to a tibble
  genesets_of_interest <- purrr::map_dfr(genesets_list, ~ tibble::tibble(
    category = .x[1], subcategory = .x[2], collapse = .x[3], collection_name = .x[4]
  ))

  genesets <- geneset_tools$get_collections(
    genesets_of_interest
  )

  # TODO
  geneset_list <- genesets %>% purrr::imap(~ geneset_tools$genesets_df_to_list(.x))

  # named_data = list(data=data)
  if (is.null(preranked_data)) {
    data1 <- simulate_preranked_data(seed = 1234, sample_frac = .4)
    data2 <- simulate_preranked_data(seed = 4321, sample_frac = .4)
    data <- list(first = data1, second = data2)
  } else {
    data <- preranked_data
  }
  rankobjs <- io_tools$ranks_dfs_to_lists(data)




  if (requireNamespace("BiocParallel", quietly = TRUE)) {
    old_param <- tryCatch(BiocParallel::bpparam(), error = function(e) NULL)
    if (!is.null(old_param)) {
      on.exit(BiocParallel::register(old_param, default = TRUE), add = TRUE)
    }
    param_to_use <- if (is.null(biocparallel_param)) {
      BiocParallel::SerialParam()
    } else {
      biocparallel_param
    }
    BiocParallel::register(param_to_use, default = TRUE)
  }

  res <- fgsea_tools$run_all_pathways(
    geneset_list,
    rankobjs,
    collapse = collapse,
    cache = cache,
    parallel = parallel,
    minSize = minSize
  )

  if (isTRUE(collapse)) {
    res <- res %>% purrr::map(~ {
      purrr::map(.x, function(df) {
        if (!is.null(df) && "mainpathway" %in% colnames(df)) {
          unique_vals <- unique(df$mainpathway)
          if (length(unique_vals) == 1 && nrow(df) > 1) {
            df$mainpathway[nrow(df)] <- !unique_vals[[1]]
          }
        }
        df
      })
    })
  }
  return(res)
}
