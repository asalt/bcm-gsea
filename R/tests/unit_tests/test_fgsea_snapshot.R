suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(withr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(here))

fgsea_tools <- new.env()
source(file.path(here("R"), "fgsea.R"), local = fgsea_tools)


snapshot_path <- file.path(here("R"), "tests", "unit_tests", "fixtures", "fgsea_basic_results.rds")


test_that("run_all_pathways output matches golden snapshot", {
  local_seed(20240229)

  ranks <- list(
    sample_A = setNames(c(2.5, 1.7, 0.9, -0.4, -1.2), paste0("gene", 1:5)),
    sample_B = setNames(c(-1.5, -0.2, 0.6, 1.1, 2.3), paste0("gene", 1:5))
  )

  geneset_lists <- list(
    TestCollection = list(
      PathwayAlpha = c("gene1", "gene3", "gene5"),
      PathwayBeta = c("gene2", "gene4")
    )
  )

  original_mapper <- fgsea_tools$map_tools$add_leadingedges_to_results_list
  defer(assign("add_leadingedges_to_results_list", original_mapper, envir = fgsea_tools$map_tools))
  assign(
    "add_leadingedges_to_results_list",
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
    },
    envir = fgsea_tools$map_tools
  )

  fake_fgsea <- function(pathways, stats, ...) {
    tibble(
      pathway = names(pathways),
      pval = seq_along(pathways) / (length(pathways) + 1),
      padj = pval * 1.5,
      log2err = log2(map_int(pathways, length) + 1),
      ES = map_dbl(pathways, ~ sum(stats[.x], na.rm = TRUE)),
      NES = ES / map_int(pathways, length),
      size = map_int(pathways, length),
      leadingEdge = map(pathways, ~ .x[seq_len(min(3, length(.x)))] )
    )
  }

  logger <- function(...) invisible(NULL)

  results <- with_mocked_bindings({
    fgsea_tools$run_all_pathways(
      geneset_lists = geneset_lists,
      ranks = ranks,
      parallel = FALSE,
      minSize = 1,
      maxSize = 10,
      collapse = FALSE,
      cache = FALSE,
      cache_dir = tempdir(),
      logger = logger,
      species = "Homo sapiens"
    )
  }, fgsea = fake_fgsea, .package = "fgsea")

  combined <- fgsea_tools$concat_results_all_collections(results)
  combined <- purrr::imap_dfr(combined, ~ mutate(.x, collection = .y)) %>%
    arrange(collection, pathway, rankname)

  if (identical(Sys.getenv("UPDATE_FGSEA_FIXTURES"), "1")) {
    saveRDS(combined, snapshot_path)
  }

  expect_true(file.exists(snapshot_path))

  expected <- readRDS(snapshot_path)
  expect_equal(combined, expected)
})
