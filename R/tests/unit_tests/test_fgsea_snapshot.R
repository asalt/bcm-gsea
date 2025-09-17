suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(withr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(here))

fgsea_tools <- new.env()
source(file.path(here("R"), "fgsea.R"), local = fgsea_tools)

source(file.path(here("R"), "tests", "unit_tests", "helpers", "fgsea_test_helpers.R"))


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

  original_mapper <- fgsea_test_install_leadingedge_stub(fgsea_tools$map_tools)
  defer(fgsea_test_restore_leadingedge_stub(fgsea_tools$map_tools, original_mapper))

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
  }, fgsea = fgsea_test_fake_fgsea, .package = "fgsea")

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
