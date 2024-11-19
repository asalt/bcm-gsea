# test_geneset_utils.R

library(assertthat)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(testthat))

geneset_tools <- new.env()
source("../geneset_utils.R", local = geneset_tools)

#' this is a mock get_collection
#' that isn't being used currently
xx_get_collection <- function(category, subcategory, species, ...) {
  vars <- list(species = species, category = category, subcategory = subcategory)
  print("Calling get_collection")
  print(vars)
}



test_that("invalid json", {
  genesets_json <- '[
    {"invalid": "H", "subcategory": ""},
    {"invalid": "C5", "subcategory": "GO:CC"}
  ]'
  genesets <- jsonlite::fromJSON(genesets_json)
  expect_error(geneset_tools$get_collections(genesets))
})


testthat::test_that(
  "test get geneset",
  {
    print("running test")
    genesets_json <- '[
      {"category": "H", "subcategory": ""},
      {"category": "C5", "subcategory": "GO:CC"}
  ]'
    genesets <- jsonlite::fromJSON(genesets_json)

    genesets <- genesets %>% dplyr::mutate(
      collection_name = stringr::str_c(category, subcategory, sep = "_")
    )

    list_of_geneset_dfs <- genesets %>% geneset_tools$get_collections()

    species <- "Homo sapiens"
    genesets_dfs <- geneset_tools$get_collections(genesets, species = species)

    genesets_lists <- purrr::map(genesets_dfs, geneset_tools$genesets_df_to_list)


    for (name in genesets$collection_name) {
      expect_true(
        name %in% names(genesets_lists)
      )
    }
  }
)
