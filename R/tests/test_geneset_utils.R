# test_geneset_utils.R

library(assertthat)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(magrittr))
source("../geneset_utils.R")


#' this is a mock get_collection
xx_get_collection <- function(category, subcategory, species, ...) {
    vars <- list(species = species, category = category, subcategory = subcategory)
    print("Calling get_collection")
    print(vars)
}

test_get_geneset <- function() {
    genesets_json <- '[
        {"category": "H", "subcategory": ""},
        {"category": "C5", "subcategory": "GO:CC"}
    ]'
    genesets <- jsonlite::parse_json(genesets_json)
    species <- "Homo sapiens"


    genesets_dfs <- .GlobalEnv$get_collections(genesets, species = species)

    genesets_lists <- purrr::map(genesets_dfs, .GlobalEnv$genesets_df_to_list)


    expected_names <- genesets %>% purrr::map(
        ~ {
            paste(.x$category, .x$subcategory, sep = "_")
        }
    )

    assertthat::has_name(genesets_lists, expected_names)

    return("test passed")
}

(test_get_geneset())
