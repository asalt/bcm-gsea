# test_geneset_utils.R

library(magrittr)
source("../R/geneset_utils.R")


#' this is a mock get_collection
get_collection <- function(category, subcategory, species, ...){
    vars <- list(species = species, category = category, subcategory = subcategory)
    print("Calling get_collection")
    print(vars)
}

test_get_geneset <- function(){

    genesets_json <- '[
        {"category": "H", "subcategory": ""},
        {"category": "C5", "subcategory": "GO:CC"}
    ]'
    genesets <- jsonlite::parse_json(genesets_json)
    species <- "Homo sapiens"


    .GlobalEnv$get_collections(genesets, species=species)


}

( test_get_geneset() )