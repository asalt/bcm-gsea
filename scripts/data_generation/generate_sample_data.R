library(tidyverse)
library(jsonlite)
library(here)

src_dir <- file.path(here("R"))
source(file.path(src_dir, "geneset_utils.R")) # yes could just call here("R", "geneset_utils.R")
source(file.path(src_dir, "io.R"))
source(file.path(src_dir, "fgsea.R"))
source(file.path(src_dir, "plot.R"))




# ranks

rank_table <- .GlobalEnv$simulate_preranked_data()

json_data <- rank_table %>%
    head() %>%
    jsonlite::toJSON() %>%
    jsonlite::prettify()

outf <- here("data", "sample", "ranks.json")
writeLines(json_data, outf)

# =============================


geneset <- .GlobalEnv$get_collection("H", "")
geneset_list <- .GlobalEnv$genesets_df_to_list(geneset)
rankobjs <- .GlobalEnv$ranks_dfs_to_lists(list(rank_table))
rankobj <- rankobjs[[1]]

gsea_res <- rankobj %>% .GlobalEnv$run_one(geneset_list)


json_data <- gsea_res %>%
    head() %>%
    jsonlite::toJSON() %>%
    jsonlite::prettify()
outf <- here("data", "sample", "gsea_res.json")
json_data %>% writeLines(outf)


# =============================


geneset_name <- names(geneset_list)[1]
geneset_collection_ids <- geneset_list[[geneset_name]]
rankorder_edge <- .GlobalEnv$get_rankorder(rankobj, geneset_collection_ids)
rankorder_edge_specific <- rankorder_edge %>% filter(id %in% geneset_collection_ids)

json_data <- rankorder_edge_specific %>%
    dplyr::select(-stat_stat) %>%
    dplyr::mutate(geneset = geneset_name) %>%
    head() %>%
    jsonlite::toJSON(pretty = TRUE)

json_data %>% writeLines(here("data", "sample", "rankorder_edge.json"))



# =============================


# plt <- gsea_res %>% .GlobalEnv$barplot_with_numbers()
