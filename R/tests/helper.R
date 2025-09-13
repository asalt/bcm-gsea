library(testthat)


generate_test_data <- function(collapse = FALSE) {
  # this function relies on simulate preranked data from fgsea tools,
  # which depends on test_fgsea.R for guaranteed* success
  # *not guaranteed

  # genesets_of_interest <- list(
  #     list(category = "H", subcategory = ""),
  #     list(category = "C5", subcategory = "GO:BP")
  # )

  genesets_of_interest <- tibble::tribble(
    ~category, ~subcategory, ~collapse, ~collection_name,
    "H", "", FALSE, "H_",
    "C5", "GO:BP", FALSE, "C5_GO:BP",
  )
  genesets <- geneset_tools$get_collections(
    genesets_of_interest
  )

  geneset_list <- genesets %>% purrr::imap(~ geneset_tools$genesets_df_to_list(.x))

  # named_data = list(data=data)
  data1 <- fgsea_tools$simulate_preranked_data(seed = 1234) %>% dplyr::sample_frac(.4)
  data2 <- fgsea_tools$simulate_preranked_data(seed = 4321) %>% dplyr::sample_frac(.4)
  data <- list(first = data1, second = data2)
  rankobjs <- io_tools$ranks_dfs_to_lists(data)
  # res <- fgsea_tools$run_all_pathways(geneset_list, rankobjs, collapse. = collapse)
  res <- fgsea_tools$run_all_pathways(geneset_list, rankobjs, collapse = collapse)
  return(res)
}
