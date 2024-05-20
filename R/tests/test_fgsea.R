# test_fgsea.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))

# source("../fgsea.R")
# source("../io.R")
# source("../geneset_utils.R")


io_tools <- new.env()
source("../io.R", local = io_tools)

geneset_tools <- new.env()
source("../geneset_utils.R", local = geneset_tools)

fgsea_tools <- new.env()
source("../fgsea.R", local = fgsea_tools)

fgsea_tools_monkeypatch <- new.env()
source("../fgsea.R", local = fgsea_tools_monkeypatch)
fgsea_tools_monkeypatch$run_one <- function(...) {
  return("success")
}


plot_tools <- new.env()
source("../plot.R", local = plot_tools)


testthat::test_that("test fgsea parse additional_info", {
  #
  geneset <- geneset_tools$get_collection("H", "")
  geneset1 <- geneset_tools$get_collection("C5", "GO:MF")

  data <- fgsea_tools$simulate_preranked_data()
  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  geneset_list1 <- geneset_tools$genesets_df_to_list(geneset1)
  geneset_lists <- list("H_" = geneset_list, "C5_GO:MF" = geneset_list1)

  ranks <- io_tools$ranks_dfs_to_lists(list(test = data))

  # genesets_info <- tibble::tribble(
  #   ~category, ~subcategory, ~collapse,
  #   "H", "", FALSE,
  #   "C3", "GO:MF", TRUE
  # ) %>% dplyr::mutate(collection_name = stringr::str_c(category, subcategory, sep = "_")) # todo put this somewhere else


  genesets_info <- tibble::tribble(
    ~category, ~subcategory,
    "H", "",
    "C5", "GO:MF",
  ) %>% dplyr::mutate(collection_name = stringr::str_c(category, subcategory, sep = "_")) # todo put this somewhere else

  testthat::expect_no_error(
    .res <- fgsea_tools_monkeypatch$run_all_pathways(
      geneset_lists = geneset_lists,
      ranks = ranks,
      parallel = FALSE,
    )
  )
  #

  #
})



test_fgsea_runone <- function() {
  # data <- .GlobalEnv$simulate_preranked_data()
  data <- fgsea_tools$simulate_preranked_data()
  geneset <- geneset_tools$get_collection("H", "")
  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]
  res <- rankobj %>% fgsea_tools$run_one(geneset_list)


  return("Success")
}

test_get_edge <- function() {
  data <- fgsea_tools$simulate_preranked_data()
  geneset <- geneset_tools$get_collection("H", "")
  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]


  res <- rankobj %>% fgsea_tools$run_one(geneset_list) # we aren't actually using this result
  # all we need for this test is the rankobj and gene list

  geneset_name <- names(geneset_list)[1]
  geneset_collection_ids <- geneset_list[[geneset_name]]

  rankorder_edge <- fgsea_tools$get_rankorder(rankobj, geneset_collection_ids)

  rankorder_edge_specific <- rankorder_edge %>% filter(id %in% geneset_collection_ids)

  assertthat::noNA(rankorder_edge_specific$stat_tick)
  assertthat::assert_that(
    all(rankorder_edge_specific$stat == rankorder_edge_specific$stat_tick),
    TRUE
  )


  return("Success")
}


test_that("test fgsea runone", {
  expect_equal(test_fgsea_runone(), "Success")
})

test_that("test get edge", {
  expect_equal(test_get_edge(), "Success")
})



test_that("test run one collapse", {
  geneset <- geneset_tools$get_collection("C5", "GO:BP")
  spike_terms <- c("CYCLE", "CHECKPOINT")
  data <- fgsea_tools$simulate_preranked_data(geneset = geneset)
  data %<>% dplyr::sample_frac(size = .75)

  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]

  res <- rankobj %>% fgsea_tools$run_one(geneset_list, collapse = TRUE)
  res_all <- rankobj %>% fgsea_tools$run_one(geneset_list, collapse = FALSE)

  testthat::expect_true(
    all(res$NES == res_all$NES)
  )

  testthat::expect_true(
    res %>% dplyr::filter(mainpathway == TRUE) %>% nrow() <=
      res_all %>%
        dplyr::filter(mainpathway == TRUE) %>%
        nrow()
  )

  testthat::expect_true(
    res %>%
      dplyr::filter(mainpathway == TRUE) %>%
      nrow() > 1
  )
})

test_that("test run all geneset lists not named.", { # this will take a while. testing if can set collapse. var
  geneset <- geneset_tools$get_collection("C5", "GO:BP")
  data <- fgsea_tools$simulate_preranked_data(geneset = geneset)
  data %<>% dplyr::sample_frac(size = .25)

  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  geneset_lists <- list(geneset_list)

  rankobjs <- io_tools$ranks_dfs_to_lists(list(test = data))
  # rankobj <- rankobjs[[1]]

  testthat::expect_error(
    fgsea_tools$run_all_pathways(
      geneset_lists = geneset_lsits,
      ranks = rankobjs,
      parallel = FALSE,
    )
  )
})


test_that("test run all ranks lists not named.", { # this will take a while. testing if can set collapse. var
  geneset <- geneset_tools$get_collection("C5", "GO:BP")
  data <- fgsea_tools$simulate_preranked_data(geneset = geneset)
  data %<>% dplyr::sample_frac(size = .25)

  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  geneset_lists <- list("C5_GO:BP" = geneset_list)

  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  # rankobj <- rankobjs[[1]]

  testthat::expect_error(
    fgsea_tools$run_all_pathways(
      geneset_lists = geneset_lsits,
      ranks = rankobjs,
      parallel = FALSE,
    )
  )
})


test_that("test run all collapse.", { # this will take a while. testing if can set collapse. var
  geneset <- geneset_tools$get_collection("C5", "GO:BP")
  spike_terms <- c("CYCLE", "CHECKPOINT")
  data <- fgsea_tools$simulate_preranked_data(geneset = geneset)
  data %<>% dplyr::sample_frac(size = .25)

  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  geneset_lists <- list("C5_GO:BP" = geneset_list)

  rankobjs <- io_tools$ranks_dfs_to_lists(list(test = data))
  # rankobj <- rankobjs[[1]]

  res <- fgsea_tools$run_all_pathways(
    geneset_lists,
    rankobjs,
    collapse = TRUE
  )

  res_all <- fgsea_tools$run_all_pathways(
    geneset_lists,
    rankobjs,
    collapse = FALSE
  )

  testthat::expect_true(
    all(res[[1]]$NES == res_all[[1]]$NES)
  )

  testthat::expect_true(
    res[[1]][[1]] %>% dplyr::filter(mainpathway == TRUE) %>% nrow() <=
      res_all[[1]][[1]] %>%
        dplyr::filter(mainpathway == TRUE) %>%
        nrow()
  )

  testthat::expect_true(
    res[[1]][[1]] %>%
      dplyr::filter(mainpathway == TRUE) %>%
      nrow() > 1
  )
  #
})
