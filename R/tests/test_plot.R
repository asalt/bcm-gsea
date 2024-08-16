# test_plot.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(here))

src_dir <- file.path(here("R"))

io_tools <- new.env()
source(file.path(file.path(src_dir, "./io.R")), local = io_tools)

geneset_tools <- new.env()
source(file.path(src_dir, "./geneset_utils.R"), local = geneset_tools)

fgsea_tools <- new.env()
source(file.path(src_dir, "./fgsea.R"), local = fgsea_tools)

plot_tools <- new.env()
source(file.path(src_dir, "./plot.R"), local = plot_tools)


#
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

# ================================ test data ================================
TEST_DATA <- generate_test_data()
TEST_DATA_withCollapse <- generate_test_data(collapse = TRUE)



# ================================ tests ================================

test_that("test generate testdata", {
  res <- TEST_DATA
  testthat::expect_true(length(res) == 2) # we expect two geneset collections

  testthat::expect_true(
    res[[names(res)[1]]] %>% length() == 2 # we expect two rankobjs in each collection
  )

  testthat::expect_true(
    res[[names(res)[2]]] %>% length() == 2 # we expect two rankobjs in each collection
  )

  testthat::expect_true(
    "data.frame" %in% class(res[[names(res)[1]]][[1]]) # there are alot of brackets here
  )
})





test_that("test test data with collapse", {
  TEST_DATA_withCollapse
  results <- TEST_DATA_withCollapse$`C5_GO:BP` %>%
    map(~ .x %>%
      pull(mainpathway) %>%
      table())

  # Iterate through each result element and perform checks
  walk(results, ~ {
    expect_true(all(c("TRUE", "FALSE") %in% names(.x)), info = "Both TRUE and FALSE should be present in mainpathway.")
    expect_true(.x["TRUE"] > 0, info = "There should be more than 0 TRUE values.")
    expect_true(.x["FALSE"] > 0, info = "There should be more than 0 FALSE values.")
  })
})


test_that("test test data without collapse", {
  results <- TEST_DATA$`C5_GO:BP` %>%
    map(~ .x %>%
      pull(mainpathway) %>%
      table())

  # Iterate through each result element and perform checks
  walk(results, ~ {
    expect_true("TRUE" %in% names(.x), info = "only TRUE should be present in mainpathway count.")
    expect_false("FALSE" %in% names(.x), info = "FALSE should not be present in mainpathway count.")
    expect_true(.x["TRUE"] > 0, info = "There should be more than 0 TRUE values.")
  })
  #
})


test_that("test formatting for barplot", {
  data <- TEST_DATA[[1]][[1]]
  out <- plot_tools$prepare_data_for_barplot(data)

  testthat::expect_true(
    all(c("leadingEdgeNum", "leadingEdgeFrac") %in% colnames(out))
  )
})


test_that("test plot a single barplot", {
  plt <- TEST_DATA$H_[[1]] %>%
    plot_tools$barplot_with_numbers()
  testthat::expect_true(
    all(
      "gg" %in% class(plt),
      "ggplot" %in% class(plt)
    )
  )
  plt_b <- ggplot2::ggplot_build(plt)
  testthat::expect_true(
    "facet" %in% names(plt_b$layout)
  )
  testthat::expect_true(
    length(plt_b$layout$facet$params) == 0
  )
})

test_that("test plot a faceted barplot", {
  df <- TEST_DATA$H_ %>% fgsea_tools$concat_results_one_collection()
  # this should be tested in test_fgsea but we can test it here too
  testthat::expect_true(
    "data.frame" %in% class(df),
    info = "df is not a data.frame. this is an fgsea_tools problem"
  )
  testthat::expect_true(
    "rankname" %in% colnames(df),
    info = "rankname not in colnames of df. this is an fgsea_tools problem"
  )

  plt <- df %>% plot_tools$barplot_with_numbers()
  testthat::expect_true(
    all(
      "gg" %in% class(plt),
      "ggplot" %in% class(plt)
    ),
    info = "plt is not a ggplot object"
  )
  plt_b <- ggplot2::ggplot_build(plt)

  testthat::expect_true(
    "facet" %in% names(plt_b$layout)
  )
  testthat::expect_true(
    length(plt_b$layout$facet$params) > 0,
    info = "the plot is not faceted, but it should be as there are multiple ranknames (so it is a long form table)"
  )
})


test_that("test heatmap of NES", {
  gsea_res <- TEST_DATA
  res_c <- gsea_res %>% fgsea_tools$concat_results_all_collections()
  ht <- res_c[[1]] %>% plot_tools$plot_results_one_collection()

  testthat::expect_true(
    "Heatmap" %in% class(ht)
  )
})

test_that("test run though all heatmaps of NES", {
  gsea_res <- TEST_DATA
  res_c <- gsea_res %>% fgsea_tools$concat_results_all_collections()
  ht_list <- plot_tools$plot_results_all_collections(res_c)
  testthat::expect_true(
    "list" %in% class(ht_list)
  )

  for (ht in ht_list) {
    testthat::expect_true(
      "Heatmap" %in% class(ht)
    )
  }
})
