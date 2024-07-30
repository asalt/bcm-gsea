# test_plot.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))
# suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(here))


io_tools <- new.env()
source("../io.R", local = io_tools)

geneset_tools <- new.env()
source("../geneset_utils.R", local = geneset_tools)

fgsea_tools <- new.env()
source("../fgsea.R", local = fgsea_tools)

plot_tools <- new.env()
source("../plot.R", local = plot_tools)


test_plot_bar_format <- function() {
  data <- fgsea_tools$simulate_preranked_data()
  geneset <- geneset_tools$get_collection("H", "")
  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]
  res <- rankobj %>% fgsea_tools$run_one(geneset_list)

  res_filtered <- res %>%
    arrange(pval) %>%
    head(5)


  out <- plot_tools$prepare_data_for_barplot(res_filtered)

  assertthat::has_name(
    out,
    c("leadingEdgeNum", "leadingEdgeFrac")
  )


  return("Success")
}


test_plot_bar <- function() {
  data <- fgsea_tools$simulate_preranked_data()
  geneset <- geneset_tools$get_collection("H", "")
  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]
  res <- rankobj %>% fgsea_tools$run_one(geneset_list)

  res_filtered <- res %>%
    arrange(pval) %>%
    head(5)


  out <- plot_tools$prepare_data_for_barplot(res_filtered)

  plt <- plot_tools$barplot_with_numbers(out)


  assertthat::assert_that(
    all(
      "gg" %in% class(plt),
      "ggplot" %in% class(plt)
    )
  )

  return("Success")
}




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


  # ht_list %>% purrr::map(~ {
  #     assert_that(
  #         "HeatmapList" %in% class(.x)
  #     )
  # })
  for (ht in ht_list) {
    testthat::expect_true(
      "Heatmap" %in% class(ht)
    )
  }
})




test_that("test test data with collapse", {
  # scratch
  # TEST_DATA$`C5_GO:BP` %>% map(~ .x %>%
  #     pull(mainpathway) %>%
  #     table())
  # TEST_DATA_withCollapse$`C5_GO:BP` %>% map(~ .x %>%
  #     pull(mainpathway) %>%
  #     table())

  # the test
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


test_that(" test test data without collapse", {
  # TEST_DATA$`C5_GO:BP` %>% map(~ .x %>%
  #     pull(mainpathway) %>%
  #     table())
  # TEST_DATA_withCollapse$`C5_GO:BP` %>% map(~ .x %>%
  #     pull(mainpathway) %>%
  #     table())


  # the test

  results <- TEST_DATA$`C5_GO:BP` %>%
    map(~ .x %>%
      pull(mainpathway) %>%
      table())

  # Iterate through each result element and perform checks
  walk(results, ~ {
    expect_true("TRUE" %in% names(.x), info = "only TRUE should be present in mainpathway count.")
    expect_false("FALSE" %in% names(.x), info = "FALSE should not be present in mainpathway count.")
    expect_true(.x["TRUE"] > 0, info = "There should be more than 0 TRUE values.") # Adjust the number based on your expectations
  })
  #
})


(test_plot_bar_format())
(test_plot_bar())


# ============================

other <- function() {
  # misc not for use
  .name <- res[1, "pathway"]

  enplot_data <- fgsea::plotEnrichmentData(geneset_list[[.name]], rankobj)

  rnkorder <- -rankobj %>% rank()

  #
  rankorder_df <- data.frame(
    id = names(rnkorder),
    rank = rnkorder,
    stat = rankobj
  )

  rankorder_edge <- rankorder_df %>% left_join(enplot_data$curve)
  rankorder_edge %<>% left_join(rename(enplot_data$ticks, stat_tick = stat))
  rankorder_edge %<>% left_join(rename(enplot_data$stats, stat_stat = stat))
  rankorder_edge$stat == rankorder_edge$stat_tick


  all(rankorder_edge$stat == rankorder_edge$stat_tick)

  rankorder_edge %<>% drop_na() %>% arrange(-ES)
  # rankorder_edge %<>% mutate( geneset_rank = 1:dim(rankorder_edge)[1])
  # i want to rank twice, once for positive es and one for negative es.
  # higher absolute value gets lower rank



  gseaParam <- 1
  ticksSize <- .2

  with(enplot_data, ggplot(data = curve) +
    geom_line(aes(x = rank, y = ES),
      color = "green"
    ) +
    geom_segment(data = ticks, mapping = aes(
      x = rank,
      y = -spreadES / 16, xend = rank, yend = spreadES / 16
    ), linewidth = ticksSize) +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "black") +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey92")
    ) +
    labs(x = "rank", y = "enrichment score"))

  with(enplot_data, ggplot(data = ticks) +
    geom_line(aes(x = rank, y = stat),
      color = "green"
    ) +
    geom_segment(data = ticks, mapping = aes(
      x = rank,
      y = -spreadES, xend = rank, yend = spreadES
    ), linewidth = ticksSize) +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, colour = "black") +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "grey92")
    ) +
    labs(x = "rank", y = "enrichment score"))

  rankorder_edge %>% ggplot(aes(x = stat_tick, y = ES)) +
    geom_point()



  dev.new()

  rankorder_edge %>%
    filter(!is.na(stat_stat)) %>%
    dim()
  ggplot(aes(x = rank, y = ES)) +
    geom_point()


  posES <- enplot_data$posES
  negES <- enplot_data$negES
  rankorder_edge %>%
    ggplot(aes(x = stat_tick, y = ES, col = rank)) +
    geom_point() +
    # scale_color_viridis_c(option = "magma") +
    scale_color_continuous(type = "viridis", option = "H") +
    # scale_color_continuous(type="viridis")+
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "blue", linetype = "dashed")
}
