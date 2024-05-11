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

plot_tools <- new.env()
source("../plot.R", local = plot_tools)




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

test_that("test run all collapse.", { # this will take a while. testing if can set collapse. var
  geneset <- geneset_tools$get_collection("C5", "GO:BP")
  spike_terms <- c("CYCLE", "CHECKPOINT")
  data <- fgsea_tools$simulate_preranked_data(geneset = geneset)
  data %<>% dplyr::sample_frac(size = .25)

  geneset_list <- geneset_tools$genesets_df_to_list(geneset)
  rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]

  res <- fgsea_tools$run_all_pathways(
    list(geneset_list),
    rankobjs,
    collapse = TRUE
  )

  res_all <- fgsea_tools$run_all_pathways(
    list(geneset_list),
    rankobjs,
    collapse = FALSE
  )

  testthat::expect_true(
    all(res[[1]]$NES == res_all[[1]]$NES)
  )

  testthat::expect_true(
    res[[1]] %>% dplyr::filter(mainpathway == TRUE) %>% nrow() <=
      res_all[[1]] %>%
        dplyr::filter(mainpathway == TRUE) %>%
        nrow()
  )

  testthat::expect_true(
    res[[1]] %>%
      dplyr::filter(mainpathway == TRUE) %>%
      nrow() > 1
  )
})


other <- function() {
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



# (test_fgsea_runone())
# (test_get_edge())
