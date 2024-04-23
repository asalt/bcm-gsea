# test_fgsea.R
source("../fgsea.R")
source("../io.R")
source("../geneset_utils.R")
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggplot2))


test_fgsea_runone <- function() {
  data <- .GlobalEnv$simulate_preranked_data()
  geneset <- .GlobalEnv$get_collection("H", "")
  geneset_list <- .GlobalEnv$genesets_df_to_list(geneset)
  # named_data = list(data=data)
  rankobjs <- .GlobalEnv$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]
  res <- rankobj %>% .GlobalEnv$run_one(geneset_list)


  return("Success")
}


test_get_edge <- function() {
  data <- .GlobalEnv$simulate_preranked_data()
  geneset <- .GlobalEnv$get_collection("H", "")
  geneset_list <- .GlobalEnv$genesets_df_to_list(geneset)
  # named_data = list(data=data)
  rankobjs <- .GlobalEnv$ranks_dfs_to_lists(list(data))
  rankobj <- rankobjs[[1]]

  res <- rankobj %>% .GlobalEnv$run_one(geneset_list) # we aren't actually using this result
  # all we need for this test is the rankobj and gene list

  geneset_name <- names(geneset_list)[1]
  geneset_collection_ids <- geneset_list[[geneset_name]]

  rankorder_edge <- .GlobalEnv$get_rankorder(rankobj, geneset_collection_ids)

  rankorder_edge_specific <- rankorder_edge %>% filter(id %in% geneset_collection_ids)

  assertthat::noNA(rankorder_edge_specific$stat_tick)
  assertthat::assert_that(
    all(rankorder_edge_specific$stat == rankorder_edge_specific$stat_tick),
    TRUE
  )


  return("Success")
}

other <- function() {
  .name <- res[1, "pathway"]

  enplot_data <- plotEnrichmentData(geneset_list[[.name]], rankobj)

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



(test_fgsea_runone())
(test_get_edge())
