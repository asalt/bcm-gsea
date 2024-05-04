suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(furrr))

src_dir <- file.path(here("R"))
source(file.path(src_dir, "./io.R"))


run_one <- function(rankobj, geneset) {
  # to look for duplicate gene names
  # rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  .maxcounts <- rankobj %>%
    names() %>%
    table() %>%
    as.data.frame() %>%
    pull(Freq) %>%
    max()
  assertthat::are_equal(.maxcounts, 1)
  fgseaRes <- fgsea(
    pathways = geneset, stats = rankobj, minSize = 15, maxSize = 500,
    # eps = 0.0
  ) # , nperm=1000)
}

run_all_rankobjs <- function(pathway, rankobjs, parallel = F) {
  # rankobjs %>% furrr::future_map( # maybe later
  if (parallel) {
    future::plan(future::multisession, workers = future::availableCores() - 1)
    rankobjs %>% furrr::future_map(
      ~ run_one(., geneset = pathway)
    )
  } else {
    rankobjs %>% purrr::map(
      ~ run_one(., geneset = pathway)
    )
  }
  # rankobjs %>% purrr::map(
  #   ~ run_one(., geneset = pathway)
  # )
}
run_all_pathways <- function(pathways_list, ranks, parallel = F) {
  pathways_list %>% purrr::map(
    ~ run_all_rankobjs(., rankobjs = ranks, parallel = parallel)
  )
}
# results_list <- run_all_pathways(pathways_list_of_lists, ranks_list)


get_rankorder <- function(rankobj, geneset) {
  enplot_data <- plotEnrichmentData(geneset, rankobj)
  rnkorder <- -rankobj %>% rank()
  rankorder_df <- data.frame(
    id = names(rnkorder),
    rank = rnkorder,
    stat = rankobj
  )

  rankorder_edge <- rankorder_df %>% left_join(enplot_data$curve)
  rankorder_edge %<>% left_join(rename(enplot_data$ticks, stat_tick = stat))
  rankorder_edge %<>% left_join(rename(enplot_data$stats, stat_stat = stat))
  rankorder_edge$stat == rankorder_edge$stat_tick


  return(rankorder_edge)
}

simulate_preranked_data <- function(...) {
  set.seed(4321)

  # geneset <- msigdbr::msigdbr(
  #   species = "Homo sapiens",
  #   category = "H",
  #   subcategory = ""
  # )

  geneset <- .GlobalEnv$get_collection("H", "")

  ifn_genes <- geneset %>%
    filter(str_detect(gs_name, "INTERFERON")) %>%
    pull(entrez_gene) %>%
    unique()

  genes <- geneset %>%
    pull(entrez_gene) %>%
    unique()

  background_genes <- setdiff(genes, ifn_genes)


  bg_values <- rnorm(n = length(background_genes))
  ifn_values <- rnorm(n = length(ifn_genes), mean = 1)
  bg_data <- data.frame(
    geneid = background_genes,
    value = bg_values
  )
  ifn_data <- data.frame(
    geneid = ifn_genes,
    value = ifn_values
  )

  data <- bind_rows(
    bg_data,
    ifn_data
  )

  return(data)
}
