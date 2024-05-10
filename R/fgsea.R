suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(furrr))

src_dir <- file.path(here("R"))

io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)

geneset_tools <- new.env()
source(file.path(src_dir, "./geneset_utils.R"), local = geneset_tools)


run_one <- function(rankobj, geneset, minSize = 15, maxSize = 500, collapse = FALSE) {
  # to look for duplicate gene names
  # rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  .maxcounts <- rankobj %>%
    names() %>%
    table() %>%
    as.data.frame() %>%
    pull(Freq) %>%
    max()
  assertthat::are_equal(.maxcounts, 1)


  set.seed(789)

  fgseaRes <- fgsea(
    pathways = geneset, stats = rankobj, minSize = minSize, maxSize = maxSize,
    # eps = 0.0
  ) # , nperm=1000)

  if (collapse) {
    collapse_results <- fgseaRes %>%
      fgsea::collapsePathways(
        pathways = geneset,
        stats = rankobj,
        pval.threshold = 0.05,
        gseaParam = 1
      )
    fgseaRes <- fgseaRes %>% dplyr::mutate(
      mainpathway = pathway %in% collapse_results$mainPathways
    )
  } else {
    fgseaRes$mainpathway <- TRUE
  }


  return(fgseaRes)
}

run_all_rankobjs <- function(pathway, rankobjs, parallel = F, minSize = 15, maxSize = 500, collapse = FALSE) {
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
run_all_pathways <- function(pathways_list, ranks, parallel = F, minSize = 15, maxSize = 500, genesets_additional_info = NULL) {
  pathways_list %>% purrr::imap(
    ~ {
      pathway_list <- .x
      pathway_name <- .y
      collapse <- FALSE

      if (!is.null(genesets_additional_info)) {
        if (!"collection_name" %in% colnames(genesets_additional_info)) {
          break("genesets_additional_info must have a 'collection_name' field")
        }
        if (!"collapse" %in% colnames(genesets_additional_info)) {
          break("collapse column not found in genesets_additional_info")
        }
        geneset_additional_info <- genesets_additional_info[[pathway_name]]
        collapse <- geneset_additional_info$collapse
      }

      pathway_list %>% run_all_rankobjs(., rankobjs = ranks, parallel = parallel, minSize = minSize, maxSize = maxSize, collapse = collapse)
    }
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

simulate_preranked_data <- function(seed = 4321, geneset = NULL, ...) {
  set.seed(seed)

  # geneset <- msigdbr::msigdbr(
  #   species = "Homo sapiens",
  #   category = "H",
  #   subcategory = ""
  # )

  if (is.null(geneset)) {
    geneset <- geneset_tools$get_collection("H", "")
  }

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
    id = background_genes,
    value = bg_values
  )
  ifn_data <- data.frame(
    id = ifn_genes,
    value = ifn_values
  )

  data <- bind_rows(
    bg_data,
    ifn_data
  )

  return(data)
}
