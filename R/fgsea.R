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

  fgseaRes$mainpathway <- TRUE
  if (!is.null(collapse) && (collapse == TRUE || collapse == "TRUE")) {
    cat("finding main pathways")
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
      ~ run_one(., geneset = pathway, minSize = minSize, maxSize = maxSize, collapse = collapse)
    )
  } else {
    rankobjs %>% purrr::map(
      ~ run_one(., geneset = pathway, minSize = minSize, maxSize = maxSize, collapse = collapse)
    )
  }
  # rankobjs %>% purrr::map(
  #   ~ run_one(., geneset = pathway)
  # )
}
xx_run_all_pathways <- function(pathways_list, ranks, parallel = F, minSize = 15, maxSize = 500, genesets_additional_info = NULL, collapse = FALSE) {
  pathways_list %>% purrr::imap(
    ~ {
      pathway_list <- .x
      pathway_name <- .y

      collapse <- collapse.

      if (!is.null(genesets_additional_info & collapse. == FALSE)) {
        if (!"collection_name" %in% colnames(genesets_additional_info)) {
          cat("genesets_additional_info must have a 'collection_name' field")
        }
        if (!"collapse" %in% colnames(genesets_additional_info)) {
          cat("collapse column not found in genesets_additional_info")
        }
        geneset_additional_info <- genesets_additional_info[genesets_additional_info$collection_name == pathway_name, ]
        collapse <- geneset_additional_info$collapse
      }


      pathway_list %>% run_all_rankobjs(., rankobjs = ranks, parallel = parallel, minSize = minSize, maxSize = maxSize, collapse = collapse)
    }
  )
}
# results_list <- run_all_pathways(pathways_list_of_lists, ranks_list)

run_all_pathways <- function(geneset_lists, ranks, parallel = FALSE, minSize = 15, maxSize = 500, genesets_additional_info = NULL, collapse = FALSE) {
  if (any(is.null(names(geneset_lists)))) {
    stop(
      cat(
        "each geneset must be named",
        "e.g. geneset_lists <- list('H_' = geneset1, 'C5_GO:BP' = geneset2)"
      ),
      call. = FALSE
    )
  }

  if (any(is.null(names(ranks)))) {
    stop(
      cat(
        "each rank must be named",
        "e.g. rank <- list(comparison1 = rank1, comparison2 = rank2)"
      ),
      call. = FALSE
    )
  }



  out <- geneset_lists %>% purrr::imap(
    ~ {
      geneset_list <- .x
      geneset_name <- .y

      current_collapse <- collapse # Use local variable for clarity

      # Check for non-null and appropriate columns of genesets_additional_info
      # this is messy but works
      if (!is.null(genesets_additional_info) && !collapse) {
        # Check for necessary columns in genesets_additional_info


        if (!"collection_name" %in% colnames(genesets_additional_info)) {
          genesets_additional_info <- genesets_additional_info %>%
            dplyr::mutate(collection_name = stringr::str_c(category, subcategory, sep = "_"))
        }

        if (!"collapse" %in% colnames(genesets_additional_info)) {
          genesets_additional_info <- genesets_additional_info %>%
            dplyr::mutate(collapse = F)
        }


        # Extract additional info specific to the current pathway
        geneset_additional_info <- genesets_additional_info[genesets_additional_info$collection_name == geneset_name, ]

        # Check if specific geneset info was found and update collapse if so
        if (nrow(geneset_additional_info) > 0) {
          current_collapse <- geneset_additional_info$collapse
        } else {
          warning("No matching geneset info found for pathway: ", geneset_name)
        }
      }
      # browser()

      # Pass the potentially updated collapse value to the next function
      geneset_list %>% run_all_rankobjs(., rankobjs = ranks, parallel = parallel, minSize = minSize, maxSize = maxSize, collapse = current_collapse)
    }
  )
  return(out)
}



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

simulate_preranked_data <- function(seed = 4321, geneset = NULL, spike_terms = c("INTERFERON"), ...) {
  set.seed(seed)

  # geneset <- msigdbr::msigdbr(
  #   species = "Homo sapiens",
  #   category = "H",
  #   subcategory = ""
  # )

  if (is.null(geneset)) {
    geneset <- geneset_tools$get_collection("H", "")
  }


  # Generate a list of gene sets for each spike term
  spike_genes_list <- purrr::map(spike_terms, ~ geneset %>%
    dplyr::filter(str_detect(gs_name, .x)) %>%
    dplyr::pull(entrez_gene) %>%
    unique())

  genes <- geneset %>%
    dplyr::pull(entrez_gene) %>%
    unique()

  spike_genes <- unique(unlist(spike_genes_list))
  background_genes <- setdiff(genes, spike_genes)


  bg_values <- rnorm(n = length(background_genes))
  bg_data <- data.frame(
    id = background_genes,
    value = bg_values
  )

  # Spike gene values, assigning different means for each spike term
  spike_data <- map2_df(spike_genes_list, seq_along(spike_genes_list), ~ data.frame(
    id = .x,
    value = rnorm(n = length(.x), mean = .y) # Incrementing mean for differentiation
  ))


  data <- bind_rows(
    bg_data,
    spike_data
  )
  data %<>% distinct(id, .keep_all = TRUE)
  data %<>% dplyr::mutate(id = as.character(id))

  return(data)
}
