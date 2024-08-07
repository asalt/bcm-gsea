suppressPackageStartupMessages(library(fgsea))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(furrr))

src_dir <- file.path(here("R"))

io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)

geneset_tools <- new.env()
source(file.path(src_dir, "./geneset_utils.R"), local = geneset_tools)


filter_on_mainpathway <- function(
    pathway_object,
    main_pathway_ratio = .1) {
  if (!"var" %in% colnames(pathway_object)) {
    stop("var column not found in the input data")
  }

  if (!"mainpathway" %in% colnames(pathway_object)) {
    pathway_object$mainpathway <- TRUE # will simply keep all
  }


  pathway_object <- pathway_object %>%
    group_by(var) %>%
    mutate(n_main = sum(mainpathway == T)) %>%
    mutate(ratio_main = n_main / n()) %>%
    ungroup()

  filtered_pathway_object <- pathway_object %>%
    dplyr::mutate(main_pathway_ratio = rowSums(!is.na(.))) %>%
    dplyr::filter(main_pathway_ratio >= main_pathway_ratio)

  return(filtered_pathway_object)
}

run_one <- function(
    rankobj,
    geneset,
    minSize = 15,
    maxSize = 500,
    collapse = FALSE) {
  # to look for duplicate gene names
  # rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  .maxcounts <- rankobj %>%
    names() %>%
    table() %>%
    as.data.frame() %>%
    pull(Freq) %>%
    max()
  assertthat::are_equal(.maxcounts, 1)


  # set.seed(789)

  fgseaRes <- fgsea(
    pathways = geneset,
    stats = rankobj,
    minSize = minSize,
    maxSize = maxSize,
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

run_all_rankobjs <- function(
    pathway,
    rankobjs,
    parallel = F,
    minSize = 15,
    maxSize = 500,
    collapse = FALSE) {
  # rankobjs %>% furrr::future_map( # maybe later
  if (parallel) {
    future::plan(future::multisession, workers = future::availableCores() - 1)
    rankobjs %>% furrr::future_map(
      ~ run_one(.,
        geneset = pathway,
        minSize = minSize,
        maxSize = maxSize,
        collapse = collapse
      )
    )
  } else {
    rankobjs %>% purrr::map(
      ~ run_one(.,
        geneset = pathway,
        minSize = minSize,
        maxSize = maxSize,
        collapse = collapse
      )
    )
  }
  # rankobjs %>% purrr::map(
  #   ~ run_one(., geneset = pathway)
  # )
}
# results_list <- run_all_pathways(pathways_list_of_lists, ranks_list)

run_all_pathways <- function(
    geneset_lists,
    ranks,
    parallel = FALSE,
    minSize = 15,
    maxSize = 500,
    genesets_additional_info = NULL,
    collapse = FALSE) {
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
      geneset_list %>% run_all_rankobjs(.,
        rankobjs = ranks,
        parallel = parallel,
        minSize = minSize,
        maxSize = maxSize,
        collapse = current_collapse
      )
    }
  )
  return(out)
}



get_rankorder <- function(
    geneset,
    rankobj,
    geneset_df = NULL) {
  # geneset_df has addl info
  # if (!class(geneset) == "character") {
  #   stop("geneset must be a character")
  # }
  # if (!class(rankobj) == "numeric") {
  #   stop("rankobj must be a named numeric vector")
  # }

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

  if (!is.null(geneset_df)) {
    rankorder_edge %<>% left_join(
      geneset_df %>%
        mutate(entrez_gene = as.character(entrez_gene)) %>%
        distinct(entrez_gene, .keep_all = TRUE),
      by = c("id" = "entrez_gene")
    )
  }

  return(
    list(
      edge = rankorder_edge,
      curve = enplot_data$curve,
      ticks = enplot_data$ticks,
      stats = enplot_data$stats,
      posES = enplot_data$posES,
      negES = enplot_data$negES,
      spreadES = enplot_data$spreadES,
      maxAbsStat = enplot_data$maxAbsStat
    )
  )
}

get_rankorder_across <- function(
    df, # long form data frame with var col
    ranks_list,
    geneset_lists,
    collection_name = "",
    topn = 25,
    limit = 120,
    title = "",
    pstat_cutoff=1,
    pstat_usetype='padj',
    main_pathway_ratio = 0.1,
    ...){

  if (!"var" %in% colnames(df)){
    stop('var not in fgesa_longdf')
    }

  df <- filter_on_mainpathway(df, main_pathway_ratio = main_pathway_ratio)
  df <- df %>% filter(!!as.symbol(pstat_usetype) < pstat_cutoff) %>%
    arrange( pval ) %>%
    distinct(pathway, .keep_all = TRUE) %>%
    head(n = limit)

  pathways_to_plot <- df$pathway

  rank_ids <- names(ranks_list)
 
  rankorders <- pathways_to_plot %>% purrr::map(~{
    if (!.x %in% names(geneset_lists)){
      cat(paste0('does not have access to geneset : ', .x))
      return()
    }
    geneset <- geneset_lists[[.x]]
    
    rank_ids %>% purrr::map(~{
      fgsea_tools$get_rankorder(
          geneset,
          ranks_list[[.x]],
          )
      }) %>% set_names(rank_ids)
    }) %>% set_names(pathways_to_plot)
  
  return(rankorders)
}
    




simulate_preranked_data <- function(
    seed = 4321,
    geneset = NULL,
    spike_terms = c("INTERFERON"),
    ...) {
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
  spike_data <- purrr::map2_df(spike_genes_list, seq_along(spike_genes_list), ~ data.frame(
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


concat_results_one_collection <- function(list_of_dfs) {
  res <- list_of_dfs %>%
    purrr::imap(
      .f = ~ {
        .x %>% dplyr::mutate(var = .y)
      }
    ) %>%
    dplyr::bind_rows()

  return(res)
}

concat_results_all_collections <- function(list_of_lists, ...) {
  .dotargs <- list(...) ## this is not used nor passed to inner func

  res <- list_of_lists %>%
    purrr::map(
      ~ {
        concat_results_one_collection(.x)
      }
    ) # %>%
  # purrr::reduce(rbind)
  return(res)
}
