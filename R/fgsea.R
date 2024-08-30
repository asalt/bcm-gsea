suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(RColorBrewer))
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

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)
log_msg <- util_tools$make_partial(util_tools$log_msg)


filter_on_mainpathway <- function(
    pathway_object,
    main_pathway_ratio = .1) {
  if (!"rankname" %in% colnames(pathway_object)) {
    stop("rankname column not found in the input data")
  }

  if (!"mainpathway" %in% colnames(pathway_object)) {
    pathway_object$mainpathway <- TRUE # will simply keep all
  }


  pathway_object <- pathway_object %>%
    group_by(rankname) %>%
    mutate(n_main = sum(mainpathway == T)) %>%
    mutate(ratio_main = n_main / n()) %>%
    ungroup()

  filtered_pathway_object <- pathway_object %>%
    dplyr::mutate(main_pathway_ratio = rowSums(!is.na(.))) %>%
    dplyr::filter(main_pathway_ratio >= main_pathway_ratio)

  return(filtered_pathway_object)
}

#' Select top pathways based on statistical cutoff
#'
#' This function selects the top pathways from a data frame based on a statistical cutoff.
#' The statistical cutoff is determined by the p-value or adjusted p-value, depending on the value of the `pstat_usetype` parameter.
#' The function arranges the data frame in descending order of the absolute value of the NES column,
#' filters the rows where the p-value or adjusted p-value is less than the specified cutoff,
#' selects the specified number of top pathways, and returns a subset of the data frame containing only the top pathways.
#'
#' @param df A data frame containing the pathways and statistical values.
#' @param pstat_cutoff The cutoff value for the p-value or adjusted p-value.
#' @param limit The maximum number of top pathways to select.
#' @param pstat_usetype The type of statistical value to use for the cutoff (either "pval" or "padj").
#'
#' @return A subset of the input data frame containing only the top pathways.
#'
#' @examples
#' df <- data.frame(
#'   pathway = c("Pathway A", "Pathway B", "Pathway C"),
#'   NES = c(1.5, -2.3, 0.8),
#'   pval = c(0.01, 0.05, 0.001),
#'   padj = c(0.05, 0.1, 0.01)
#' )
#' select_topn(df, pstat_cutoff = 0.05, limit = 2, pstat_usetype = "pval")
#' # Returns a data frame with Pathway A and Pathway C
#' select_topn(df, pstat_cutoff = 0.05, limit = 2, pstat_usetype = "padj")
#' # Returns a data frame with Pathway A and Pathway B
#'
#' @seealso \code{\link{arrange}}, \code{\link{filter}}, \code{\link{slice_head}}, \code{\link{pull}}
#'
#' @import dplyr
#'
#' @export
select_topn <- function(df,
                        pstat_cutoff = 1,
                        limit = 120,
                        pstat_usetype = c("pval", "padj"),
                        to_include = NULL, # extra pathways to expilcitly include
                        ...) {
  pstat_usetype <- match.arg(pstat_usetype)

  if (!"data.frame" %in% class(df)) {
    stop(
      cat("df should be a data frame")
    )
  }

  if (!"NES" %in% colnames(df)) {
    stop(
      cat("NES should be a column in df")
    )
  }

  if (!pstat_usetype %in% colnames(df)) {
    stop(
      cat(paste0(pstat_usetype, " should be a column in df"))
    )
  }

  top_pathways <- df %>%
    arrange(-abs(NES)) %>%
    distinct(pathway, .keep_all = TRUE) %>%
    filter(!!as.symbol(pstat_usetype) < pstat_cutoff) %>%
    slice_head(n = limit) %>%
    pull(pathway)
  if (!is.null(to_include)) {
    top_pathways <- union(top_pathways, to_include)
  }
  subdf <- df %>% filter(pathway %in% top_pathways)
  return(subdf)
}

fgsea_cache_manager <- function(
    rankobj,
    geneset,
    minSize = 15,
    maxSize = 500,
    collapse = FALSE,
    cache = TRUE,
    cache_dir = NULL,
    logger = NULL,
    final_result = NULL,
    do_load = is.null(final_result),
    save = !is.null(final_result),
    ...) {
  if (is.null(logger)) logger <- log_msg
  get_hash_val <- function() {
    rlang::hash(
      c(
        as.character(rankobj),
        as.character(geneset),
        minSize,
        maxSize,
        collapse
      )
    )
  }
  hashval <- get_hash_val()
  if (do_load) {
    cache_load <- io_tools$load_from_cache(hashval, cache_dir = cache_dir)
    if (!is.null(cache_load)) {
      logger(msg = paste0("cache hit: ", hashval))
      return(cache_load)
    } else {
      return(NULL)
    }
  } else if (save) {
    io_tools$write_to_cache(object = final_result, filename = hashval, cache_dir = cache_dir)
  }
}

run_one <- function(
    rankobj = NULL,
    geneset = NULL,
    minSize = 15,
    maxSize = 500,
    collapse = FALSE,
    logger = NULL,
    ...) {
  if (is.null(logger)) logger <- log_msg
  logger(msg = paste0("starting run_one"))

  # to look for duplicate gene names
  # rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  .maxcounts <- rankobj %>%
    names() %>%
    table() %>%
    as.data.frame() %>%
    pull(Freq) %>%
    max()
  assertthat::are_equal(.maxcounts, 1)
  # this doesn't actually error out??
  # set.seed(789)

  fgseaRes <- tryCatch(
    {
      # Attempt to run fgsea
      fgseaRes <- fgsea(
        pathways = geneset,
        stats = rankobj,
        minSize = minSize,
        maxSize = maxSize
      )
      # return(fgseaRes)  # Return the result if successful
    },
    error = function(e) {
      cat("Error in FGSEA: ", e$message, "\n")
      return(NULL)
    }
  )

  if (length(collapse) != 1) { # ??
    # stop("Expected a single logical value for 'collapse'")
    collapse <- collapse[[1]]
  }

  fgseaRes$mainpathway <- TRUE
  if (!is.null(collapse) && collapse) {
    cat("finding main pathways")
    logger(msg = "finding main pathways")
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
    collapse = FALSE,
    cache = TRUE,
    cache_dir = NULL,
    logger = NULL,
    ...) {
  if (is.null(logger)) logger <- log_msg
  logger(msg = paste0("starting run_all_rankobjs"))
  logger(msg = paste0("parallel is ", parallel))
  # rankobjs %>% furrr::future_map( # maybe later
  if (parallel == TRUE) {
    workers <- future::availableCores() - 1
    logger(msg = paste0("using ", workers, " workers"))
    .map_func <- furrr::future_map
  } else {
    .map_func <- purrr::map
  }

  fgsea_args <- list(
    geneset = pathway,
    minSize = minSize,
    maxSize = maxSize,
    collapse = collapse,
    cache = cache,
    cache_dir = cache_dir,
    logger = logger
  )

  if (!is.null(cache) && cache == TRUE) {
    logger(msg = "caching is enabled")
    cache_results <- rankobjs %>%
      purrr::map(~ {
        fgsea_cache_manager(.x, fgsea_args)
      }) %>%
      Filter(Negate(is.null), .)
    .to_do <- setdiff(names(rankobjs), names(cache_results))
    rankobjs <- rankobjs[.to_do]
  } else {
    cache_results <- NULL
  }

  results <- rankobjs %>% .map_func(~ do.call(run_one, c(list(rankobj = .), fgsea_args)))
  # results <- rankobjs %>% .map_func(
  #   ~ run_one(.,
  #     fgsea_args
  #     # geneset = pathway,
  #     # minSize = minSize,
  #     # maxSize = maxSize,
  #     # collapse = collapse,
  #     # cache = cache,
  #     # cache_dir = cache_dir,
  #     # logger = logger
  #   )
  # )

  if (!is.null(cache) && cache == TRUE) {
    purrr::walk2(rankobjs, results, ~ {
      fgsea_cache_manager(.x, fgsea_args, final_result = .y, save = TRUE)
    })
  }

  final_results <- c(
    Filter(function(x) !is.null(x), cache_results),
    results
  )

  return(final_results)
}

run_all_pathways <- function(
    geneset_lists,
    ranks,
    parallel = FALSE,
    minSize = 15,
    maxSize = 500,
    genesets_additional_info = NULL,
    collapse = FALSE,
    cache = TRUE,
    cache_dir = NULL,
    logger = NULL,
    ...) {
  if (any(is.null(names(geneset_lists)))) {
    stop(
      cat(
        "each geneset must be named",
        "e.g. geneset_lists <- list('H_' = geneset1, 'C5_GO:BP' = geneset2)"
      ),
      call. = FALSE
    )
  }

  if (is.null(logger)) logger <- log_msg

  if (is.null(cache_dir)) cache_dir <- here("cache")

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
          logger(msg = paste0("No matching geneset info found for pathway: ", geneset_name))
          warning("No matching geneset info found for pathway: ", geneset_name)
        }
      }

      logger(msg = paste0("starting ", geneset_name))
      # Pass the potentially updated collapse value to the next function
      # could add cache check here instead
      results <- geneset_list %>% run_all_rankobjs(.,
        rankobjs = ranks,
        parallel = parallel,
        minSize = minSize,
        maxSize = maxSize,
        collapse = current_collapse,
        cache = cache,
        cache_dir = cache_dir,
        logger = logger
      )
      return(results)
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

  rankorder_edge <- rankorder_df %>% left_join(enplot_data$curve, by = "rank")
  rankorder_edge %<>% left_join(
    rename(enplot_data$ticks, stat_tick = stat),
    by = "rank"
  )
  # rankorder_edge %<>% left_join(rename(enplot_data$stats, stat_stat = stat, by = "rank"))
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
    df, # long form data frame with rankname col
    ranks_list,
    geneset_lists,
    collection_name = "",
    topn = 25,
    limit = 120,
    title = "",
    pstat_cutoff = 1,
    pstat_usetype = "padj",
    main_pathway_ratio = 0.1,
    ...) {
  if (!"rankname" %in% colnames(df)) {
    stop("rankname not in fgesa_longdf")
  }

  df <- filter_on_mainpathway(df, main_pathway_ratio >= main_pathway_ratio)
  df <- df %>%
    filter(!!as.symbol(pstat_usetype) < pstat_cutoff) %>%
    arrange(pval) %>%
    distinct(pathway, .keep_all = TRUE) %>%
    head(n = limit)

  pathways_to_plot <- df$pathway

  rank_ids <- names(ranks_list)

  rankorders <- pathways_to_plot %>%
    purrr::map(~ {
      if (!.x %in% names(geneset_lists)) {
        cat(paste0("does not have access to geneset : ", .x))
        return()
      }
      geneset <- geneset_lists[[.x]]

      rank_ids %>%
        purrr::map(~ {
          fgsea_tools$get_rankorder(
            geneset,
            ranks_list[[.x]],
          )
        }) %>%
        set_names(rank_ids)
    }) %>%
    set_names(pathways_to_plot)

  return(rankorders)
}


combine_rankorders_on_sample <- function(
    rankorders,
    metadata = NULL,
    ...) {
  if (!is.null(metadata)) {
    if (!"rankname" %in% colnames(metadata)) {
      warn("metadata must have a 'rankname' column")
      metadata <- NULL
    }
    if (!"facet" %in% colnames(metadata)) {
      metadata$facet <- metadata$rankname
    }
  }

  # if (is.null(metadata)) {
  #   warn("metadata is null, making standin")
  #   metadata <- data.frame(id = unique(rankorders[[1]]$rankname))
  #   rownames(metadata) <- metadata$id
  #   metadata$dummy <- "a"
  # }

  res_list <- rankorders %>%
    purrr::imap(~ {
      pw_name <- .y
      list_of_rankorders <- .x # this is a list of the "rankorder" info
      # names incldue:
      # edge (df), curve (df), ticks, stats, posES, negES, spreadES, maxAbsStat

      curves <- list_of_rankorders %>%
        purrr::imap(~ {
          .x$curve %>% mutate(pathway = pw_name, rankname = .y)
        }) %>%
        bind_rows()
      if (!is.null(metadata)) curves %<>% left_join(metadata, by = "rankname") # by = "rankname"

      edges <- list_of_rankorders %>%
        purrr::imap(~ {
          .x$edge %>% mutate(pathway = pw_name, rankname = .y)
        }) %>%
        bind_rows()
      if (!is.null(metadata)) edges %<>% left_join(metadata, by = "rankname")

      ticks <- list_of_rankorders %>%
        purrr::imap(~ {
          .x$ticks %>% mutate(pathway = pw_name, rankname = .y)
        }) %>%
        bind_rows()
      if (!is.null(metadata)) ticks %<>% left_join(metadata, by = "rankname")

      stats <- list_of_rankorders %>%
        purrr::imap(~ {
          .x$stats %>% mutate(pathway = pw_name, rankname = .y)
        }) %>%
        bind_rows()
      if (!is.null(metadata)) stats %<>% left_join(metadata, by = "rankname")

      res <- list(
        curve = curves,
        edge = edges,
        ticks = ticks,
        stats = stats
      )

      return(res)
    })
  return(res_list)
}



simulate_preranked_data <- function(
    seed = 4321,
    geneset = NULL,
    spike_terms = c("INTERFERON"),
    sample_frac = 1.0,
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
  data %<>% dplyr::sample_frac(size = sample_frac)

  return(data)
}


concat_results_one_collection <- function(list_of_dfs) {
  res <- list_of_dfs %>%
    purrr::imap(
      .f = ~ {
        .x %>% dplyr::mutate(rankname = .y)
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
  return(res)
}
