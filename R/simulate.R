suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dplyr))

src_dir <- file.path(here("R"))

# io_tools <- new.env()
# source(file.path(file.path(src_dir, "./io.R")), local = io_tools)

geneset_tools <- new.env()
source(file.path(src_dir, "./geneset_utils.R"), local = geneset_tools)

# io_tools <- new.env()
# source(file.path(src_dir, "./io.R"), local = io_tools)

# ================================

make_random_gct <- function(nrow = 10, ncol = 4) {
  set.seed(369)
  nrow <- max(nrow, 1)
  ncol <- max(ncol, 1)
  .mat <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  .rids <- seq(1, dim(.mat)[1]) %>% as.character()
  .cids <- seq(1, dim(.mat)[2]) %>% as.character()
  .cids <- paste0("X", .cids)
  .cdesc <- data.frame(
    metavar1 = sample(letters[1:5], ncol, replace = T),
    metavar2 = sample(letters[1:5], ncol, replace = T)
  )
  .rdesc <- data.frame(
    rdesc = paste0("gene", seq(1, nrow))
  )
  gct <- cmapR::GCT(mat = .mat, rid = .rids, cid = .cids, cdesc = .cdesc, rdesc = .rdesc)
  gct
}


.cache <- list()
simulate_preranked_data <- function(...){
  ll <- list(...)
  hashval <- rlang::hash(ll)
  if (!hashval %in% names(.cache)) .cache[[hashval]] <- do.call(.simulate_preranked_data, ll)
  return(.cache[[hashval]]) 

}
  
  

.simulate_preranked_data <- function(
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



generate_test_data <- function(collapse = FALSE,
                               pathways = c("H", "GO:BP")
                               ) {
  # this function relies on simulate preranked data from fgsea tools,
  # which depends on test_fgsea.R for guaranteed* success
  # *not guaranteed

  # genesets_of_interest <- list(
  #     list(category = "H", subcategory = ""),
  #     list(category = "C5", subcategory = "GO:BP")
  # )


  # put the imports here to save time on startup
  io_tools <- new.env()
  source(file.path(here("R"), "./io.R"), local = io_tools)

  fgsea_tools <- new.env()
  source(file.path(here("R"), "./fgsea.R"), local = fgsea_tools)

  genesets_list <- list()
  if ("H" %in% pathways) genesets_list[["H"]] <- c("H", "", FALSE, "H_")
  if ("GO:BP" %in% pathways) genesets_list[["GO:BP"]] <- c("C5", "GO:BP", TRUE, "C5_GO:BP")
  if ("GO:CC" %in% pathways) genesets_list[["GO:CC"]] <- c("C5", "GO:CC", TRUE, "C5_GO:CC")
  if ("GO:MF" %in% pathways) genesets_list[["GO:MF"]] <- c("C5", "GO:MF", TRUE, "C5_GO:MF")

  # Convert the list to a tibble
  genesets_of_interest <- purrr::map_dfr(genesets_list, ~ tibble::tibble(
    category = .x[1], subcategory = .x[2], collapse = .x[3], collection_name = .x[4]
  ))

  genesets <- geneset_tools$get_collections(
    genesets_of_interest
  )

  #TODO 
  geneset_list <- genesets %>% purrr::imap(~ geneset_tools$genesets_df_to_list(.x))

  # named_data = list(data=data)
  data1 <- simulate_preranked_data(seed = 1234, sample_frac = .4)
  data2 <- simulate_preranked_data(seed = 4321, sample_frac = .4)
  data <- list(first = data1, second = data2)
  rankobjs <- io_tools$ranks_dfs_to_lists(data)




  res <- fgsea_tools$run_all_pathways(geneset_list, rankobjs, collapse = collapse)
  return(res)
}


