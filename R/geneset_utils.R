# geneset_utils.R
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(memoise))
suppressPackageStartupMessages(library(dplyr))


get_collection_raw <- function(
    category,
    subcategory,
    species = "Homo sapiens",
    cache = TRUE) {
  #
  collection_id <- paste(category, subcategory, make.names(species), sep = "_")

  cache_dir <- here("cache")
  collection_id_path <- file.path(cache_dir, collection_id)

  if (cache && fs::file_exists(collection_id_path)) {
    cat(paste0("reading ", collection_id, " from ", cache_dir, "\n"))
    return(readr::read_tsv(collection_id_path, show_col_types = FALSE))
  }

  # Data fetching
  df <- msigdbr::msigdbr(
    species = species,
    category = category,
    subcategory = subcategory
  )

  if (cache == TRUE) {
    if (!fs::dir_exists(cache_dir)) fs::dir_create(cache_dir)
    if (!fs::file_exists(collection_id_path)) {
      readr::write_tsv(df, collection_id_path)
      cat(paste0("writing ", collection_id, " to ", cache_dir, "\n"))
    }
  }

  return(df)
}

get_collection <- memoise::memoise(get_collection_raw) # this is convienent for interactive sessions

# get_collections <- function(list_of_collections, species = "Homo sapiens") {
#   res <- list_of_collections %>% purrr::map(
#     ~ {
#       list_name <- paste(.x$category, .x$subcategory, sep = "_")
#       collection <- get_collection(
#         category = .x$category,
#         subcategory = .x$subcategory,
#         species = species,
#       )
#       setNames(list(collection), list_name)
#     }
#   )
#   res_reduced <- purrr::reduce(res, c) # 1 level list names set appropriately
#   return(res_reduced)
# }

get_collections <- function(dataframe_obj, species = "Homo sapiens") {
  if (!"collection_name" %in% colnames(dataframe_obj)) {
    dataframe_obj <- dplyr::mutate(dataframe_obj,
      collection_name = stringr::str_c(category, subcategory, sep = "_")
    )
  }
  res <- dataframe_obj %>% purrr::pmap(
    function(category, subcategory, ...) {
      list_name <- paste(category, subcategory, sep = "_")
      if (category == "C5" & subcategory == "All") {
        .collections <- c("GO:BP", "GO:CC", "GO:MF") %>%
          purrr::map(~ {
            get_collection(
              category = category,
              subcategory = .x,
              species = species
            )
          })
        collection <- dplyr::bind_rows(.collections)
      } else {
        collection <- get_collection(
          category = category,
          subcategory = subcategory,
          species = species,
        )
      }
      setNames(list(collection), list_name)
    }
  )
  res_reduced <- purrr::reduce(res, c) # 1 level list names set appropriately
  return(res_reduced)
}



# list_of_geneset_dfs <- pathways_of_interest %>%
#   purrr::map(  # we use pmap for rowwise access to multiple values (?)
#     ~ {
#     # Here you can customize how you want to name each list element based on the inputs
#     list_name <- paste(.x$category, .x$subcategory, sep = "_")
#     list_data <- get_genesets(.x$category, .x$subcategory)
#     # Return a named list for each row
#     setNames(list(list_data), list_name)
#   }) %>%
#   purrr::reduce(c) # reduce a list to a single value by applying a binary function, in this case c.

get_pathway_info <- function(gsname) {
  pathways_dfs[[gsname]]
}

# now turn each pathway dataframe into a named list
# Transform each DataFrame and convert it into a named list of gene ids
genesets_df_to_list <- function(list_of_geneset_dfs) {
  genesets_list <- list_of_geneset_dfs %>%
    # mutate(gs_fullname = str_c(gs_exact_source, gs_name, sep=" ")) %>%
    # group_by(gs_fullname) %>%
    group_by(gs_name) %>%
    summarise(entrez_gene_ids = list(as.character(entrez_gene)), .groups = "drop") %>%
    tibble::deframe()
  genesets_list
}
