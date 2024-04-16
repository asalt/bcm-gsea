# geneset_utils.R

get_collection <- function(category, subcategory, species = "Homo sapiens", ...) {
  msigdbr::msigdbr(species = SPECIES, category = category, subcategory = subcategory)
}


get_collections <- function(list_of_collections, species = "Homo sapiens"){
    list_of_collections %>% purrr::map(
        ~{
            get_collection(
                category = .x$category,
                subcategory = .x$subcategory,
                species = species,
            )

        }

    )

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

get_pathway_info <- function(gsname){
  pathways_dfs[[gsname]]
}

# now turn each pathway dataframe into a named list
# Transform each DataFrame and convert it into a named list of gene ids
pathways_df_to_list <- function(list_of_geneset_dfs){
  pathways_list <- list_of_geneset_dfs %>%
    #mutate(gs_fullname = str_c(gs_exact_source, gs_name, sep=" ")) %>%
    #group_by(gs_fullname) %>%
    group_by(gs_name) %>%
    summarise(entrez_gene_ids = list(as.character(entrez_gene)), .groups = 'drop') %>%
    tibble::deframe()
  pathways_list
}