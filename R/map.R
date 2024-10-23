# map.R
# Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
# library(writexl)
# library(org.Hs.eg.db) # For human gene annotations

# required_bioc <- c("org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db") # Add more as needed
# installed_bioc <- rownames(installed.packages())
# for (pkg in required_bioc) {
#   if (!pkg %in% installed_bioc) {
#     BiocManager::install(pkg)
#   }
# }


# in the future we will need to support others as well
# Define the gene mapping function
map_entrez_to_symbol <- function(entrez_list, species = "Homo sapiens") {

  # Define species-specific annotation packages
  species_db <- list(
    "Homo sapiens" = "org.Hs.eg.db",
    "Mus musculus" = "org.Mm.eg.db",
    "Rattus novaris"   = "org.Rn.eg.db" # check name for this one
    # Add more species and their corresponding annotation packages here
  )

  # Validate species input
  if (!(species %in% names(species_db))) {
    stop("Unsupported species. Please choose from: ", paste(names(species_db), collapse = ", "))
  }

  # Load the appropriate annotation package
  db_package <- species_db[[species]]
  if (!requireNamespace(db_package, quietly = TRUE)) {
    stop("Annotation package ", db_package, " not found. Please install it.")
  }
  suppressPackageStartupMessages(library(db_package, character.only = TRUE))

  # Map Entrez IDs to Gene Symbols
  gene_symbols <- mapIds(
    get(db_package),
    keys = entrez_list,
    column = "SYMBOL",
    keytype = "ENTREZID",
    multiVals = "first" # Take the first symbol if multiple exist
  )

  # Create a named vector for mapping
  mapping <- setNames(gene_symbols, entrez_list)
  # unloadNamespace("org.Hs.eg.db")
  # unloadNamespace("AnnotationDbi")
  return(mapping)
}

extract_entrezids <- function(fgsea_res){

  # print(head(fgsea_res))
  # print(head(fgsea_res$leadingEdge))
  # print(class(fgsea_res$leadingEdge))

  if (!"data.frame" %in% class(fgsea_res)){
    stop("leadingEdge not in df")
  }

  if (!"leadingEdge" %in% colnames(fgsea_res)){
    stop("leadingEdge not in df")
  }

  if (class(fgsea_res$leadingEdge) == "character"){
    unique_entrez <- fgsea_res %>%
      ungroup()
      pull(leadingEdge) %>%
      str_split(pattern = "/") %>%
      unlist() %>%
      str_trim() %>%
      unique()
    # Remove any empty strings
    unique_entrez <- unique_entrez[unique_entrez != ""]
  } else if (class(fgsea_res$leadingEdge) == "list"){
    unique_entrez <- fgsea_res %>%
      ungroup() %>%
      pull(leadingEdge) %>%
      #as.character() %>%
      unlist() %>%
      unique()
    # do somethng else
  } else{  #?
    1+1
    stop("??")
  }
  # print(head(unique_entrez))
  return(unique_entrez)
}

format_entrezids <- function(fgsea_res, mapping){

  if (is.null(mapping)) stop("must provide mapping")
  if (class(fgsea_res$leadingEdge) == "character")  {
    fgsea_res <- fgsea_res %>% mutate( leadingEdge = str_split(leadingEdge, '/'))
  }

  # Function to replace Entrez IDs with symbols or keep original if no mapping
  replace_ids <- function(ids) {
    ids <- Filter(Negate(is.na), ids) # Remove NAs
    symbols <- sapply(ids, function(id) {
      if (id %in% names(mapping)) {
        symbol <- mapping[[id]]
        if (!is.na(symbol)) {
          return(symbol)
        } else {
          return(id) # Keep original ID if no symbol
        }
      } else {
        return(id) # Keep original ID if not found
      }
    })
    paste(symbols, collapse = "/")
  }

  fgsea_res <- fgsea_res %>% mutate(
    leadingEdge_entrezid = map_chr(leadingEdge, ~ paste(.x, collapse = "/")),
    leadingEdge_genesymbol = map_chr(leadingEdge, ~replace_ids(.x))
  )

  return(fgsea_res)
}

add_leadingedges_to_results_list <- function(fgsea_res_list, species = "Homo sapiens"){

  print(paste0("mapping geneids to symbols for ", species))

  if (!"list" %in% class(fgsea_res_list)){
    stop("fgsea_res_list should be a list")
  }

  entrez_ids <- fgsea_res_list %>%
    purrr::map(~extract_entrezids(.x)) %>%
    purrr::flatten_chr()
  entrez_ids <- unique(entrez_ids)

  gene_symbol_mapping <- map_entrez_to_symbol(entrez_ids, species = species)
  # now assign back
  output <- fgsea_res_list %>% purrr::map(~{
    format_entrezids(.x, mapping = gene_symbol_mapping)
  })

  return(output)

}
