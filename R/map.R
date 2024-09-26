# map.R
# Install CRAN packages if not already installed
required_cran <- c("dplyr", "tidyr", "stringr", "purrr", "writexl")
installed_cran <- rownames(installed.packages())
for (pkg in required_cran) {
  if (!pkg %in% installed_cran) {
    install.packages(pkg)
  }
}

# Install Bioconductor packages if not already installed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

required_bioc <- c("org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db") # Add more as needed
installed_bioc <- rownames(installed.packages())
for (pkg in required_bioc) {
  if (!pkg %in% installed_bioc) {
    BiocManager::install(pkg)
  }
}

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)
library(org.Hs.eg.db) # For human gene annotations
# in the future we will need to support others as well


# Define the gene mapping function
map_entrez_to_symbol <- function(entrez_list, species = "human") {
  # Define species-specific annotation packages
  species_db <- list(
    human = "org.Hs.eg.db",
    mouse = "org.Mm.eg.db",
    rat   = "org.Rn.eg.db"
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
  library(get(db_package), character.only = TRUE)

  # Split, unlist, and get unique Entrez IDs
  unique_entrez <- entrez_list %>%
    str_split(pattern = "/") %>%
    unlist() %>%
    str_trim() %>%
    unique()

  # Remove any empty strings
  unique_entrez <- unique_entrez[unique_entrez != ""]

  # Map Entrez IDs to Gene Symbols
  gene_symbols <- mapIds(
    get(db_package),
    keys = unique_entrez,
    column = "SYMBOL",
    keytype = "ENTREZID",
    multiVals = "first" # Take the first symbol if multiple exist
  )

  # Create a named vector for mapping
  mapping <- setNames(gene_symbols, unique_entrez)

  # Function to replace Entrez IDs with symbols or keep original if no mapping
  replace_ids <- function(id_string) {
    ids <- str_split(id_string, pattern = "/")[[1]] %>% str_trim()
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

  # Apply the replacement to the entire list
  symbol_mapped <- map_chr(entrez_list, replace_ids)

  return(symbol_mapped)
}


example_usage <- function() {
  res_mapped <- res %>%
    mutate(
      leadingEdge_symbols = map(leadingEdge, map_entrez_to_symbol)
    )

  # res_pivoted <- res_mapped %>%
  #   pivot_wider(
  #     names_from = rankname,
  #     values_from = c(pval, padj, log2err, ES, NES, size, leadingEdge, mainpathway, n_main, ratio_main, main_pathway_ratio),
  #     names_sep = "_"
  #   )

  res_pivoted <- res_mapped %>%
    pivot_wider(
      names_from = rankname,
      values_from = c(pval, padj, log2err, ES, NES, size, leadingEdge, mainpathway, n_main, ratio_main, main_pathway_ratio),
      names_glue = "{rankname}_{.value}"
    )



  # Define parameters
  species <- "human" # Change as needed: "human", "mouse", "rat", etc.
  pivot_flag <- FALSE # Set to TRUE to enable pivoting
  output_excel <- "processed_res.xlsx" # Output Excel file name

  # Process the res table
  res_processed <- res %>%
    # Ensure 'rankname' is treated as a character
    mutate(rankname = as.character(rankname)) %>%
    # Create a new column with original leadingEdge Entrez IDs
    mutate(le_entrez = map_chr(leadingEdge, ~ paste(.x, collapse = "/"))) %>%
    # Create a new column with mapped gene symbols
    mutate(le_symbol = map_entrez_to_symbol(le_entrez, species = species)) %>%
    # Optionally, remove the original 'leadingEdge' list column
    select(-leadingEdge)

  # View the first few rows to verify
  print(head(res_processed))

  if (pivot_flag) {
    res_pivoted <- res_processed %>%
      pivot_wider(
        names_from = rankname,
        values_from = c(pval, padj, log2err, ES, NES, size, le_entrez, le_symbol, mainpathway, n_main, ratio_main, main_pathway_ratio),
        names_sep = "_"
        # Alternatively, use names_glue for more complex naming
        # names_glue = "{rankname}_{.value}"
      )

    # Replace the processed data with the pivoted version
    res_final <- res_pivoted
  } else {
    res_final <- res_processed
  }

  # Export the final table to Excel
  # If pivoting is enabled, it's already handled in res_final
  # Otherwise, res_final contains the processed table without pivoting
  
  # Write to Excel
  write_xlsx(res_final, path = output_excel)
  
  cat("Processed data has been written to", output_excel, "\n")

}
