suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(fs))



trycatch <- function(expr, silent = TRUE) {
  tryCatch(expr, error = function(e) {
    if (!silent) {
      message("An error occurred: ", e$message)
    }
  })
}


data_dir <- "test_data" %>% fs::path_abs()
output_dir <- "test_output" %>% fs::path_abs()

setup <- function() {
  if (!fs::dir_exists(data_dir)) fs::dir_create(data_dir)
  if (!fs::dir_exists(output_dir)) fs::dir_create(data_dir)
}

teardown <- function() {
  if (fs::dir_exists(data_dir)) fs::dir_delete(data_dir)
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
}

test_render <- function() {
  rmarkdown::render("../../run.Rmd",
    output_format = "html_document",
    output_dir = output_dir,
    output_file = "test_defaults.html",
    params = list(
      rankfiledir = "testdata",
      genesets_json = '[{"category": "H", "subcategory": ""}]'
    )
  )
}

test_invalid_dir <- function() {
  rmarkdown::render("../../run.Rmd",
    output_format = "html_document",
    output_dir = output_dir,
    output_file = "test_invalid_dir.html",
    params = list(
      rankfiledir = "does-not-exist",
      genesets_json = '[{"category": "H", "subcategory": ""}]'
    )
  )
}

test_one <- function() {
  geneset <- msigdbr::msigdbr(
    species = "Homo sapiens",
    category = "H",
    subcategory = ""
  )


  genes <- geneset %>%
    pull(entrez_gene) %>%
    unique()

  for (f in c("groupA_vs_B_dirB.tsv", "group_A_vs_C_dirB.tsv")) {
    .values <- rnorm(n = length(genes))
    .data <- data.frame(
      geneid = genes,
      signedlogp = .values
    )
    .out <- file.path(data_dir, f)
    write_tsv(.data, .out)
  }

  rmarkdown::render("../../run.Rmd",
    output_format = "html_document",
    output_dir = output_dir,
    output_file = "test_full.html",
    params = list(
      rankfiledir = data_dir,
      genesets_json = '[{"category": "H", "subcategory": ""}]'
    )
  )
}


setup()
test_render()
test_invalid_dir()
test_one()

# teardown()
