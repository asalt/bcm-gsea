suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(fs))

io_tools <- new.env()
source("../io.R", local = io_tools)

geneset_tools <- new.env()
source("../geneset_utils.R", local = geneset_tools)

fgsea_tools <- new.env()
source("../fgsea.R", local = fgsea_tools)

# ==================================
data_dir <- "test_data" %>% fs::path_abs()
output_dir <- "test_output" %>% fs::path_abs()

# ==================================


trycatch <- function(expr, silent = TRUE) {
  tryCatch(expr, error = function(e) {
    if (!silent) {
      message("An error occurred: ", e$message)
    }
  })
}



setup <- function() {
  if (!fs::dir_exists(data_dir)) fs::dir_create(data_dir)
  if (!fs::dir_exists(output_dir)) fs::dir_create(data_dir)

  datas1 <- purrr::map(1:3, ~ fgsea_tools$simulate_preranked_data(seed = 4321))
  datas1 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas2 <- purrr::map(1:3, ~ fgsea_tools$simulate_preranked_data(seed = 1234))
  datas2 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas3 <- purrr::map(1:3, ~ fgsea_tools$simulate_preranked_data(seed = 9999))
  datas3 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas <-
    c(
      datas1,
      datas2,
      datas3
    )

  names(datas) <- c(
    paste0("Group A vs B dirB ", seq_along(datas1)),
    paste0("Group C vs D dirB ", seq_along(datas2)),
    paste0("Group E vs F dirB ", seq_along(datas3))
  )

  ._ <- datas %>% purrr::imap(~ write_tsv(.x, file.path(data_dir, paste0(.y, ".tsv"))))
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
      savedir = output_dir,
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
      savedir = output_dir,
      genesets_json = '[{"category": "H", "subcategory": ""}]'
    )
  )
}

test_one <- function() {
  # geneset <- msigdbr::msigdbr(
  #   species = "Homo sapiens",
  #   category = "H",
  #   subcategory = ""
  # )


  # genes <- geneset %>%
  #   pull(entrez_gene) %>%
  #   unique()

  # for (f in c("groupA_vs_B_dirB.tsv", "group_A_vs_C_dirB.tsv")) {
  #   .values <- rnorm(n = length(genes))
  #   .data <- data.frame(
  #     geneid = genes,
  #     signedlogp = .values
  #   )
  #   .out <- file.path(data_dir, f)
  #   write_tsv(.data, .out)
  # }

  rmarkdown::render("../../run.Rmd",
    output_format = "html_document",
    output_dir = output_dir,
    output_file = "test_full.html",
    params = list(
      rankfiledir = data_dir,
      savedir = file.path(output_dir, "test_full"),
      genesets_json = '[{"category": "H", "subcategory": ""}]'
    )
  )
}


setup()

testthat::test_that(
  "test base render",
  {
    testthat::expect_no_error(test_render())
    # assertthat::file_ex
    assertthat::are_equal(
      fs::file_exists(
        file.path(output_dir, "test_defaults.html")
      ),
      TRUE
    )
  }
)

# test_render()
test_invalid_dir()
test_one()

# teardown()
