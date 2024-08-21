options(error = function() { 
            # print(dump.frames())
            traceback(1)
            cat("\n")
})

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(here))

io_tools <- new.env()
source(file.path(here("R"), "io.R"), local = io_tools)

geneset_tools <- new.env()
source(file.path(here("R"), "./geneset_utils.R"), local = geneset_tools)

fgsea_tools <- new.env()
source(file.path(here("R"), "./fgsea.R"), local = fgsea_tools)

run_env <- new.env()
source(file.path(here("R"), "./run.R"), local = run_env)

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
    paste0("group_A_vs_B_dirB ", seq_along(datas1)),
    paste0("group_C_vs_D_dirB ", seq_along(datas2)),
    paste0("group_E_vs_F_dirB ", seq_along(datas3))
  )

  ._ <- datas %>% purrr::imap(~ write_tsv(.x, file.path(data_dir, paste0(.y, ".tsv"))))

  .mat <- base::Reduce(
    accumulate = F,
    f = function(...) full_join(..., by = "id"),
    x = datas
  ) %>% as.data.frame()

  rownames(.mat) <- .mat$id
  .mat$id <- NULL
  .meta <- data.frame(
    id = colnames(.mat),
    group = c(rep("A", 3), rep("B", 3), rep("C", 3))
  )
  rownames(.meta) <- .meta$id
  .rdesc <- data.frame(id = rownames(.mat), dummy = "X")
  rownames(.rdesc) <- rownames(.mat)
  gct <- new("GCT",
    mat = .mat %>% as.matrix(),
    cdesc = .meta,
    rdesc = .rdesc
  )
  cmapR::write_gctx(gct, file.path(data_dir, "test.gct"))
}

teardown <- function() {
  if (fs::dir_exists(data_dir)) fs::dir_delete(data_dir)
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
}

# == begin tests

setup()


testthat::test_that("test main function with valid parameters", {

  params <- list(
    rankfiledir = data_dir,
    volcanodir = data_dir,
    savedir = output_dir,
    ranks_from = "volcano",
    genesets = list( list(category="H", subcategory="", collapse=FALSE) )
  )

  testthat::expect_no_error({
    print("Function is being called")
    run_env$run(params)
  })

})



# test_invalid_dir()
# test_one()

# teardown()

# test_invalid_dir <- function() {
#   rmarkdown::render("../../run.Rmd",
#     output_format = "html_document",
#     output_dir = output_dir,
#     output_file = "test_invalid_dir.html",
#     params = list(
#       rankfiledir = "does-not-exist",
#       savedir = output_dir,
#       genesets_json = '[{"category": "H", "subcategory": ""}]'
#     )
#   )
# }

# test_render <- function() {
#   rmarkdown::render("../../run.Rmd",
#     # output_format = "html_document",
#     output_dir = output_dir,
#     output_file = "test_defaults.html",
#     params = list(
#       rankfiledir = data_dir,
#       volcanodir = data_dir,
#       gct_path = NULL,
#       savedir = output_dir,
#       ranks_from = "volcano",
#       genesets_json = '[{"category": "H", "subcategory": ""}]'
#     )
#   )
# }


# test_one <- function() {
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

#   rmarkdown::render("../../run.Rmd",
#     output_format = "html_document",
#     output_dir = output_dir,
#     output_file = "test_full.html",
#     params = list(
#       rankfiledir = data_dir,
#       savedir = file.path(output_dir, "test_full"),
#       genesets_json = '[{"category": "H", "subcategory": ""}]'
#     )
#   )
# }



# testthat::test_that(
#   "test base render",
#   {
#     testthat::expect_no_error(test_render())
#     # assertthat::file_ex
#     assertthat::are_equal(
#       fs::file_exists(
#         file.path(output_dir, "test_defaults.html")
#       ),
#       TRUE
#     )
#   }
# )

# test_render()
