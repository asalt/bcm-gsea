
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(msigdbr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(testthat))

# io_tools <- new.env()
# source(file.path(here("R"), "io.R"), local = io_tools)

# geneset_tools <- new.env()
# source(file.path(here("R"), "./geneset_utils.R"), local = geneset_tools)

# fgsea_tools <- new.env()
# source(file.path(here("R"), "./fgsea.R"), local = fgsea_tools)

source(file.path(here("R", "lazyloader.R")))
sim_tools <- get_tool_env("simulate")
run_tools <- get_tool_env("run")

data_dir <- "test_data" %>% fs::path_abs()
output_dir <- "test_output" %>% fs::path_abs()
test_gct_path <- file.path(data_dir, "test.gct") %>% fs::path_abs()




setup <- function() {
  if (!fs::dir_exists(data_dir)) fs::dir_create(data_dir)
  if (!fs::dir_exists(output_dir)) fs::dir_create(data_dir)

  datas1 <- purrr::map(1:3, ~ sim_tools$simulate_preranked_data(seed = 4321))
  datas1 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas2 <- purrr::map(1:3, ~ sim_tools$simulate_preranked_data(seed = 1234))
  datas2 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas3 <- purrr::map(1:3, ~ sim_tools$simulate_preranked_data(seed = 9999))
  datas3 %<>% purrr::map(~ .x %>% mutate(value = value + rnorm(nrow(.), sd = .1)))
  datas <-
    c(
      datas1,
      datas2,
      datas3
    )

  names(datas) <- c(
    paste0("group_A_", seq_along(datas1)),
    paste0("group_B_", seq_along(datas2)),
    paste0("group_C_", seq_along(datas3))
  )

  # ._ <- datas %>% purrr::imap(~ {
  #   # if (!file.exists(
  #   write_tsv(.x, file.path(data_dir, paste0(.y, ".tsv")))
  # })

  .mat <- base::Reduce(
    accumulate = F,
    f = function(...) full_join(..., by = "id"),
    x = datas
  ) %>% as.data.frame()
  colnames(.mat) <- c("id", names(datas)) # lost the names

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
  # test_gct_path <- file.path(data_dir, "test.gct")
  if (!file.exists(test_gct_path)) cmapR::write_gct(gct, test_gct_path, appenddim = F)
  # return(test_gct_path)
}

teardown <- function() {
  if (fs::dir_exists(data_dir)) fs::dir_delete(data_dir)
  if (fs::dir_exists(output_dir)) fs::dir_delete(output_dir)
}


setup()


params <- list(
  rankfiledir = data_dir,
  # volcanodir = data_dir,
  savedir = output_dir,
  gct_path = test_gct_path,
  ranks_from = "gct",
  zscore_emat = TRUE,
  zscore_emat_groupby = NA,
  combine_by = "group",
  barplot = list(do_individual = FALSE, do_combined = FALSE),
  genesets = list(
    list(category = "H", subcategory = "", collapse = FALSE) # ,
    # list(category="C5", subcategory="GO:BP", collapse=TRUE)
  ),
  enplot = list(do_individual = F, do_combined = F, limit = 4),
  heatmap_gene = list(do=TRUE, limit=4),
  heatmap_gsea = list(do=TRUE),
  pca = list(do=FALSE)
)

  # run_tools$run(params)


testthat::test_that("test ssgsea", {
  testthat::expect_no_error({
    print("Function is being called")
    run_tools$run(params)
  })
})