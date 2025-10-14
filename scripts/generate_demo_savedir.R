suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(BiocParallel))

source(here("R", "lazyloader.R"))

sim_tools <- get_tool_env("simulate")
run_tools <- get_tool_env("run")

BiocParallel::register(BiocParallel::SerialParam())

args <- commandArgs(trailingOnly = TRUE)
output_root <- if (length(args) >= 1) {
  fs::path_abs(args[[1]])
} else {
  fs::path_abs(here("dev", "demo_savedir"))
}

input_dir <- fs::path(output_root, "input")
savedir <- fs::path(output_root, "results")

fs::dir_create(input_dir, recurse = TRUE)
fs::dir_create(savedir, recurse = TRUE)

set.seed(369)
groups <- c("A", "B", "C")
datasets <- purrr::map(groups, function(group_label) {
  purrr::map(1:3, ~ sim_tools$simulate_preranked_data(seed = sample.int(10000, 1))) %>%
    purrr::set_names(~ paste0("group_", group_label, "_", seq_along(.x)))
}) %>%
  purrr::flatten()

value_shift <- purrr::map(datasets, ~ .x %>%
  mutate(value = value + rnorm(nrow(.x), sd = 0.2)))

matrix_df <- Reduce(
  f = function(lhs, rhs) full_join(lhs, rhs, by = "id"),
  x = value_shift
) %>%
  as.data.frame()

colnames(matrix_df) <- c("id", names(value_shift))
rownames(matrix_df) <- matrix_df$id
matrix_df$id <- NULL

metadata <- tibble::tibble(
  id = colnames(matrix_df),
  group = stringr::str_extract(id, "(?<=group_)[A-Z]")
) %>%
  as.data.frame()
rownames(metadata) <- metadata$id

row_metadata <- tibble::tibble(
  id = rownames(matrix_df),
  feature = rownames(matrix_df)
) %>%
  as.data.frame()
rownames(row_metadata) <- row_metadata$id

gct_obj <- new("GCT",
  mat = as.matrix(matrix_df),
  cdesc = metadata,
  rdesc = row_metadata
)

gct_path <- fs::path(input_dir, "demo.gct")
if (!fs::file_exists(gct_path)) {
  cmapR::write_gct(gct_obj, gct_path, appenddim = FALSE)
}

params <- list(
  savedir = savedir,
  rankfiledir = savedir,
  gct_path = gct_path,
  ranks_from = "gct",
  zscore_emat = TRUE,
  zscore_emat_groupby = FALSE,
  combine_by = NULL,
  model = list(),
  models = list(),
  model_file = "",
  genesets = list(
    list(category = "H", subcategory = "", collapse = FALSE)
  ),
  barplot = list(do_individual = FALSE, do_combined = FALSE),
  bubbleplot = list(do_individual = FALSE, do_combined = FALSE),
  heatmap_gene = list(do = FALSE),
  heatmap_gsea = list(do = FALSE),
  enplot = list(do_individual = FALSE, do_combined = FALSE),
  pca = list(do = FALSE),
  advanced = list(
    parallel = FALSE,
    cache = FALSE,
    replace = TRUE,
    quiet = TRUE,
    cachedir = fs::path(savedir, "cache"),
    logfile = fs::path(savedir, "run.log")
  )
)

run_tools$run(params)

cat(savedir, "\n")
