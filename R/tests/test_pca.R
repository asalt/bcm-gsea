suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(PCAtools))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(testthat))


src_dir <- file.path(here("R"))
pca_tools <- new.env()
source(file.path(src_dir, "pca.R"), local = pca_tools)




context("Testing do_one function")

test_that("do_one stops with non-existent required columns", {
  fake_data <- data.frame(NES = rnorm(10), var = letters[1:10])
  expect_error(pca_tools$do_one(fake_data), "pathway column not found")
})

test_that("do_one computes PCA correctly with minimal inputs", {
  # Assuming a minimal correct dataset
  fake_data <- data.frame(pathway = rep("path", 10), NES = rnorm(10), var = letters[1:10], mainpathway = sample(c(TRUE, FALSE), 10, replace = TRUE))
  result <- pca_tools$do_one(fake_data)
  expect_true("pca" %in% class(result))  # Check if PCA result is returned, it should be pca obj from PCAtools::pca
})

test_that("do_one handles different main_pathway_ratios correctly", {
  # Test with various ratios
  fake_data <- data.frame(pathway = rep("path", 10), NES = rnorm(10), var = letters[1:10], mainpathway = sample(c(TRUE, FALSE), 10, replace = TRUE))
  low_ratio_result <- pca_tools$do_one(fake_data, main_pathway_ratio = 0.05)
  high_ratio_result <- pca_tools$do_one(fake_data, main_pathway_ratio = 0.95)
  expect_true(nrow(low_ratio_result$loadings) <= nrow(high_ratio_result$loading))
})


context("Testing do_all function")

test_that("do_all applies do_one to multiple GSEA objects correctly", {
  fake_list <- list(
    data1 = data.frame(pathway = rep("path1", 10), NES = rnorm(10), var = letters[1:10]),
    data2 = data.frame(pathway = rep("path2", 10), NES = rnorm(10), var = letters[1:10])
  )
  results <- pca_tools$do_all(fake_list)
  expect_equal(length(results), 2)
  expect_true(all(sapply(results, function(x) "pca" %in% class(x))))
})

# test_that("do_all handles empty lists gracefully", {
#   expect_error(pca_tools$do_all(list()), "Error in do_one")  # Depending on how do_one handles empty data
# })



context("Testing plot_biplot function")

test_that("plot_biplot doesnot crash if not enough pcs", {

  fake_data <- data.frame(
     pathway = rep("pathway1", 10),
     NES = rnorm(10),
     var = letters[1:10],
     mainpathway = sample(c(TRUE, FALSE), 10, replace = TRUE)
  )
  pca_object <- pca_tools$do_one(fake_data)
  expect_warning(pca_tools$plot_biplot(pca_object, colby = "nonexistent"))
})

test_that("plot_biplot works", {

  fake_data <- data.frame(
     pathway = c(rep("pathway1", 10), rep("pathway2", 10), rep("pathway3", 10)),
     NES = rnorm(30),
     var = rep(letters[1:10], 3),
     mainpathway = sample(c(TRUE, FALSE), 30, replace = TRUE)
  )
  pca_object <- pca_tools$do_one(fake_data)
  plts <- pca_tools$plot_biplot(pca_object)
  for (plt in plts){
   testthat::expect_true(
      "ggplot" %in% class(plt)
      # all(
      #   "gg" %in% class(plt),
      #   "ggplot" %in% class(plt)
      # )
    )}
})

test_that("plot_biplot handles non-existent color by metadata gracefully", {

  fake_data <- data.frame(
     pathway = c(rep("pathway1", 10), rep("pathway2", 10), rep("pathway3", 10)),
     NES = rnorm(30),
     var = rep(letters[1:10], 3),
     mainpathway = sample(c(TRUE, FALSE), 30, replace = TRUE)
  )
  pca_object <- pca_tools$do_one(fake_data)
    # pca_tools$plot_biplot(pca_object, colby = "nonexistent")
  expect_warning(pca_tools$plot_biplot(pca_object, colby = "nonexistent"), regexp="nonexistent not found in metadata")
})

# test_that("plot_biplot creates a plot with the correct dimensions", {
# 
#   fake_data <- data.frame(pathway = rep("path", 10), NES = rnorm(10), var = letters[1:10], mainpathway = sample(c(TRUE, FALSE), 10, replace = TRUE))
#   pca_object <- pca_tools$do_one(fake_data)
#   plot_result <- pca_tools$plot_biplot(pca_object, top_pc = 2)
#   expect_equal(length(plot_result), choose(2, 2))  # 2 choose 2 = 1 plot for PC1 vs PC2
# })

