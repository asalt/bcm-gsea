suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(withr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))

source("../io.R")

test_that("ranks_dfs_to_lists returns correct list structure and naming", {
  # Create sample data frames
  df1 <- data.frame(
    geneid = c("gene1", "gene2", "gene3"),
    value = c(2.3, -1.5, 0.7)
  )
  df2 <- data.frame(
    geneid = c("gene4", "gene5"),
    value = c(-0.5, 1.2)
  )

  # Apply the function
  result <- ranks_dfs_to_lists(list(df1, df2))

  # Check if the result is a list
  expect_is(result, "list")

  # Check the length of the list
  expect_equal(length(result), 2)

  # Check contents of the first list element
  expect_named(result[[1]], c("gene1", "gene2", "gene3"))
  expect_equal(result[[1]], c(gene1 = 2.3, gene2 = -1.5, gene3 = 0.7))

  # Check contents of the second list element
  expect_named(result[[2]], c("gene4", "gene5"))
  expect_equal(result[[2]], c(gene4 = -0.5, gene5 = 1.2))
})

test_that("create_rnkfiles processes files correctly", {
  withr::with_tempdir({
    # Create a temporary directory and some sample files
    fs::dir_create("volcano_test")
    write_lines("GeneID\tValue\nGene1\t0.5\nGene2\t-1.2", "volcano_test/group_test1_data.tsv")
    write_lines("GeneID\tValue\nGene3\t1.5\nGene4\t-0.3", "volcano_test/group_test2_data.tsv")

    # Test the function
    result <- create_rnkfiles("volcano_test")
    expect_true("test1" %in% names(result))
    expect_true("test2" %in% names(result))
    expect_equal(nrow(result[["test1"]]), 2)
    expect_equal(nrow(result[["test2"]]), 2)
  })
})


test_that("write_rnkfiles writes files correctly", {
  withr::with_tempdir("rnk_test", {
    lst <- list(
      test1 = tibble(GeneID = c("Gene1", "Gene2"), value = c(0.5, -1.2)),
      test2 = tibble(GeneID = c("Gene3", "Gene4"), value = c(1.5, -0.3))
    )
    write_rnkfiles(lst, "rnk_test")
    expect_true(fs::file_exists("rnk_test/test1.rnk"))
    expect_true(fs::file_exists("rnk_test/test2.rnk"))
  })
})



test_that("load_rnkfiles loads and processes files correctly", {
  withr::with_tempdir("rnk_test", {
    write_lines("Gene1\t0.5\nGene2\t-1.2", "file1.rnk")
    write_lines("Gene3\t1.5\nGene4\t-0.3", "file2.rnk")

    result <- load_rnkfiles(c("file1.rnk", "file2.rnk"))
    expect_equal(length(result), 2)
    expect_equal(nrow(result[[1]]), 2)
    expect_equal(nrow(result[[2]]), 2)
    expect_equal(result[[1]]$geneid[1], "Gene1")
    expect_true(is.numeric(result[[1]]$value))
  })
})

test_that("create_rnkfiles handles missing directory correctly", {
  expect_error(create_rnkfiles("non_existent_directory"))
})
