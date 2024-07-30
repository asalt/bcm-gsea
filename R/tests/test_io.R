suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(withr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(here))


src_dir <- file.path(here("R"))
io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)

test_that("ranks_dfs_to_lists returns correct list structure and naming", {
  # Create sample data frames
  df1 <- data.frame(
    id = c("gene1", "gene2", "gene3"),
    value = c(2.3, -1.5, 0.7)
  )
  df2 <- data.frame(
    id = c("gene4", "gene5"),
    value = c(-0.5, 1.2)
  )

  # Apply the function
  result <- io_tools$ranks_dfs_to_lists(list(df1, df2))

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

test_that("create_rnkfiles_from_volcano processes files correctly", {
  withr::with_tempdir({
    # Create a temporary directory and some sample files
    fs::dir_create("volcano_test")
    write_lines("GeneID\tvalue\nGene1\t0.5\nGene2\t-1.2", "volcano_test/group_test1_data.tsv")
    write_lines("GeneID\tvalue\nGene3\t1.5\nGene4\t-0.3", "volcano_test/group_test2_data.tsv")

    # Test the function
    result <- io_tools$create_rnkfiles_from_volcano("volcano_test")
    expect_true("test1" %in% names(result))
    expect_true("test2" %in% names(result))
    expect_equal(nrow(result[["test1"]]), 2)
    expect_equal(nrow(result[["test2"]]), 2)
  })
})

test_that("create_rnkfiles_from_volcano processes files rename", {
  withr::with_tempdir({
    # Create a temporary directory and some sample files
    fs::dir_create("volcano_test")
    write_lines("GeneID\tValue\nGene1\t0.5\nGene2\t-1.2", "volcano_test/group_test1_data.tsv")
    write_lines("GeneID\tValue\nGene3\t1.5\nGene4\t-0.3", "volcano_test/group_test2_data.tsv")

    # Test the function
    result <- io_tools$create_rnkfiles_from_volcano("volcano_test", id_col = "GeneID", value_col = "Value")
    expect_true("test1" %in% names(result))
    expect_true("test2" %in% names(result))
    expect_equal(nrow(result[["test1"]]), 2)
    expect_equal(nrow(result[["test2"]]), 2)
    expect_true("value" %in% colnames(result[["test1"]]))

    expect_true("id" %in% colnames(result[["test1"]]))
  })
})


test_that("write_rnkfiles writes files correctly", {
  withr::with_tempdir("rnk_test", {
    lst <- list(
      test1 = tibble(GeneID = c("Gene1", "Gene2"), value = c(0.5, -1.2)),
      test2 = tibble(GeneID = c("Gene3", "Gene4"), value = c(1.5, -0.3))
    )
    io_tools$write_rnkfiles(lst, "rnk_test")
    expect_true(fs::file_exists("rnk_test/test1.rnk"))
    expect_true(fs::file_exists("rnk_test/test2.rnk"))
  })
})



test_that("load_rnkfiles loads and processes files correctly", {
  withr::with_tempdir("rnk_test", {
    write_lines("Gene1\t0.5\nGene2\t-1.2", "file1.rnk")
    write_lines("Gene3\t1.5\nGene4\t-0.3", "file2.rnk")

    result <- io_tools$load_rnkfiles(c("file1.rnk", "file2.rnk")) # returns a dataframe with columns: id, value
    expect_equal(length(result), 2)
    expect_equal(nrow(result[[1]]), 2)
    expect_equal(nrow(result[[2]]), 2)
    expect_equal(result[[1]]$id[1], "Gene1")
    expect_true(is.numeric(result[[1]]$value))
  })
})

test_that("create_rnkfiles_from_volcano handles missing directory correctly", {
  expect_error(create_rnkfiles("non_existent_directory"))
})



test_that("make random gct works", {
  result <- io_tools$make_random_gct(50, 5)
  # expect_true("mat" %in% names(result)) # names(result) is NULL gct object has no names
  # expect_true("rids" %in% names(result))
  # expect_true("cids" %in% names(result))
  # expect_true("cdesc" %in% names(result))
  expect_equal(nrow(result@mat), 50)
  expect_equal(ncol(result@mat), 5)
  expect_equal(length(result@rid), 50)
  expect_equal(length(result@cid), 5)
  expect_equal(nrow(result@cdesc), 5)
  expect_equal(ncol(result@cdesc), 3) # expected based on how make_random_gct is coded. hardcoded id and 2 metadata cols

  # expect_success( # test we can melt
  #   result %>% melt_gct()
  # )
})

# Test that the function creates an object of the correct type
test_that("make_random_gct returns a GCT object", {
  expect_s4_class(io_tools$make_random_gct(), "GCT")
})

# Test that the function outputs have correct dimensions
test_that("make_random_gct outputs have correct dimensions", {
  gct <- io_tools$make_random_gct(10, 4)
  expect_equal(dim(gct@mat), c(10, 4))
  expect_equal(length(gct@rid), 10)
  expect_equal(length(gct@cid), 4)
})

# Test for consistent output given a set seed
test_that("make_random_gct produces consistent output with set seed", {
  gct1 <- io_tools$make_random_gct(10, 4)
  gct2 <- io_tools$make_random_gct(10, 4)
  expect_equal(gct1@mat, gct2@mat)
  expect_equal(gct1@cdesc, gct2@cdesc)
})

# Test edge cases
test_that("make_random_gct handles zero dimensions appropriately", {
  gct_zero_rows <- io_tools$make_random_gct(0, 4)
  expect_equal(dim(gct_zero_rows@mat), c(1, 4)) # just make it 1 if entered as zero
  # will probably encounter this in practice

  gct_zero_cols <- io_tools$make_random_gct(10, 0)
  expect_equal(dim(gct_zero_cols@mat), c(10, 1))
})

# Optionally, test metadata consistency
test_that("Metadata columns are correctly sampled", {
  gct <- io_tools$make_random_gct(10, 4)
  expect_true(all(gct@cdesc$metavar1 %in% letters[1:5]))
  expect_true(all(gct@cdesc$metavar2 %in% letters[1:5]))
})



test_that("create_rnkfiles_from_gct object", {
  withr::with_tempdir({
    temp_dir <- tempdir() # Retrieve the current temporary directory

    # Construct the file path within the temporary directory

    # Create a GCT object and attempt to write it to a GCTx format
    gct_object <- io_tools$make_random_gct(10, 4)
    file_basename <- file.path(temp_dir, "test")
    full_path <- paste0(file_basename, "_n4x10.gct")
    write_gct(gct_object, file_basename)

    # Check if the file was created successfully
    expect_true(file.exists(full_path), info = "The GCTX file was not created.")

    # Assuming create_rnkfiles_from_emat reads a GCTx file and returns a GCT object
    list_of_ranks1 <- io_tools$create_rnkfiles_from_emat(full_path, apply_z_score = FALSE)
    list_of_ranks2 <- io_tools$create_rnkfiles_from_emat(full_path, apply_z_score = TRUE)

    means1 <- list_of_ranks1 %>% map(~ .x$value %>%
      as.numeric() %>%
      mean())
    means2 <- list_of_ranks2 %>% map(~ .x$value %>%
      as.numeric() %>%
      mean())

    expect_true(all(means1 > .1))
    expect_true(all(means2 < .1), info = paste0("Means1: ", means1, " Means2: ", means2))

    # Check if the resulting object is a GCT class
  })
})
