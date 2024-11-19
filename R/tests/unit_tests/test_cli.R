suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(argparser))
suppressPackageStartupMessages(library(testthat))

cli_tools <- new.env()
source(file.path(here("R"), "./cli.R"), local = cli_tools)


test_that("filetype coercion works", {
  expect_error(as("nonexistent_file.toml", "filetype"), "does not exist")
})


test_that("argument parser works", {
  # Mock arguments
  parser <- cli_tools$get_parser()

  # Test with a non-existent file
  expect_error(parse_args(parser, c("nonexistent_file.toml")), "does not exist")

  # Test with an existing file
  temp_file <- tempfile()
  file.create(temp_file)
  args <- parse_args(parser, c(temp_file))
  expect_equal(args$config, temp_file)
})


test_that("cli works", {
  expect_silent(
    system2(
      c(
        "Rscript",
        file.path(here("R"), "cli.R"),
        "--help"
      )
    )
  )
})
