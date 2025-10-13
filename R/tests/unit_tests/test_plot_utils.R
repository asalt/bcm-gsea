suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(here))

src_dir <- file.path(here("R"))
plot_utils <- new.env()
source(file.path(src_dir, "./plot_utils.R"), local = plot_utils)


util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)
make_partial <- util_tools$make_partial
get_args <- util_tools$get_args
get_arg <- util_tools$get_arg
log_msg <- util_tools$make_partial(util_tools$log_msg)


context("Utility functions")

test_that("get_args returns correct preset arguments", {
  f <- function() {}
  attr(f, "preset_args") <- list(a = 1, b = 2)

  expect_equal(get_args(f), list(a = 1, b = 2))
  expect_equal(get_args(function() {}), list()) # Should return an empty list for functions without preset_args
})

test_that("get_arg returns correct argument values", {
  f <- function() {}
  attr(f, "preset_args") <- list(a = 1, b = 2)

  expect_equal(get_arg(f, "a"), 1)
  expect_equal(get_arg(f, "b"), 2)
  expect_equal(get_arg(f, "c"), "") # Test for non-existent arg should return ""
})
