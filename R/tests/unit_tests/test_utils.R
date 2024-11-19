suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(here))

src_dir <- file.path(here("R"))
# ==

testthat::test_that("test_create_env", {
  # Create a new environment

  src_dir <- file.path(here("R"))
  util_tools <- new.env()
  source(file.path(src_dir, "./utils.R"), local = util_tools)

  testthat::expect_true(
    !is.null(util_tools$make_partial) && ("function" %in% class(util_tools$make_partial))
  )

  testthat::expect_true(
    !is.null(util_tools$get_arg) && ("function" %in% class(util_tools$get_arg))
  )

  make_partial <- util_tools$make_partial
})

# ==

util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)

io_tools <- new.env()
source(file.path(src_dir, "./io.R"), local = io_tools)

make_partial <- util_tools$make_partial
get_arg <- util_tools$get_arg
get_args <- util_tools$get_args




testthat::test_that("test get_args", {
  f <- function() {}
  attr(f, "preset_args") <- list(a = 1, b = 2)

  expect_equal(get_args(f), list(a = 1, b = 2))
  expect_equal(get_args(function() {}), list()) # Should return an empty list for functions without preset_args
})



testthat::test_that("test scale gct", {
  gct <- io_tools$make_random_gct(10, 6)
  newgct <- util_tools$scale_gct(gct)
  testthat::expect_equal(
    gct@mat %>% dim(),
    newgct@mat %>% dim()
  )
})

# logging

testthat::test_that("test log msg", {
  .f <- "test.log"
  testthat::expect_error(util_tools$log_msg(info = "test", filename = .f), NA)
  # This is the standard way to assert that a block of code should execute without any errors in testthat.
  testthat::expect_true(fs::file_exists(.f))
  testthat::expect_true(stringr::str_detect(readLines(.f), "INFO"))
  fs::file_delete(.f)
})


testthat::test_that("test log msg levels", {
  .f <- "test.log"
  if (fs::file_exists(.f)) fs::file_delete(.f)
  util_tools$log_msg(debug = "test", filename = .f)
  testthat::expect_true(
    stringr::str_detect(readLines(.f), "DEBUG")
  )
  fs::file_delete(.f)
})
