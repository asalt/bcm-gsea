suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(future))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(here))

src_dir <- file.path(here("R"))
parallel_tools <- new.env()
source(file.path(src_dir, "parallel_heatmap.R"), local = parallel_tools)

get_workers <- function(job_count, desired_workers = NULL) {
  parallel_tools$resolve_multisession_workers(
    job_count = job_count,
    workers = desired_workers,
    reserve = 0L
  )
}

multisession_available <- function(workers) {
  ok <- TRUE
  prev_plan <- future::plan()
  on.exit(future::plan(prev_plan), add = TRUE)
  tryCatch({
    future::plan(future::multisession, workers = workers)
    invisible(NULL)
  }, error = function(e) {
    ok <<- FALSE
  })
  ok
}

test_that("run_multisession_jobs falls back to sequential execution", {
  jobs <- list(1, 2, 3)
  job_fn <- function(x) x * 2

  res <- parallel_tools$run_multisession_jobs(
    jobs = jobs,
    job_fn = job_fn,
    workers = 1L,
    reserve = 0L
  )

  expect_equal(res$workers, 1L)
  expect_equal(res$results, list(2, 4, 6))
  expect_equal(res$errors, vector("list", length(jobs)))
})

test_that("run_multisession_jobs executes in parallel when workers are available", {
  jobs <- as.list(1:6)
  job_fn <- function(x) {
    Sys.sleep(0.05)
    x^2
  }

  resolved <- get_workers(length(jobs), desired_workers = 2L)
  if (resolved < 2L) {
    skip("insufficient workers for multisession test")
  }
  if (!multisession_available(resolved)) {
    skip("multisession backend unavailable in this environment")
  }

  res <- parallel_tools$run_multisession_jobs(
    jobs = jobs,
    job_fn = job_fn,
    workers = 2L,
    reserve = 0L
  )

  expect_gte(res$workers, 2L)
  expect_equal(res$results, purrr::map(jobs, ~ .x^2))
  expect_true(all(purrr::map_lgl(res$errors, is.null)))
})

test_that("run_multisession_jobs captures errors without stopping", {
  jobs <- list(1, "boom", 3)
  job_fn <- function(x) {
    if (!is.numeric(x)) stop("non-numeric job")
    x + 1
  }

  res <- parallel_tools$run_multisession_jobs(
    jobs = jobs,
    job_fn = job_fn,
    workers = 1L,
    reserve = 0L,
    stop_on_error = FALSE
  )

  expect_equal(res$results[[1]], 2)
  expect_null(res$results[[2]])
  expect_equal(res$results[[3]], 4)
  expect_s3_class(res$errors[[2]], "error")
})
