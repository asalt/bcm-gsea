suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(here))



# src_dir <- file.path(here("R"))
# sim_tools <- new.env()
# source(file.path(src_dir, "simulate.R"), local = sim_tools)

source(file.path(here("R", "lazyloader.R")))
sim_tools <- get_tool_env("simulate")

# ==

testthat::test_that("test simulate", {
  data <- sim_tools$simulate_preranked_data()
  testthat::expect_true(
    "data.frame" %in% class(data)
  )

  testthat::expect_true(
    "id" %in% colnames(data)
  )

  testthat::expect_true(
    "value" %in% colnames(data)
  )
})


# unfortunately right now this depends on io.R and fgesa.R working
test_that("test generate testdata", {
  res <- sim_tools$generate_test_data()
  testthat::expect_true("C5_GO:BP" %in% names(res))
  testthat::expect_true(length(res) == 2) # we expect two geneset collections

  testthat::expect_true(
    res[[names(res)[1]]] %>% length() == 2 # we expect two rankobjs in each collection
  )

  testthat::expect_true(
    res[[names(res)[2]]] %>% length() == 2 # we expect two rankobjs in each collection
  )

  testthat::expect_true(
    "data.frame" %in% class(res[[names(res)[1]]][[1]]) # there are alot of brackets here
  )
})

test_that("test test data with collapse", {
  TEST_DATA_withCollapse <- sim_tools$generate_test_data(collapse = TRUE)
  TEST_DATA_withCollapse
  results <- TEST_DATA_withCollapse$`C5_GO:BP` %>%
    map(~ .x %>%
      pull(mainpathway) %>%
      table())

  # Iterate through each result element and perform checks
  walk(results, ~ {
    expect_true(all(c("TRUE", "FALSE") %in% names(.x)), info = "Both TRUE and FALSE should be present in mainpathway.")
    expect_true(.x["TRUE"] > 0, info = "There should be more than 0 TRUE values.")
    expect_true(.x["FALSE"] > 0, info = "There should be more than 0 FALSE values.")
  })
})


test_that("test test data without collapse", {
  TEST_DATA <- sim_tools$generate_test_data()
  results <- TEST_DATA$`C5_GO:BP` %>%
    map(~ .x %>%
      pull(mainpathway) %>%
      table())

  # Iterate through each result element and perform checks
  walk(results, ~ {
    expect_true("TRUE" %in% names(.x), info = "only TRUE should be present in mainpathway count.")
    expect_false("FALSE" %in% names(.x), info = "FALSE should not be present in mainpathway count.")
    expect_true(.x["TRUE"] > 0, info = "There should be more than 0 TRUE values.")
  })
  #
})
