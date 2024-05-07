# test_plot.R
source("../fgsea.R")
source("../io.R")
source("../geneset_utils.R")
source("../plot.R")
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(assertthat))


test_plot_bar_format <- function() {
    data <- .GlobalEnv$simulate_preranked_data()
    geneset <- .GlobalEnv$get_collection("H", "")
    geneset_list <- .GlobalEnv$genesets_df_to_list(geneset)
    # named_data = list(data=data)
    rankobjs <- .GlobalEnv$ranks_dfs_to_lists(list(data))
    rankobj <- rankobjs[[1]]
    res <- rankobj %>% .GlobalEnv$run_one(geneset_list)

    res_filtered <- res %>%
        arrange(pval) %>%
        head(5)


    out <- .GlobalEnv$prepare_data_for_barplot(res_filtered)

    assertthat::has_name(
        out,
        c("leadingEdgeNum", "leadingEdgeFrac")
    )


    return("Success")
}


test_plot_bar <- function() {
    data <- .GlobalEnv$simulate_preranked_data()
    geneset <- .GlobalEnv$get_collection("H", "")
    geneset_list <- .GlobalEnv$genesets_df_to_list(geneset)
    # named_data = list(data=data)
    rankobjs <- .GlobalEnv$ranks_dfs_to_lists(list(data))
    rankobj <- rankobjs[[1]]
    res <- rankobj %>% .GlobalEnv$run_one(geneset_list)

    res_filtered <- res %>%
        arrange(pval) %>%
        head(5)


    out <- .GlobalEnv$prepare_data_for_barplot(res_filtered)

    plt <- .GlobalEnv$barplot_with_numbers(out)


    assertthat::assert_that(
        all(
            "gg" %in% class(plt),
            "ggplot" %in% class(plt)
        )
    )

    return("Success")
}

(test_plot_bar_format())
(test_plot_bar())



#
generate_test_data <- function() {
    genesets_of_interest <- list(
        list(category = "H", subcategory = ""),
        list(category = "C5", subcategory = "GO:BP")
    )
    genesets <- .GlobalEnv$get_collections(
        genesets_of_interest
    )

    geneset_list <- genesets %>% purrr::imap(~ .GlobalEnv$genesets_df_to_list(.x))

    # named_data = list(data=data)
    data1 <- .GlobalEnv$simulate_preranked_data(seed = 1234)
    data2 <- .GlobalEnv$simulate_preranked_data(seed = 4321)
    data <- list(first = data1, second = data2)
    rankobjs <- .GlobalEnv$ranks_dfs_to_lists(data)
    res <- .GlobalEnv$run_all_pathways(geneset_list, rankobjs)
    return(res)
}

test_that("test generate testdata", {
    res <- generate_test_data()
    assert_that(length(res) == 2)
    assert_that(
        res[[names(res)[1]]] %>% length(), 2
    )

    assert_that(
        "data.frame" %in% class(res[[names(res)[1]]][[1]]) # there are alot of brackets here
    )
})

test_that("test concat results one collection", {
    res <- generate_test_data()
    res1 <- res[[names(res)[1]]]
    assert_that(
        "list" %in% class(res1)
    )

    res1_c <- res1 %>% concat_results_one_collection()
    asssert_that(
        "data.frame" %in% class(res1_c)
    )

    assert_that(
        "var" %in% colnames(res1_c)
    )

    assert_that(
        sort(unique(res1_c$var)) == c("first", "second")
    )
})


test_that("test concat results all collection", {
    res <- generate_test_data()
    res_c <- res %>% concat_results_all_collections()
    asssert_that(
        "list" %in% class(res_c)
    )
})


test_that("test heatmap of NES", {
    res <- generate_test_data()
    res_c <- res %>% concat_results_all_collections()
    ht <- res_c[[1]] %>% plot_results_one_collection()
    assert_that(
        "HeatmapList" %in% class(ht)
    )
})



test_that("test run though all heatmaps of NES", {
    res <- generate_test_data()
    res_c <- res %>% concat_results_all_collections()
    ht_list <- plot_results_all_collections(res_c)
    assert_that(
        "list" %in% class(ht_list)
    )


    ht_list %>% purrr::walk(~ {
        assert_that(
            "HeatmapList" %in% class(.x)
        )
    })
})
