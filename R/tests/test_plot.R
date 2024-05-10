# test_plot.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(assertthat))
suppressPackageStartupMessages(library(here))


io_tools <- new.env()
source("../io.R", local = io_tools)

geneset_tools <- new.env()
source("../geneset_utils.R", local = geneset_tools)

fgsea_tools <- new.env()
source("../fgsea.R", local = fgsea_tools)

plot_tools <- new.env()
source("../plot.R", local = plot_tools)


test_plot_bar_format <- function() {
    data <- fgsea_tools$simulate_preranked_data()
    geneset <- geneset_tools$get_collection("H", "")
    geneset_list <- geneset_tools$genesets_df_to_list(geneset)
    rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
    rankobj <- rankobjs[[1]]
    res <- rankobj %>% fgsea_tools$run_one(geneset_list)

    res_filtered <- res %>%
        arrange(pval) %>%
        head(5)


    out <- plot_tools$prepare_data_for_barplot(res_filtered)

    assertthat::has_name(
        out,
        c("leadingEdgeNum", "leadingEdgeFrac")
    )


    return("Success")
}


test_plot_bar <- function() {
    data <- fgsea_tools$simulate_preranked_data()
    geneset <- geneset_tools$get_collection("H", "")
    geneset_list <- geneset_tools$genesets_df_to_list(geneset)
    rankobjs <- io_tools$ranks_dfs_to_lists(list(data))
    rankobj <- rankobjs[[1]]
    res <- rankobj %>% fgsea_tools$run_one(geneset_list)

    res_filtered <- res %>%
        arrange(pval) %>%
        head(5)


    out <- plot_tools$prepare_data_for_barplot(res_filtered)

    plt <- plot_tools$barplot_with_numbers(out)


    assertthat::assert_that(
        all(
            "gg" %in% class(plt),
            "ggplot" %in% class(plt)
        )
    )

    return("Success")
}




#
generate_test_data <- function() {
    genesets_of_interest <- list(
        list(category = "H", subcategory = ""),
        list(category = "C5", subcategory = "GO:BP")
    )
    genesets <- geneset_tools$get_collections(
        genesets_of_interest
    )

    geneset_list <- genesets %>% purrr::imap(~ geneset_tools$genesets_df_to_list(.x))

    # named_data = list(data=data)
    data1 <- fgsea_tools$simulate_preranked_data(seed = 1234)
    data2 <- fgsea_tools$simulate_preranked_data(seed = 4321)
    data <- list(first = data1, second = data2)
    rankobjs <- io_tools$ranks_dfs_to_lists(data)
    res <- fgsea_tools$run_all_pathways(geneset_list, rankobjs)
    return(res)
}

test_that("test generate testdata", {
    res <- generate_test_data()
    assert_that(length(res) == 2) # we expect two geneset collections

    assert_that(
        res[[names(res)[1]]] %>% length() == 2 # we expect two rankobjs in each collection
    )

    assert_that(
        res[[names(res)[2]]] %>% length() == 2 # we expect two rankobjs in each collection
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

    res1_c <- res1 %>% plot_tools$concat_results_one_collection()
    assert_that(
        "data.frame" %in% class(res1_c)
    )

    assert_that(
        "var" %in% colnames(res1_c)
    )

    assert_that(
        all(sort(unique(res1_c$var)) == c("first", "second"))
    )
})


test_that("test concat results all collection", {
    res <- generate_test_data()
    res_c <- res %>% plot_tools$concat_results_all_collections()
    assert_that(
        "list" %in% class(res_c)
    )
})


test_that("test heatmap of NES", {
    res <- generate_test_data()
    res_c <- res %>% plot_tools$concat_results_all_collections()
    ht <- res_c[[1]] %>% plot_tools$plot_results_one_collection()
    assert_that(
        "HeatmapList" %in% class(ht)
    )
})



test_that("test run though all heatmaps of NES", {
    res <- generate_test_data()
    res_c <- res %>% plot_tools$concat_results_all_collections()
    ht_list <- plot_tools$plot_results_all_collections(res_c)
    assert_that(
        "list" %in% class(ht_list)
    )


    # ht_list %>% purrr::map(~ {
    #     assert_that(
    #         "HeatmapList" %in% class(.x)
    #     )
    # })
    for (ht in ht_list) {
        assert_that(
            "HeatmapList" %in% class(ht)
        )
    }
})



(test_plot_bar_format())
(test_plot_bar())
