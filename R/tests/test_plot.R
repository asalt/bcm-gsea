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
