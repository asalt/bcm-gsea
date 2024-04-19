library(fgsea)
library(msigdbr)


run_one <- function(rankobj, geneset){
  # to look for duplicate gene names
  # rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  .maxcounts <- rankobj %>% names %>% table %>% as.data.frame %>% pull(Freq) %>% max
  assertthat::are_equal(.maxcounts, 1)
  fgseaRes <- fgsea(pathways = geneset, stats = rankobj, minSize=15, maxSize=500,
                    #eps = 0.0
                    )#, nperm=1000)
  fgseaRes
}

run_all_rankobjs <- function(pathway, rankobjs){
  rankobjs %>%  purrr::map(
    ~run_one(., geneset = pathway)
  )
}
run_all_pathways <- function(pathways_list, ranks){
  pathways_list %>% purrr::map(
    ~run_all_rankobjs(., rankobjs = ranks)
  )
}

# results_list <- run_all_pathways(pathways_list_of_lists, ranks_list)

simulate_preranked_data <- function(...){

  set.seed(4321)

  geneset <- msigdbr::msigdbr(species = "Homo sapiens",
      category = "H",
      subcategory = ""
      )
  genes <- geneset %>% pull(entrez_gene) %>% unique()


  .values <- rnorm(n=length(genes))
  .data <- data.frame(
      geneid = genes,
      signedlogp = .values
  )

  return(.data)



}