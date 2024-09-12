library(here)
library(DBI)
library(RSQLite)
library(stringr)

sim_tools <- new.env()
source(file.path(here("R"), "simulate.R"), local = sim_tools) 

db_tools <- new.env()
source(file.path(here("R"), "db.R"), local = db_tools) 

geneset_tools <- new.env()
source(file.path(here("R"), "geneset_utils.R"), local = geneset_tools)

TESTDB <- file.path("test_data", "test.db")
if (!file.exists("test_data")) dir.create("test_data")

setup <- function() {

  print(TESTDB)
  db_tools$initialize_db(TESTDB)
  con <- db_tools$get_con(TESTDB)

  test_data <- sim_tools$generate_test_data(pathways = c("H", "GO:BP") )
  genesets_info <- list(
                    "H_" = geneset_tools$get_collection(category="H", subcategory = ""),
                    "C5_GO:BP" = geneset_tools$get_collection(category="C5", subcategory = "GO:BP")
                    )
  genesets_list_of_lists <- purrr::map(genesets_info, geneset_tools$genesets_df_to_list)

  collections <- names(test_data)
  rank_names <- names(test_data[[1]])

  for (rank_name in rank_names){
      db_tools$insert_rankobj(con = con, rank_name = rank_name)
  }

  
  for (collection in collections){
      collection_id <- db_tools$insert_collection(con, collection)
      for (pathway_name in names(genesets_list_of_lists[[collection]])) {
        db_tools$insert_pathway(
            con,
            collection_id=collection_id,
            pathway_name = pathway_name,
            members = str_c(genesets_list_of_lists[[collection]][[pathway_name]])
                )
       }
  }
}
