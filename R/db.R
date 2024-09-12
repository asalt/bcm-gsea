library(here)
library(DBI)
library(RSQLite)
library(stringr)

# Function to initialize the database from the SQL file
get_con <- function(db_path = file.path(here("sql"), "rankorder_data.db")){
  con <- dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

initialize_db <- function(db_path = file.path(here("sql"), "rankorder_data.db"), sql_file = file.path(here("sql"), "init_db.sql")) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  # Read the SQL commands from the file
  sql_commands <- readLines(sql_file) 
  sql_commands <- Filter(\(x) !str_starts(x, '-') && !x=='', sql_commands)
  # print(sql_commands)
  sql_script <- paste(sql_commands, collapse = "")
  sql_statements <- unlist(strsplit(sql_script, ";"))

  # Execute each statement separately
  for (statement in sql_statements) {
    statement <- trimws(statement)
    if (nchar(statement) > 0) {
        print(statement)
      dbExecute(con, statement)
    }
  }
  dbDisconnect(con)
  message("Database initialized successfully.")
}
# initialize_db <- function(...){
#     tryCatch({do.call(.ititiali})
# }

# # Initialize the database (run this once)
# initialize_db()
#returns collection_id
insert_collection <- function(con, collection_name){
    res <- dbGetQuery(con, 'select collection_id from Collections where name = ?', params = c(collection_name))
    if (nrow(res) > 0) {
      warning(paste0(collection_name, " already exists, skipping"))
      return(res[1,])
    }

    dbExecute(con, "INSERT INTO Collections (name) VALUES (?)", params = collection_name)
    # dbExecute(con, "COMMIT")
    res <- dbGetQuery(con, 'select collection_id from Collections where name = ?', params = c(collection_name))
    return(res[1,])
}

get_pathway_id <- function(con, pathway_name){
    res <- dbGetQuery(con, 'select pathway_id from Pathways where name = ?', params = c(pathway_name))
    if (nrow(res) == 0) return(NULL)
    return(res[1,])
}

insert_pathway <- function(con, collection_id = NULL, collection_name = NULL, pathway_name = NULL, members = NULL){
    # dbExecute(con, "INSERT INTO Collection (name) VALUES (?)", params = collection_name)

    if (is.null(pathway_name)) pathway_name <- "empty"
    if (is.null(members)) members <- ""

    maybe_pathway_id <- get_pathway_id(con, pathway_name)

    if (!is.null(maybe_pathway_id)) {
        warning(paste0("pathway ", pathway_name, " already present in db"))
        return()
    }


    .res <- dbGetQuery(con, 'select rankobj_id from RankObjects where name = ?', params = c('second'))

    if (is.null(collection_id) && !is.null(collection_name)){
      res <- dbGetQuery(con, 'select collection_id from Collections where name = ?', params = c(collection_name))
      if (nrow(res) == 0){ # then create
          collection_id <- insert_collection(con, connection_name)
      } else{
          collection_id <- insert_collection(con, 'empty')
      }
    }
    
    dbExecute(con, "INSERT INTO Pathways (name, ids, collection_id) VALUES (?, ?, ?)", params = c(pathway_name,
                                                                                                  str_c(members, collapse='/'),
                                                                                                  collection_id)
    )

}


insert_rankobj <- function(con, rank_name){

    res <- dbGetQuery(con, 'select rankobj_id from RankObjects where name = ?', params = c('second'))
    if (nrow(res) > 0) {
        warning(paste0("rankname ", rank_name, " already exists, cannot insert"))
        return()
    }
    dbExecute(con, "INSERT INTO RankObjects (name) VALUES (?)", params = rank_name)
    # dbExecute(con, "COMMIT")
}

insert_stats <- function(con, rankobj_id, stats_data) {
  # Assuming stats_data is a data frame with columns: rank, stat
  dbExecute(con, 
    "INSERT INTO Stats (rankobj_id, rank, stat) VALUES (?, ?, ?)", 
    params = lapply(1:nrow(stats_data), function(i) {
      c(rankobj_id, stats_data$rank[i], stats_data$stat[i])
    })
  )
  # dbExecute(con, "COMMIT")
}
insert_stats_bulk <- function(con, rankobj_id=NULL, rankname=NULL, stats_data) {
  # Start a transaction
  dbExecute(con, "BEGIN TRANSACTION")

  if (is.null(rankobj_id)) {
      sql <- "SELECT * from RankObjects where name == ? LIMIT 1"
      res <- dbExecute(con, sql, params = rankname)
  }

  
  # Prepare SQL for inserting into the Stats table
  sql <- "INSERT INTO Stats (rankobj_id, rank, stat) VALUES (?, ?, ?)"
  
  # Loop over the data in batches
  for (i in 1:nrow(stats_data)) {
    dbExecute(con, sql, params = list(rankobj_id, stats_data$rank[i], stats_data$stat[i]))
    
    # Optional: Show progress for large datasets
    if (i %% 1000 == 0) cat("Inserted row", i, "\n")
  }
  
  # Commit the transaction
  dbExecute(con, "COMMIT")
  
  message("Bulk insert completed.")
}


get_stats <- function(con, rankobj_id) {
  dbGetQuery(con, "SELECT * FROM Stats WHERE rankobj_id = ?", params = list(rankobj_id))
}

insert_stats_prepared <- function(con, rankobj_id, stats_data) {
  # Start a transaction
  dbExecute(con, "BEGIN TRANSACTION")
  
  # Prepare the statement once
  stmt <- dbSendStatement(con, "INSERT INTO Stats (rankobj_id, rank, stat) VALUES (?, ?, ?)")
  
  # Bind and execute for each row
  for (i in 1:nrow(stats_data)) {
    dbBind(stmt, list(rankobj_id, stats_data$rank[i], stats_data$stat[i]))
    
    # Optional: Show progress for large datasets
    if (i %% 1000 == 0) cat("Inserted row", i, "\n")
  }
  
  # Clear the statement and commit the transaction
  dbClearResult(stmt)
  dbExecute(con, "COMMIT")
  
  message("Bulk insert with prepared statements completed.")
}

# # Example usage
# con <- dbConnect(RSQLite::SQLite(), "rankorder_data.db")
#
# # Example data
# stats_data <- data.frame(rank = 1:4383, stat = rnorm(4383))  # Large data
#
# # Insert stats using prepared statements
# insert_stats_prepared(con, 1, stats_data)
#
# dbDisconnect(con)
#
