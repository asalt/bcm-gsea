library(here)
library(DBI)
library(RSQLite)
library(stringr)

# Function to initialize the database from the SQL file
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

insert_stats <- function(con, rankobj_id, stats_data) {
  # Assuming stats_data is a data frame with columns: rank, stat
  dbExecute(con, 
    "INSERT INTO Stats (rankobj_id, rank, stat) VALUES (?, ?, ?)", 
    params = lapply(1:nrow(stats_data), function(i) {
      c(rankobj_id, stats_data$rank[i], stats_data$stat[i])
    })
  )
}
insert_stats_bulk <- function(con, rankobj_id, stats_data) {
  # Start a transaction
  dbExecute(con, "BEGIN TRANSACTION")
  
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
