if (!requireNamespace("assertthat", quietly = TRUE)) install.packages("assertthat")
if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
if (!requireNamespace("magrittr", quietly = TRUE)) install.packages("magrittr")
library(magrittr)

library(assertthat)
library(RSQLite)


# Specify the path to your SQLite database
database_path <- "../db.sqlite"

# Connect to the SQLite database
# If the database does not exist at the specified path, it will be created.
conn <- dbConnect(SQLite(), dbname = database_path)

# Now you can use conn to interact with the database

# Read the SQL commands from init_db.sql
sql_commands <- readLines("../sql/init_db.sql")

# Collapse the SQL commands into a single string (if necessary)
sql_commands <- paste(sql_commands, collapse = "\n")


# split on ';'.
commands_list <- strsplit(sql_commands, ";")[[1]]


executeMultipleSQL <- function(conn, commandsList) {
  # Split the commands by semicolon
  
  # Trim, filter out empty commands, and execute each command
  numExecuted <- commandsList %>%
    purrr::map_chr(~trimws(.x)) %>%              # Trim whitespace
    purrr::keep(~nchar(.x) > 0) %>%              # Keep non-empty commands
    purrr::map(~dbExecute(conn, .x)) %>%         # Execute each command
    length()                              # Count successful executions
  
  # Return the number of successfully executed SQL commands
  return(numExecuted)
}


num_executed <- purrr::map(commands_list, ~executeMultipleSQL(conn=conn, commandsList=.x))
print(paste("Executed", num_executed, "SQL commands."))




# checks


check_tables_exist <- function(conn) {
  # SQL command to get the list of tables in the SQLite database
  query <- "SELECT name FROM sqlite_master WHERE type='table'"
  
  # Execute the query and retrieve the table names
  result <- dbGetQuery(conn, query)
  
  # Extract the table names to a vector
  table_names <- result$name
  
  # Define a vector of expected table names
  expected_tables <- c("comparison_groups", "pathways", "gsea_parameters", "fgsea_results")
  
  # Check if all expected tables are in the retrieved table names list
  missing_tables <- setdiff(expected_tables, table_names)
  
  # Assert that no tables are missing (missing_tables should be empty)
  assert_that(length(missing_tables) == 0, 
              msg = paste("Missing tables:", paste(missing_tables, collapse = ", ")))
  
  # If the assertion passes, print a success message
  print("All expected tables exist in the database.")
}

# Call the function to check for table existence
check_tables_exist(conn)


