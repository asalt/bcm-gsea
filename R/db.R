suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(stringr))

util_tools <- new.env()
source(file.path(here("R"), "utils.R"), local = util_tools)
log_msg <- util_tools$make_partial(util_tools$log_msg)

# Function to initialize the database from the SQL file
get_con <- function(db_path = file.path(here("sql"), "rankorder_data.db")){
  con <- dbConnect(RSQLite::SQLite(), db_path)
  return(con)
}

close_con <- function(con){
  dbDisconnect(con)
}

initialize_db <- function(db_path = file.path(here("sql"), "rankorder_data.db"), sql_file = file.path(here("sql"), "init_db.sql")) {
  log_msg(info="Initializing database...")
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
        log_msg(debug=statement)
      dbExecute(con, statement)
    }
  }
  # dbExecute(con, "COMMIT")
  dbDisconnect(con)
  log_msg(info="Database initialized successfully.")
}


# returns collection_id
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

insert_pathway <- function(con, collection_id = NULL, collection_name = NULL, pathway_name = NULL, members = NULL, id_type = "entrez"){
    # dbExecute(con, "INSERT INTO Collection (name) VALUES (?)", params = collection_name)

    if (is.null(pathway_name)) pathway_name <- "empty"
    if (is.null(members)) members <- ""

    maybe_pathway_id <- get_pathway_id(con, pathway_name)

    if (!is.null(maybe_pathway_id)) {
        warning(paste0("pathway ", pathway_name, " already present in db"))
        return()
    }

    if (is.null(collection_id) && !is.null(collection_name)){
      res <- dbGetQuery(con, 'select collection_id from Collections where name = ?', params = c(collection_name))
      if (nrow(res) == 0){ # then create
          collection_id <- insert_collection(con, connection_name)
      } else{
          collection_id <- insert_collection(con, 'empty')
      }
    }

    dbExecute(con, "INSERT INTO Pathways (name, ids, id_type, collection_id) VALUES (?, ?, ?, ?)",
     params = c(pathway_name, str_c(members, collapse='/'), id_type, collection_id)
    )

    pathway_id <- get_pathway_id(con, pathway_name)
    return(pathway_id)

}


insert_results <- function(con, rank_id = NULL, rank_name = NULL, collection_id = NULL, collection_name = NULL, results = NULL){

  if (is.null(rankobj_id)) {
      if (is.null(rank_name)) stop("both rank_id and rank_name cannot be null")
      sql <- "SELECT * from RankObjects where name == ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = rank_name)
      if (nrow(res) == 0){
        warning("no rank name found, creating")
        rank_id <- insert_rankobj(con, rank_name)
      } else {
        rank_id <- res[1,]
      }
  }

  if (is.null(collection_id)) {
      if (is.null(collection_name)) stop("both collection_id and collection_name cannot be null")
      sql <- "SELECT * from Collections where name == ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = collection_name)
      if (nrow(res) == 0){
        warning("no collection name found, creating")
        collection_id <- insert_collection(con, collection_name)
      } else {
        collection_id <- res[1,]
      }
  }

  on.exit({
    dbRollback(con)
    message("Transaction rolled back due to an error.")
  }, add = TRUE)

  dbBegin(con)
  stmt <- dbSendStatement(con, "INSERT INTO CollectionResults (rankobj_id, collection_id, pathway_id, pval, padj, log2err, es, nes, size, leadingEdge, mainpathway) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
  for (i in 1:nrow(results)) {
    pathway_id <- get_pathway_id(con, results$pathway[i])
    if (is.null(pathway_id)) {
      pathway_id <- insert_pathway(con, collection_id=collection_id, pathway_name=results$pathway[i])
    }
    dbBind(stmt,
     list(
      rankobj_id,
      collection_id,
      pathway_id,
      results$pval[i],
      results$padj[i],
      results$log2err[i],
      results$es[i],
      results$nes[i],
      results$size[i],
      results$leadingEdge[i],
      int(results$mainpathway[i]))
    )

    # Optional: Show progress for large datasets
    if (i %% 1000 == 0) cat("Inserted row", i, "\n")
  }

  # Clear the statement and commit the transaction
  dbClearResult(stmt)
  dbExecute(con, "COMMIT")
  message("Bulk insert with prepared statements completed.")
  on.exit(NULL, add = FALSE)

}

insert_rankobj <- function(con, rank_name){

    res <- dbGetQuery(con, 'select rankobj_id from RankObjects where name = ?', params = c('second'))
    if (nrow(res) > 0) {
        warning(paste0("rankname ", rank_name, " already exists, cannot insert"))
        return(res[1,])
    }
    dbExecute(con, "INSERT INTO RankObjects (name) VALUES (?)", params = rank_name)
    # dbExecute(con, "COMMIT")
    res <- dbGetQuery(con, 'select rankobj_id from RankObjects where name = ?', params = c('second'))
    return(res[1,])
}

insert_ranks <- function(con, rankobj_id = NULL, rankobj_name = NULL, ranks_data = NULL) {

  if (!"data.frame" %in% class(ranks_data)) {
    ranks_data <- data.frame(id = names(ranks_data), value=ranks_data, rank=rank(-ranks_data))
  }
  if (!"rank" %in% colnames(ranks_data)) {
    ranks_data[['rank']] <- rank(-ranks_data$value)
  }

  # Assuming ranks_data is a data frame with columns: rank, gene
  if (is.null(rankobj_id)) {
      sql <- "SELECT rankobj_id from RankObjects where name = ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = rankobj_name)
      if (nrow(res) == 0){
        warning("no rank name found, creating")
        rankobj_id <- insert_rankobj(con, rankobj_name)
      } else{
        rankobj_id <- res[1,]
      }
  }

  # ====

  on.exit({
    dbRollback(con)
    message("Transaction rolled back due to an error.")
  }, add = TRUE)

  dbBegin(con)
  # Prepare the statement once
  stmt <- dbSendStatement(con, "INSERT INTO Ranks (rankobj_id, id_type, bio_idstr, rank, value) VALUES (?, ?, ?, ?, ?)")
  # Bind and execute for each row
  for (i in 1:nrow(ranks_data)) {
    dbBind(stmt, list(rankobj_id, "entrez", ranks_data$id[i], ranks_data$rank[i], ranks_data$value[i]))
    # Optional: Show progress for large datasets
    if (i %% 1000 == 0) cat("Inserted row", i, "\n")
  }

  # Clear the statement and commit the transaction
  dbClearResult(stmt)
  dbExecute(con, "COMMIT")
  message("Bulk insert with prepared statements completed.")
  on.exit(NULL, add = FALSE)

}


insert_curve <- function(
  con,
  rankobj_id = NULL,
  rankobj_name = NULL,
  pathway_id = NULL,
  pathway_name = NULL,
  curve_data = NULL
){


  if (!"data.frame" %in% class(curve_data)) {
    stop("curve_data must be a data frame with columns: rank, stat")
  }
  if (!all(c("rank", "ES") %in% colnames(curve_data))) {
    stop("curve_data must have columns: rank, ES")
  }

  # get rankobj id if not provided but name is
  if (is.null(rankobj_id)) {
      sql <- "SELECT rankobj_id from RankObjects where name = ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = rankobj_name)
      if (nrow(res) == 0){
        warning("no rank name found, creating")
        log_msg(warning="no rank name found, creating")
        rankobj_id <- insert_rankobj(con, rankobj_name)
      } else{
        rankobj_id <- res[1,]
      }
  }

  # get collection id if not provided but name is
  if (is.null(pathway_id)) {
      sql <- "SELECT pathway_id from Pathways where name = ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = pathway_name)
      if (nrow(res) == 0){
        warning("no pathway id found, ")
        log_msg(warning="no pathway id found, ")
        return()
        # pathway_id <- insert_pathway(con, rankobj_name)
      } else{
        pathway_id <- res[1,]
      }
  }


  # ==

  on.exit({
    dbRollback(con)
    message("Transaction rolled back.")
  }, add = TRUE)

  dbBegin(con)
  stmt <- dbSendStatement(con, "INSERT INTO Curves (rankobj_id, pathway_id, rank, ES) VALUES (?, ?, ?, ?)")

  # Bind and execute for each row
  for (i in 1:nrow(ranks_data)) {
    dbBind(stmt, list(rankobj_id, pathway_id, curve_data$rank[i], curve_data$ES[i]))
    # Optional: Show progress for large datasets
    if (i %% 1000 == 0) cat("Inserted row", i, "\n")
  }

  # Clear the statement and commit the transaction
  dbClearResult(stmt)
  dbExecute(con, "COMMIT")
  message("Bulk insert with prepared statements completed.")
  on.exit(NULL, add = FALSE)

}

get_plot_enrichmentdata_by_pathway <- function(
  con,
  rankobj_id = NULL,
  rankobj_name = NULL,
  pathway_id = NULL,
  pathway_name = NULL
 ) {

  # get rankobj id if not provided but name is
  if (is.null(rankobj_id)) {
      sql <- "SELECT rankobj_id from RankObjects where name = ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = rankobj_name)
      if (nrow(res) == 0){
        warning("no rank name found, creating")
        rankobj_id <- insert_rankobj(con, rankobj_name)
      } else{
        rankobj_id <- res[1,]
      }
  }

  # get collection id if not provided but name is
  if (is.null(pathway_id)) {
      sql <- "SELECT pathway_id from Pathways where name = ? LIMIT 1"
      res <- dbGetQuery(con, sql, params = pathway_name)
      if (nrow(res) == 0){
        warning("no pathway id found, ")
        return()
        # pathway_id <- insert_pathway(con, rankobj_name)
      } else{
        pathway_id <- res[1,]
      }
  }


  rankdata <- dbGetQuery(con, "SELECT * FROM Ranks WHERE rankobj_id = ?", params = list(rankobj_id))
  rankdata %<>% rename(stat = value)
  rankdata %<>% arrange(rank)

  curvedata <- dbGetQuery(con, "SELECT * FROM Curves WHERE rankobj_id = ? & pathway_id = ?", params = list(rankobj_id, pathway_id))

  pathway <- dbGetQuery(con, "SELECT * FROM Pathways WHERE pathway_id = ?", params = list(pathway_id))
  members <- pathway$ids %>% strsplit("/") %>% unlist()

  ticks <- rankdata %>% filter(bio_idstr %in% members) %>% arrange(-stat)

  dbGetQuery(con, "SELECT * FROM Curves WHERE pathway_id = ?", params = list(pathway_id))


  maxAbsStat <- rankdata$stat %>% abs() %>% max()
  posES = max(curvedata$ES, na.rm = T)
  negES = min(curvedata$ES, na.rm = T)
  spreadES = posES - negES


  list(
    stats = rankdata %>% select(rank, stat),
    curve = curvedata %>% select(rank, ES),
    ticks = ticks %>% select(rank, stat),
    maxAbsStat = maxAbsStat,
    posES = posES,
    negES = negES,
    spreadES = spreadES
  )


}