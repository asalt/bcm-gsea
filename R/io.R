suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(cmapR))
suppressPackageStartupMessages(library(here))
# suppressPackageStartupMessages(library(janitor))

src_dir <- file.path(here("R"))
source(file.path(src_dir, "utils.R"))


util_tools <- new.env()
source(file.path(src_dir, "./utils.R"), local = util_tools)

log_msg <- util_tools$make_partial(util_tools$log_msg)

# ==

make_random_gct <- function(nrow = 10, ncol = 4) {
  set.seed(369)
  nrow <- max(nrow, 1)
  ncol <- max(ncol, 1)
  .mat <- matrix(runif(nrow * ncol), nrow = nrow, ncol = ncol)
  .rids <- seq(1, dim(.mat)[1]) %>% as.character()
  rownames(.mat) <- .rids

  .cids <- seq(1, dim(.mat)[2]) %>% as.character()
  .cids <- paste0("X", .cids)
  .cdesc <- data.frame(
    id = .cids,
    metavar1 = sample(letters[1:5], ncol, replace = T),
    metavar2 = sample(letters[1:5], ncol, replace = T)
  )
  .rdesc <- data.frame(
    id = rownames(.mat),
    rdesc = rownames(.mat)
  )
  gct <- cmapR::GCT(
    mat = .mat,
    rid = .rids,
    cid = .cids,
    cdesc = .cdesc,
    rdesc = .rdesc
  )
  #
  return(gct)
  #
}

create_rnkfiles_from_emat <- function(
    emat,
    apply_z_score = FALSE,
    ...) {
  gct <- cmapR::parse_gctx(emat)
  if (apply_z_score) {
    .new <- gct@mat %>%
      apply(MARGIN = 1, FUN = .GlobalEnv$myzscore) %>%
      t() %>%
      as.matrix()
    colnames(.new) <- colnames(mat(gct))
    gct@mat <- .new
  }
  # gct@

  # Initialize a list to hold each new matrix
  list_of_matrices <- list()

  # Loop through each column of the matrix
  for (i in 1:ncol(gct@mat)) {
    # Create a new matrix for each column with row names and the column of interest
    new_mat <- cbind(id = rownames(gct@mat), value = gct@mat[, i])

    # Convert the matrix to data frame for more intuitive row and column handling (optional)
    new_df <- as.data.frame(new_mat)

    # Store the matrix in the list
    list_of_matrices[[colnames(gct@mat)[i]]] <- new_df
  }

  # Output or return the list of matrices
  return(list_of_matrices)
}



create_rnkfiles_from_volcano <- function(
    volcanodir,
    id_col = "GeneID",
    value_col = "value") {
  if (is.null(volcanodir)) {
    stop("volcanodir not defined")
  }

  if (!fs::dir_exists(volcanodir)) {
    stop("volcanodir does not exist")
  }

  (volcanofiles <- fs::dir_ls(path = volcanodir, regexp = ".*tsv"))
  log_msg(msg = paste0("Found ", length(volcanofiles), " tsv files"))

  lst <- volcanofiles %>%
    purrr::set_names(nm = ~ basename(.) %>%
      fs::path_ext_remove()) %>% # set names first
    purrr::map(~ {
      .table <- read_tsv(.x, show_col_types = F)
      if (value_col %in% colnames(.table)) {
        .table <- .table %>% rename(value = !!value_col)
      }
      if (id_col %in% colnames(.table)) {
        .table <- .table %>% rename(id = !!id_col)
      }
      return(.table)
    })


  log_msg(msg = "trying to shorten names")

  shorternames <- names(lst) %>%
    stringr::str_extract(., pattern = "(?<=group_)(.*)(?=_*)")

  log_msg(msg = paste0("shorter names are ", paste0(shorternames, "\n")))
  if (!any(is.na(shorternames))) {
    names(lst) <- shorternames
  } else {
    log_msg(msg = "nas in shorter names, not reassigning")
  }

  lst
}


write_rnkfiles <- function(
    lst,
    dir = "rnkfiles") {
  if (!fs::dir_exists(dir)) fs::dir_create(dir)
  lst %>% purrr::iwalk( # .x is the value, .y is the name
    ~ {
      .outname <- fs::path_join(
        c(dir, paste0(.y, ".rnk"))
      )
      if (!fs::file_exists(.outname)) {
        .x %>%
          dplyr::select(id, value) %>%
          write_tsv(.outname, col_names = FALSE)
        print(paste0("Wrote ", .outname))
      }
    }
  )
}

load_rnkfiles <- function(rnkfiles) {
  data <- map(rnkfiles, ~ readr::read_tsv(.x,
    col_names = c("id", "value"),
    show_col_types = F
  ) %>%
    mutate(
      id = as.character(id),
      value = as.numeric(value)
    ) %>%
    # arrange(value) %>% # do not change order of files here
    drop_na())
  data
}


ranks_dfs_to_lists <- function(rnkdfs) {
  ranks_list <- rnkdfs %>% purrr::map(
    ~ with(.x, setNames(value, id))
  )
  return(ranks_list)
}

load_genesets_from_json <- function(json_str) {
  genesets_of_interest <- jsonlite::fromJSON(json_str)
  genesets_of_interest <- genesets_of_interest %>% dplyr::mutate(
    collection_name = stringr::str_c(category, subcategory, sep = "_")
  )
  return(genesets_of_interest)
}


save_gsea_results <- function(
    results_list,
    savedir = NULL) {
  if (is.null(savedir)) savedir <- "gsea_tables"
  if (!file.exists(savedir)) fs::dir_create(savedir)
  names(results_list) %>%
    purrr::map(
      ~ {
        collection_name <- .x
        names(results_list[[collection_name]]) %>%
          purrr::map(
            ~ {
              comparison_name <- .x
              result <- results_list[[collection_name]][[comparison_name]]
              print(collection_name)
              print(comparison_name)

              outf <- paste0(
                make.names(collection_name),
                "_",
                make.names(comparison_name),
                ".tsv"
              )
              outf <- file.path(savedir, outf)
              # one last check here
              result %<>% mutate(leadingEdge = sapply(leadingEdge, paste, collapse="/"))
              log_msg(msg=paste0("Writing: ", outf, "..."))
              result %>% readr::write_tsv(outf)
              log_msg(msg="done")
              # if (!fs::file_exists(outf)) result %>% readr::write_tsv(outf)
            }
          )
      }
    )
}
