library(magrittr)
library(purrr)
library(fs)
library(readr)
library(stringr)




create_rnkfiles <- function(volcanodir){
  if (is.null(volcanodir)){
    stop("volcanodir not defined")
  }

  if (!fs::dir_exists(volcanodir)){
    stop("volcanodir does not exist")
  }

  ( volcanofiles <- fs::dir_ls(path=volcanodir, regexp = ".*tsv") )
  lst <- volcanofiles %>%
    purrr::set_names( nm = ~basename(.) %>% fs::path_ext_remove() ) %>%   # set names first
    purrr::map(~read_tsv(.x, show_col_types = F) )
  shorternames <- names(lst) %>% stringr::str_extract(., pattern = "(?<=group_)(.*)(?=_di*)")
  names(lst) <- shorternames
  lst

}


write_rnkfiles <- function(lst, dir = "rnkfiles" ){
  if (!fs::dir_exists(dir)) fs::dir_create(dir)
  ._ <- lst %>% purrr::imap( # .x is the value, .y is the name
    ~{
      .outname <- fs::path_join(
        c( dir, paste0(.y, ".rnk") )
      )
      if (!fs::file_exists(.outname)){
          .x %>% select(GeneID, signedlogP) %>% write_tsv(.outname, col_names = FALSE)
          print(paste0("Wrote ", .outname))
      }

  }
)
}

load_rnkfiles <- function(rnkfiles){

    data <- map(rnkfiles, ~ readr::read_tsv(.x,
                                            col_names = c("geneid", "value"),
                                            show_col_types = F
                ) %>%
                mutate(geneid = as.character(geneid),
                       signedlogp = as.numeric(signedlogp)
                ) %>%
                # arrange(signedlogp) %>% # do not change order of files here
                drop_na()
    )
    data

}


ranks_dfs_to_lists <- function(rnkdfs){
  ranks_list <- rnkdfs %>% purrr::map(
    ~ with(.x, setNames(signedlogp, geneid))
  )
  return(ranks_list)
}
