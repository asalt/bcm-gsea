suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(here))

source(file.path(here("R"), "lazyloader.R"))

util_tools <- get_tool_env("utils")
plot_utils <- get_tool_env("plot_utils")
fgsea_tools <- get_tool_env("fgsea")
plot_tools <- get_tool_env("plot")

make_partial <- util_tools$make_partial
get_args <- util_tools$get_args
get_arg <- util_tools$get_arg
log_msg <- util_tools$make_partial(util_tools$log_msg)

prepare_data_for_bubble <- function(df) {
  if (!"pval" %in% colnames(df)) {
    df <- df %>% mutate(pval = padj)
  }

  plot_tools$prepare_data_for_barplot(df) %>%
    mutate(
      padj = ifelse(is.na(padj), 1, padj),
      pval = ifelse(is.na(pval), 1, pval),
      plot_leading_edge = pmax(leadingEdgeNum, 1),
      sig_pval = !is.na(pval) & pval < 0.05,
      outline_val = ifelse(sig_pval, "black", "transparent"),
      fill_value = sign(NES) * (1 - pmin(pval, 1)),
      sig_label = ifelse(sig_pval, "*", "")
    )
}

bubble_plot <- function(
    df,
    title = "",
    subtitle = NULL,
    save_func = NULL,
    facet_order = NULL,
    nes_range = NULL,
    size_range = c(3.0, 9.0),
    ...) {

  sel <- prepare_data_for_bubble(df)
  log_msg(msg = paste0("bubble_plot: received ", nrow(df), " rows, plotting ", nrow(sel), " after prep"))

  if (nrow(sel) == 0) {
    log_msg(warning = paste0("bubble_plot: no rows to plot for title '", title, "'"))
    return(NULL)
  }

  formatted_title <- title %>%
    str_replace_all("_", " ") %>%
    str_wrap(width = 42)

  formatted_subtitle <- subtitle %>%
    purrr::when(
      is.null(.) ~ NULL,
      TRUE ~ (.) %>% str_replace_all("_", " ") %>% str_wrap(width = 60)
    )

  custom_labeller <- function(value) {
    vapply(value, function(label) {
      label %>%
        str_replace_all("_", " ") %>%
        str_wrap(width = 36)
    }, character(1))
  }

  if (!is.null(facet_order) && "rankname" %in% colnames(sel)) {
    sel <- sel %>%
      mutate(rankname = factor(rankname, levels = facet_order, ordered = TRUE)) %>%
      arrange(rankname)
  }

  sel <- sel %>%
    mutate(
      plot_leading_edge = pmax(1, plot_leading_edge)
    )

  p <- ggplot(sel, aes(x = NES, y = pathway)) +
    geom_point(
      aes(
        size = plot_leading_edge,
        fill = fill_value,
        color = outline_val
      ),
      shape = 21,
      stroke = 1.0,
      alpha = 0.9
    ) +
    scale_fill_gradient2(
      low = "#084594",
      mid = "#ffffff",
      high = "#b30000",
      midpoint = 0,
      limits = c(-1, 1),
      na.value = "#f7f7f7",
      guide = guide_colourbar(title = "1 - pval")
    ) +
    scale_size(
      range = size_range,
      guide = guide_legend(title = "Leading edge genes")
    ) +
    scale_color_identity() +
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.12))) +
    geom_text(
      aes(label = sig_label),
      color = "black",
      size = 3.4,
      vjust = 0.45
    ) +
    labs(
      title = formatted_title,
      subtitle = formatted_subtitle,
      x = "NES",
      y = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 6.6, face = "bold"),
      axis.text.x = element_text(size = 7.0),
      plot.title = element_text(size = 10, face = "bold"),
      legend.position = "right"
    )

  if (is.null(nes_range)) {
    nes_max <- suppressWarnings(max(sel$NES, na.rm = TRUE))
    nes_min <- suppressWarnings(min(sel$NES, na.rm = TRUE))
    if (!is.finite(nes_max) || !is.finite(nes_min)) {
      nes_range <- NULL
    } else if (nes_min < 0 && nes_max > 0) {
      max_abs <- max(abs(nes_min), abs(nes_max))
      nes_range <- c(-max_abs, max_abs)
    } else {
      nes_range <- c(min(nes_min, 0, na.rm = TRUE), max(nes_max, 0, na.rm = TRUE))
    }
  } else {
    if (length(nes_range) != 2) {
      stop("nes_range should be a numeric vector of length 2")
    }
  }

  if (!is.null(nes_range) && all(is.finite(nes_range))) {
    p <- p + coord_cartesian(xlim = c(nes_range[1], nes_range[2]))
  }

  if ("rankname" %in% colnames(sel) && length(unique(sel$rankname)) > 1) {
    p <- p + facet_wrap(~rankname, labeller = as_labeller(custom_labeller))
    num_panels <- length(unique(sel$rankname))
    ncol <- ceiling(sqrt(num_panels))
    nrow <- ceiling(num_panels / ncol)
  } else {
    ncol <- 1
    nrow <- 1
  }

  panel_width_in <- 4.0
  panel_height_in <- 3.4
  total_width_in <- 2.2 + (panel_width_in * ncol)
  total_height_in <- panel_height_in * nrow + (length(unique(sel$pathway))^1.1 / 8)

  if (!is.null(save_func)) {
    save_result <- save_func(
      plot_code = function() {
        print(p)
      },
      width = total_width_in,
      height = total_height_in
    )
    if (is.null(save_result)) {
      log_msg(msg = paste0(
        "bubble_plot: skipped saving '", title, "' (existing file)"
      ))
    } else {
      log_msg(msg = paste0(
        "bubble_plot: saved plot '", title, "' with dimensions ",
        round(total_width_in, 2), "x", round(total_height_in, 2)
      ))
    }
  }

  p
}

all_bubble_plots <- function(
    results_list,
    save_func = NULL,
    facet_order = NULL,
    limit = 20,
    size_range = c(3.0, 9.0),
    ...) {
  if (!is.null(save_func)) {
    existing_filename <- get_arg(save_func, "filename")
    if (!nzchar(existing_filename)) {
      base_filename <- util_tools$safe_filename("bubble", fallback = "bubble")
      save_func <- make_partial(save_func, filename = base_filename)
    }
  }

  results_list %>% purrr::imap(
    ~ {
      collection_name <- .y
      list_of_comparisons <- .x
      list_of_comparisons %>% purrr::imap(
        ~ {
          dataframe <- .x
          comparison_name <- .y

          purrr::map(limit, function(.limit) {
           sel <- fgsea_tools$select_topn(dataframe, limit = .limit, pstat_cutoff = 1)
            log_msg(msg = paste0(
              "bubble all: collection=", collection_name,
              " comparison=", comparison_name,
              " limit=", .limit,
              " selected_rows=", nrow(sel)
            ))
            nes_max <- suppressWarnings(max(abs(dataframe$NES), na.rm = TRUE))
            nes_range <- if (is.finite(nes_max)) c(-nes_max, nes_max) else NULL

            local_save_func <- save_func
            if (!is.null(local_save_func)) {
              collection_dir <- util_tools$safe_path_component(collection_name)
              comparison_dir <- util_tools$safe_path_component(comparison_name)
              filename <- util_tools$safe_filename(
                "bubble",
                collection_dir,
                comparison_dir,
                paste0("n", nrow(sel)),
                fallback = "bubble"
              )
              path <- util_tools$safe_subdir(get_arg(local_save_func, "path"), collection_dir, "bubbles")
              local_save_func <- make_partial(local_save_func, filename = filename, path = path)
              log_msg(msg = paste0(
                "bubble all: saving to ", file.path(path, paste0(filename, ".pdf"))
              ))
            }

            rank_label <- sel$rankname %>% na.omit() %>% unique()
            rank_label <- if (length(rank_label) == 0) {
              NULL
            } else {
              rank_label %>%
                str_replace_all("_", " ") %>%
                paste(collapse = ", ")
            }
            subtitle_text <- if (!is.null(rank_label)) {
              paste0("rank: ", rank_label, " • top ", .limit)
            } else {
              paste0("top ", .limit, " pathways")
            }

            bubble_plot(
              sel,
              title = comparison_name,
              subtitle = subtitle_text,
              save_func = local_save_func,
              facet_order = facet_order,
              nes_range = nes_range,
              size_range = size_range,
              ...
            )
          })
        }
      )
    }
  )
}

do_combined_bubble_plots <- function(
    results_list,
    save_func = NULL,
    facet_order = NULL,
    limit = 20,
    size_range = c(3.0, 9.0),
    ...) {
  genesets <- names(results_list)

  purrr::map(genesets, function(geneset_name) {
    fgsea_res_list <- results_list[[geneset_name]]

    purrr::map(limit, function(.limit) {
     res <- fgsea_res_list %>% bind_rows(.id = "rankname")
      res <- fgsea_tools$select_topn(res, limit = .limit, pstat_cutoff = 1)
      n_sel <- res %>% distinct(pathway) %>% nrow()
      log_msg(msg = paste0(
        "bubble combined: geneset=", geneset_name,
        " limit=", .limit,
        " selected_rows=", nrow(res),
        " distinct_pathways=", n_sel
      ))
      nes_max <- suppressWarnings(max(abs(res$NES), na.rm = TRUE))
      nes_range <- if (is.finite(nes_max)) c(-nes_max, nes_max) else NULL

      local_save_func <- save_func
      if (!is.null(local_save_func)) {
        geneset_dir <- util_tools$safe_path_component(geneset_name)
        filename <- util_tools$safe_filename(
          "bubble",
          geneset_dir,
          paste0("n", n_sel),
          "all",
          fallback = "bubble_all"
        )
        path <- util_tools$safe_subdir(get_arg(local_save_func, "path"), geneset_dir, "bubbles")
        local_save_func <- make_partial(local_save_func, filename = filename, path = path)
        log_msg(msg = paste0(
          "bubble combined: saving to ", file.path(path, paste0(filename, ".pdf"))
        ))
      }

      bubble_plot(
        res,
        title = geneset_name,
        subtitle = paste0("top ", .limit, " pathways"),
        save_func = local_save_func,
        facet_order = facet_order,
        nes_range = nes_range,
        size_range = size_range,
        ...
      )
    })
  })
}
