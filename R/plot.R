suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ComplexHeatmap))




make_heatmap <- function(.gct, row_note = "", scale = T) {
  # .gct <- subgct
  # .gct@cdesc$treat <-
  #   factor(.gct@cdesc$treat , levels = c("untreated", "carboplatin", "IMT", "carboplatin_IMT"), ordered = T)

  ca <- ComplexHeatmap::columnAnnotation(
    group = .gct@cdesc$group,
    col = list(
      group = c(
        `168EC` = "blue",
        `Ox_hTERT` = "red",
        `No_Ox` = "yellow"
      )
    )
  )


  .legend_width <- unit(2.5, "cm")
  .cbar_title <- "zscore"
  heatmap_legend_param <- list(
    title = .cbar_title,
    direction = "vertical",
    just = "bottom",
    legend_width = .legend_width
  )


  # .note <- paste0(description, '\nNES ', sprintf("%.2f", NES), '  pvalue: ', pval)


  ht <- ComplexHeatmap::Heatmap(
    .gct@mat %>% apply(1, function(x) scale(x, center = T, scale = scale)) %>% t(),
    row_labels = .gct@rdesc$rdesc,
    column_labels = .gct@cdesc$id,
    column_split = .gct@cdesc$treat,
    top_annotation = ca,
    heatmap_legend_param = heatmap_legend_param,
    row_names_gp = grid::gpar(fontsize = 7),
    column_names_gp = grid::gpar(fontsize = 7),
    cluster_column_slices = FALSE,
    column_names_side = "top",
  )

  ht
}


custom_labeller <- function(value) {
  wrapped_labels <- sapply(value, function(label) {
    label %>%
      str_replace_all("_", " ") %>%
      str_wrap(width = 30)
  })
  return(wrapped_labels)
}



prepare_data_for_barplot <- function(df) {
  df_renamed <- df %>%
    mutate(pathway = str_remove(pathway, "HALLMARK_")) %>%
    mutate(pathway = str_remove(pathway, "KEGG_")) %>%
    mutate(pathway = str_remove(pathway, "GOMF_")) %>%
    mutate(pathway = str_remove(pathway, "REACTOME_"))
  df <- df_renamed

  sel <- df %>%
    arrange(-abs(NES)) %>%
    arrange(-NES) %>%
    mutate(pathway = str_replace_all(pathway, "_", " ") %>% str_wrap(width = 40)) %>%
    mutate(pathway = factor(pathway, levels = unique(pathway), ordered = T)) %>%
    arrange(pathway) # %>%
  sel %<>% mutate(leadingEdgeNum = str_count(leadingEdge, ",") + 1)
  sel %<>% mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size))
  sel %<>% mutate(outline_val = if_else(padj < .05, "black", NA))
  sel
}



#' @param df dataframe with columns pathway, NES, padj, leadingEdge, size
#' @param title title of the plot
barplot_with_numbers <- function(df, title = "") {
  sel <- df %>%
    arrange(-abs(NES)) %>%
    arrange(-NES) %>%
    mutate(pathway = str_replace_all(pathway, "_", " ") %>% str_wrap(width = 40)) %>%
    mutate(pathway = factor(pathway, levels = unique(pathway), ordered = T)) %>%
    arrange(pathway) # %>%
  sel %<>% mutate(leadingEdgeNum = str_count(leadingEdge, ",") + 1)
  sel %<>% mutate(leadingEdgeFrac = paste0(leadingEdgeNum, "/", size))
  sel %<>% mutate(outline_val = if_else(padj < .05, "black", NA))

  p <- sel %>%
    ggplot2::ggplot(
      aes(
        y = pathway,
        x = NES,
        # size = leadingEdgeNum,
        fill = padj,
        # fill = rankname
        # color = outline_val,
        # col = id
      )
    ) +
    # scale_color_gradient(low = "#0000ffee", high = "#ff0000ee") +  # Adjust colors to represent p-values
    # geom_point() +
    scale_fill_gradient2(high = "grey", mid = "#ba2020", low = "#c92020", midpoint = .25) +
    geom_col(aes(color = sel$outline_val)) +
    scale_color_identity() +
    labs(title = title) +
    # scale_color_manual(values=c("black", 'blue'))+
    # scale_size(range = c(4, 12)) +  # Adjust point sizes
    geom_text(
      aes(
        label = leadingEdgeFrac,
        x = sign(NES) * .4,
      ),
      color = "white",
      fontface = "bold",
      size = 2.4,
      # position = position_dodge(width = 0.8),
      vjust = 0.5, hjust = 0.5
    ) +
    theme_bw()
  if ("rankname" %in% colnames(df) && (length(unique(df$rankname)) > 1)) {
    p <- p + facet_wrap(~rankname, labeller = as_labeller(custom_labeller))
  }
  p
}







edgeplot1 <- function(...) {
  rankorder_edge %>%
    filter(!is.na(stat_stat)) %>%
    dim()
  ggplot(aes(x = rank, y = ES)) +
    geom_point()

  posES <- enplot_data$posES
  negES <- enplot_data$negES
  p <- rankorder_edge %>%
    ggplot(aes(x = stat_tick, y = ES, col = rank)) +
    geom_point() +
    scale_color_continuous(type = "viridis", option = "H") +
    geom_hline(yintercept = posES, colour = "red", linetype = "dashed") +
    geom_hline(yintercept = negES, colour = "red", linetype = "dashed")

  p
}
