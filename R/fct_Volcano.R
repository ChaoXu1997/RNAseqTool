#' Volcano
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom stringr str_glue
#'
#' @import ggplot2 tibble

volcano_plot_func <- function(DEGs,title ="Volcano Plot"
                              ,colr_up = "#FC4E2A"
                              ,colr_down = "#4393C3"
                              ,colr_not = "#00000033"
                              ,xlim = c(-10,10)
                              ,xbr = 5
                              ) {
  #print(head(DEGs))
  Down <-table(DEGs$change)[1]
  Up <- table(DEGs$change)[3]
  text <- stringr::str_glue("Down:{Down} Up:{Up} \n|log2FC|>=1, p-adj<=0.05")
  # treat <- str_split(name," vs ",simplify = T)[1]
  # mock <- str_split(name," vs ",simplify = T)[2]
  deg <- DEGs %>%
    #tibble::rownames_to_column("Name") %>%as.data.frame() %>%
    #dplyr::rename(change = Change) %>%
    as.data.frame() %>%
    dplyr::filter(-log10(padj) != Inf)
  break_y <- max(-log10(deg$padj)) %/% 6
  max_y <- break_y * 7
  p <-
    deg %>%
    ggplot() +
    geom_point(aes(
      x = log2FoldChange, y = -log10(padj),
      color = change, size = abs(log2FoldChange),alpha=.6
    )) +
    scale_color_manual(values = c(colr_down, colr_not, colr_up)) + # "#4393C3","#00000033","#FC4E2A"
    scale_size_continuous(limits = c(0, 5), #range = c(-0.5, 6),
                          guide = "none") +
    theme_bw() +
    theme(legend.position = "right") +
    guides(color = guide_legend(override.aes = list(size = 4)),alpha = "none") +
    ggtitle(title) +#"Volcano Plot of Transcriptome for {treat} vs {mock}"
    scale_x_continuous(limits = c(xlim[1], xlim[2]), breaks = seq(xlim[1], xlim[2], xbr)) +
    scale_y_continuous(limits = c(0, max_y), breaks = seq(0, max_y, break_y)) +
    annotate("text", x = -6, y = max_y, label = text, size = 5) +
    geom_hline(
      yintercept = -log10(0.05),
      size = 0.7,
      color = "grey",
      lty = "dashed"
    ) +
    geom_vline(
      xintercept = c(-log2(2), log2(2)),
      size = 0.7,
      color = "grey",
      lty = "dashed"
    ) +
    theme_classic(base_size = 16) +
    theme(plot.title = element_text(size = 15, hjust = 0.5)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  return(p)
}

# deg <- openxlsx::read.xlsx("./data/DEseq2-results.xlsx",rowNames = T)
# library(tidyverse)
#
# volcano_plot_func(deg)



