#' PCA_plot
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import tibble ggplot2 ggbiplot
pca_plot_func <- function(
    expr_tpm, group_list,
    title="Principal Component Analysis",
    colr=c("red","green","yellow","purple"),
    addEllipses = T,
    addLables=T ,
    dotSize = 2,
    xlim = c(-150,150),
    ylim = c(-150,150)
  ) {
  #library(ggbiplot)
  # sampleInfo <- data$sampleInfo() #%>%
  #   #dplyr::filter(Sample %in% picker)
  # expr_tpm <- data$expr() #%>%
  #   #dplyr::select(picker)
  expr_tpm <-  expr_tpm%>%
    dplyr::distinct(Gene,.keep_all = T) %>%
    tibble::column_to_rownames("Gene")

  t.dat <- as.data.frame(t(expr_tpm)) %>%
    dplyr::select(which(apply(., 2, var) != 0))

  data.pca <- prcomp(t.dat, scale. = TRUE)
  # groupPca<- data.frame(type=group_list_rna)
  # rownames(groupPca)<- rownames(t.dat)
  lable_text <- data.pca$x %>% as.data.frame() %>% rownames_to_column("sample")

  p <- ggbiplot::ggbiplot(data.pca,
                          obs.scale = 1, var.scale = 1,
                          groups = group_list,
                          ellipse = F, circle = F, var.axes = F
  ) +
    # scale_color_brewer(palette = "Set1")  +
    geom_point(aes(color = groups),size = dotSize) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.8, color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.8, color = "gray")
    #ggforce::geom_mark_ellipse(aes(fill = group_list, color = group_list), alpha = 0.1, show.legend = F) +
    # scale_color_manual(values = colr) +
    # scale_fill_manual(values = colr) +
    # theme(legend.direction = "horizontal", legend.position = "top") +
    # ylim(ylim[1], ylim[2]) + xlim(xlim[1], xlim[2]) +
    # ggtitle(title) +
    # theme_bw() + theme(panel.grid = element_line(colour = NA)) +
    # theme_bw(base_size = 14)
  if(addEllipses){
    p <-
      p +
      ggforce::geom_mark_ellipse(aes(fill = group_list, color = group_list), alpha = 0.1, show.legend = F)
  }
  p <- p +
    scale_color_manual(values = colr) +
    scale_fill_manual(values = colr) +
    theme(legend.direction = "horizontal", legend.position = "top") +
    ylim(ylim[1], ylim[2]) + xlim(xlim[1], xlim[2]) +
    ggtitle(title) +
    theme_bw() + theme(panel.grid = element_line(colour = NA)) +
    theme_bw(base_size = 14)
  if(addLables){
    p <-
      p +
      ggrepel::geom_text_repel(data=lable_text,aes(x=PC1,y=PC2,label=sample))
  }
  return(p)
}
# t(t.dat)
# library(tidyverse)
# expr_tpm <- openxlsx::read.xlsx("./data/expr_CPM.xlsx")
# sample <- openxlsx::read.xlsx("./data/sample_info.xlsx")
# p <- pca_plot_func(expr_tpm,sample$Group)
# dir.create("data/plot")
# saveRDS(p,file = "./data/plot/PCA.rds")
