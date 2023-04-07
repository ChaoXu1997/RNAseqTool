#' geneTrend
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import Mfuzz
#' @importFrom e1071 cmeans


# library(cluster)
# install.packages("factoextra")
# library(factoextra)
# Total withim sum of square
# p <- fviz_nbclust(expr_mean, kmeans, method = "wss", k.max = 12) +
#   geom_vline(xintercept = 4, linetype = 2)
# p

# Gap Statistic
# gap_clust <- clusGap(expr_mean, kmeans, 20, B = 1, verbose = interactive())
# gap_clust
# p <- fviz_gap_stat(gap_clust)
# p

# Mfuzz

# groupInfo <- openxlsx::read.xlsx("./data/Mfuzz_groupleIfo.xlsx")
# expr_raw <- openxlsx::read.xlsx("./data/Mfuzz_expr.xlsx",rowNames = T) %>%  #head() %>%
#   t() %>% scale() %>%  as.data.frame()
# expr <- expr_raw[match(groupInfo$Sample,rownames(expr_raw)),]
# expr_mean <- aggregate(expr,by = list(groupInfo$Group),mean,na.rm= T) %>%
#   column_to_rownames(var = "Group.1") %>%
#   t() %>% as.data.frame()
mfuzz_ana <- function(
  data,
  groupInfo,
  c_value =4,
  filterNA = 0.25,
  fillNA = "mean",
  filterSD = 0.3

  ){
  # clean expr
  library(e1071)
  expr_raw <- data %>%  tibble::column_to_rownames("Gene") %>% #head() %>%
    t() %>% scale() %>%  as.data.frame()
  expr <- expr_raw[match(groupInfo$Sample,rownames(expr_raw)),]
  expr_mean <- aggregate(expr,by = list(groupInfo$Group),mean,na.rm= T) %>%
    column_to_rownames(var = "Group.1") %>%
    t()
  # Mfuzz
  eset <- new("ExpressionSet", exprs = expr_mean)
  eset <- Mfuzz::filter.NA(eset, thres = filterNA)
  eset <- Mfuzz::fill.NA(eset, mode = fillNA)  #c('mean','median','knn','knn')
  eset <- Mfuzz::filter.std(eset, min.std = filterSD,visu=F)
  m_value <- Mfuzz::mestimate(eset)
  cl <- Mfuzz::mfuzz(eset, c = c_value, m = m_value)
  return(list(
    eset = eset,
    cl = cl
  ))

}
# groupInfo <- openxlsx::read.xlsx("./data/Mfuzz_groupleIfo.xlsx")
#data <- openxlsx::read.xlsx("./data/Mfuzz_expr.xlsx",rowNames = F)
library(Mfuzz)
#library(tidyverse)
#mfuzz <- mfuzz_ana(data,groupInfo ,5)
# acore.list <- acore(mfuzz$eset,mfuzz$cl) %>% `names<-`(paste0("Cluster ",seq_along(.)))
# length(acore.list)
# names(acore.list)
# view(acore.list[[1]])


# all plot
mfuzz_all_plot <- function(
    eset,
    cl,
    levels,
    colors=c("#76BA99", "#EB4747", "#996699","#f1c40f","#ea8685","#775039","#b83570","#e4b8d5","#9ebc19"),
    pointSize = 3,
    lineWidth = .8,
    baseSize = 14
  ){
  # centers_melt <- as.data.frame(cl$centers) %>%
  #   dplyr::mutate(Clusters= paste0("Cluster ",rownames(.))) %>%
  #   reshape2::melt()
  centers <- as.data.frame(cl$centers)
  centers$Clusters <- paste0("Cluster ",rownames(centers))
  centers_melt <- reshape2::melt(centers)
  p <- ggplot(centers_melt,aes(
    factor(variable,levels = levels)
    ,value,color=Clusters,group=Clusters)) +
    geom_point(size=pointSize) +
    geom_line(linewidth=lineWidth) +
    scale_color_manual(values = colors) +
    xlab("") + ylab("Expression (Z-score)") +
    theme_classic(base_size =baseSize)+
    theme(plot.title = element_text(size=20,hjust = 0.5))+
    theme(legend.position = "right") +
    theme(axis.ticks=element_line(linetype=1,color="black",size=1),
          axis.line=element_line(linetype=1,color="black",size=1),
          axis.text =element_text(size=13,color = "black") )
  return(p)
}
#mfuzz_all_plot(mfuzz[["eset"]],mfuzz[["cl"]],unique(groupInfo$Group))





# single plot
mfuzz_single_plot <- function(
    eset,
    cl,
    plotN = 1,
    levels,
    mm_colr_low = "#e6ee9c",
    mm_colr_high = "#dd2c00",
    mm_midpoint = .5,
    title_colr = "black",
    lineWidth = .5,
    center_lindWidth = 1,
    center_pointSize = 2,
    baseSize = 14

  ){
  # cluster genes
  genes <- names(cl$cluster)[cl$cluster == plotN]
  # mm
  mm_sub <- as.data.frame(cl$membership) %>% #head() %>%
    dplyr::select(all_of(plotN)) %>%
    tibble::rownames_to_column("gene") %>%
    dplyr::rename(mm = as.character( plotN)) %>%
    dplyr::filter(gene %in% genes)
  # expr
  expr_mean_sub <- exprs(eset) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("gene") %>%
    dplyr::filter(gene %in% genes)
  pdat <- merge(expr_mean_sub,mm_sub) %>%
    reshape2::melt(id.vars = c("mm","gene"))
  centers_melt_sub <- cl$centers %>%
    reshape2::melt() %>%
    dplyr::filter(Var1 == plotN )
  p <- ggplot() +
    geom_line(data = pdat,mapping=aes(
      factor(variable,levels = levels),
      value,color=mm,group=gene),size=.5) +
    scale_colour_gradient2(
      low =mm_colr_low,high = mm_colr_high,midpoint = mm_midpoint,
      breaks = c(seq(0,1,0.2)),limits = c(0,1)
    )+
    xlab("") + ylab("Expression (Z-score)") +
    geom_point(data = centers_melt_sub,mapping=aes(
      factor(Var2,levels = levels),
      value ),size=center_pointSize)+
    geom_line(data = centers_melt_sub,mapping=aes(
                                                  factor(Var2,levels = levels),
                                                  value,group=Var1),size=center_lindWidth) +
    labs(title = paste0("Cluster ",plotN)) +
    theme_classic(base_size = baseSize)+
    theme(
      legend.position = "right",
      axis.line=element_line(linetype=1,color="black",size=1),
      plot.title = element_text(size = 30, color = title_colr,hjust=0.5)
    )
  return(p)

}

#mfuzz_single_plot(eset,cl,1,unique(groupInfo$Group)[6:1])
