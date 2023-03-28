#' DESeq2
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import DESeq2


func_DEseq2 <- function(expr,sampleInfo,fc=2,fdr=0.05,contrast_pair){

  group_list <- sampleInfo[sampleInfo$Group %in% contrast_pair ,]
  expr_done <- as.data.frame(expr) %>% tibble::column_to_rownames("Gene") %>% .[,colnames(.) %in% group_list$Sample]
  colData <- data.frame(row.names = colnames(expr_done),group_list = group_list$Group)
  print(colData)
  dds <- DESeqDataSetFromMatrix(countData = expr_done,colData = colData,design = ~ group_list)
  dds2 <- DESeq(dds)
  tmp <- results(dds2, contrast = c("group_list",contrast_pair[2],contrast_pair[1]))
  deg <- as.data.frame(tmp[order(tmp$padj),]) %>% tibble::rownames_to_column("Gene")
  #head(DEG_DESeq2)
  deg <- na.omit(deg)
  loc_up <- intersect(which(deg$log2FoldChange>log2(fc)),which(deg$padj<fdr))
  loc_down <- intersect(which(deg$log2FoldChange<(-log2(fc))),which(deg$padj<fdr))
  deg$change <- "Not"
  deg$change[loc_up] <- "Up"
  deg$change[loc_down] <- "Down"
  return(deg)

}
