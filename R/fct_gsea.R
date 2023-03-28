#' gsea
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom GseaVis gseaNb
#' @importFrom clusterProfiler GSEA


#?clusterProfiler::GSEA()
gesa_func <- function(
    gene,
    db,
    gene_type,
    pvalue = 0.05,
    pAdjustMethod = "BH"
    #TERM2GENE = db %>% dplyr::select(term,all_of(gene_type)) %>% `colnames<-`(c("TERM","GENE")) %>% dplyr::mutate(across(everything(),as.character)) %>% as.data.frame(),
    #TERM2NAME = db[,c(1,5)]
){
  e_res <- clusterProfiler::GSEA(
    geneList = gene,
    exponent = 1,
    minGSSize = 10,
    maxGSSize = 500,
    eps = 1e-10,
    pvalueCutoff = pvalue,
    pAdjustMethod = pAdjustMethod,
    gson = NULL,
    TERM2GENE = db %>% dplyr::select(term,all_of(gene_type)) %>% `colnames<-`(c("TERM","GENE")) %>% dplyr::mutate(dplyr::across(everything(),as.character)) %>% as.data.frame(),
    #TERM2NAME = TERM2NAME,
    verbose = F,
    seed = FALSE
  )
  return(e_res)
}

# gene <- openxlsx::read.xlsx("./data/gene_log2FC.xlsx")
# head(gene)
# geneList <- gene$log2FoldChange %>% `names<-`(gene$Gene)
# geneList <- geneList[order(geneList,decreasing = T)]
# egsea <- gesa_func(geneList,db,"symbol")
# e_res <- egsea@result %>% tibble::remove_rownames()


# gese plot
# gsea_plot_func <- function(
#   e_res,
#   geneSetID,
#   color = "green",
#   base_size = 12,
#   pvalue_table  = T,
#   ES_geom = "line"
# ){
#   enrichplot::gseaplot2(
#     e_res,
#     geneSetID = geneSetID,
#     color =color,
#     base_size = base_size,
#     pvalue_table  = pvalue_table,
#     ES_geom = ES_geom
#   )
# }

# library(GseaVis)
# mygene <-c("GSTA1",'LDHD','MGST1','PHGDH','PIR',"is")
# library(tidyverse)
gsea_plot_func <- function(
    object = egsea,
    addGene = NULL,
    geneSetID = c("oxidoreductase activity","carbohydrate binding"),
    addPval = T,
    pvalSize = 4,
    pvalX = 1,pvalY = 1,
    subPlot = 3,
    termWidth = 40,
    base_size = 12,
    arrowType = 'open',
    geneCol = 'red',
    markTopgene = F,
    topGeneN = 1,

    rankSeq = 2000,
    htCol =  c("#08519C", "#A50F15"),
    rankCol = c("#08519C", "white", "#A50F15"),
    curveCol = c("#76BA99", "#EB4747", "#996699","#f1c40f","#ea8685","#775039","#b83570","#e4b8d5","#9ebc19"),



    newGsea = F,
    pCol = 'black',
    ncol=1,
    newCurveCol = c("#336699", "white", "#993399"),
    newHtCol = c("#336699", "white", "#993399"),
    addPoin= T
  ){
  GseaVis::gseaNb(object = object,
         geneSetID = geneSetID,
         addPval = addPval,
         subPlot = subPlot,
         termWidth = termWidth,
         rankSeq = rankSeq,
         htCol =  htCol,
         rankCol = rankCol,
         curveCol = curveCol,


         addGene = addGene,
         arrowType =arrowType,
         geneCol = geneCol,
         markTopgene = markTopgene,
         topGeneN = topGeneN,

         pvalX = pvalX,pvalY = pvalY,
         pvalSize = pvalSize,
         base_size = base_size,
         newGsea = newGsea,
         newCurveCol = newCurveCol,
         newHtCol = newHtCol,
         addPoin= addPoin,
         pCol = pCol,
         pHjust = 1,
         ncol=ncol
  )
}
# gsea_plot_func(newGsea = F)



# head(e_res)
# ?gseaplot2
# enrichplot::gseaplot2(
#   egsea,
#   geneSetID = c("oxidoreductase activity","carbohydrate binding"),
#   #title =  c("oxidoreductase activity"),
#   color = c("red","green"),
#   base_size = 12,
#   pvalue_table  = T,
#   ES_geom = "line"
# )
#
# gsea_plot_func(egsea,"oxidoreductase activity")
