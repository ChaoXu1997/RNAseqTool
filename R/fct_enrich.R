#' enrich
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom clusterProfiler enricher
#'
#' @importFrom stringr str_split
#' @importFrom forcats fct_reorder2

# class enrich
enrich_func <- function(gene,db,gene_type,p=0.05,padj_method="BH"){
  e_res <-
  clusterProfiler::enricher(
    gene = gene,
    pvalueCutoff = 0.05,
    pAdjustMethod = padj_method,
    TERM2GENE = db %>% dplyr::select(term,all_of(gene_type)) %>% `colnames<-`(c("TERM","GENE")) %>% dplyr::mutate(across(everything(),as.character)) %>% as.data.frame(),
    TERM2NAME = db[,c(1,5)]
  )
  if(is.null(e_res)){
    return(NULL)
  }else{
    e_res@result %>%
      dplyr::rename(
        Description=ID,
        Database = Description
      ) %>%
      dplyr::mutate(EnrichFactor=
                      as.numeric(stringr::str_split(GeneRatio,"/",simplify = T)[,1])/as.numeric(stringr::str_split(BgRatio,"/",simplify = T)[,1])
      ) %>%
      dplyr::filter(pvalue <= p) %>%
      tibble::remove_rownames()
  }

}


# db <- msigdbr_func(database= c(db_collections$Database_num[17]))
# ?clusterProfiler::enricher()
# ego <-
#   clusterProfiler::enricher(
#     gene = db$entrezid[1:1000],
#     pvalueCutoff = 0.01,
#     pAdjustMethod = "BH",
#     TERM2GENE = db[,c(1,3)] %>%mutate(across(everything(),as.character)) %>% as.data.frame(),
#     TERM2NAME = db[,c(1,5)]
#   )@result
# View(ego)
# rm(ego)
# is.null(ego)


# library(magrittr)
# ego <- enrich_func(db$entrezid[1:1000],db,"entrezid")
# table(ego$Database)
# View(ego)

short_str <- function(chr) {
  # chr <- test$Description[4]
  vchr <- as.vector(stringr::str_split(chr, " ", simplify = T))
  ln <- length(as.vector(stringr::str_split(chr, " ", simplify = T)))
  if (ln > 7) {
    str <- paste0(vchr[1:7], collapse = " ")
    str <- paste0(str, "...")
  } else {
    str <- paste0(vchr, collapse = " ")
  }
  return(str)
}

enrich_plot_func <- function(
    eRes,showCategory = 10,
    line_colr= c("#7FFF00", "#FF82AB", "#63B8FF"),
    dot_colr=c("red","purple"),
    title="Enrichment Analysis Plot"  ,
    linewidth = 1.2
  ){
  #ego %>%
  eRes$Description <- sapply(eRes$Description,short_str)
  #eRes$Description <- forcats::fct_reorder2(eRes$Description,eRes$Count,eRes$EnrichFactor,.desc =F)
  p <-
  as.data.frame(eRes) %>%
    dplyr::group_by(Database) %>%
    dplyr::top_n(-showCategory,pvalue) %>%
    #dplyr::mutate(Description=sapply(Description,short_str)) %>%
    dplyr::mutate(Description = forcats::fct_reorder2(Description,Count,EnrichFactor,.desc =F)) %>%
    ggplot() +
    geom_segment(
      aes(x= 0,y= Description,xend=as.numeric(EnrichFactor), yend=Description,colour=Database),
      linewidth = linewidth
    )+
    scale_color_manual(values = line_colr) +
    ggnewscale:: new_scale_color()  +
    geom_point(aes(EnrichFactor,Description,color= pvalue,size = Count)) +
    scale_color_gradient(low = dot_colr[1], high = dot_colr[2], limits = c(0, 0.05), breaks = seq(0, 0.05, 0.01)) +
    scale_size(range= c(4,8)) +
    labs(x = "EnrichFactor") +
    ggtitle(title) +
    theme_bw(base_size = 14)
  return(p)

}
#enrich_plot_func(ego)
