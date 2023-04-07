#' msigdbr
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @import msigdbr
#'



# gene test
# dat <- openxlsx::read.xlsx("./data/DEseq2-results.xlsx")
# gene <- dat$Gene[dat$change=="Up"]

# about the MSigDB Human collections
#db_collections <- msigdbr::msigdbr_collections()
#openxlsx::write.xlsx(db_collections,file = "./data/db_collections.xlsx")
# db_collections <- openxlsx::read.xlsx("./data/db_collections.xlsx") %>%
#   mutate(Database_num=paste0(Database," (",num_genesets,")"))
# db_collections <- openxlsx::read.xlsx("./data/db_collections.xlsx")

species <- as.data.frame(msigdbr::msigdbr_species())[,1]
#db_collections <- openxlsx::read.xlsx("./data/db_collections.xlsx")
# function
msigdbr_func <- function(species = "Homo sapiens", database= db_collections$Database_num[2]){#,species = "Homo sapiens",category = "C5",subcategory = c("BP","CC","MF")
  #database= c(db_collections$Database_num[14])
  purrr::map_dfr(as.list(database),function(x){
    pos <- match(x,db_collections$Database_num)
    gs_cat <- db_collections$gs_cat[pos]
    gs_subcat <- if(is.na(db_collections$gs_subcat[pos])){
      ""
    }else{
      db_collections$gs_subcat[pos]
    }
    Database <- db_collections$Database[pos]
    msigdbr::msigdbr(species = species, category = gs_cat, subcategory = gs_subcat) %>%
      dplyr::mutate(Database=Database)
  })%>%
    dplyr::select(gs_name,gene_symbol,entrez_gene,ensembl_gene,Database) %>%
    `colnames<-`(c("term","symbol","entrezid","ensembl","Database")) %>%
    dplyr::mutate(term= gsub("^BIOCARTA_|^KEGG_|^PID_|^REACTOME_|^WP_|^GOBP_|^GOCC_|^GOMF_|^HP|^HALLMARK_","",term)) %>%
    dplyr::mutate(term=stringr::str_to_lower(term)) %>%
    dplyr::mutate(term=gsub("_"," ",term))
}
# rm(db)
# db <- msigdbr_func(database= c(db_collections$Database_num[15:17]))
# View(db)

# tmp <- msigdbr_func(database= db_collections$Database_num[17])
# head(tmp)
# tail(tmp)
# gsub("^BIOCARTA_|^KEGG_|^PID_|^REACTOME_|^WP_|^GOBP_|^GOCC_|^GOMF_|^HP|^HALLMARK_","",as.data.frame(head(tmp))[,1])


