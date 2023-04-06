#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize=60*1024^2)
  options(bitmapType='cairo')

  # Your application server logic
  # output$colors <- colorPickr()

  # page home
  mod_page_home_server("page_home_1")

  # page PCA
  #mod_page_pca_server("page_pca_1")

  # page DEGs
  #mod_page_DEseq2_server("page_DEseq2_1")

  # page Volcano
  #mod_page_volcano_server("page_volcano_1")

  # page Enrich go
  #mod_page_enrich_go_server("page_enrich_go_1")

  # page gsea
  #mod_page_gsea_server("page_gsea_1")

  # page Mfuzz
  #mod_page_geneTrend_server("page_geneTrend_1")

  # page WGCNA
  #mod_page_wgcna_server("page_wgcna_1")
}
