#' page_home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_home_ui <- function(id){
  ns <- NS(id)
  # carousel
  carouseDat <- openxlsx::read.xlsx("data/carousel.xlsx") %>%
    dplyr::arrange(order)

  fluidRow(
    column(
      5,
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        column(
          12,
          align = "center",
          tags$p(tags$i("RNAseqTool: A Shiny App for Interactive RNAseq Analysis and Visualization"),
                 style="color:#95a5a6;font-style:italic;text-indent:0px")%>%
            animateAppend("float",speed = "fast"),
          tags$img(
            src = "www/favicon.png",
            width = 103.7*2,
            height = 120*2
          )
        )
      ),
      fluidRow(
        column(
          9,
          offset = 2,
          tags$h5("RNAseqTool provides an interactive and user-friendly method for users to analyze RNAseq data,
                  which could be a great solution for RNAseq data analysis and visualization.
                  Here are some potential features that the Shiny App could include:"),
        )

      ),
      fluidRow(
        column(
          8,
          offset = 2,
          align = "left",
          blockQuote(
            tags$h6(
              tags$ul(
                tags$li(
                  "Interactive Visualization"
                ),
                tags$li(
                  "Principal Component Analysis (PCA)"
                ),
                tags$li(
                  "Differential Gene Expression Analysis"
                ),
                tags$li(
                  "Gene Annotation & Functional analysis"
                ),
                tags$li(
                  "Gene Trend Analysis"
                ),
                tags$li(
                  "WGCNA Analysis"
                ),
                tags$li(
                  "Data Export: PDF & XLSX"
                ),
                tags$li(
                  "Continuous Development & Improvement"
                )
              )
            )
            , color = "teal")
        )
      )
    ),
    column(
      7,
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      carousel(# carousel
        id = "mycarousel",
        width = 12,
        indicators = TRUE,
        .list = lapply(carouseDat$order,function(i){
          carouselItem(
            caption = carouseDat$caption[i],
            align = "center",
            tags$img(src = paste0("www/",carouseDat$png_path[i]),width="700",height="500")
          )
        })




      )#carousel
    )

  )
}

#' page_home Server Functions
#'
#' @noRd
mod_page_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_page_home_ui("page_home_1")

## To be copied in the server
# mod_page_home_server("page_home_1")
