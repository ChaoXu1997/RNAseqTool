#' DEseq2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DEseq2_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      12
    )
  )


}

#' DEseq2 Server Functions
#'
#' @noRd
mod_DEseq2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_DEseq2_ui("DEseq2_1")

## To be copied in the server
# mod_DEseq2_server("DEseq2_1")
