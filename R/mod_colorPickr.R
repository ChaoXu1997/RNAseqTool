#' colorPickr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import paletteer
mod_colorPickr_ui <- function(id,pos,label){
  ns <- NS(id)
  colorPickr(
    inputId = ns("colr"),
    label =label,
    update = "save",
    interaction = list(
      clear = F,
      save = T
    ),
    useAsButton = T,
    width = "160px",
    pickr_width = "20%",
    selected = if(pos <= 10 ){
      as.character(paletteer::paletteer_d("yarrr::basel"))[pos]
    } else {
      as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10]
    },
      #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
    swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
  )
}

#' colorPickr Server Functions
#'
#' @noRd
mod_colorPickr_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    reactive({input$colr})
  })
}

## To be copied in the UI
# mod_colorPickr_ui("colorPickr_1")

## To be copied in the server
# mod_colorPickr_server("colorPickr_1")
