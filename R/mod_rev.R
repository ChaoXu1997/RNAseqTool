#' rev UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import spsComps
mod_rev_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
     column(
       2,
       actionBttn(
         ns("rev"),
         label = "Rev",
         size = "sm",
         color = "royal",
         style = "material-circle",
         icon = icon("arrow-right-arrow-left")
       ),

     ),
     column(
       10,
       tags$b(tags$h4(
         textOutput(ns("ct")),
         style = "text-align:center"
       ))
     )
    ),
    spsHr()

  )
}

#' rev Server Functions
#'
#' @noRd
mod_rev_server <- function(id,vecs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    vec <- reactiveVal({vecs})
    observeEvent(input$rev,{
      newVec <- rev(vec())
      vec(newVec)
    })
    output$ct <- renderText({
      #tags$p("text")

      paste0(vec()[2]," vs ",vec()[1])
    })

    return(vec)


  })
}

## To be copied in the UI
# mod_rev_ui("rev_1")

## To be copied in the server
# mod_rev_server("rev_1")
