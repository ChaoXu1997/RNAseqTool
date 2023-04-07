#' page_volcano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_volcano_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        7,
        align = "center",
        box(
          id = ns("box1"),
          width = 12,
          title ="Plot",
          plotOutput(
            ns("plot"),
            width = 800,
            height = 500
          )
        ) # B1
      ),
      column(
        5,
        box(
          id = ns("box2"),
          width = 12,
          title = "Parameter Setting",
          fluidRow(
            column(
              12,
              align = "center",
              actionBttn(
                ns("plot"),
                label = "Render Polt",
                size = "md"
              ),
              mod_download_ui(ns("download_1"))
            )
          ),
          spsHr(),
          fluidRow(
            column(
              6,
              fileInput(
                ns("file_DEseq2"),
                "Upload your DEseq2_result.xlsx",
                width = "350px"
              )


            ),
            column(
              4,
              tags$br(),
              actionBttn(
                ns("act_expl_DEseq2"),
                label = "Example",
                size = "sm"
              ),
              tags$br(),
              downloadLink(
                ns("dnld_expl_DEseq2"),
                "Example.xlsx"
              )
            )
          ),
          fluidRow(
            column(
              8,
              uiOutput(ns("ui_sheetName"))
            ),
            column(
              4,
              tags$br(),
              actionBttn(
                ns("chooseSheet"),
                label = "Choose sheet",
                size = "sm"
              )
            )
          ),
          fluidRow(
            column(
              4,
              mod_colorPickr_ui(ns("colorPickr_1"),2,"Up")
            ),
            column(
              4,
              mod_colorPickr_ui(ns("colorPickr_2"),10,"Not")
            ),
            column(
              4,
              mod_colorPickr_ui(ns("colorPickr_3"),1,"down")
            )
          ),
          fluidRow(
            column(
              12,
              textInput(
                ns("title"),
                label = "Change the title:",
                placeholder = "Volcano Plot"

              )
            )
          ),
          fluidRow(
            column(
              4,
              numericInput(
                ns("xbr"),
                label = "x-Breaks:",
                value = 5,
                step = 1,
                max = 20,
                min = 1
              )
            ),
            column(
              6,
              numericRangeInput(
                ns("xlim"),
                label = "X-axis range:",
                value =c(-10,10),
                step = 1
              )
            )
          )
        )# B2
      )
    ),
    fluidRow(
      column(
        7,
        box(
          id = ns("box3"),
          width = 12,
          title = "Data",
          tags$h3(
            textOutput(ns("text_sheetnames"))
          ),
          rhandsontable::rHandsontableOutput(ns("hot_DEGs")),
          textOutput(ns("tt"))
        )# B3
      )
    )
  )
}

#' page_volcano Server Functions
#'
#' @noRd
mod_page_volcano_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data <- reactiveValues(
      DEGs=NULL,
      sheetnames=NULL,
      example=NULL
    )


    # DEGs data
    #req(reactive({input$file_DEseq2}))
    observeEvent(input$file_DEseq2,{
      data$sheetnames <- reactive({openxlsx::getSheetNames(input$file_DEseq2$datapath)})
      data$example <- F
    })



    observeEvent(input$act_expl_DEseq2,{
      #data$sheetnames <- reactive({openxlsx::getSheetNames("data/DEseq2-results.xlsx")})
      data$sheetnames <- reactive({RNAseqTool::sheetnames})
      data$example <- T

    })

    ## choose sheet
    observeEvent(input$chooseSheet,{
      data$DEGs <- if(data$example){
        #reactive({openxlsx::read.xlsx("./data/DEseq2-results.xlsx",sheet = isolate(input$sheetname),rowNames = T)})
        reactive({RNAseqTool::DEseq2_res})
      }else{
        req(input$file_DEseq2)
        reactive({openxlsx::read.xlsx(input$file_DEseq2$datapath,sheet = isolate(input$sheetname),rowNames = T)})
      }

      output$hot_DEGs <-
        rhandsontable::renderRHandsontable({

          if(!is.null(data$DEGs())){
            rhandsontable::rhandsontable(data$DEGs(),readOnly = F,width = "auto",height = 400)%>%
              rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
          }

        })

    })
      # sheet picker ui
      output$ui_sheetName <- renderUI({
        if(!is.null(data$sheetnames)){
          tagList(
            pickerInput(
              inputId = ns("sheetname"),
              label = "SheetNames",
              choices = data$sheetnames(),
              selected = NULL
            )
          )
        }
      })

      up <- mod_colorPickr_server("colorPickr_1")
      down <- mod_colorPickr_server("colorPickr_3")
      not <- mod_colorPickr_server("colorPickr_2")
      xlim <- reactive({input$xlim})
      xbr <- reactive({input$xbr})
      # plot
      # p <- reactive({
      #   volcano_plot_func(
      #     DEGs=data$DEGs(),
      #     colr_up = up(),
      #     colr_down = down(),
      #     colr_not = not(),
      #     xlim = xlim(),
      #     xbr = xbr()
      #
      #   )
      #
      # })



      p <- eventReactive(input$plot,{
        volcano_plot_func(
          DEGs=isolate(data$DEGs()),
          colr_up = isolate(up()),
          colr_down = isolate(down()),
          colr_not = isolate(not()),
          xlim = isolate(xlim()),
          xbr = isolate(xbr()),
          title = isolate(input$title)

        )
      })

      output$plot <-renderPlot({
        #shinipsum::random_ggplot()
        p()
      })





    # output$hot_DEGs <-
    #   rhandsontable::renderRHandsontable({
    #
    #     if(!is.null(data$DEGs())){
    #       rhandsontable::rhandsontable(data$DEGs(),readOnly = F,width = "auto",height = 400)%>%
    #         rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
    #     }
    #
    #   })

    output$text_sheetnames <- renderText({
      input$sheetname
    })

    # download plot
    mod_download_server("download_1",p,"Volcano","pdf",width = 8,height = 6)


    # download example
    output$dnld_expl_DEseq2 <- downloadHandler(
      filename = function() {
        paste("DEseq2-results_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/DEseq2-results.xlsx",file)
      }
    )

  })
}

## To be copied in the UI
# mod_page_volcano_ui("page_volcano_1")

## To be copied in the server
# mod_page_volcano_server("page_volcano_1")
