#' page_pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinyWidgets colourpicker
#'
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash box
mod_page_pca_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      6,
      box(
        width = 12,
        id = "",
        headerBorder = T,
        title = "Plot",
        fluidRow(
          column(
            12,
            plotOutput(
              ns("plot"),
              width = "600px",
              height = "600px"
            )
          )
        )
      ),
      box(
        title = "Upload Data",
        width = 12,
        fluidRow(
          column(
            6,
            offset = 5,
            # actionBttn(
            #   ns("clear"),
            #   label = "Clear",
            #   size = "md"
            # ),
            actionBttn(
              ns("run"),
              label = "renderPlot",
              size = "md"
            ),
            mod_download_ui(ns("download_1"),"PDF")


          )
        ),
        spsHr(),
        fluidRow(
          column(
            12,
            fluidRow(
              column(
                6,
                fileInput(
                  ns("file_expr"),
                  "Upload your expr file",
                  width = "350px"
                )

              ),
              column(
                4,
                tags$br(),
                actionBttn(
                  ns("act_expl_expr"),
                  label = "Example",
                  size = "sm"
                ),
                tags$br(),
                downloadLink(
                  ns("dnld_expl_expr"),
                  "Example.xlsx"
                )
              )
            ),
            fluidRow(
              rhandsontable::rHandsontableOutput(ns("hot_expr"))
            ),
            fluidRow(
              column(
                6,
                fileInput(
                  ns("file_sampleInfo"),
                  "Upload your sampleInfo file",
                  width = "350px"
                )

              ),
              column(
                4,
                tags$br(),
                actionBttn(
                  ns("act_expl_sampleInfo"),
                  label = "Example",
                  size = "sm"
                ),
                tags$br(),
                downloadLink(
                  ns("dnld_expl_sampleInfo"),
                  "Example.xlsx"
                )
              )
            ),
            fluidRow(
              rhandsontable::rHandsontableOutput(ns("hot_sampleInfo"))
            )
          )
        )
      )

    ),
    column(
      6,
      box(
        width = 12,
        title = "Parameter Setting",
        fluidRow(
          column(
            12,
            textInput(
              ns("title"),
              label = "Change the title:",
              value = "Principal Component Analysis",
              placeholder = "Principal Component Analysis"

            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput(ns("picker"))
          )
        ),
        tags$hr(),
        fluidRow(
          column(
            12,
            offset = 0,
            tags$b(tags$g("You can change the colors here:")),
            uiOutput(ns("colrs")),
          )
        ),
        tags$hr(),
        fluidRow(
          column(
            4,
            tags$g(tags$b("Ellipse:")),
            switchInput(
              ns("switch_ellipse"),
              label = "Ellipse",
              value = T,
              onStatus = "success",
              offStatus = "danger",
              size= "normal"
            )
          ),
          column(
            4,
            tags$g(tags$b("Label:")),
            switchInput(
              ns("switch_label"),
              label = "Label",
              value = T,
              onStatus = "success",
              offStatus = "danger",
              size= "small"
            )
          ),
          column(
            4,
            numericInput(
              ns("num_dot"),
              label = "Dot size:",
              value = 2,
              step = 1,
              max = 20,
              min = 1
            )
          )
        ),
        tags$hr(),
        fluidRow(
          column(
            6,
            numericRangeInput(
              ns("xlim"),
              label = "X-axis range:",
              value =c(-150,150)
            )
          ),
          column(
            6,
            numericRangeInput(
              ns("ylim"),
              label = "Y-axis range:",
              value =c(-150,150)
            )
          )
        )#,
        #fluidRow(textOutput(ns("ttt")))

      )
    )
  )
}

#' page_pca Server Functions
#'
#' @noRd
mod_page_pca_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # data
    data <- reactiveValues(
      expr = reactive({NULL}),
      sampleInfo = reactive({NULL})
    )


    # expr data
    req(reactive({input$file_expr}))
    observeEvent(input$file_expr,{
      data$expr <- reactive({openxlsx::read.xlsx(input$file_expr$datapath,rowNames = F)})
    })
    observeEvent(input$act_expl_expr,{
      data$expr <- reactive({openxlsx::read.xlsx("./data/expr_CPM.xlsx",rowNames = F)})
    })

    output$hot_expr <-
    rhandsontable::renderRHandsontable({

      if(!is.null(data$expr())){
        rhandsontable::rhandsontable(data$expr(),readOnly = F,width = "auto",height = 400)%>%
          rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
      }

    })

    # sample info data
    observeEvent(input$file_sampleInfo,{
      req(reactive({input$file_sampleInfo}))
      data$sampleInfo <- reactive({openxlsx::read.xlsx(input$file_sampleInfo$datapath)})
    })
    observeEvent(input$act_expl_sampleInfo,{
      data$sampleInfo <- reactive({openxlsx::read.xlsx("./data/sample_info.xlsx")})
    })

    output$hot_sampleInfo <-
      rhandsontable::renderRHandsontable({
        if(!is.null(data$sampleInfo())){
          rhandsontable::rhandsontable(data$sampleInfo(),readOnly = F,width = "auto",height = 400)%>%
            rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
        }

      })









    observe({
      if(!is.null(data$sampleInfo()[,2])){
        label <- reactive({unique(data$sampleInfo()[,2])})
        group <- reactive({
          tmp <- 1:length(unique(data$sampleInfo()[,2]))

          return(tmp)
        })
        output$colrs <- renderUI({
          tagList(
            lapply(split(group(),1:(length(group())%/%3 + 1)),function(col){
              fluidRow(
                lapply(col,function(x){
                  #tags$div(column(3,mod_colorPickr_ui(ns(paste0("colorPickr_",x)),x,label()[x])))
                  tags$div(
                    column(
                      3,
                      colorPickr(
                        inputId = ns(paste0("colorPickr_",x)),
                        label =label()[x],
                        update = "save",
                        interaction = list(
                          clear = F,
                          save = T
                        ),
                        useAsButton = T,
                        width = "160px",
                        pickr_width = "20%",
                        selected = if(x <= 10 ){
                          as.character(paletteer::paletteer_d("yarrr::basel"))[x]
                        } else {
                          as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(x)%%10]
                        },
                        #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                        swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                      )
                    )
                  )

                })
              )
            })
          )
        })
      }
    })





    colors <-reactive({
      unlist(
        lapply(1:length(unique(data$sampleInfo()[,2])),function(x){
          input[[paste0("colorPickr_",x)]]
        })
      )
    })



    #output$ttt <- renderPrint({input$switch_ellipse})

    p <- eventReactive(input$run,{
      isolate(
        pca_plot_func(
          expr_tpm=data$expr(),
          group_list=data$sampleInfo()[,2],
          colr = colors(),
          dotSize = input$num_dot,
          title= input$title,
          addEllipses = input$switch_ellipse,
          addLables=input$switch_label,
          xlim = input$xlim,
          ylim = input$ylim
        )
      )
    })


    output$plot <- renderPlot({
      p()
    })
    mod_download_server("download_1",p,"PCA","pdf")

    # eample download
    output$dnld_expl_expr <- downloadHandler(
      filename = function() {
        paste("expr_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/expr_CPM.xlsx",file)
      }
    )

    output$dnld_expl_sampleInfo <- downloadHandler(
      filename = function() {
        paste("sampleInfo_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/sample_info.xlsx",file)
      }
    )


  })
}


## To be copied in the UI
# mod_page_pca_ui("page_pca_1")

## To be copied in the server
# mod_page_pca_server("page_pca_1")
