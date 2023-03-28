#' page_DEseq2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bs4Dash
mod_page_DEseq2_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      4,
      box(
        width = 12,
        id = ns("box1"),
        title = "Upload Data",
        fluidRow(
          column(
            12,
            actionBttn(
              ns("act_next"),
              label = "Click to submit data",
              size = "md"
            ),
            align = "center"
          )
        ),
        spsHr("primary"),
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
    ),
    column(
      3,
      box(
        width = 12,
        id = ns("box2"),
        title = "Parameter Setting",
        fluidRow(
          column(
            12,
            actionBttn(
              ns("act_submit"),
              label = "Click to start analysis",
              size = "md"
            ),
            align="center"

          )
        ),
        spsHr("primary"),
        fluidRow(
          column(
            12,
            numericInput(
              ns("fc"),
              "Fold Change",
              value = 2,
              step = 0.5

            ),
            numericInput(
              ns("fdr"),
              "ajust Pvalue",
              value = 0.05,
              step = 0.01

            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput(ns("contrast")),
            textOutput(ns("tt"))
          )
        )
      )
    ),
    column(
      5,
      box(
        width = 12,
        id = ns("box3"),
        title = "DEseq2 Results",
        #status = "primary",
        # pgPaneUI(
        #   pane_id = ns("progress"),
        #   titles = c("DEseq2 analysis progress"),
        #   pg_ids = c("pro1"),
        #   title_main = "DEseq2 analysis progress",
        #   opened = TRUE,
        #   top = "30%",
        #   right = "50%"
        # ),
        fluidRow(
          column(
            12,
            mod_download_ui(ns("download_1"),"DEseq2 results"),
            align = "center"
          )
        ),
        spsHr("primary"),
        shinyWidgets::progressBar(
          id = ns("pb1"),
          title = "DEseq2 analysis progress:",
          display_pct = T,
          value = 0,
          status = "primary",
          striped = TRUE,
          size = "md"
        ),
        uiOutput(ns("pgui"))


      )

    )
  )
}

#' page_DEseq2 Server Functions
#'
#' @noRd
mod_page_DEseq2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_rev_server("rev_1",c("a","b"))
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
      data$expr <- reactive({openxlsx::read.xlsx("./data/expr_count.xlsx",rowNames = F)})
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

    cgroup <- reactive({
      if(!is.null(data$sampleInfo())){
        as.list(
          as.data.frame(
            combn(unique(data$sampleInfo()[,2]),2)
          )
        )
      }
    })

    observeEvent(input$act_next,{
      output$contrast <- renderUI({
        if(!is.null(cgroup())){
          lapply(1:length(cgroup()),function(x){
            mod_rev_ui(ns(paste0("Rev",x)))
          })
        }
      })
    })

    cgroup_done <- eventReactive(input$act_next,{
      if(!is.null(cgroup())){
        unlist(
          lapply(1:length(cgroup()),function(x){
            mod_rev_server(paste0("Rev",x),c(cgroup()[[x]]))
          })
        )
      }
    })

    output$tt <- renderText({
      str(cgroup_done())
    })






  observeEvent(input$act_submit,{
    res_DEGs <- reactive({
      if(!is.null(isolate(cgroup_done()))){
        tmp <-
          # lapply(isolate(cgroup_done()),function(cp){
          #   func_DEseq2(
          #     data$expr(),
          #     data$sampleInfo(),
          #     fc = 2,
          #     fdr = 0.05,
          #     contrast_pair= cp()
          #   )
          # })
          lapply(1:length(isolate(cgroup_done())),function(cp){

            updateProgressBar(
              session = session,
              id = "pb1",
              value = (100/length(isolate(cgroup_done()))) * cp
            )

            func_DEseq2(
              data$expr(),
              data$sampleInfo(),
              fc = isolate(input$fc),
              fdr = isolate(input$fdr),
              contrast_pair= c(isolate(cgroup_done())[[cp]]())
            )
          })

        names(tmp) <- lapply(cgroup_done(),function(x){
          paste0(x()[2]," vs ", x()[1])
        })
        return(tmp)
      }
    })
    # result download
    mod_download_server("download_1",res_DEGs,"DEseq2-results","xlsx")

    output$pgui <- renderUI({
      if(!is.null(res_DEGs())){
        lapply(1:length(names(res_DEGs())),function(x){
          tags$div(
            accordion(
              id = ns(paste0("ac",x)),
              accordionItem(
                title=names(res_DEGs())[x],
                status = "danger",
                collapsed = T,
                rhandsontable::rHandsontableOutput(ns(paste0("hot_res",x)))
              )
            )
          )
        })
      }
    })

    observe({
      lapply(1:length(names(res_DEGs())),function(x){
        output[[paste0("hot_res",x)]] <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(as.data.frame(res_DEGs()[[x]]),readOnly = F,width = "auto",height = 400)%>%
            rhandsontable::hot_cols(manualColumnResiz=F,columnSorting=T)
        })
      })
    })
  })





  # eample download
  output$dnld_expl_expr <- downloadHandler(
    filename = function() {
      paste("expr_template_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      file.copy("./data/expr_count.xlsx",file)
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
# mod_page_DEseq2_ui("page_DEseq2_1")

## To be copied in the server
# mod_page_DEseq2_server("page_DEseq2_1")
