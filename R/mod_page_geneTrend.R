#' page_geneTrend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_geneTrend_ui <- function(id){
  ns <- NS(id)
  tagList(# Tags
    fluidRow(
      column(
        3,
        box(#bs1
          id = "box1",
          width = 12,
          title = "Parameter Setting",
          fluidRow(#exampl s
            column(
              12,
              fluidRow(
                column(
                  8,
                  fileInput(
                    ns("file_expr"),
                    "Upload your expr file",
                    width = "auto"
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
                column(
                  8,
                  fileInput(
                    ns("file_sampleInfo"),
                    "Upload your sampleInfo file",
                    width = "auto"
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
              )

            )
          ),#exampl e
          spsHr(),
          fluidRow( # accordion fluidRow
            column(
              12,
              accordion(
                id = ns("accordion"),
                width = "auto",
                accordionItem( # accordionItem 1
                  title = "Mfuzz Settings",
                  status = "teal",
                  collapsed = F,
                  fluidRow(
                    column(
                      4,
                      numericInput(
                        ns("filterNA"),
                        "filter.NA:",
                        value = 0.25,
                        step = 0.05

                      )
                    ),
                    column(
                      4,
                      pickerInput(
                        ns("fillNA"),
                        label = "fill.NA:",
                        multiple = F,
                        choices = c("mean","median"),
                        selected = "mean",
                        options = pickerOptions(
                          actionsBox = TRUE,
                          #title = "",
                          #header = "This is a title",
                          width = "auto",
                          liveSearch = F
                        )
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        ns("filterSD"),
                        "filter.std:",
                        value = 0.3,
                        step = 0.1

                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      align = "center",
                      numericInput(
                        ns("c_value"),
                        "Enter the number of clusters:",
                        value = 5,
                        step = 1

                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      actionBttn(
                        ns("act_mfuzz"),
                        label = "Click to start Mfuzz analysis",
                        size = "md"
                      ),
                      align="center"
                    )
                  )
                ), # accordionItem 1
                accordionItem( # accordionItem 2
                  title = "Clusters  Plot Settings",
                  status = "teal",
                  collapsed = T,
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("pointSize"),
                        "pointSize",
                        value = 3,
                        step = 0.5

                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("lineWidth"),
                        "lineWidth",
                        value = 0.8,
                        step = 0.2

                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      align = "center",
                      numericInput(
                        ns("baseSize"),
                        "baseSize",
                        value = 14,
                        step = 1

                      )
                    )
                  )
                ), # accordionItem 2
                accordionItem( # accordionItem 3
                  title = "Individual Cluster Plot Settings",
                  status = "teal",
                  collapsed = T,
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("sclineWidth"),
                        "lineWidth",
                        value = 0.5,
                        step = .1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("scbaseSize"),
                        "baseSize",
                        value = 14,
                        step = 1
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("center_lindWidth"),
                        "Center pointSize",
                        value = 1,
                        step = .1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("center_pointSize"),
                        "Center pointSize",
                        value = 2,
                        step = 0.5
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      4,
                      colorPickr(
                        inputId = ns("mm_colr_low"),
                        label ="mmLow",
                        update = "save",
                        interaction = list(
                          clear = F,
                          save = T
                        ),
                        useAsButton = F,
                        width = "auto",
                        pickr_width = "20%",
                        selected = "#e6ee9c",
                        #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                        swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        ns("mm_midpoint"),
                        "midPoint",
                        value = .5,
                        step = .1
                      )
                    ),
                    column(
                      4,
                      colorPickr(
                        inputId = ns("mm_colr_high"),
                        label ="mmHigh",
                        update = "save",
                        interaction = list(
                          clear = F,
                          save = T
                        ),
                        useAsButton = F,
                        width = "auto",
                        pickr_width = "20%",
                        selected = "#dd2c00",
                        #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                        swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                      )
                    )
                  )
                ) # accordionItem 3

              )
            )
          )# accordion fluidRow




        )#be1
      ),
      column(
        9,
        fluidRow(
          column(
            12,
            box(#bs2
              id = ns("box2"),
              width = 12,
              title = "Plot" ,
              fluidRow(
                column(
                  12,
                  align = "center",
                  accordion(
                    id = "acc_levels",
                    accordionItem(
                      title = "Set the order of the x-axis:",
                      status = "teal",
                      collapsed = T,
                      shinyjqui::orderInput(inputId = ns('levels'), label = "", items = NULL,item_class = 'success')
                    )
                  )

                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  uiOutput(ns("cluster"))



                )
              )
            )#be2
          )
        ),
        fluidRow(
          column(
            12,
            box(
              id = ns("bx_res"),
              width =12,
              title = "Result",
              bs4Dash::tabsetPanel(
                id = ns("tabSet_res"),
                tabPanel(
                  title = "Input",
                  fluidRow(
                    column(
                      3,
                      tags$b(tags$h3("SampleInfo:",style = "text-align:left")),
                      rhandsontable::rHandsontableOutput(ns("hot_sampleInfo"))
                    ),
                    column(
                      9,
                      tags$b(tags$h3("Expression data:",style = "text-align:left")),
                      rhandsontable::rHandsontableOutput(ns("hot_expr"))
                    )
                  )
                ),
                tabPanel(
                  title = "Result",
                  fluidRow(
                    column(
                      8,
                      align = "left",
                      tags$b(tags$h3("Mfuzz result:"))
                    ),
                    column(
                      4,
                      align = "right",
                      mod_download_ui(ns("download_xlsx"),"xlsx","sm")
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      # useAttendant(),
                      # attendantBar("progress-bar-mfuzz",
                      #              striped = T,
                      #              animated = T),
                      useWaitress(color = "#7bed9f"),
                      rhandsontable::rHandsontableOutput(ns("hot_mfuzz"))
                      #textOutput(ns("tt"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )#Tage
}

#' page_geneTrend Server Functions
#'
#' @noRd
mod_page_geneTrend_server <- function(id){
  moduleServer( id, function(input, output, session){
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
      data$expr <- reactive({openxlsx::read.xlsx("./data/Mfuzz_expr.xlsx",rowNames = F)})
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
      data$sampleInfo <- reactive({openxlsx::read.xlsx("./data/Mfuzz_groupleIfo.xlsx")})
    })

    output$hot_sampleInfo <-
      rhandsontable::renderRHandsontable({
        if(!is.null(data$sampleInfo())){
          rhandsontable::rhandsontable(data$sampleInfo(),readOnly = F,width = "auto",height = 400)%>%
            rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
        }

      })

    # levels
    observe({
      shinyjqui::updateOrderInput(
        session,
        inputId = "levels",
        label = '',#X-axis order:
        items = unique(data$sampleInfo()$Group),
        connect = NULL,
        item_class = 'success'
      )
    })
    # mfuzz result
    # progress bar
    #att <- Attendant$new("progress-bar-mfuzz", hide_on_max = F)
    att <- Waitress$new(theme = "overlay-percent")

    # enrich result
    mfuzz <- eventReactive(input$act_mfuzz,{
      #att$auto()
      #on.exit(att$done())
      att$notify()
      #att$set(20)
      att$auto(1,2000)
      shinyCatch(
        return(
          mfuzz_ana(
              data$expr(),
              data$sampleInfo(),
              c_value =input$c_value,
              filterNA = input$filterNA,
              fillNA = input$fillNA,
              filterSD = input$filterSD
            )

        )

      )

    })
    # output$tt <- renderPrint({
    #   p
    # })


    # update pagination
    observeEvent(input$act_mfuzz,{
      updateAccordion(
        id = "accordion",
        selected =2
      )
    })

    observeEvent(input$act_mfuzz,{
      updateTabsetPanel(
        session,
        inputId = "tabSet_res",
        selected = "Result"
      )
    })


    acore.list <- reactive({
      Mfuzz::acore(mfuzz()$eset,cl=mfuzz()$cl,min.acore=0.5) %>% `names<-`(paste0("Cluster ",seq_along(.)))
    })


    #enrich result hotOutput
    observeEvent(input$act_mfuzz,{
      output$hot_mfuzz <- rhandsontable::renderRHandsontable({
        if (!is.null(acore.list())) {
          #on.exit(att$done())
          on.exit(att$close())
          rhandsontable::rhandsontable(
            acore.list()[[1]]
            ,
            readOnly = F,
            width = "auto",
            height = 400
          ) %>%
            rhandsontable::hot_cols(manualColumnResiz = T, columnSorting =T)
        }else {
          if (is.null(acore.list())){
            #on.exit(att$done())
            on.exit(att$close())
          }
        }
      })

    })



    output$cluster <- renderUI({
      do.call(bs4Dash::tabsetPanel,c(id = ns("tabSet"),vertical = T,type = "pills",lapply(c(0,seq_along(1:input$c_value)),function(i){
        tabPanel(
          title=if(i ==0){
            "All Cluster"
          }else{
            paste0('Cluster ', i)
          },
          status= "success",
          fluidRow(
            column(
              12,
              renderPlot({
                if(i==0){
                  mfuzz_all_plot(
                    mfuzz()$eset,
                    cl=mfuzz()$cl,
                    levels=input$levels,
                    colors=c("#76BA99", "#EB4747", "#996699","#f1c40f","#ea8685","#775039","#b83570","#e4b8d5","#9ebc19"),
                    pointSize = 3,
                    lineWidth = .8,
                    baseSize = 14
                  )
                  #shinipsum::random_ggplot()
                }else{
                  mfuzz_single_plot(
                    mfuzz()$eset,
                    cl=mfuzz()$cl,
                    plotN = i,
                    input$levels,
                    mm_colr_low = input$mm_colr_low,
                    mm_colr_high = input$mm_colr_high,
                    mm_midpoint =  input$mm_midpoint,
                    title_colr = "black",
                    lineWidth =input$sclineWidth,
                    center_lindWidth = input$center_lindWidth,
                    center_pointSize = input$center_pointSize,
                    baseSize = input$scbaseSize
                  )
                }


              },width = 800,height = 600)
            )
          ),
          fluidRow(
            column(
              12,
              align = "right",
              mod_download_ui(ns(paste0("download_pdf",i)),"PDF","sm")
            )
          )
        )
      })))
    })

    # pdf download
    observeEvent(input$levels,{
      lapply(c(0,seq_along(1:input$c_value)),function(i){
        p <- reactive({
          if(i==0){
            mfuzz_all_plot(
              mfuzz()$eset,
              cl=mfuzz()$cl,
              levels=input$levels,
              colors=c("#76BA99", "#EB4747", "#996699","#f1c40f","#ea8685","#775039","#b83570","#e4b8d5","#9ebc19"),
              pointSize = 3,
              lineWidth = .8,
              baseSize = 14
            )
            #shinipsum::random_ggplot()
          }else{
            mfuzz_single_plot(
              mfuzz()$eset,
              cl=mfuzz()$cl,
              plotN = i,
              input$levels,
              mm_colr_low = input$mm_colr_low,
              mm_colr_high = input$mm_colr_high,
              mm_midpoint =  input$mm_midpoint,
              title_colr = "black",
              lineWidth =input$sclineWidth,
              center_lindWidth = input$center_lindWidth,
              center_pointSize = input$center_pointSize,
              baseSize = input$scbaseSize
            )
          }
        })
        mod_download_server(paste0("download_pdf",i),p,paste0("Cluster ",i),"pdf")
      })
    })

    # Mfuzz xlsx download
    observeEvent(input$act_mfuzz,{
      mod_download_server("download_xlsx",acore.list,paste0("Mfuzz-results"),"xlsx")
    })

    # eample download
    output$dnld_expl_expr <- downloadHandler(
      filename = function() {
        paste("expr_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/Mfuzz_expr.xlsx",file)
      }
    )

    output$dnld_expl_sampleInfo <- downloadHandler(
      filename = function() {
        paste("smpleInfo_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/Mfuzz_groupleIfo.xlsx",file)
      }
    )
  })
}

## To be copied in the UI
# mod_page_geneTrend_ui("page_geneTrend_1")

## To be copied in the server
# mod_page_geneTrend_server("page_geneTrend_1")
