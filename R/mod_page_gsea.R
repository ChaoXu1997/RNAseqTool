#' page_gsea UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_gsea_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        box(
          id ="box1",
          width = 12,
          title = "Data & Parameters",
          fluidRow(
            column(
              8,
              fileInput(
                ns("file_geneList"),
                "Upload your geneList file",
                width = "350px"
              )

            ),
            column(
              4,
              tags$br(),
              actionBttn(
                ns("act_expl_geneList"),
                label = "Example",
                size = "sm"
              ),
              tags$br(),
              downloadLink(
                ns("dnld_expl_geneList"),
                "Example.xlsx"
              )
            )
          ),
          spsHr(),
          fluidRow(
            column(
              12,
              pagination(
                id = ns("page"),
                selected = "Enrich Settings",
                align = "center",
                size = "md",
                previousBtn = "«",
                nextBtn = "»",
                paginationItem(
                  "Enrich Settings",
                  fluidRow(
                    column(
                      12,
                      radioGroupButtons(
                        inputId = ns("pk_gene_type"),
                        label = "Gene ID type:",
                        choices = c("symbol","entrezid", "ensembl"),
                        checkIcon = list(
                          yes = tags$i(class = "fa fa-check-square",
                                       style = "color: steelblue"),
                          no = tags$i(class = "fa fa-square-o",
                                      style = "color: steelblue"))
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      pickerInput(
                        ns("pk_database"),
                        label = "Select database (multiple choices allowed)",
                        multiple = TRUE,
                        choices = db_collections$Database_num,
                        selected = db_collections$Database_num[15:17],
                        options = pickerOptions(
                          actionsBox = TRUE,
                          title = "Please select a month",
                          #header = "This is a title",
                          width = "400px",
                          liveSearch = T
                        ),
                        width ="auto"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      pickerInput(
                        ns("pk_species"),
                        label = "Select species (Latin name):",
                        multiple = F,
                        choices = species,
                        selected = species[10],
                        options = pickerOptions(
                          actionsBox = TRUE,
                          title = "Please select a month",
                          #header = "This is a title",
                          width = "400px",
                          liveSearch = T
                        ),
                        width ="auto"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("pvalue"),
                        "Pvalue:",
                        value = 0.05,
                        step = 0.01

                      )
                    ),
                    column(
                      6,
                      pickerInput(
                        ns("pmethod"),
                        label = "pAdjustMethod:",
                        selected = "BH",
                        choices =c(
                          "holm",
                          "hochberg",
                          "hommel",
                          "bonferroni",
                          "BH",
                          "BY",
                          "fdr",
                          "none"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      actionBttn(
                        ns("act_enrich_gsea"),
                        label = "Click to start enrichment",
                        size = "md"
                      ),
                      align="center"
                    )
                  )
                ),
                paginationItem(
                  "Plot Settings",
                  disabled = T,
                  fluidRow(
                    column(
                      12,
                      uiOutput(ns("geneseID"))

                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      radioGroupButtons(
                        inputId = ns("subPlot"),
                        label = "subPlot",
                        choices = c(1,2,3),
                        justified = TRUE,
                        size = "xs",
                        selected = 3,
                        checkIcon = list(
                          yes = icon("ok",
                                     lib = "glyphicon")),
                        width  = "auto"

                      )
                    )

                  ),
                  fluidRow(
                    column(
                      6,
                      numericInput(
                        ns("termWidth"),
                        "termWidth",
                        value = 40,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        ns("baseSize"),
                        "baseSize",
                        value = 12,
                        step = 1
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      tags$b(tags$h5("addPval:")),
                      switchInput(
                        inputId = ns("addPval"),
                        value = F,
                        size = "large",
                        onStatus = "success",
                        offStatus = "danger"
                      )
                    ),
                    column(
                      3,
                      numericInput(
                        ns("pvalX"),
                        "pvalX",
                        value = 1,
                        step = 0.1
                      )
                    ),
                    column(
                      3,
                      offset = 0,
                      numericInput(
                        ns("pvalY"),
                        "pvalX",
                        value = 1,
                        step = 0.1
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      tags$b(tags$h5("autoMark gene:")),
                      switchInput(
                        inputId = ns("markTogene"),
                        value = F,
                        size = "large",
                        onStatus = "success",
                        offStatus = "danger"
                      )
                    ),
                    column(
                      3,
                      offset = 0,
                      numericInput(
                        ns("topGeneN"),
                        "topGeneN",
                        value = 5,
                        step = 1
                      )
                    ),
                    column(
                      3,
                      numericInput(
                        ns("psize"),
                        "pvalSize",
                        value = 4,
                        step = 1
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      8,
                      textAreaInput(
                        ns("addGene"),
                        label = "Mark gene when autoMarkgene=OFF:",
                        placeholder = NULL,
                        value = NULL,
                        height = 200,
                        width = "100%"
                      )
                    ),
                    column(
                      4,
                      fluidRow(
                        column(
                          12,
                          tags$br(),
                          tags$br(),
                          colorPickr(
                            inputId = ns("geneColr"),
                            label ="GeneColor",
                            update = "save",
                            interaction = list(
                              clear = F,
                              save = T
                            ),
                            useAsButton = F,
                            width = "auto",
                            pickr_width = "20%",
                            selected = "black",
                            #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                            swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                          )

                        )
                      ),
                      fluidRow(
                        column(
                          12,
                          radioGroupButtons(
                            inputId = ns("arrowType"),
                            label = "arrowType",
                            choices = c("open",
                                        "closed"),
                            justified = F,
                            direction = "vertical",
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square",
                                           style = "color: steelblue"),
                              no = tags$i(class = "fa fa-square-o",
                                          style = "color: steelblue"))
                          )
                        )
                      )

                    )
                  )
              )
            )
            )
          )

        )
      ),
      column(
        9,
        fluidRow(
          column(
            12,
            box(
              id ="box2",
              width = 12,
              title = "Plot",
              fluidRow(
                column(
                  3,
                  fluidRow(
                    column(
                      5,
                      align = "center",
                      actionBttn(
                        ns("act_plot"),
                        label = "renderPlot",
                        size = "sm"

                      )

                    ),
                    column(
                      7,
                      align = "center",
                      mod_download_ui(ns("download_pdf"),"PDF","sm")
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      radioGroupButtons(
                        inputId = ns("style"),
                        label = "Plot style:",
                        choices = c("Classic","NewGsea"),
                        justified = TRUE,
                        checkIcon = list(
                          yes = icon("ok",
                                     lib = "glyphicon"))
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      accordion(
                        id= ns("acc"),
                        accordionItem(
                          title = "Classic style",
                          fluidRow(
                            column(
                              12,
                              numericInput(
                                ns("rankSeq"),
                                "rank axixs breaks:",
                                value = 2000,
                                step = 100
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              align = "center",
                              colorPickr(
                                inputId = ns("htCol_high"),
                                label ="highRank colr:",
                                update = "save",
                                interaction = list(
                                  clear = F,
                                  save = T
                                ),
                                useAsButton = T,
                                width = "auto",
                                pickr_width = "20%",
                                selected = "#A50f15",
                                #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                                swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                              )
                            ),
                            column(
                              6,
                              align = "center",
                              colorPickr(
                                inputId = ns("htCol_low"),
                                label ="lowRank colr:",
                                update = "save",
                                interaction = list(
                                  clear = F,
                                  save = T
                                ),
                                useAsButton = T,
                                width = "auto",
                                pickr_width = "20%",
                                selected = "#08519c",
                                #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                                swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                              )
                            )
                          )

                        ),
                        accordionItem(
                          title = "NewGsea style",
                          fluidRow(
                            column(
                              6,
                              align = "center",
                              colorPickr(
                                inputId = ns("pcolr"),
                                label ="pvalue colr:",
                                update = "save",
                                interaction = list(
                                  clear = F,
                                  save = T
                                ),
                                useAsButton = F,
                                width = "auto",
                                pickr_width = "20%",
                                selected = "#08519c",
                                #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                                swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                              )
                            ),
                            column(
                              6,
                              numericInput(
                                ns("ncol"),
                                "Columns:",
                                value = 1,
                                step = 1
                              )
                            )

                          ),
                          fluidRow(
                            column(
                              12,
                              align = "center",
                              tags$b(tags$h5("addPoint:")),
                              switchInput(
                                inputId = ns("addPoin"),
                                value = F,
                                size = "large",
                                onStatus = "success",
                                offStatus = "danger"
                              )

                            )
                          ),
                          fluidRow(
                            column(
                              6,
                              align = "center",
                              colorPickr(
                                inputId = ns("new_htCol_high"),
                                label ="highRank colr:",
                                update = "save",
                                interaction = list(
                                  clear = F,
                                  save = T
                                ),
                                useAsButton = T,
                                width = "auto",
                                pickr_width = "20%",
                                selected = "#993399",
                                #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                                swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                              )
                            ),
                            column(
                              6,
                              align = "center",
                              colorPickr(
                                inputId = ns("new_htCol_low"),
                                label ="lowRank colr:",
                                update = "save",
                                interaction = list(
                                  clear = F,
                                  save = T
                                ),
                                useAsButton = T,
                                width = "auto",
                                pickr_width = "20%",
                                selected = "#08519c",
                                #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                                swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                              )
                            )
                          )


                        )
                      )
                    )
                  )
                ),
                column(
                  9,
                  plotOutput(
                    ns("plot"),
                    width = 800,
                    height = 500
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            box(
              id ="box3",
              width = 12,
              title = "GSEA Results",
              fluidRow(
                column(
                  3,
                  tags$b(tags$h3("geneList:",style = "text-align:center")),
                  rhandsontable::rHandsontableOutput(ns("hot_geneList"))
                ),
                column(
                  9,
                  fluidRow(
                    column(
                      8,
                      align = "left",
                      tags$b(tags$h3("GSEA result:"))
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
                      useAttendant(),
                      attendantBar("progress-bar-gsea",
                                   striped = T,
                                   animated = T),
                      rhandsontable::rHandsontableOutput(ns("hot_enrich"))
                    )
                  )


                )
              )#,
              # fluidRow(
              #   textOutput(ns("tt"))
              # )
            )
          )
        )
      )
    )
  )

}

#' page_gsea Server Functions
#'
#' @noRd
mod_page_gsea_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # data
    data <- reactiveValues(
      geneList = reactive({NULL})
    )
    # geneList data
    req(reactive({input$file_geneList}))
    observeEvent(input$file_geneListr,{
      data$geneList <- reactive({openxlsx::read.xlsx(input$file_geneList$datapath,rowNames = F)})
    })
    observeEvent(input$act_expl_geneList,{
      data$geneList <- reactive({openxlsx::read.xlsx("./data/gene_log2FC.xlsx",rowNames = F)})
    })
    # geneList hotOutput
    output$hot_geneList <-
      rhandsontable::renderRHandsontable({
        if(!is.null(data$geneList())){
          rhandsontable::rhandsontable(data$geneList(),readOnly = F,width = "auto",height = 400)%>%
            rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
        }
      })

    # prepare geneList
    gene <- reactive({
      geneL <- data$geneList()$log2FoldChange %>% `names<-`(data$geneList()$Gene)
      geneL <- geneL[order(geneL,decreasing = T)]
      return(geneL)
    })


    # progress bar
    att <- Attendant$new("progress-bar-gsea", hide_on_max = F)

    # enrich result
    egsea <- eventReactive(input$act_enrich_gsea,{
      # db
      att$set(1)
      att$auto()
      db <- msigdbr_func(input$pk_species,c(input$pk_database))
      shinyCatch(
        return(gesa_func(gene(),db,input$pk_gene_type,input$pvalue,input$pmethod))

      )
    })

    # update pagination
    observeEvent(input$act_enrich_gsea,{
      updatePagination(
        "page",
        selected ="Plot Settings",
        disabled = F
      )
    })



    # enrich result hotOutput
    observeEvent(input$act_enrich_gsea,{
      output$hot_enrich <- rhandsontable::renderRHandsontable({
        if (!is.null(egsea())) {
          on.exit(att$done())
          rhandsontable::rhandsontable(
            egsea()@result %>%
              tibble::remove_rownames() %>%
              dplyr::select(-ID)
            ,
            readOnly = F,
            width = "auto",
            height = 400
          ) %>%
            rhandsontable::hot_cols(manualColumnResiz = T, columnSorting =T)
        }else {
          if (is.null(egsea())){
            on.exit(att$done())
          }
        }
      })

    })
    # rneder genesetID ui
    output$geneseID <- renderUI({
      pickerInput(
        ns("pk_enrich_id"),
        label = "Select pathway terms (multiple choices allowed)",
        multiple = TRUE,
        choices = egsea()@result$ID,
        selected = egsea()@result$ID[1],
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Please select a month",
          #header = "This is a title",
          width = "400px",
          liveSearch = T
        ),
        width ="auto"
      )
    })


    # update accordion
    observeEvent(input$style,{
      updateAccordion(
        id = "acc",
        selected = if(input$style == "Classic"){
          1
        }else { 2}
      )
    })
    newGSEA <- reactive({
      if(input$style == "Classic"){
        F
      }else{ T}

    })

    # add genes
    addGene <- reactive({
      if(input$addGene == "" & input$markTogene == F){
        NULL
      }else {
        input$addGene
      }
    })


    p <- eventReactive(input$act_plot,{
      isolate({
        gsea_plot_func(
          egsea(),
          addGene = addGene(),
          geneSetID = input$pk_enrich_id,
          addPval = input$addPval,
          pvalSize = input$psize,
          pvalX = input$pvalX,
          pvalY = input$pvalY,
          subPlot = input$subPlot,
          termWidth = input$termWidth,
          base_size = input$baseSize,
          arrowType = input$arrowType,
          geneCol = input$geneColr,
          markTopgene = input$markTogene,
          topGeneN = input$topGeneN,
          rankSeq = input$rankSeq,
          htCol =  c(input$htCol_low, input$htCol_high),
          rankCol = c(input$htCol_low, "white", input$htCol_high),
          #curveCol = c("#76BA99", "#EB4747", "#996699","#f1c40f","#ea8685","#775039","#b83570","#e4b8d5","#9ebc19"),
          newGsea = newGSEA(),
          pCol = input$pcolr,
          ncol=input$ncol,
          newCurveCol = c(input$new_htCol_low, "white", input$new_htCol_high),
          newHtCol = c(input$new_htCol_low, "white", input$new_htCol_high),
          addPoin= input$addPoin
        )
      })
    })


    # gsea plot
    observe({
      output$plot <- renderPlot({
        p()
      })
    })

    # download
    mod_download_server("download_pdf",p,"GSEA","pdf",width = 10.5,height = 6)

    egsea_save <- reactive({
      egsea()@result %>%
        tibble::remove_rownames() %>%
        dplyr::select(-ID)
    })

    mod_download_server("download_xlsx",egsea_save,"Enrich","xlsx")


    # download example
    output$dnld_expl_geneList <- downloadHandler(
      filename = function() {
        paste("geneList_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/gene_log2FC.xlsx",file)
      }
    )
    #output$tt <- renderPrint({addGene()})

  })
}

## To be copied in the UI
# mod_page_gsea_ui("page_gsea_1")

## To be copied in the server
# mod_page_gsea_server("page_gsea_1")
