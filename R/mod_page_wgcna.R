#' page_wgcna UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_wgcna_ui <- function(id){
  ns <- NS(id)
  useAttendant()
  fluidRow(
    column(
      12,
      bs4Dash::tabBox(
        id =ns("taBox"),
        title = NULL,
        selected = NULL,
        width = 12,
        tabPanel(#tab1
          title = "Step 1",
          fluidRow(
            column(#para
              3,
              fluidRow(#exampls
                column(
                  12,
                  fluidRow(
                    column(
                      8,
                      fileInput(
                        ns("file_expr"),
                        "Upload your datExpr file",
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
                        "Upload your datTraits file",
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
              fluidRow(
                column(
                  #offset = 6,
                  6,
                  align = "center",
                  numericInput(
                    ns("cutHeight"),
                    "cutHeight",
                    15,
                    width = "50%",
                    step = 1
                  )
                )
              ),
              fluidRow(
                  column(
                    6,
                    align = "center",
                    actionBttn(
                      ns("act_cutHeight"),
                      "CutTree&Plot",
                      icon = icon("scissors"),
                      size = "md"
                    )
                  ),
                  column(
                    6,
                    align = "center",
                    actionBttn(
                      ns("step2"),
                      label = "Next Step",
                      size = "md"
                    )
                  )

              )

            ),#para
            column(
              9,
              fluidRow(
                column(
                  6,
                  fluidRow(
                    column(
                      12,
                      align = "center",
                      tags$h3("Raw sampleTree:")
                    )
                  ),
                  plotOutput(ns("sampleTree"))

                ),
                column(
                  6,
                  fluidRow(
                    column(
                      12,
                      align = "center",
                      tags$h3("sampleTree:"),
                      mod_download_ui(ns("download_pdf_sapleTree"),"PDF","sm")
                    )
                  ),
                  plotOutput(ns("sampleTree2"))
                )
              )
            )
          ),
          fluidRow(
            column(
              6,
              tags$b(tags$h3("datExpr:")),
              rhandsontable::rHandsontableOutput(ns("hot_datExpr"))
            ),
            column(
              6,
              tags$b(tags$h3("datTrait:")),
              rhandsontable::rHandsontableOutput(ns("hot_datTrait"))
            )
          )
        ),#tab1

        tabPanel(#tab2
          title = "Step 2",
          fluidRow(
            column(
              2,
              fluidRow(
                column(
                  #offset = 6,
                  12,
                  align = "center",
                  numericInput(
                    ns("abline"),
                    "abline:",
                    0.8,
                    width = "50%",
                    step = 0.1
                  )
                )
              ),
              fluidRow(
                column(
                  #offset = 6,
                  12,
                  align = "center",
                  numericInput(
                    ns("power_value"),
                    "Power value:",
                    6,
                    width = "50%",
                    step = 1
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("act_power"),
                    "Get Power",
                    icon = icon("grip-lines"),
                    size = "md"
                  )
                )

              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align = "center",
                  attendantBar("progress-bar-net",
                               striped = T,
                               animated = T,
                               width = "50%")
                )
              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("step3"),
                    label = "Next Step",
                    size = "md"
                  )
              )

              )
            ),
            column(
              5,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$h3("Choose a power value:"),
                  mod_download_ui(ns("download_pdf_power"),"PDF","sm")
                )
              ),
              plotOutput(
                ns("power_plot"),
                width = "auto",
                height = 600
              )
            ),
            column(
              5,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$h3("Construction of the gene network and identification of modules:"),
                  mod_download_ui(ns("download_pdf_net"),"PDF","sm")
                )
              ),
              plotOutput(
                ns("net_plot"),
                width = "auto",
                height = 600
              )
            )
          )
        ),#tab2
        tabPanel(#tab3
          title = "Step 3",
          fluidRow(
            column(
              2,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$b(tags$h6("Choose a module:")),
                  pickerInput(
                    ns("module"),
                    label = "",
                    multiple = F,
                    choices = NULL,
                    selected = NULL,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      #title = "",
                      #header = "This is a title",
                      width = "auto",
                      liveSearch = T
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$b(tags$h6("Choose a trait:")),
                  pickerInput(
                    ns("trait"),
                    label = "",
                    multiple = F,
                    choices = NULL,
                    selected = NULL,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      #title = "",
                      #header = "This is a title",
                      width = "auto",
                      liveSearch = T
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("act_scatter"),
                    label = "renderSctterPlot",
                    size = "md"
                  )
                )

              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("step4"),
                    label = "Next Step",
                    size = "md"
                  )
                )

              )
            ),
            column(
              5,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$h3("Quantifying module–trait associations:"),
                  mod_download_ui(ns("download_pdf_heatmap"),"PDF","sm")
                )
              ),
              plotOutput(
                ns("heatmap_plot"),
                width = "auto",
                height = 850
              )
            ),
            column(
              5,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$h3("Identifying genes with high Gene Significance (GS) and Module Membership (MM):"),
                  mod_download_ui(ns("download_pdf_scatter"),"PDF","sm")
                )
              ),
              plotOutput(
                ns("scatter_plot"),
                width = "auto"#,
                #height = 850

              )
            )
          ),
          fluidRow(
            column(
              2,
              align = "center",
              mod_download_ui(ns("download_MM_GS"),"xlsx","sm")
            ),
            column(
              5,
              tags$b(tags$h3("Gene Significance:")),
              rhandsontable::rHandsontableOutput(ns("hot_GS"))
            ),
            column(
              5,
              tags$b(tags$h3("Module Membership:")),
              rhandsontable::rHandsontableOutput(ns("hot_MM"))
            )
          )
        ),#tab3,
        tabPanel(#tab4
          title = "Step 4",
          fluidRow(
            column(
              2,
              fluidRow(
                column(
                  #offset = 6,
                  12,
                  align = "center",
                  numericInput(
                    ns("nSelect"),
                    "nSelect",
                    400,
                    width = "50%",
                    step = 50
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("act_tom"),
                    label = "renderTOMplot",
                    size = "md"
                  )
                )

              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align="center",
                  mod_download_ui(ns("download_pdf_tom"),"PDF","sm")
                )
              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align = "center",
                  attendantBar("progress-bar-tom",
                               striped = T,
                               animated = T,
                               width = "50%")
                )
              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("step5"),
                    label = "Next Step",
                    size = "md"
                  )
                )

              )
            ),
            column(
              5,
              tags$h4("Visualizing the gene network:"),
              plotOutput(
                ns("tom_plot"),
                width = "auto",
                height = 800
              )
            )
          )
        ),#tab4,
        tabPanel(#tab5
          title = "Step 5",
          fluidRow(
            column(
              2,
              fluidRow(
                column(
                  12,
                  align = "center",
                  tags$b(tags$h6("Choose a trait:")),
                  pickerInput(
                    ns("trait_step_d"),
                    label = "",
                    multiple = F,
                    choices = NULL,
                    selected = NULL,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      #title = "",
                      #header = "This is a title",
                      width = "auto",
                      liveSearch = T
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  align = "center",
                  actionBttn(
                    ns("act_plotEN"),
                    label = "plotEigengeneNetworks",
                    size = "md"
                  )
                )

              ),
              tags$br(),
              fluidRow(
                column(
                  12,
                  align="center",
                  mod_download_ui(ns("download_pdf_plotEN"),"PDF","sm")
                )
              )
            ),
            column(
              5,
              tags$h4("Visualizing the network of eigengenes:"),
              plotOutput(
                ns("EN_plot"),
                width = "auto",
                height = 1100
              )
            )
          )
        )#tab5
      )
    )
  )
}

#' page_wgcna Server Functions
#'
#' @noRd
mod_page_wgcna_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # progress bar
    att_net <- Attendant$new("progress-bar-net", hide_on_max = F)
    att_tom <- Attendant$new("progress-bar-tom", hide_on_max = F)

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
      data$expr <- reactive({openxlsx::read.xlsx("./data/WGCNA_LiverFemale3600.xlsx",rowNames = F)})
    })

    output$hot_datExpr <-
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
      data$sampleInfo <- reactive({openxlsx::read.xlsx("./data/WGCNA_ClinicalTraits.xlsx")})
    })

    output$hot_datTrait <-
      rhandsontable::renderRHandsontable({
        if(!is.null(data$sampleInfo())){
          rhandsontable::rhandsontable(data$sampleInfo(),readOnly = F,width = "auto",height = 400)%>%
            rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
        }

      })

    # datExpr0
    datExpr0 <- reactive({datExpr0_func(data$expr())})
    sampleTree =reactive({ hclust(dist(datExpr0()), method = "average")})
    datExpr <- reactive({
      datExpr_func(datExpr0(),
                   sampleTree(),
                   cutHeight = input$cutHeight)
    })



    observe({
      if(!is.null(data$expr())){
        output$sampleTree <- renderImage({
          width  <- session$clientData$`output_page_wgcna_1-sampleTree_width`
          height <- session$clientData$`output_page_wgcna_1-sampleTree_height`


          filename <- sampleTree_plot(sampleTree(),input$cutHeight,"png", width , height)

          list(src = filename,
               contentType = 'image/png',
               width = width,
               height = height,
               alt = "This is alternate text")
          #list(src = filename)
        }, deleteFile = T)
      }
    })

    # Define numbers of genes and samples
    nGenes = reactive({ncol(datExpr())})
    nSamples = reactive({nrow(datExpr())})

    # datTraits
    datTraits <- reactive({
      datTrait(data$sampleInfo(),datExpr())
    })
    # sampleTree2
    sampleTree2 = reactive({hclust(dist(datExpr()), method = "average")})
    # sampleTree2 Plot
    observeEvent(input$act_cutHeight,{
      output$sampleTree2 <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-sampleTree2_width`
        height <- session$clientData$`output_page_wgcna_1-sampleTree2_height`
        filename <- sampleTree2_func(isolate(sampleTree2()),isolate(datTraits()),"png", width , height)
        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text2")
        #list(src = filename)
      }, deleteFile = T)
      sampleTree_pdf <- reactive({
        # width  <- session$clientData$`output_page_wgcna_1-sampleTree2_width`
        # height <- session$clientData$`output_page_wgcna_1-sampleTree2_height`
        sampleTree2_func(isolate(sampleTree2()),isolate(datTraits()),"pdf", 12 , 8)
      })
      # sampleTree_pdf download
      mod_download_server("download_pdf_sapleTree",sampleTree_pdf,"sampleTree","file")
    })

    # to step 2
    observeEvent(input$step2,{
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "taBox",
        "Step 2"

      )
    })

    observeEvent(input$step3,{
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "taBox",
        "Step 3"

      )
    })

    observeEvent(input$step4,{
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "taBox",
        "Step 4"

      )
    })
    observeEvent(input$step5,{
      updateTabsetPanel(
        session = getDefaultReactiveDomain(),
        "taBox",
        "Step 5"

      )
    })

    # power Plot
    observeEvent(input$step2,{
      output$power_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-power_plot_width`
        height <- session$clientData$`output_page_wgcna_1-power_plot_height`
        filename <- power_func(isolate(datExpr()),input$abline,"png", width , height)
        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text4")
        #list(src = filename)
      }, deleteFile = T)
      power_pdf <- reactive({
        # width  <- session$clientData$`output_page_wgcna_1-sampleTree2_width`
        # height <- session$clientData$`output_page_wgcna_1-sampleTree2_height`
        power_func(isolate(datExpr()),input$abline,"pdf", 10 , 10)
      })
      # sampleTree_pdf download
      mod_download_server("download_pdf_power",power_pdf,"Power","file")
    })

    # net
    net <- eventReactive(input$act_power,{
      on.exit(att_net$done())
      att_net$set(1)
      att_net$auto()
      blockwiseModules_func(datExpr(),input$power_value)
    })

    # net plot
    observeEvent(input$act_power,{
      output$net_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-net_plot_width`
        height <- session$clientData$`output_page_wgcna_1-net_plot_height`
        filename <- plotDendroAndColors_plot(net(),"png", width , height)
        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text4")
        #list(src = filename)
      }, deleteFile = T)
      net_pdf <- reactive({
        # width  <- session$clientData$`output_page_wgcna_1-sampleTree2_width`
        # height <- session$clientData$`output_page_wgcna_1-sampleTree2_height`
        plotDendroAndColors_plot(net(),"pdf", 8 , 6)
      })
      # sampleTree_pdf download
      mod_download_server("download_pdf_net",net_pdf,"Network-DendroAndColors","file")
    })

    # moduleLabels
    moduleLabels = reactive({net()$colors})
    moduleColors = reactive({labels2colors(net()$colors)})
    MEs <- reactive({
      MEs0 = moduleEigengenes(datExpr(), moduleColors())$eigengenes
      orderMEs(MEs0)
    })
    observeEvent(input$step3,{
      output$heatmap_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-heatmap_plot_width`
        height <- session$clientData$`output_page_wgcna_1-heatmap_plot_height`
        filename <- labeledHeatmap_plot(MEs(),datTraits(),nSamples(),"png", width , height)
        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text4")
        #list(src = filename)
      }, deleteFile = T)
      heatmap_pdf <- reactive({
        # width  <- session$clientData$`output_page_wgcna_1-sampleTree2_width`
        # height <- session$clientData$`output_page_wgcna_1-sampleTree2_height`
        labeledHeatmap_plot(MEs(),datTraits(),nSamples(),"pdf", 10 , 12)
      })
      # sampleTree_pdf download
      mod_download_server("download_pdf_heatmap",heatmap_pdf,"labeledHeatmap","file")
    })


    # names (colors) of the modules
    modNames = reactive({substring(names(MEs()), 3)})
    # names  of the traits
    traitNames <- reactive({names(datTraits())})


    # geneModuleMembership
    geneModuleMembership = reactive({
      geneModuleMembership <- as.data.frame(cor(datExpr(), MEs(), use = "p"));
      names(geneModuleMembership) = paste("MM.", modNames(), sep="");
      return(geneModuleMembership)
    })
    MMPvalue = reactive({
      MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership()), nSamples()));
      names(MMPvalue) = paste("p.MM.", modNames(), sep="")
      return(MMPvalue)
    })



    # geneTraitSignificance
    geneTraitSignificance =  reactive({
      geneTraitSignificance.all = apply(datTraits(),2,function(col){
        as.data.frame(cor(datExpr(), col, use = "p"))
      })
      geneTraitSignificance <- do.call(cbind,geneTraitSignificance.all ) %>% `colnames<-`(names(geneTraitSignificance.all ))
      names(geneTraitSignificance) = paste("GS.", names(geneTraitSignificance), sep="");
      return(geneTraitSignificance)
    })
    GSPvalue = reactive({
      GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance()), nSamples()));
      names(GSPvalue) = paste("p.GS.", names(GSPvalue), sep="")
      return(GSPvalue)
    })



    observeEvent(input$step3,{
      updatePickerInput(
        session,
        "module",
        #label = NULL,
        #selected = NULL,
        choices = modNames(),
        #choicesOpt = NULL,
        #options = NULL,
        clearOptions = FALSE
      )
      updatePickerInput(
        session,
        "trait",
        #label = NULL,
        #selected = NULL,
        choices = names(datTraits()),
        #choicesOpt = NULL,
        #options = NULL,
        #clearOptions = FALSE
      )
    })

    # scatter Plot
    observeEvent(input$act_scatter,{
      output$scatter_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-scatter_plot_width`
        height <- session$clientData$`output_page_wgcna_1-scatter_plot_height`
        filename <- isolate(
          verboseScatterplot_plot(
            geneModuleMembership(),geneTraitSignificance(),input$module, modNames(),moduleColors(),input$trait,traitNames(),"png", width , width
          )
        )

        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = width,
             alt = "This is alternate text5")
        #list(src = filename)
      }, deleteFile = T)
      scatter_pdf <- isolate(
        reactive({
          verboseScatterplot_plot(
            geneModuleMembership(),geneTraitSignificance(),input$module, modNames(),moduleColors(),input$trait,traitNames(),"pdf", 8 , 8.5
          )
        })
      )
      # sampleTree_pdf download
      mod_download_server("download_pdf_scatter",scatter_pdf,"verboseScatter","file")
    })

    # GS & MM download
    observeEvent(input$step3,{
      output$hot_MM <-
        rhandsontable::renderRHandsontable({
          if(!is.null(geneModuleMembership())){
            rhandsontable::rhandsontable(geneModuleMembership(),readOnly = F,width = "auto",height = 400)%>%
              rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
          }

        })
      output$hot_GS <-
        rhandsontable::renderRHandsontable({
          if(!is.null(geneTraitSignificance())){
            rhandsontable::rhandsontable(geneTraitSignificance(),readOnly = F,width = "auto",height = 400)%>%
              rhandsontable::hot_cols(manualColumnResiz=T,columnSorting=T)
          }

        })
      mod_download_server(
        "download_MM_GS",
        reactive({
          list(
            MM=geneModuleMembership(),
            MMPvalue=MMPvalue(),
            GS=geneTraitSignificance(),
            GSPvalue=GSPvalue()
          )
        }),
        "MM&GS",
        "xlsx",
        rowNames = T
      )
    })

    # TOM plot
    # scatter Plot
    observeEvent(input$act_tom,{
      output$tom_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-tom_plot_width`
        height <- session$clientData$`output_page_wgcna_1-tom_plot_height`
        on.exit(att_tom$done())
        att_tom$set(1)
        att_tom$auto()
        filename <- isolate(
          TOMplot_plot(
            datExpr(),input$power_value,input$nSelect,nGenes(),moduleColors(),"png", width , width
          )
        )

        list(src = filename,
             #contentType = 'image/png',
             width = width,
             height = width,
             alt = "This is alternate text6")
        #list(src = filename)
      }, deleteFile = T)
      tom_pdf <- isolate(
        reactive({
          on.exit(att_tom$done())
          att_tom$set(1)
          att_tom$auto()
          TOMplot_plot(
            datExpr(),input$power_value,input$nSelect,nGenes(),moduleColors(),"pdf", 8 , 8.5
          )
        })
      )
      # sampleTree_pdf download
      mod_download_server("download_pdf_tom",tom_pdf,"TOMPlot","file")
    })

  # EN plot
    observeEvent(input$step5,{
      updatePickerInput(
        session,
        "trait_step_d",
        #label = NULL,
        #selected = NULL,
        choices = names(datTraits()),
        #choicesOpt = NULL,
        #options = NULL,
        #clearOptions = FALSE
      )
    })

    observeEvent(input$act_plotEN,{
      output$EN_plot <- renderImage({
        width  <- session$clientData$`output_page_wgcna_1-EN_plot_width`
        height <- session$clientData$`output_page_wgcna_1-EN_plot_height`
        #filename <- #isolate(
          # plotEigengeneNetworks_plot(
          #   MEs(),datTraits(),input$trait_step_d,"png", width , height
          # )
          # Isolate weight from the clinical traits
          dat = as.data.frame(datTraits()[,input$trait_step_d])
          names(dat) = input$trait_step_d
          # Add the weight to existing module eigengenes
          MET = orderMEs(cbind(MEs(), dat))
          outfile.png <- tempfile(fileext = ".png")
          png(outfile.png,width , height )
          plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2),
                                marHeatmap = c(3,4,1,2), cex.lab = 0.8,
                                xLabelsAngle = 90)
          dev.off()
          outfile.pdf <- tempfile(fileext = ".pdf")
        #)

        list(src = outfile.png,
             #contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text7")

      }, deleteFile = T)
    })

    # EN plot download
    observeEvent(input$act_plotEN,{
      dat = as.data.frame(datTraits()[,input$trait_step_d])
      names(dat) = input$trait_step_d
      # Add the weight to existing module eigengenes
      MET = orderMEs(cbind(MEs(), dat))
      outfile.pdf <- reactive({tempfile(fileext = ".pdf")})
      pdf(outfile.pdf(),8 , 14 )
      plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2),
                            marHeatmap = c(3,4,1,2), cex.lab = 0.8,
                            xLabelsAngle = 90)
      dev.off()
      mod_download_server("download_pdf_plotEN",outfile.pdf,"EigengeneNetworks","file")
    })


    # eample download
    output$dnld_expl_expr <- downloadHandler(
      filename = function() {
        paste("datExpr_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/WGCNA_LiverFemale3600.xlsx",file)
      }
    )

    output$dnld_expl_sampleInfo <- downloadHandler(
      filename = function() {
        paste("datTraits_template_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        file.copy("./data/WGCNA_ClinicalTraits.xlsx",file)
      }
    )


 })
}

## To be copied in the UI
# mod_page_wgcna_ui("page_wgcna_1")

## To be copied in the server
# mod_page_wgcna_server("page_wgcna_1")
