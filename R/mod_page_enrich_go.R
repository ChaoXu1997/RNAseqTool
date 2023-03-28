#' page_enrich_go UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import waiter


mod_page_enrich_go_ui <- function(id){
  ns <- NS(id)
  tagList(
    spsDepend("shinyCatch"), # optional
  fluidRow(
    column(
      3,
      box(
        id = "box1",
        width = 12,
        title = "Data & Parameters",
        fluidRow(
          column(
            12,
            actionBttn(
              ns("act_expl"),
              label = "Example",
              size = "sm"
            )
          )
        ),
        fluidRow(
          column(
            12,
            textAreaInput(
              ns("gene"),
              label = "Enter the gene here:",
              placeholder = NULL,
              height = 200,
              width = "100%"
            )
          )
        ),
        spsHr(),
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
                  ns("act_enrich"),
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
                textInput(
                  ns("title"),
                  label = "Enter the title:",
                  value = "Enrichment Analysis Plot"
                )
              )
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  ns("showcategory"),
                  "showCategory",
                  value = 10,
                  step = 1

                )
              ),
              column(
                6,
                numericInput(
                  ns("linewidth"),
                  "ajust Pvalue",
                  value = 1.2,
                  step = 0.1
                )
              )
            ),
            tags$b(tags$h5("Dot colors(pvlue):")),
            fluidRow(
              column(
                6,
                align = "center",
                colorPickr(
                  inputId = ns("colr_low"),
                  label ="Low",
                  update = "save",
                  interaction = list(
                    clear = F,
                    save = T
                  ),
                  useAsButton = T,
                  width = "160px",
                  pickr_width = "20%",
                  selected = "#d63031",
                  #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                  swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                )
              ),
              column(
                6,
                align = "center",
                colorPickr(
                  inputId = ns("colr_high"),
                  label ="High",
                  update = "save",
                  interaction = list(
                    clear = F,
                    save = T
                  ),
                  useAsButton = T,
                  width = "160px",
                  pickr_width = "20%",
                  selected = "#b8e994",
                  #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                  swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                )
              )
            ),
            tags$b(tags$h5("line colors(databese):")),
            fluidRow(
              column(
                12,
                align = "center",
                uiOutput(ns("colr_db"))
              )
            ),
            fluidRow(
              column(
                12,
                actionBttn(
                  ns("act_plot"),
                  label = "Click to renderPlot",
                  size = "md"
                ),
                align="center"
              )
            )
          )
        )
      )
    ),
    column(
      9,
      id = ns("col1"),
      fluidRow(
        column(
          12,
          box(
            id = "box2",
            width = 12,
            title = "Plot",
            fluidRow(
              column(
                12,
                align = "right",
                mod_download_ui(ns("download_pdf_enrich"),"PDF","sm")
              )
            ),
            fluidRow(
              column(
                12,
                align = "center",
                plotOutput(
                  ns("plot"),
                  width = 900,
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
            id = "box3",
            width = 12,
            title = "Enrich Results",
            fluidRow(
              column(
                12,
                align = "right",
                mod_download_ui(ns("download_xlsx"),"xlsx","sm")
              )
            ),
            fluidRow(
              column(
                12,
                useAttendant(),
                attendantBar("progress-bar",
                             striped = T,
                             animated = T),
                rhandsontable::rHandsontableOutput(ns("hot_enrich"))
              )
            )
          )
        )
      )
    )

  )
  )
}

#' page_enrich-go Server Functions
#'
#' @noRd
mod_page_enrich_go_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Example
    observeEvent(input$act_expl,{
      gene <- read.table("./data/genes.txt")[,1]
      updateTextAreaInput(
        inputId = "gene" ,
        value =paste(gene,collapse ="\n")
      )
    })



    genes <- reactive({
      genes <-
      stringr::str_trim(
        stringr::str_split_1(
          input$gene,
          "\n"
        ),
        "both"
      )
    genes[!stringr::str_detect(genes,"^$")]
    })

    # example genes
    # observeEvent(input$act_expl,{
    #   updateTextAreaInput(
    #     inputId = "gene",
    #     value  = paste(readRDS("./data/genes_entrezid.rds"),collapse = "\n")
    #   )
    # })




    output$out <- renderPrint({
      #genes <- str_trim(c(letters[1:5],"c ",""),"both")
      head(p())

    })
  # enrich results
    att <- Attendant$new("progress-bar", hide_on_max = F)


    e_result <- eventReactive(input$act_enrich,{
      # db
      att$set(1)
      att$auto()
      db <- msigdbr_func(input$pk_species,c(input$pk_database))
      shinyCatch(
        return(enrich_func(genes(),db,input$pk_gene_type,input$pvalue,input$pmethod)),
        blocking_level = "warning"
      )
    })





    observeEvent(input$act_enrich,{
      output$hot_enrich <- rhandsontable::renderRHandsontable({
        if (!is.null(e_result())) {
          on.exit(att$done())
          rhandsontable::rhandsontable(
            e_result(),
            readOnly = F,
            width = "auto",
            height = 400
          ) %>%
            rhandsontable::hot_cols(manualColumnResiz = T, columnSorting =T)
        }else {
          if (is.null(e_result())){
            on.exit(att$done())
          }
        }
      })

    })

    # update pagination
    observeEvent(input$act_enrich,{
      updatePagination(
        "page",
        selected ="Plot Settings",
        disabled = F
      )
    })

   # render line color ui
    output$colr_db <- renderUI({
        lapply(1:length(input$pk_database),function(n){
         fluidRow(
            tags$div(
              column(
                12,
                align = "center",
                colorPickr(
                  inputId = ns(paste0("db",n)),
                  label =input$pk_database[n],
                  update = "save",
                  interaction = list(
                    clear = F,
                    save = T
                  ),
                  useAsButton = F,
                  width = "auto",
                  pickr_width = "20%",
                  selected = as.character(paletteer::paletteer_d("yarrr::basel"))[n],
                  #as.character(paletteer::paletteer_d("yarrr::basel"))[as.numeric(pos)%%10],
                  swatches = as.character(paletteer::paletteer_d("yarrr::basel"))
                )
              )
            )
          )
        })
    })

    colors_db <- reactive({
      lapply(1:length(input$pk_database),function(n){
        input[[paste0("db",n)]]
      })
    })

    # plot
    p <- eventReactive(input$act_plot,{
      enrich_plot_func(
        eRes=e_result(),
        showCategory = isolate(input$showcategory),
        dot_colr=c(isolate(input$colr_low),isolate(input$colr_high)),
        line_colr=isolate(unlist(colors_db())),
        title= isolate(input$title),
        linewidth=isolate(input$linewidth)
      )
    })

    output$plot <- renderPlot({
      p()
    })


    # download
    mod_download_server("download_pdf_enrich",p,"Enrich","pdf",width = 10,height = 6)
    mod_download_server("download_xlsx",e_result,"Enrich_","xlsx")

  })
}

## To be copied in the UI
# mod_page_enrich_go_ui("page_enrich_go_1")

## To be copied in the server
# mod_page_enrich_go_server("page_enrich_go_1")
