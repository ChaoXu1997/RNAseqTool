#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_download_ui <- function(id,fileType="PDF",size ="md"){
  ns <- NS(id)
  downloadBttn(
    ns("downloadData"),
    label = paste0("Download ",fileType),
    size = size
  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id,file,name,fileType="pdf",width = 8,height = 6,rowNames = T){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$downloadData <- downloadHandler(
      filename = function() {
        if(fileType != "file"){
          paste(name, "_",Sys.Date(), '.',fileType, sep='')
        }else {
          paste(name, "_",Sys.Date(), '.',"pdf", sep='')
        }
      },
      content = function(con) {
        if(fileType == "pdf"){
          ggplot2::ggsave(file(),filename = con,width = width,height = width)
        }else if(fileType == "Rdata"){
          save(file(),file = con)
        }else if(fileType == "xlsx"){
          openxlsx::write.xlsx(file(),file = con,rowNames =rowNames)
        }else if(fileType == "png"){
          ggplot2::ggsave(file(),filename = con,width = width,height = width)
        }else if(fileType == "file"){
          file.copy(file(),con)
        }
      }
    )




  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")
