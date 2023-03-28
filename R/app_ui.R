#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash shinipsum fontawesome
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "RNAseqTool",
          color = "lightblue",
          href = "https://adminlte.io/themes/v3",
          image = "www/github.png"
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        collapsed = F,
        status = "lightblue",
        minified=T,
        fixed=F,
        compact=F,
        expandOnHover=T,
        sidebarMenu(
          menuItem(
            "Home",
            tabName = "home",
            icon = tags$i(fa("house", fill = "#78e08f"))
          ),
          menuItem(
            "PCA",
            tabName = "pca",
            icon = tags$i(fa("slack", fill = "#78e08f"))
          ),
          menuItem(
            "DEseq2",
            tabName = "DEseq2",
            icon = tags$i(fa("arrow-down-up-across-line",fill = "#78e08f"))
          ),
          menuItem(
            "volcano",
            tabName = "volcano",
            icon = tags$i(fa("chart-area", fill = "#78e08f"))
          ),
          menuItem(
            "normEnrich",
            tabName = "normEnrich",
            icon = tags$i(fa("chart-bar", fill = "#78e08f"))
          ),
          menuItem(
            "GSEA",
            tabName = "gsea",
            icon = tags$i(fa("chart-line", fill = "#78e08f"))
          ),
          menuItem(
            "Gene trend analysis",
            tabName = "geneTrend",
            icon = tags$i(fa("arrow-trend-up", fill = "#78e08f"))
          ),
          menuItem(
            "WGCNA",
            tabName = "wgcna",
            icon = tags$i(fa("vector-square", fill = "#78e08f"))
          )

        )
      ),
      body = bs4Dash::dashboardBody(
          bs4Dash::tabItems(
            tabItem(
              "home",
              shinyWidgets::setBackgroundImage(src = "www/wave.svg", shinydashboard = T),
              mod_page_home_ui("page_home_1")
            ),
            tabItem(
              "pca",
              mod_page_pca_ui("page_pca_1")
            ),
            tabItem(
              "DEseq2",
              mod_page_DEseq2_ui("page_DEseq2_1")

            ),
            tabItem(
              "volcano",
              mod_page_volcano_ui("page_volcano_1")
            ),
            tabItem(
              "normEnrich",
              mod_page_enrich_go_ui("page_enrich_go_1")
            ),
            tabItem(
              "gsea",
              mod_page_gsea_ui("page_gsea_1")
            ),
            tabItem(
              "geneTrend",
              mod_page_geneTrend_ui("page_geneTrend_1")
            ),
            tabItem(
              "wgcna",
              mod_page_wgcna_ui("page_wgcna_1")
            )
          )
      ),
      footer = bs4Dash::dashboardFooter(
        left = div("Developed by xuchao",class="text-primary"),
        right = div(Sys.Date(),class="text-primary")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RNAexprApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
