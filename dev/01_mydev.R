# shiny app prototyping
# remotes::install_github("Thinkr-open/shinipsum")
library(shinipsum)

golem::use_recommended_deps(
  pkg =golem:: get_golem_wd(),
  recommended = c(
    "shiny", "DT", "attempt", "glue", "htmltools", "golem",
    "bs4Dash"
  )
)
attachment::att_amend_desc()



# add dependent packages
usethis::use_package("bs4Dash")
usethis::use_package("shinyWidgets")
usethis::use_package("tidyverse")
usethis::use_package("slickR")
usethis::use_package("spsComps")
usethis::use_package("shinipsum")
usethis::use_package("rhandsontable")
usethis::use_package("tibble")

# go ui file
rstudioapi::navigateToFile("R/app_ui.R")

# go server file
rstudioapi::navigateToFile("R/app_server.R")


# add raw data
usethis::use_data_raw("raw_data")



# add fun
golem::add_fct("see_example")
golem::add_fct("PCA_plot")
golem::add_fct("test")
golem::add_fct("DESeq2")
golem::add_fct("Volcano")
golem::add_fct("msigdbr")
golem::add_fct("enrich")
golem::add_fct("gsea")
golem::add_fct("geneTrend")
golem::add_fct("WGCNA")


# add module
golem::add_module("page_home")
golem::add_module("page_pca")
golem::add_module("colorPickr")
golem::add_module("download")
golem::add_module("page_DEseq2")
golem::add_module("rev")
golem::add_module("page_volcano")
golem::add_module("page_enrich_go")
golem::add_module("page_gsea")
golem::add_module("page_geneTrend")
golem::add_module("page_wgcna")



# Remove current favicon
golem::remove_favicon()
# Add a new one
golem::use_favicon(path = "README/hex-RNATool.png")










golem::run_dev()




devtools::install_github("ChaoXu1997/RNAseqTool")
RNAseqTool::run_app()





# install local
remotes::install_local()



# instal shiny server
golem::add_shinyserver_file()

