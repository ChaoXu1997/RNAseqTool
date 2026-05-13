#' MSigDB Database Functions
#'
#' @description Functions for accessing MSigDB gene set databases via msigdbr
#' @noRd
#'
#' @importFrom msigdbr msigdbr msigdbr_collections msigdbr_species

#' Get list of available MSigDB databases
#'
#' @return List with databases (character vector) and details (data frame)
get_msigdbr_databases <- function() {
  collections <- msigdbr::msigdbr_collections()

  # Format database names as "Category:Subcategory" for display
  db_names <- character(nrow(collections))
  for (i in seq_len(nrow(collections))) {
    cat <- collections$gs_cat[i]
    subcat <- collections$gs_subcat[i]
    if (is.na(subcat) || subcat == "") {
      db_names[i] <- cat
    } else {
      db_names[i] <- paste0(cat, ":", subcat)
    }
  }

  list(
    databases = db_names,
    details = as.data.frame(collections)
  )
}

#' Get list of available species in msigdbr
#'
#' @return Character vector of species names
get_msigdbr_species <- function() {
  as.character(msigdbr::msigdbr_species()$species_name)
}

#' Fetch gene set database from msigdbr
#'
#' @param species Species name (default: "Homo sapiens")
#' @param database Database identifier(s), e.g. "C5:GO:BP" or c("C5:GO:BP", "C5:GO:CC")
#' @return Data frame with columns: term, symbol, entrezid, ensembl, Database
msigdbr_func <- function(species = "Homo sapiens", database = "C5:GO:BP") {
  purrr::map_dfr(as.list(database), function(db_str) {
    # Parse database string "C5:GO:BP" -> gs_cat="C5", gs_subcat="GO:BP"
    parts <- strsplit(db_str, ":")[[1]]
    gs_cat <- parts[1]
    gs_subcat <- if (length(parts) >= 2) paste(parts[-1], collapse = ":") else NA_character_

    # Get display name
    if (is.na(gs_subcat)) {
      db_display <- gs_cat
    } else {
      db_display <- paste0(gs_cat, ":", gs_subcat)
    }

    # Fetch from msigdbr
    result <- msigdbr::msigdbr(species = species, category = gs_cat, subcategory = gs_subcat)
    result <- result %>%
      dplyr::mutate(Database = db_display)

    result %>%
      dplyr::select(gs_name, gene_symbol, entrez_gene, ensembl_gene, Database) %>%
      `colnames<-`(c("term", "symbol", "entrezid", "ensembl", "Database")) %>%
      dplyr::mutate(
        # Strip common prefixes from term names
        term = gsub("^BIOCARTA_|^KEGG_|^PID_|^REACTOME_|^WP_|^GOBP_|^GOCC_|^GOMF_|^HP|^HALLMARK_", "", term),
        term = stringr::str_to_lower(term),
        term = gsub("_", " ", term)
      )
  })
}
