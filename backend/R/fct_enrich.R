#' Enrichment Analysis Functions
#'
#' @description Functions for gene enrichment analysis using clusterProfiler
#' @noRd
#'
#' @importFrom clusterProfiler enricher
#' @importFrom stringr str_split
#' @importFrom forcats fct_reorder2

#' Shorten long strings to a maximum number of words
#'
#' @param chr Character string to shorten
#' @param max_words Maximum number of words (default: 7)
#' @return Shortened string
short_str <- function(chr, max_words = 7) {
  vchr <- as.vector(stringr::str_split(chr, " ", simplify = TRUE))
  ln <- length(vchr)
  if (ln > max_words) {
    str <- paste0(vchr[1:max_words], collapse = " ")
    str <- paste0(str, "...")
  } else {
    str <- paste0(vchr, collapse = " ")
  }
  return(str)
}

#' Run enrichment analysis on a gene list
#'
#' @param gene Character vector of gene identifiers
#' @param db Data frame with columns: term, symbol/entrezid/ensembl, Database
#' @param gene_type Column name in db to match against ("symbol", "entrezid", or "ensembl")
#' @param p P-value cutoff (default: 0.05)
#' @param padj_method P-value adjustment method (default: "BH")
#' @return Data frame with enrichment results, or NULL if no results
enrich_func <- function(gene, db, gene_type = "symbol", p = 0.05, padj_method = "BH") {
  if (is.null(gene) || length(gene) == 0) {
    return(NULL)
  }

  # Prepare TERM2GENE mapping
  term2gene <- db %>%
    dplyr::select(term, dplyr::all_of(gene_type)) %>%
    `colnames<-`(c("TERM", "GENE")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    as.data.frame()

  # Prepare TERM2NAME mapping (term -> Database description)
  term2name <- db[, c("term", "Database")] %>% unique()

  # Run enrichment
  e_res <- tryCatch({
    clusterProfiler::enricher(
      gene = gene,
      pvalueCutoff = p,
      pAdjustMethod = padj_method,
      TERM2GENE = term2gene,
      TERM2NAME = term2name
    )
  }, error = function(e) {
    warning(paste("Enrichment analysis failed:", e$message))
    return(NULL)
  })

  if (is.null(e_res)) {
    return(NULL)
  }

  # Process results
  result_df <- e_res@result %>%
    dplyr::rename(
      Description = ID,
      Database = Description
    ) %>%
    dplyr::mutate(
      EnrichFactor =
        as.numeric(stringr::str_split(GeneRatio, "/", simplify = TRUE)[, 1]) /
        as.numeric(stringr::str_split(BgRatio, "/", simplify = TRUE)[, 1])
    ) %>%
    dplyr::filter(pvalue <= p) %>%
    tibble::remove_rownames()

  if (nrow(result_df) == 0) {
    return(NULL)
  }

  return(result_df)
}

#' Generate enrichment lollipop plot (bar-like segments with dots)
#'
#' @param eRes Data frame with enrichment results
#' @param showCategory Number of top categories to show (default: 10)
#' @param line_colr Colors for segment lines (default: c("#7FFF00", "#FF82AB", "#63B8FF"))
#' @param dot_colr Colors for dot gradient (default: c("red", "purple"))
#' @param title Plot title (default: "Enrichment Analysis Plot")
#' @param linewidth Line width for segments (default: 1.2)
#' @return ggplot object
enrich_plot_func <- function(
    eRes,
    showCategory = 10,
    line_colr = c("#7FFF00", "#FF82AB", "#63B8FF"),
    dot_colr = c("red", "purple"),
    title = "Enrichment Analysis Plot",
    linewidth = 1.2) {

  if (is.null(eRes) || nrow(eRes) == 0) {
    stop("No enrichment results to plot")
  }

  # Shorten long descriptions
  eRes$Description <- sapply(eRes$Description, short_str)

  # Select top categories
  plot_data <- as.data.frame(eRes) %>%
    dplyr::group_by(Database) %>%
    dplyr::top_n(-showCategory, pvalue) %>%
    dplyr::mutate(Description = forcats::fct_reorder2(Description, Count, EnrichFactor, .desc = FALSE))

  # Build lollipop plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0, y = Description,
        xend = as.numeric(EnrichFactor), yend = Description,
        colour = Database
      ),
      linewidth = linewidth
    ) +
    ggplot2::scale_color_manual(values = line_colr) +
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes(EnrichFactor, Description, color = pvalue, size = Count)
    ) +
    ggplot2::scale_color_gradient(
      low = dot_colr[1], high = dot_colr[2],
      limits = c(0, 0.05), breaks = seq(0, 0.05, 0.01)
    ) +
    ggplot2::scale_size(range = c(4, 8)) +
    ggplot2::labs(x = "EnrichFactor") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw(base_size = 14)

  return(p)
}

#' Generate enrichment bar plot
#'
#' @param eRes Data frame with enrichment results
#' @param showCategory Number of top categories to show (default: 10)
#' @param fill_color Bar fill color (default: "#4DBBD5")
#' @param title Plot title (default: "Enrichment Bar Plot")
#' @return ggplot object
enrich_barplot <- function(
    eRes,
    showCategory = 10,
    fill_color = "#4DBBD5",
    title = "Enrichment Bar Plot") {

  if (is.null(eRes) || nrow(eRes) == 0) {
    stop("No enrichment results to plot")
  }

  # Shorten long descriptions
  eRes$Description <- sapply(eRes$Description, short_str)

  # Select top categories by p-value
  plot_data <- as.data.frame(eRes) %>%
    dplyr::top_n(-showCategory, pvalue) %>%
    dplyr::mutate(Description = forcats::fct_reorder(Description, Count, .desc = FALSE))

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_bar(
      ggplot2::aes(x = Description, y = Count, fill = pvalue),
      stat = "identity"
    ) +
    ggplot2::scale_fill_gradient(
      low = "#FF6347", high = "#4169E1",
      limits = c(0, 0.05), breaks = seq(0, 0.05, 0.01)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "Gene Count", fill = "pvalue") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw(base_size = 14)

  return(p)
}

#' Generate enrichment dot plot
#'
#' @param eRes Data frame with enrichment results
#' @param showCategory Number of top categories to show (default: 10)
#' @param title Plot title (default: "Enrichment Dot Plot")
#' @return ggplot object
enrich_dotplot <- function(
    eRes,
    showCategory = 10,
    title = "Enrichment Dot Plot") {

  if (is.null(eRes) || nrow(eRes) == 0) {
    stop("No enrichment results to plot")
  }

  # Shorten long descriptions
  eRes$Description <- sapply(eRes$Description, short_str)

  # Select top categories
  plot_data <- as.data.frame(eRes) %>%
    dplyr::top_n(-showCategory, pvalue) %>%
    dplyr::mutate(
      Description = forcats::fct_reorder(Description, EnrichFactor, .desc = FALSE),
      GeneRatioNum = as.numeric(stringr::str_split(GeneRatio, "/", simplify = TRUE)[, 1]) /
        as.numeric(stringr::str_split(GeneRatio, "/", simplify = TRUE)[, 2])
    )

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = GeneRatioNum, y = Description,
        color = pvalue, size = Count
      )
    ) +
    ggplot2::scale_color_gradient(
      low = "red", high = "blue",
      limits = c(0, 0.05)
    ) +
    ggplot2::scale_size_continuous(range = c(3, 8)) +
    ggplot2::labs(x = "Gene Ratio", y = "", color = "pvalue", size = "Count") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw(base_size = 14)

  return(p)
}

#' Render enrichment plot to SVG string
#'
#' @param eRes Data frame with enrichment results
#' @param params Named list of plot parameters
#' @return Character string containing SVG
render_enrich_svg <- function(eRes, params) {
  plot_type <- params$plot_type %||% "bar"
  showCategory <- params$showCategory %||% 10
  title <- params$title %||% "Enrichment Analysis"
  line_colr <- params$line_colr %||% c("#7FFF00", "#FF82AB", "#63B8FF")
  dot_colr <- params$dot_colr %||% c("red", "purple")
  fill_color <- params$fill_color %||% "#4DBBD5"
  linewidth <- params$linewidth %||% 1.2

  if (plot_type == "bar") {
    p <- enrich_barplot(eRes, showCategory = showCategory, fill_color = fill_color, title = title)
  } else if (plot_type == "dot") {
    p <- enrich_dotplot(eRes, showCategory = showCategory, title = title)
  } else {
    # Default lollipop plot
    p <- enrich_plot_func(
      eRes,
      showCategory = showCategory,
      line_colr = line_colr,
      dot_colr = dot_colr,
      title = title,
      linewidth = linewidth
    )
  }

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 10, height = max(6, showCategory * 0.5))
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Extract gene list from DESeq2 results
#'
#' @param degs Data frame with Gene and change columns
#' @param direction Direction: "Up", "Down", or "All"
#' @return Character vector of gene names
extract_genes_from_deseq2 <- function(degs, direction = "Up") {
  if (is.null(degs) || nrow(degs) == 0) {
    return(character(0))
  }

  if (direction == "Up") {
    genes <- degs$Gene[degs$change == "Up"]
  } else if (direction == "Down") {
    genes <- degs$Gene[degs$change == "Down"]
  } else {
    # All DEGs
    genes <- degs$Gene[degs$change != "Not"]
  }

  as.character(genes)
}

#' Convert enrichment result to downloadable data frame
#'
#' @param eRes Data frame with enrichment results
#' @return Data frame formatted for download
enrich_to_dataframe <- function(eRes) {
  if (is.null(eRes) || nrow(eRes) == 0) {
    return(data.frame(
      Pathway = character(0),
      P_value = numeric(0),
      Adjusted_P = numeric(0),
      Gene_Count = integer(0),
      Gene_Ratio = character(0),
      Background_Ratio = character(0),
      Genes = character(0),
      Database = character(0),
      Enrich_Factor = numeric(0)
    ))
  }

  data.frame(
    Pathway = eRes$Description,
    P_value = eRes$pvalue,
    Adjusted_P = eRes$p.adjust,
    Gene_Count = eRes$Count,
    Gene_Ratio = eRes$GeneRatio,
    Background_Ratio = eRes$BgRatio,
    Genes = eRes$geneID,
    Database = eRes$Database,
    Enrich_Factor = eRes$EnrichFactor
  )
}
