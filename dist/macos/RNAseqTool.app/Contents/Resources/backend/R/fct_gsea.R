#' GSEA Analysis Functions
#'
#' @description Functions for Gene Set Enrichment Analysis using clusterProfiler
#' @noRd
#'
#' @importFrom clusterProfiler GSEA
#' @importFrom GseaVis gseaNb

#' Run GSEA analysis on a ranked gene list
#'
#' @param gene Named numeric vector (gene -> log2FC), sorted decreasing
#' @param db Data frame with columns: term, symbol/entrezid/ensembl, Database
#' @param gene_type Column name in db to match against ("symbol", "entrezid", or "ensembl")
#' @param pvalue P-value cutoff (default: 0.05)
#' @param pAdjustMethod P-value adjustment method (default: "BH")
#' @return GSEA result object, or NULL if no results
gsea_func <- function(gene, db, gene_type = "symbol", pvalue = 0.05, pAdjustMethod = "BH") {
  if (is.null(gene) || length(gene) == 0) {
    return(NULL)
  }

  # Ensure gene is a named numeric vector
  if (is.null(names(gene))) {
    stop("gene must be a named numeric vector")
  }

  # Remove NA values
  gene <- gene[!is.na(gene)]

  if (length(gene) == 0) {
    return(NULL)
  }

  # Ensure gene is sorted decreasing
  gene <- sort(gene, decreasing = TRUE)

  # Prepare TERM2GENE mapping
  term2gene <- db %>%
    dplyr::select(term, dplyr::all_of(gene_type)) %>%
    `colnames<-`(c("TERM", "GENE")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    as.data.frame()

  # Run GSEA
  e_res <- tryCatch({
    clusterProfiler::GSEA(
      geneList = gene,
      exponent = 1,
      minGSSize = 10,
      maxGSSize = 500,
      eps = 1e-10,
      pvalueCutoff = pvalue,
      pAdjustMethod = pAdjustMethod,
      gson = NULL,
      TERM2GENE = term2gene,
      verbose = FALSE,
      seed = FALSE
    )
  }, error = function(e) {
    warning(paste("GSEA analysis failed:", e$message))
    return(NULL)
  })

  return(e_res)
}

#' Build geneList from DESeq2 results
#'
#' @param degs Data frame with Gene and log2FoldChange columns
#' @return Named numeric vector sorted decreasing
build_genelist_from_deseq2 <- function(degs) {
  if (is.null(degs) || nrow(degs) == 0) {
    return(numeric(0))
  }

  # Extract gene names and log2FC
  gene_names <- as.character(degs$Gene)
  log2fc <- as.numeric(degs$log2FoldChange)

  # Remove NA values
  valid_idx <- !is.na(log2fc)
  gene_names <- gene_names[valid_idx]
  log2fc <- log2fc[valid_idx]

  # Create named vector
  geneList <- stats::setNames(log2fc, gene_names)

  # Sort decreasing
  geneList <- sort(geneList, decreasing = TRUE)

  return(geneList)
}

#' Generate GSEA plot using GseaVis
#'
#' @param object GSEA result object
#' @param geneSetID Gene set ID to plot
#' @param addGene Genes to highlight (character vector)
#' @param addPval Whether to add p-value (default: TRUE)
#' @param pvalSize P-value text size (default: 4)
#' @param pvalX P-value x position (default: 1)
#' @param pvalY P-value y position (default: 1)
#' @param subPlot Subplot type: 1, 2, or 3 (default: 3)
#' @param termWidth Term name width (default: 40)
#' @param base_size Base font size (default: 12)
#' @param arrowType Arrow type: 'open' or 'closed' (default: 'open')
#' @param geneCol Gene highlight color (default: 'red')
#' @param markTopgene Whether to mark top genes (default: FALSE)
#' @param topGeneN Number of top genes to mark (default: 1)
#' @param rankSeq Rank sequence length (default: 2000)
#' @param htCol Heatmap colors (default: c("#08519C", "#A50F15"))
#' @param rankCol Rank colors (default: c("#08519C", "white", "#A50F15"))
#' @param curveCol Curve colors (default: green/red/purple palette)
#' @param newGsea Whether to use new GSEA style (default: FALSE)
#' @param pCol P-value color (default: 'black')
#' @param ncol Number of columns for multiple plots (default: 1)
#' @param newCurveCol New curve colors (default: c("#336699", "white", "#993399"))
#' @param newHtCol New heatmap colors (default: c("#336699", "white", "#993399"))
#' @param addPoin Whether to add points (default: TRUE)
#' @return ggplot object or NULL
gsea_plot_func <- function(
    object = NULL,
    geneSetID = NULL,
    addGene = NULL,
    addPval = TRUE,
    pvalSize = 4,
    pvalX = 1,
    pvalY = 1,
    subPlot = 3,
    termWidth = 40,
    base_size = 12,
    arrowType = 'open',
    geneCol = 'red',
    markTopgene = FALSE,
    topGeneN = 1,
    rankSeq = 2000,
    htCol = c("#08519C", "#A50F15"),
    rankCol = c("#08519C", "white", "#A50F15"),
    curveCol = c("#76BA99", "#EB4747", "#996699", "#f1c40f", "#ea8685", "#775039", "#b83570", "#e4b8d5", "#9ebc19"),
    newGsea = FALSE,
    pCol = 'black',
    ncol = 1,
    newCurveCol = c("#336699", "white", "#993399"),
    newHtCol = c("#336699", "white", "#993399"),
    addPoin = TRUE
) {
  if (is.null(object)) {
    stop("GSEA result object is required")
  }

  if (is.null(geneSetID)) {
    stop("geneSetID is required")
  }

  # Check if geneSetID exists in results
  if (!geneSetID %in% object@result$ID) {
    stop(paste("geneSetID", geneSetID, "not found in GSEA results"))
  }

  p <- tryCatch({
    GseaVis::gseaNb(
      object = object,
      geneSetID = geneSetID,
      addGene = addGene,
      addPval = addPval,
      pvalSize = pvalSize,
      pvalX = pvalX,
      pvalY = pvalY,
      subPlot = subPlot,
      termWidth = termWidth,
      base_size = base_size,
      arrowType = arrowType,
      geneCol = geneCol,
      markTopgene = markTopgene,
      topGeneN = topGeneN,
      rankSeq = rankSeq,
      htCol = htCol,
      rankCol = rankCol,
      curveCol = curveCol,
      newGsea = newGsea,
      pCol = pCol,
      pHjust = 1,
      ncol = ncol,
      newCurveCol = newCurveCol,
      newHtCol = newHtCol,
      addPoin = addPoin
    )
  }, error = function(e) {
    warning(paste("GSEA plot generation failed:", e$message))
    return(NULL)
  })

  return(p)
}

#' Render GSEA plot to SVG string
#'
#' @param gsea_result GSEA result object
#' @param params Named list of plot parameters
#' @return Character string containing SVG
render_gsea_svg <- function(gsea_result, params) {
  geneSetID <- params$geneSetID
  if (is.null(geneSetID)) {
    stop("geneSetID is required for plotting")
  }

  # Extract plot parameters with defaults
  addGene <- params$addGene
  addPval <- params$addPval %||% TRUE
  pvalSize <- params$pvalSize %||% 4
  pvalX <- params$pvalX %||% 1
  pvalY <- params$pvalY %||% 1
  subPlot <- params$subPlot %||% 3
  termWidth <- params$termWidth %||% 40
  base_size <- params$base_size %||% 12
  arrowType <- params$arrowType %||% 'open'
  geneCol <- params$geneCol %||% 'red'
  markTopgene <- params$markTopgene %||% FALSE
  topGeneN <- params$topGeneN %||% 1
  rankSeq <- params$rankSeq %||% 2000
  newGsea <- params$newGsea %||% FALSE
  pCol <- params$pCol %||% 'black'
  ncol <- params$ncol %||% 1
  addPoin <- params$addPoin %||% TRUE

  # Handle color parameters
  htCol <- params$htCol
  if (is.null(htCol)) {
    htCol <- c("#08519C", "#A50F15")
  } else if (is.character(htCol) && length(htCol) == 1) {
    htCol <- strsplit(htCol, ",")[[1]]
  }

  rankCol <- params$rankCol
  if (is.null(rankCol)) {
    rankCol <- c("#08519C", "white", "#A50F15")
  } else if (is.character(rankCol) && length(rankCol) == 1) {
    rankCol <- strsplit(rankCol, ",")[[1]]
  }

  curveCol <- params$curveCol
  if (is.null(curveCol)) {
    curveCol <- c("#76BA99", "#EB4747", "#996699", "#f1c40f", "#ea8685", "#775039", "#b83570", "#e4b8d5", "#9ebc19")
  } else if (is.character(curveCol) && length(curveCol) == 1) {
    curveCol <- strsplit(curveCol, ",")[[1]]
  }

  newCurveCol <- params$newCurveCol
  if (is.null(newCurveCol)) {
    newCurveCol <- c("#336699", "white", "#993399")
  } else if (is.character(newCurveCol) && length(newCurveCol) == 1) {
    newCurveCol <- strsplit(newCurveCol, ",")[[1]]
  }

  newHtCol <- params$newHtCol
  if (is.null(newHtCol)) {
    newHtCol <- c("#336699", "white", "#993399")
  } else if (is.character(newHtCol) && length(newHtCol) == 1) {
    newHtCol <- strsplit(newHtCol, ",")[[1]]
  }

  # Generate plot
  p <- gsea_plot_func(
    object = gsea_result,
    geneSetID = geneSetID,
    addGene = addGene,
    addPval = addPval,
    pvalSize = pvalSize,
    pvalX = pvalX,
    pvalY = pvalY,
    subPlot = subPlot,
    termWidth = termWidth,
    base_size = base_size,
    arrowType = arrowType,
    geneCol = geneCol,
    markTopgene = markTopgene,
    topGeneN = topGeneN,
    rankSeq = rankSeq,
    htCol = htCol,
    rankCol = rankCol,
    curveCol = curveCol,
    newGsea = newGsea,
    pCol = pCol,
    ncol = ncol,
    newCurveCol = newCurveCol,
    newHtCol = newHtCol,
    addPoin = addPoin
  )

  if (is.null(p)) {
    stop("Failed to generate GSEA plot")
  }

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 10, height = 8)
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Convert GSEA result to downloadable data frame
#'
#' @param gsea_result_df Data frame from GSEA result (gsea_result@result)
#' @return Data frame formatted for download
gsea_to_dataframe <- function(gsea_result_df) {
  if (is.null(gsea_result_df) || nrow(gsea_result_df) == 0) {
    return(data.frame(
      Pathway = character(0),
      P_value = numeric(0),
      Adjusted_P = numeric(0),
      NES = numeric(0),
      Enrichment_Score = numeric(0),
      Set_Size = integer(0),
      Rank = integer(0),
      Leading_Edge = character(0),
      Core_Enrichment = character(0)
    ))
  }

  data.frame(
    Pathway = gsea_result_df$Description,
    P_value = gsea_result_df$pvalue,
    Adjusted_P = gsea_result_df$p.adjust,
    NES = gsea_result_df$NES,
    Enrichment_Score = gsea_result_df$enrichmentScore,
    Set_Size = gsea_result_df$setSize,
    Rank = gsea_result_df$rank,
    Leading_Edge = gsea_result_df$leading_edge,
    Core_Enrichment = gsea_result_df$core_enrichment
  )
}

#' Save GSEA plot as RData file
#'
#' @param plot_obj ggplot object
#' @param file_path Path to save the RData file
save_gsea_rdata <- function(plot_obj, file_path) {
  if (is.null(plot_obj)) {
    stop("No plot object to save")
  }
  save(plot_obj, file = file_path)
}

#' Reconstruct GSEA result object from data frame
#'
#' @param result_df Data frame with GSEA results
#' @param geneList Named numeric vector used for GSEA
#' @return GSEA result object (mock)
reconstruct_gsea_result <- function(result_df, geneList = NULL) {
  # Create a mock GSEA result-like object for plotting
  # Note: This is a simplified version. Full reconstruction would need the original GSEA object.
  list(
    result = result_df,
    geneList = geneList
  )
}
