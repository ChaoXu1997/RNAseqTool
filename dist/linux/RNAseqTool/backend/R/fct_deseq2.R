#' DESeq2 Analysis Functions
#'
#' @description Functions for differential expression analysis using DESeq2
#' @noRd
#'
#' @import DESeq2

#' Run DESeq2 differential expression analysis
#'
#' @param expr Data frame with Gene column + sample columns (raw counts, integers)
#' @param sampleInfo Data frame with Sample, Group columns
#' @param contrast_pair Character vector of length 2: c("Control", "Treatment")
#' @param fc Fold change threshold (default: 2)
#' @param fdr FDR threshold (default: 0.05)
#' @return Data frame with Gene, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj, change
#' @examples
#' \dontrun{
#'   result <- func_deseq2(expr, sampleInfo, contrast_pair = c("Control", "Treatment"))
#' }
func_deseq2 <- function(expr, sampleInfo, contrast_pair, fc = 2, fdr = 0.05) {
  # Validate inputs
  if (is.null(expr) || nrow(expr) == 0) {
    stop("Expression matrix is empty")
  }
  if (is.null(sampleInfo) || nrow(sampleInfo) == 0) {
    stop("Sample information is empty")
  }
  if (length(contrast_pair) != 2) {
    stop("contrast_pair must be a vector of length 2")
  }
  if (!all(contrast_pair %in% sampleInfo$Group)) {
    stop("contrast_pair values must exist in sampleInfo$Group")
  }

  # Filter samples for the contrast pair
  group_list <- sampleInfo[sampleInfo$Group %in% contrast_pair, ]

  # Prepare expression matrix: set Gene as rownames, filter columns
  expr_done <- as.data.frame(expr) %>%
    tibble::column_to_rownames("Gene") %>%
    .[, colnames(.) %in% group_list$Sample]

  # Create colData for DESeq2
  colData <- data.frame(
    row.names = colnames(expr_done),
    group_list = group_list$Group[match(colnames(expr_done), group_list$Sample)]
  )

  # Run DESeq2
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = expr_done,
    colData = colData,
    design = ~group_list
  )
  dds2 <- DESeq2::DESeq(dds)

  # Get results with contrast
  tmp <- DESeq2::results(dds2, contrast = c("group_list", contrast_pair[2], contrast_pair[1]))

  # Convert to data frame, order by padj
  deg <- as.data.frame(tmp[order(tmp$padj), ]) %>%
    tibble::rownames_to_column("Gene")

  # Remove NA values
  deg <- na.omit(deg)

  # Classify genes based on FC and FDR thresholds
  loc_up <- intersect(
    which(deg$log2FoldChange > log2(fc)),
    which(deg$padj < fdr)
  )
  loc_down <- intersect(
    which(deg$log2FoldChange < -log2(fc)),
    which(deg$padj < fdr)
  )

  deg$change <- "Not"
  deg$change[loc_up] <- "Up"
  deg$change[loc_down] <- "Down"

  return(deg)
}

#' Get all contrast pairs from sample information
#'
#' @param sampleInfo Data frame with Sample, Group columns
#' @return List of contrast pairs (each pair is a character vector of length 2)
get_contrast_pairs <- function(sampleInfo) {
  groups <- unique(sampleInfo$Group)
  if (length(groups) < 2) {
    stop("At least 2 groups are required for DESeq2 analysis")
  }

  # Generate all pairwise combinations
  pairs <- combn(groups, 2, simplify = FALSE)

  # Return as list of character vectors
  lapply(pairs, as.character)
}

#' Run DESeq2 for all contrast pairs
#'
#' @param expr Data frame with Gene column + sample columns (raw counts)
#' @param sampleInfo Data frame with Sample, Group columns
#' @param contrast_pairs List of contrast pairs (optional, auto-generated if NULL)
#' @param fc Fold change threshold (default: 2)
#' @param fdr FDR threshold (default: 0.05)
#' @return List with results per contrast pair and summary
run_deseq2_all_pairs <- function(expr, sampleInfo, contrast_pairs = NULL, fc = 2, fdr = 0.05) {
  # Auto-generate contrast pairs if not provided
  if (is.null(contrast_pairs)) {
    contrast_pairs <- get_contrast_pairs(sampleInfo)
  }

  results <- list()
  errors <- list()

  for (pair in contrast_pairs) {
    pair_name <- paste(pair, collapse = "_vs_")
    tryCatch({
      deg <- func_deseq2(expr, sampleInfo, contrast_pair = pair, fc = fc, fdr = fdr)
      results[[pair_name]] <- deg

      # Log summary
      up_count <- sum(deg$change == "Up")
      down_count <- sum(deg$change == "Down")
      message(sprintf("Contrast %s: %d Up, %d Down DEGs", pair_name, up_count, down_count))
    }, error = function(e) {
      errors[[pair_name]] <- e$message
      warning(sprintf("Failed to run DESeq2 for %s: %s", pair_name, e$message))
    })
  }

  list(
    results = results,
    errors = errors,
    summary = data.frame(
      contrast = names(results),
      up_genes = sapply(results, function(x) sum(x$change == "Up")),
      down_genes = sapply(results, function(x) sum(x$change == "Down")),
      total_genes = sapply(results, nrow)
    )
  )
}
