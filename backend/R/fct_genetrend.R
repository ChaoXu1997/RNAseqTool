#' GeneTrend Mfuzz Analysis Functions
#'
#' @description Functions for Mfuzz gene trend clustering analysis
#' @noRd
#'
#' @import Mfuzz
#' @importFrom e1071 cmeans

#' Mfuzz clustering analysis
#'
#' @param data Data frame with Gene column + sample columns (numeric expression values)
#' @param groupInfo Data frame with Sample and Group columns
#' @param c_value Number of clusters (default: 4)
#' @param filterNA Threshold for filtering NA values (default: 0.25)
#' @param fillNA Method for filling NA values: 'mean', 'median', 'knn' (default: 'mean')
#' @param filterSD Minimum standard deviation threshold (default: 0.3)
#' @return List with eset (ExpressionSet) and cl (Mfuzz clustering result)
mfuzz_ana <- function(
  data,
  groupInfo,
  c_value = 4,
  filterNA = 0.25,
  fillNA = "mean",
  filterSD = 0.3
) {
  # Load required library
  library(e1071)

  # Clean and prepare expression data
  # data should have Gene column + sample columns
  expr_raw <- data %>%
    tibble::column_to_rownames("Gene") %>%
    t() %>%
    scale() %>%
    as.data.frame()

  # Reorder samples to match groupInfo
  expr <- expr_raw[match(groupInfo$Sample, rownames(expr_raw)), ]

  # Calculate mean expression per group
  expr_mean <- aggregate(
    expr,
    by = list(groupInfo$Group),
    mean,
    na.rm = TRUE
  ) %>%
    tibble::column_to_rownames("Group.1") %>%
    t()

  # Create ExpressionSet
  eset <- methods::new("ExpressionSet", exprs = as.matrix(expr_mean))

  # Filter NA
  eset <- Mfuzz::filter.NA(eset, thres = filterNA)

  # Fill NA
  eset <- Mfuzz::fill.NA(eset, mode = fillNA)

  # Filter low SD genes
  eset <- Mfuzz::filter.std(eset, min.std = filterSD, visu = FALSE)

  # Calculate fuzzifier parameter
  m_value <- Mfuzz::mestimate(eset)

  # Run Mfuzz clustering
  cl <- Mfuzz::mfuzz(eset, c = c_value, m = m_value)

  return(list(
    eset = eset,
    cl = cl
  ))
}

#' Generate Mfuzz all-clusters trend plot
#'
#' @param eset ExpressionSet from mfuzz_ana
#' @param cl Mfuzz clustering result
#' @param levels Group levels for x-axis ordering
#' @param colors Character vector of colors for clusters
#' @param pointSize Size of points (default: 3)
#' @param lineWidth Width of lines (default: 0.8)
#' @param baseSize Base font size (default: 14)
#' @return ggplot object
mfuzz_all_plot <- function(
  eset,
  cl,
  levels,
  colors = c("#76BA99", "#EB4747", "#996699", "#f1c40f", "#ea8685", "#775039", "#b83570", "#e4b8d5", "#9ebc19"),
  pointSize = 3,
  lineWidth = 0.8,
  baseSize = 14
) {
  # Extract cluster centers
  centers <- as.data.frame(cl$centers)
  centers$Clusters <- paste0("Cluster ", rownames(centers))

  # Melt data for ggplot
  centers_melt <- reshape2::melt(centers)

  # Create plot
  p <- ggplot2::ggplot(
    centers_melt,
    ggplot2::aes(
      x = factor(variable, levels = levels),
      y = value,
      color = Clusters,
      group = Clusters
    )
  ) +
    ggplot2::geom_point(size = pointSize) +
    ggplot2::geom_line(linewidth = lineWidth) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::xlab("") +
    ggplot2::ylab("Expression (Z-score)") +
    ggplot2::theme_classic(base_size = baseSize) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 20, hjust = 0.5),
      legend.position = "right",
      axis.ticks = ggplot2::element_line(linetype = 1, color = "black", size = 1),
      axis.line = ggplot2::element_line(linetype = 1, color = "black", size = 1),
      axis.text = ggplot2::element_text(size = 13, color = "black")
    )

  return(p)
}

#' Generate Mfuzz single cluster plot
#'
#' @param eset ExpressionSet from mfuzz_ana
#' @param cl Mfuzz clustering result
#' @param plotN Cluster number to plot
#' @param levels Group levels for x-axis ordering
#' @param mm_colr_low Low color for membership gradient (default: "#e6ee9c")
#' @param mm_colr_high High color for membership gradient (default: "#dd2c00")
#' @param mm_midpoint Midpoint for color gradient (default: 0.5)
#' @param title_colr Title color (default: "black")
#' @param lineWidth Width of gene lines (default: 0.5)
#' @param center_lineWidth Width of center line (default: 1)
#' @param center_pointSize Size of center points (default: 2)
#' @param baseSize Base font size (default: 14)
#' @return ggplot object
mfuzz_single_plot <- function(
  eset,
  cl,
  plotN = 1,
  levels,
  mm_colr_low = "#e6ee9c",
  mm_colr_high = "#dd2c00",
  mm_midpoint = 0.5,
  title_colr = "black",
  lineWidth = 0.5,
  center_lineWidth = 1,
  center_pointSize = 2,
  baseSize = 14
) {
  # Get genes in this cluster
  genes <- names(cl$cluster)[cl$cluster == plotN]

  # Extract membership values
  mm_sub <- as.data.frame(cl$membership) %>%
    dplyr::select(dplyr::all_of(plotN)) %>%
    tibble::rownames_to_column("gene") %>%
    dplyr::rename(mm = as.character(plotN)) %>%
    dplyr::filter(gene %in% genes)

  # Extract expression data
  expr_mean_sub <- Biobase::exprs(eset) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("gene") %>%
    dplyr::filter(gene %in% genes)

  # Merge and melt
  pdat <- merge(expr_mean_sub, mm_sub) %>%
    reshape2::melt(id.vars = c("mm", "gene"))

  # Extract center data
  centers_melt_sub <- cl$centers %>%
    reshape2::melt() %>%
    dplyr::filter(Var1 == plotN)

  # Create plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = pdat,
      mapping = ggplot2::aes(
        x = factor(variable, levels = levels),
        y = value,
        color = mm,
        group = gene
      ),
      size = lineWidth
    ) +
    ggplot2::scale_colour_gradient2(
      low = mm_colr_low,
      high = mm_colr_high,
      midpoint = mm_midpoint,
      breaks = c(seq(0, 1, 0.2)),
      limits = c(0, 1)
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Expression (Z-score)") +
    ggplot2::geom_point(
      data = centers_melt_sub,
      mapping = ggplot2::aes(
        x = factor(Var2, levels = levels),
        y = value
      ),
      size = center_pointSize
    ) +
    ggplot2::geom_line(
      data = centers_melt_sub,
      mapping = ggplot2::aes(
        x = factor(Var2, levels = levels),
        y = value,
        group = Var1
      ),
      linewidth = center_lineWidth
    ) +
    ggplot2::labs(title = paste0("Cluster ", plotN)) +
    ggplot2::theme_classic(base_size = baseSize) +
    ggplot2::theme(
      legend.position = "right",
      axis.line = ggplot2::element_line(linetype = 1, color = "black", size = 1),
      plot.title = ggplot2::element_text(size = 30, color = title_colr, hjust = 0.5)
    )

  return(p)
}

#' Render Mfuzz all-clusters plot to SVG
#'
#' @param eset ExpressionSet
#' @param cl Mfuzz clustering result
#' @param levels Group levels
#' @param params Named list of plot parameters
#' @return Character string containing SVG
render_genetrend_all_svg <- function(eset, cl, levels, params = list()) {
  colors <- params$colors %||% c("#76BA99", "#EB4747", "#996699", "#f1c40f", "#ea8685", "#775039", "#b83570", "#e4b8d5", "#9ebc19")
  pointSize <- params$pointSize %||% 3
  lineWidth <- params$lineWidth %||% 0.8
  baseSize <- params$baseSize %||% 14

  if (is.list(colors)) {
    colors <- unlist(colors)
  }

  p <- mfuzz_all_plot(
    eset = eset,
    cl = cl,
    levels = levels,
    colors = colors,
    pointSize = pointSize,
    lineWidth = lineWidth,
    baseSize = baseSize
  )

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 10, height = 6)
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Render Mfuzz single cluster plot to SVG
#'
#' @param eset ExpressionSet
#' @param cl Mfuzz clustering result
#' @param plotN Cluster number
#' @param levels Group levels
#' @param params Named list of plot parameters
#' @return Character string containing SVG
render_genetrend_single_svg <- function(eset, cl, plotN, levels, params = list()) {
  mm_colr_low <- params$mm_colr_low %||% "#e6ee9c"
  mm_colr_high <- params$mm_colr_high %||% "#dd2c00"
  mm_midpoint <- params$mm_midpoint %||% 0.5
  baseSize <- params$baseSize %||% 14

  p <- mfuzz_single_plot(
    eset = eset,
    cl = cl,
    plotN = plotN,
    levels = levels,
    mm_colr_low = mm_colr_low,
    mm_colr_high = mm_colr_high,
    mm_midpoint = mm_midpoint,
    baseSize = baseSize
  )

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 8, height = 6)
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Extract cluster membership data
#'
#' @param cl Mfuzz clustering result
#' @return Data frame with Gene, Cluster, Membership columns
extract_cluster_members <- function(cl) {
  cluster_assignments <- cl$cluster
  membership_matrix <- as.data.frame(cl$membership)

  result <- data.frame(
    Gene = names(cluster_assignments),
    Cluster = paste0("Cluster ", cluster_assignments),
    stringsAsFactors = FALSE
  )

  # Add membership score for assigned cluster
  result$Membership <- sapply(seq_len(nrow(result)), function(i) {
    gene <- result$Gene[i]
    cluster <- cluster_assignments[i]
    membership_matrix[gene, cluster]
  })

  return(result)
}
