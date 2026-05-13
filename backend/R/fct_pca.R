#' PCA Analysis Functions
#'
#' @description Functions for PCA computation and plotting
#' @noRd

#' Compute PCA from expression matrix
#'
#' @param expr_tpm Data frame with Gene column + sample columns (numeric)
#' @param group_list Factor or character vector of group assignments
#' @return List with coordinates (data frame), variance_explained (numeric vector)
compute_pca <- function(expr_tpm, group_list) {
  # Remove duplicate genes, set Gene as rownames
  expr_tpm <- expr_tpm %>%
    dplyr::distinct(Gene, .keep_all = TRUE) %>%
    tibble::column_to_rownames("Gene")

  # Transpose: samples as rows, genes as columns
  t.dat <- as.data.frame(t(expr_tpm)) %>%
    dplyr::select(which(apply(., 2, var) != 0))

  # Run PCA
  data.pca <- prcomp(t.dat, scale. = TRUE)

  # Extract coordinates
  coords <- data.pca$x[, 1:min(5, ncol(data.pca$x))] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample")

  # Add group info
  if (!is.null(group_list)) {
    coords$group <- as.character(group_list)
  }

  # Variance explained per component
  variance_explained <- summary(data.pca)$importance[2, ]

  list(
    coordinates = coords,
    variance_explained = variance_explained
  )
}

#' Generate PCA plot as SVG
#'
#' @param coordinates Data frame from compute_pca with sample, PC1, PC2, group columns
#' @param title Plot title
#' @param colr Character vector of colors per group
#' @param addEllipses Whether to add confidence ellipses
#' @param addLabels Whether to add sample labels
#' @param dotSize Point size
#' @param xlim X-axis limits
#' @param ylim Y-axis limits
#' @param variance_explained Named numeric vector of variance explained per PC
#' @return ggplot object
pca_plot <- function(
    coordinates,
    title = "Principal Component Analysis",
    colr = c("red", "green", "yellow", "purple"),
    addEllipses = TRUE,
    addLabels = TRUE,
    dotSize = 2,
    xlim = c(-150, 150),
    ylim = c(-150, 150),
    variance_explained = NULL
) {
  # Build axis labels with variance explained if available
  xlab <- "PC1"
  ylab <- "PC2"
  if (!is.null(variance_explained)) {
    # Ensure it's a named numeric vector, not a list
    ve <- unlist(variance_explained)
    if ("PC1" %in% names(ve)) {
      xlab <- sprintf("PC1 (%.1f%%)", as.numeric(ve[["PC1"]]) * 100)
    }
    if ("PC2" %in% names(ve)) {
      ylab <- sprintf("PC2 (%.1f%%)", as.numeric(ve[["PC2"]]) * 100)
    }
  }

  p <- ggplot2::ggplot(coordinates, ggplot2::aes(x = PC1, y = PC2, color = group)) +
    ggplot2::geom_point(size = dotSize) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.8, color = "gray") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "gray") +
    ggplot2::scale_color_manual(values = colr) +
    ggplot2::scale_fill_manual(values = colr) +
    ggplot2::theme(legend.direction = "horizontal", legend.position = "top") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_line(colour = NA))

  if (addEllipses) {
    p <- p +
      ggplot2::stat_ellipse(ggplot2::aes(fill = group), geom = "polygon", alpha = 0.1, show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = colr)
  }

  if (addLabels) {
    p <- p +
      ggrepel::geom_text_repel(ggplot2::aes(label = sample), show.legend = FALSE)
  }

  return(p)
}

#' Render PCA plot to SVG string
#'
#' @param coordinates Data frame from compute_pca
#' @param params Named list of plot parameters
#' @param variance_explained Named numeric vector
#' @return Character string containing SVG
render_pca_svg <- function(coordinates, params, variance_explained = NULL) {
  title <- params$title %||% "Principal Component Analysis"
  colr <- params$colors %||% c("red", "green", "yellow", "purple")
  addEllipses <- params$ellipse %||% TRUE
  addLabels <- params$labels %||% TRUE
  dotSize <- params$dotSize %||% 2
  xlim <- params$xlim %||% c(-150, 150)
  ylim <- params$ylim %||% c(-150, 150)

  # Unlist if coming from JSON (list -> numeric vector)
  if (is.list(xlim)) xlim <- unlist(xlim)
  if (is.list(ylim)) ylim <- unlist(ylim)
  xlim <- as.numeric(xlim)
  ylim <- as.numeric(ylim)

  p <- pca_plot(
    coordinates = coordinates,
    title = title,
    colr = colr,
    addEllipses = addEllipses,
    addLabels = addLabels,
    dotSize = dotSize,
    xlim = xlim,
    ylim = ylim,
    variance_explained = variance_explained
  )

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 8, height = 6)
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}
