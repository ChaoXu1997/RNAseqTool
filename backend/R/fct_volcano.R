#' Volcano Plot Functions
#'
#' @description Functions for generating volcano plots from DESeq2 results
#' @noRd
#'
#' @import ggplot2 tibble

#' Generate volcano plot from DESeq2 results
#'
#' @param DEGs Data frame with Gene, log2FoldChange, padj, change columns
#' @param title Plot title (default: "Volcano Plot")
#' @param colr_up Color for upregulated genes (default: "#FC4E2A")
#' @param colr_down Color for downregulated genes (default: "#4393C3")
#' @param colr_not Color for non-significant genes (default: "#00000033")
#' @param xlim X-axis limits (default: c(-10, 10))
#' @param xbr X-axis break interval (default: 5)
#' @return ggplot object
volcano_plot_func <- function(DEGs,
                              title = "Volcano Plot",
                              colr_up = "#FC4E2A",
                              colr_down = "#4393C3",
                              colr_not = "#00000033",
                              xlim = c(-10, 10),
                              xbr = 5) {
  # Ensure correct column types
  DEGs$log2FoldChange <- as.numeric(DEGs$log2FoldChange)
  DEGs$padj <- as.numeric(DEGs$padj)
  
  # Count Up/Down genes
  change_table <- table(DEGs$change)
  Down <- sum(change_table[names(change_table) == "Down"])
  Up <- sum(change_table[names(change_table) == "Up"])
  text <- stringr::str_glue("Down:{Down} Up:{Up} \n|log2FC|>=1, p-adj<=0.05")

  # Filter out infinite/NA padj values
  deg <- DEGs %>%
    as.data.frame() %>%
    dplyr::filter(!is.na(padj), is.finite(-log10(padj)))

  # Calculate y-axis breaks with safety check
  max_log_padj <- max(-log10(deg$padj), na.rm = TRUE)
  if (!is.finite(max_log_padj) || max_log_padj <= 0) max_log_padj <- 10
  break_y <- max(1, max_log_padj %/% 6)
  max_y <- break_y * 7

  # Build plot
  p <- deg %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(
      x = log2FoldChange, y = -log10(padj),
      color = change, size = abs(log2FoldChange), alpha = .6
    )) +
    ggplot2::scale_color_manual(values = c(colr_down, colr_not, colr_up)) +
    ggplot2::scale_size_continuous(limits = c(0, 5), guide = "none") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 4)), alpha = "none") +
    ggplot2::ggtitle(title) +
    ggplot2::scale_x_continuous(
      limits = c(xlim[1], xlim[2]),
      breaks = seq(xlim[1], xlim[2], xbr)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max_y),
      breaks = seq(0, max_y, break_y)
    ) +
    ggplot2::annotate("text", x = -6, y = max_y, label = text, size = 5) +
    ggplot2::geom_hline(
      yintercept = -log10(0.05),
      linewidth = 0.7,
      color = "grey",
      lty = "dashed"
    ) +
    ggplot2::geom_vline(
      xintercept = c(-log2(2), log2(2)),
      linewidth = 0.7,
      color = "grey",
      lty = "dashed"
    ) +
    ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15, hjust = 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())

  return(p)
}

#' Render volcano plot to SVG string
#'
#' @param DEGs Data frame with Gene, log2FoldChange, padj, change columns
#' @param params Named list of plot parameters
#' @return Character string containing SVG
render_volcano_svg <- function(DEGs, params) {
  title <- params$title %||% "Volcano Plot"
  colr_up <- params$colr_up %||% "#FC4E2A"
  colr_down <- params$colr_down %||% "#4393C3"
  colr_not <- params$colr_not %||% "#00000033"
  xlim <- params$xlim %||% c(-10, 10)
  xbr <- params$xbr %||% 5

  # Unlist if coming from JSON (list -> numeric vector)
  if (is.list(xlim)) xlim <- unlist(xlim)
  xlim <- as.numeric(xlim)

  # Convert to numeric if xlim is a list
  if (is.list(xlim)) {
    xlim <- unlist(xlim)
  }

  p <- volcano_plot_func(
    DEGs = DEGs,
    title = title,
    colr_up = colr_up,
    colr_down = colr_down,
    colr_not = colr_not,
    xlim = xlim,
    xbr = xbr
  )

  # Render to SVG
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  ggplot2::ggsave(tmp, plot = p, device = "svg", width = 8, height = 6)
  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Validate DESeq2 result data frame
#'
#' @param df Data frame to validate
#' @return List with $valid (logical) and $message (character or NULL)
validate_deseq2_data <- function(df) {
  required_cols <- c("Gene", "log2FoldChange", "padj", "change")
  missing_cols <- setdiff(required_cols, colnames(df))

  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }

  list(valid = TRUE, message = NULL)
}

#' Save ggplot object as RData
#'
#' @param plot_obj ggplot object
#' @param file_path Path to save RData file
#' @return Logical indicating success
save_volcano_rdata <- function(plot_obj, file_path) {
  volcano_plot <- plot_obj
  save(volcano_plot, file = file_path)
  TRUE
}
