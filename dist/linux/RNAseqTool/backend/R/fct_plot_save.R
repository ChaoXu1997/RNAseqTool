#' Plot Save/Load Functions for RDS serialization
#'
#' @description Functions for saving and loading ggplot objects with parameters
#' @noRd

PLOT_SAVE_VERSION <- "1.0.0"

#' Save ggplot object and parameters to RDS bytes
#'
#' @param module Character string identifying the module (e.g., "pca", "volcano")
#' @param plot_obj ggplot object to save
#' @param params Named list of plot parameters
#' @return Raw vector of RDS data
save_plot_rds <- function(module, plot_obj, params) {
  if (is.null(plot_obj)) {
    stop("No plot object to save")
  }

  data <- list(
    version = PLOT_SAVE_VERSION,
    module = module,
    plot = plot_obj,
    params = params,
    timestamp = Sys.time()
  )

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(data, file = tmp)
  raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
  return(raw_bytes)
}

#' Load ggplot object and parameters from RDS bytes
#'
#' @param raw_bytes Raw vector of RDS data
#' @return List with status, module, plot (ggplot), params, and svg (rendered)
load_plot_rds <- function(raw_bytes) {
  # Write raw bytes to temp file
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  writeBin(raw_bytes, tmp)

  # Load RDS
  data <- tryCatch({
    readRDS(tmp)
  }, error = function(e) {
    stop("Invalid RDS file: ", e$message)
  })

  # Validate structure
  if (is.null(data$plot)) {
    stop("No plot object found in RDS file")
  }

  # Render plot to SVG
  svg_tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(svg_tmp), add = TRUE)
  ggplot2::ggsave(svg_tmp, plot = data$plot, device = "svg", width = 8, height = 6)
  svg_content <- readLines(svg_tmp, warn = FALSE)
  svg_string <- paste(svg_content, collapse = "\n")

  return(list(
    status = "ok",
    module = data$module,
    plot = data$plot,
    params = data$params,
    svg = svg_string
  ))
}
