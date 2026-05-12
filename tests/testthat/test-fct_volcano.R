library(testthat)

# Source Volcano functions directly
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_volcano.R"))

# --- Unit tests for volcano_plot_func ---

test_that("volcano_plot_func returns ggplot object with valid DEGs", {
  degs <- data.frame(
    Gene = paste0("G", 1:10),
    log2FoldChange = c(3, -2, 0.5, 4, -3, 0.1, 2.5, -1.5, 0.8, -0.3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005, 0.8, 0.02, 0.03, 0.6, 0.7),
    change = c("Up", "Down", "Not", "Up", "Down", "Not", "Up", "Down", "Not", "Not")
  )

  p <- volcano_plot_func(degs)

  expect_s3_class(p, "ggplot")
})

test_that("volcano_plot_func accepts custom title", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  p <- volcano_plot_func(degs, title = "My Volcano Plot")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "My Volcano Plot")
})

test_that("volcano_plot_func accepts custom colors", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  p <- volcano_plot_func(degs,
    colr_up = "#FF0000",
    colr_down = "#0000FF",
    colr_not = "#88888833"
  )

  expect_s3_class(p, "ggplot")
})

test_that("volcano_plot_func accepts custom xlim and xbr", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  p <- volcano_plot_func(degs, xlim = c(-5, 5), xbr = 1)

  expect_s3_class(p, "ggplot")
})

test_that("volcano_plot_func filters infinite padj values", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, Inf, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  p <- volcano_plot_func(degs)

  expect_s3_class(p, "ggplot")
})

# --- Unit tests for render_volcano_svg ---

test_that("render_volcano_svg returns SVG string", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )
  params <- list(
    title = "Test Volcano",
    colr_up = "#FC4E2A",
    colr_down = "#4393C3",
    colr_not = "#00000033",
    xlim = c(-10, 10),
    xbr = 5
  )

  svg <- render_volcano_svg(degs, params)

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
})

test_that("render_volcano_svg uses defaults for missing params", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )
  params <- list()

  svg <- render_volcano_svg(degs, params)

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
})

# --- Unit tests for validate_deseq2_data ---

test_that("validate_deseq2_data accepts valid DEGs data", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    baseMean = c(100, 200, 300, 400, 500),
    log2FoldChange = c(3, -2, 0.5, 4, -3),
    lfcSE = c(0.1, 0.2, 0.15, 0.1, 0.2),
    stat = c(10, -8, 2, 15, -10),
    pvalue = c(0.0001, 0.001, 0.1, 0.00001, 0.0005),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  result <- validate_deseq2_data(degs)

  expect_true(result$valid)
  expect_null(result$message)
})

test_that("validate_deseq2_data rejects data missing required columns", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, -2, 0.5, 4, -3)
  )

  result <- validate_deseq2_data(degs)

  expect_false(result$valid)
  expect_true(grepl("Missing", result$message))
})
