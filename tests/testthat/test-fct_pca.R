library(testthat)

# Source PCA functions directly
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_pca.R"))

# --- Unit tests for compute_pca ---

test_that("compute_pca returns correct structure", {
  expr_tpm <- data.frame(
    Gene = c("G1", "G2", "G3", "G4", "G5"),
    S1 = c(10, 20, 30, 40, 50),
    S2 = c(15, 25, 35, 45, 55),
    S3 = c(12, 22, 32, 42, 52)
  )
  group_list <- c("A", "A", "B")
  result <- compute_pca(expr_tpm, group_list)

  expect_true("coordinates" %in% names(result))
  expect_true("variance_explained" %in% names(result))
  expect_true("sample" %in% colnames(result$coordinates))
  expect_true("PC1" %in% colnames(result$coordinates))
  expect_true("PC2" %in% colnames(result$coordinates))
  expect_true("group" %in% colnames(result$coordinates))
  expect_equal(nrow(result$coordinates), 3)
})

test_that("compute_pca handles duplicate genes by keeping first", {
  expr_tpm <- data.frame(
    Gene = c("G1", "G1", "G2", "G3"),
    S1 = c(10, 11, 20, 30),
    S2 = c(15, 16, 25, 35)
  )
  group_list <- c("A", "B")
  result <- compute_pca(expr_tpm, group_list)

  # Should have 2 samples after dedup of genes
  expect_equal(nrow(result$coordinates), 2)
})

test_that("compute_pca removes zero-variance genes", {
  expr_tpm <- data.frame(
    Gene = c("G1", "G2", "G3"),
    S1 = c(10, 20, 30),
    S2 = c(10, 25, 35),
    S3 = c(10, 22, 32)
  )
  # G1 has zero variance across samples
  group_list <- c("A", "A", "B")
  result <- compute_pca(expr_tpm, group_list)

  # Should still work, removing G1
  expect_equal(nrow(result$coordinates), 3)
  expect_true(length(result$variance_explained) >= 2)
})

test_that("compute_pca variance_explained is named with PC labels", {
  expr_tpm <- data.frame(
    Gene = paste0("G", 1:10),
    S1 = runif(10, 1, 100),
    S2 = runif(10, 1, 100),
    S3 = runif(10, 1, 100),
    S4 = runif(10, 1, 100)
  )
  group_list <- c("A", "A", "B", "B")
  result <- compute_pca(expr_tpm, group_list)

  expect_true("PC1" %in% names(result$variance_explained))
  expect_true("PC2" %in% names(result$variance_explained))
  expect_true(result$variance_explained["PC1"] > 0)
  expect_true(result$variance_explained["PC1"] <= 1)
})

# --- Unit tests for pca_plot ---

test_that("pca_plot returns ggplot object", {
  coords <- data.frame(
    sample = c("S1", "S2", "S3", "S4"),
    PC1 = c(-50, -30, 40, 60),
    PC2 = c(-20, 10, -30, 50),
    group = c("A", "A", "B", "B")
  )
  p <- pca_plot(coords, colr = c("red", "blue"))

  expect_s3_class(p, "ggplot")
})

test_that("pca_plot accepts custom title", {
  coords <- data.frame(
    sample = c("S1", "S2"),
    PC1 = c(-10, 10),
    PC2 = c(-5, 5),
    group = c("A", "B")
  )
  p <- pca_plot(coords, title = "My Custom PCA", colr = c("red", "blue"))

  expect_s3_class(p, "ggplot")
})

# --- Unit tests for render_pca_svg ---

test_that("render_pca_svg returns SVG string", {
  coords <- data.frame(
    sample = c("S1", "S2"),
    PC1 = c(-10, 10),
    PC2 = c(-5, 5),
    group = c("A", "B")
  )
  params <- list(
    title = "Test PCA",
    colors = c("red", "blue"),
    ellipse = FALSE,
    labels = FALSE,
    dotSize = 3
  )
  svg <- render_pca_svg(coords, params, variance_explained = c(PC1 = 0.8, PC2 = 0.15))

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
})

test_that("render_pca_svg uses defaults for missing params", {
  coords <- data.frame(
    sample = c("S1", "S2"),
    PC1 = c(-10, 10),
    PC2 = c(-5, 5),
    group = c("A", "B")
  )
  params <- list()
  svg <- render_pca_svg(coords, params)

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
})
