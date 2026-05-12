# Test file for plot save/load RDS functions
library(testthat)

source("../R/fct_plot_save.R")

# --- save_plot_rds tests ---

test_that("save_plot_rds returns raw bytes for valid ggplot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  params <- list(title = "Test Plot", colors = c("red", "blue"))

  raw_bytes <- save_plot_rds("pca", p, params)

  expect_true(is.raw(raw_bytes))
  expect_true(length(raw_bytes) > 0)
})

test_that("save_plot_rds includes module in output", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  params <- list(title = "Volcano Test")

  raw_bytes <- save_plot_rds("volcano", p, params)

  # Load back and check module
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  writeBin(raw_bytes, tmp)
  data <- readRDS(tmp)

  expect_equal(data$module, "volcano")
  expect_s3_class(data$plot, "ggplot")
  expect_equal(data$params$title, "Volcano Test")
})

test_that("save_plot_rds preserves all params", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  params <- list(
    title = "My Plot",
    colr_up = "#FF0000",
    colr_down = "#0000FF",
    xlim = c(-10, 10),
    dotSize = 3,
    ellipse = TRUE
  )

  raw_bytes <- save_plot_rds("pca", p, params)

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  writeBin(raw_bytes, tmp)
  data <- readRDS(tmp)

  expect_equal(data$params$title, "My Plot")
  expect_equal(data$params$colr_up, "#FF0000")
  expect_equal(data$params$xlim, list(-10, 10))
  expect_equal(data$params$dotSize, 3)
  expect_true(data$params$ellipse)
})

# --- load_plot_rds tests ---

test_that("load_plot_rds restores ggplot and params", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  params <- list(title = "Test", colors = c("red", "blue"))
  raw_bytes <- save_plot_rds("pca", p, params)

  result <- load_plot_rds(raw_bytes)

  expect_equal(result$status, "ok")
  expect_equal(result$module, "pca")
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$params$title, "Test")
  expect_equal(result$params$colors, c("red", "blue"))
})

test_that("load_plot_rds returns SVG content", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg)) +
    ggplot2::geom_point()

  params <- list(title = "SVG Test")
  raw_bytes <- save_plot_rds("pca", p, params)

  result <- load_plot_rds(raw_bytes)

  expect_true(nchar(result$svg) > 0)
  expect_true(grepl("<svg", result$svg))
})

test_that("load_plot_rds rejects invalid RDS", {
  raw_bytes <- charToRaw("not a valid RDS file")

  expect_error(load_plot_rds(raw_bytes), "Invalid RDS file")
})

test_that("load_plot_rds rejects missing plot", {
  data <- list(module = "pca", params = list(title = "Test"))
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(data, file = tmp)
  raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)

  expect_error(load_plot_rds(raw_bytes), "No plot object")
})

# --- Round-trip test ---

test_that("round-trip save and load preserves data", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(cyl))) +
    ggplot2::geom_point(size = 3) +
    ggplot2::ggtitle("MPG vs Weight")

  params <- list(
    title = "MPG vs Weight",
    dotSize = 3,
    colors = c("red", "green", "blue"),
    xlim = c(-5, 5)
  )

  raw_bytes <- save_plot_rds("pca", p, params)
  result <- load_plot_rds(raw_bytes)

  expect_equal(result$params$title, "MPG vs Weight")
  expect_equal(result$params$dotSize, 3)
  expect_equal(result$params$colors, c("red", "green", "blue"))
})
