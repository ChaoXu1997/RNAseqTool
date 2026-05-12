library(testthat)
library(plumber)

# Integration tests for PCA API endpoints

test_that("POST /api/analyze/pca returns task_id", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  # Load demo data to build request
  env <- new.env()
  load(file.path(oldwd, "backend", "data", "expr_norm.rda"), envir = env)
  norm_data <- get(ls(env)[1], envir = env)

  env2 <- new.env()
  load(file.path(oldwd, "backend", "data", "sampleInfo.rda"), envir = env2)
  sample_data <- get(ls(env2)[1], envir = env2)

  body <- list(
    norm = as.data.frame(norm_data),
    sampleInfo = as.data.frame(sample_data)
  )

  res <- pr$route("POST", "/api/analyze/pca", body)
  expect_equal(res$status, 200)
  expect_true("taskId" %in% names(res$body))
})

test_that("POST /api/analyze/pca validates input", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  # Missing norm data
  body <- list(sampleInfo = data.frame(Sample = "S1", Group = "A"))
  res <- pr$route("POST", "/api/analyze/pca", body)
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})

test_that("POST /api/plot/pca returns SVG", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    coordinates = data.frame(
      sample = c("S1", "S2"),
      PC1 = c(-10, 10),
      PC2 = c(-5, 5),
      group = c("A", "B")
    ),
    params = list(
      title = "Test PCA",
      colors = c("red", "blue"),
      ellipse = FALSE,
      labels = FALSE,
      dotSize = 3
    )
  )

  res <- pr$route("POST", "/api/plot/pca", body)
  expect_equal(res$status, 200)
  expect_true("svg" %in% names(res$body))
  expect_true(grepl("<svg", res$body$svg))
})
