library(testthat)
library(plumber)

# Integration tests for Volcano API endpoints

test_that("POST /api/plot/volcano returns SVG with DEGs data", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    degs = list(
      Gene = paste0("G", 1:5),
      log2FoldChange = c(3, -2, 0.5, 4, -3),
      padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
      change = c("Up", "Down", "Not", "Up", "Down")
    ),
    params = list(
      title = "Test Volcano",
      colr_up = "#FC4E2A",
      colr_down = "#4393C3",
      colr_not = "#00000033",
      xlim = c(-10, 10),
      xbr = 5
    )
  )

  res <- pr$route("POST", "/api/plot/volcano", body)
  expect_equal(res$status, 200)
  expect_true("svg" %in% names(res$body))
  expect_true(grepl("<svg", res$body$svg))
})

test_that("POST /api/plot/volcano validates input - missing degs", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    params = list(title = "Test")
  )

  res <- pr$route("POST", "/api/plot/volcano", body)
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})

test_that("POST /api/plot/volcano uses defaults for missing params", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    degs = list(
      Gene = paste0("G", 1:5),
      log2FoldChange = c(3, -2, 0.5, 4, -3),
      padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
      change = c("Up", "Down", "Not", "Up", "Down")
    )
  )

  res <- pr$route("POST", "/api/plot/volcano", body)
  expect_equal(res$status, 200)
  expect_true("svg" %in% names(res$body))
  expect_true(grepl("<svg", res$body$svg))
})
