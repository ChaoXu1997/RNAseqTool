library(testthat)
library(plumber)

# Integration tests for the plumber API endpoints
# These tests create a local plumber router and test endpoints directly

test_that("GET /api/health returns ok status", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  res <- pr$route("GET", "/api/health", list())
  expect_equal(res$status, 200)
  expect_equal(res$body$status, "ok")
})

test_that("POST /api/data/demo loads expr_raw data", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  # Change working directory so relative data path works
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  res <- pr$route("POST", "/api/data/demo", list(type = "expr_raw"))
  expect_equal(res$status, 200)
  expect_equal(res$body$status, "ok")
  expect_true("data" %in% names(res$body))
  expect_true("preview" %in% names(res$body$data))
  expect_true("rows" %in% names(res$body$data))
  expect_true("cols" %in% names(res$body$data))
  expect_true(res$body$data$rows > 0)
})

test_that("POST /api/data/demo loads sampleInfo data", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  res <- pr$route("POST", "/api/data/demo", list(type = "sampleInfo"))
  expect_equal(res$status, 200)
  expect_equal(res$body$status, "ok")
  expect_true("data" %in% names(res$body))
})

test_that("POST /api/data/demo returns error for invalid type", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  res <- pr$route("POST", "/api/data/demo", list(type = "invalid_type"))
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})
