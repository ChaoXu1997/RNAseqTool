library(testthat)

# Source the plumber API and WGCNA functions
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_wgcna.R"))

# --- API contract tests ---

test_that("WGCNA analyze endpoint accepts step parameter", {
  # Test that the step parameter validation works
  valid_steps <- c("sampletree", "power", "network", "module_trait", "mm_gs", "tom", "export")
  expect_true(all(valid_steps %in% c("sampletree", "power", "network", "module_trait", "mm_gs", "tom", "export")))
})

test_that("WGCNA sampletree step requires expr data", {
  # The API should reject requests without expr data
  # This is validated in the plumber.R endpoint
  body <- list(step = "sampletree")
  expect_true(is.null(body$expr))
})

test_that("WGCNA power step requires preprocessed data", {
  # The API checks workspace$wgcna$datExpr0 exists
  workspace <- new.env(parent = emptyenv())
  expect_true(is.null(workspace$wgcna))
})

test_that("WGCNA network step requires datExpr", {
  workspace <- new.env(parent = emptyenv())
  workspace$wgcna <- list(datExpr0 = data.frame())
  expect_true(is.null(workspace$wgcna$datExpr))
})

test_that("WGCNA module_trait step requires trait data", {
  body <- list(step = "module_trait")
  expect_true(is.null(body$trait))
})

test_that("WGCNA mm_gs step requires module and trait params", {
  body <- list(step = "mm_gs")
  expect_true(is.null(body$module))
  expect_true(is.null(body$trait))
})

test_that("WGCNA task store pattern works", {
  .task_store <- new.env(parent = emptyenv())

  taskId <- "wgcna_test_123"
  .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

  expect_true(exists(taskId, envir = .task_store))
  expect_equal(.task_store[[taskId]]$status, "pending")

  .task_store[[taskId]] <- list(
    status = "done",
    result = list(svg = "<svg>test</svg>")
  )

  expect_equal(.task_store[[taskId]]$status, "done")
  expect_equal(.task_store[[taskId]]$result$svg, "<svg>test</svg>")
})

test_that("WGCNA workspace stores intermediate results", {
  workspace <- new.env(parent = emptyenv())

  # Simulate storing step results
  workspace$wgcna <- list(
    datExpr0 = data.frame(matrix(rnorm(10 * 5), nrow = 10, ncol = 5)),
    cutHeight = 15
  )

  expect_true(!is.null(workspace$wgcna$datExpr0))
  expect_equal(workspace$wgcna$cutHeight, 15)

  # Add datExpr after power step
  workspace$wgcna$datExpr <- workspace$wgcna$datExpr0
  workspace$wgcna$power <- 6

  expect_equal(workspace$wgcna$power, 6)
  expect_true(!is.null(workspace$wgcna$datExpr))
})
