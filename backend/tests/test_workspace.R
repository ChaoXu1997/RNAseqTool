# Test file for workspace serialization functions
library(testthat)

source("../R/fct_workspace.R")

test_that("get_workspace_status returns empty for empty workspace", {
  ws <- new.env(parent = emptyenv())
  steps <- get_workspace_status(ws)
  expect_equal(steps, character(0))
})

test_that("get_workspace_status detects DESeq2", {
  ws <- new.env(parent = emptyenv())
  ws$deseq2 <- list(result = data.frame(x = 1))
  steps <- get_workspace_status(ws)
  expect_true("DESeq2" %in% steps)
})

test_that("get_workspace_status detects all modules", {
  ws <- new.env(parent = emptyenv())
  ws$deseq2 <- list(data = 1)
  ws$pca <- list(data = 1)
  ws$enrich <- list(data = 1)
  ws$gsea <- list(data = 1)
  ws$genetrend <- list(data = 1)
  ws$wgcna <- list(data = 1)
  
  steps <- get_workspace_status(ws)
  expect_equal(sort(steps), c("DESeq2", "Enrich", "GSEA", "GeneTrend", "PCA", "WGCNA"))
})

test_that("serialize_workspace produces valid RDS", {
  ws <- new.env(parent = emptyenv())
  ws$deseq2 <- list(result = data.frame(gene = c("A", "B"), fc = c(2, 3)))
  
  raw_bytes <- serialize_workspace(ws)
  expect_true(is.raw(raw_bytes))
  expect_true(length(raw_bytes) > 0)
})

test_that("deserialize_workspace restores data correctly", {
  # Setup source workspace
  ws_src <- new.env(parent = emptyenv())
  ws_src$deseq2 <- list(result = data.frame(gene = c("A", "B"), fc = c(2, 3)))
  ws_src$pca <- list(coords = data.frame(PC1 = c(1, 2), PC2 = c(3, 4)))
  
  # Serialize
  raw_bytes <- serialize_workspace(ws_src)
  
  # Deserialize to new workspace
  ws_dest <- new.env(parent = emptyenv())
  result <- deserialize_workspace(raw_bytes, ws_dest)
  
  expect_equal(result$status, "ok")
  expect_true("DESeq2" %in% result$steps)
  expect_true("PCA" %in% result$steps)
  expect_equal(ws_dest$deseq2$result$gene, c("A", "B"))
  expect_equal(ws_dest$pca$coords$PC1, c(1, 2))
})

test_that("deserialize_workspace rejects invalid version", {
  # Create data with wrong version
  data <- list(version = "99.99.99", deseq2 = NULL)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(data, file = tmp)
  raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
  
  ws <- new.env(parent = emptyenv())
  expect_error(
    deserialize_workspace(raw_bytes, ws),
    "Version mismatch"
  )
})

test_that("deserialize_workspace rejects invalid RDS", {
  ws <- new.env(parent = emptyenv())
  raw_bytes <- charToRaw("not a valid RDS file")
  expect_error(
    deserialize_workspace(raw_bytes, ws),
    "Invalid RDS file"
  )
})
