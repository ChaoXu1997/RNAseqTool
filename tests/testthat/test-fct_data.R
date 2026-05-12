library(testthat)

# Source the helper functions directly for unit tests
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_data.R"))

# --- Unit tests for validate_data ---

test_that("validate_data rejects empty data", {
  df <- data.frame()
  result <- validate_data(df, "expr_raw")
  expect_false(result$valid)
  expect_match(result$message, "empty", ignore.case = TRUE)
})

test_that("validate_data accepts valid raw expression matrix", {
  df <- data.frame(Gene = c("A", "B"), S1 = c(10, 20), S2 = c(30, 40))
  result <- validate_data(df, "expr_raw")
  expect_true(result$valid)
})

test_that("validate_data rejects raw expression with non-numeric values", {
  df <- data.frame(Gene = c("A", "B"), S1 = c("x", "y"))
  result <- validate_data(df, "expr_raw")
  expect_false(result$valid)
  expect_match(result$message, "numeric", ignore.case = TRUE)
})

test_that("validate_data accepts valid sampleInfo", {
  df <- data.frame(Sample = c("S1", "S2"), Group = c("A", "B"))
  result <- validate_data(df, "sampleInfo")
  expect_true(result$valid)
})

test_that("validate_data rejects sampleInfo with missing columns", {
  df <- data.frame(ID = c("S1", "S2"), Condition = c("A", "B"))
  result <- validate_data(df, "sampleInfo")
  expect_false(result$valid)
  expect_match(result$message, "Missing required columns", ignore.case = TRUE)
})

test_that("validate_data accepts valid deseq2_result", {
  df <- data.frame(Gene = c("A", "B"), log2FoldChange = c(2.5, -1.3), padj = c(0.01, 0.05))
  result <- validate_data(df, "deseq2_result")
  expect_true(result$valid)
})

test_that("validate_data rejects deseq2_result with missing columns", {
  df <- data.frame(Gene = c("A", "B"), fc = c(2.5, -1.3))
  result <- validate_data(df, "deseq2_result")
  expect_false(result$valid)
})

test_that("validate_data accepts valid genelist", {
  df <- data.frame(Gene = c("A", "B", "C"))
  result <- validate_data(df, "genelist")
  expect_true(result$valid)
})

test_that("validate_data rejects unknown type", {
  df <- data.frame(x = 1)
  result <- validate_data(df, "unknown_type")
  expect_false(result$valid)
  expect_match(result$message, "Unknown data type", ignore.case = TRUE)
})

# --- Unit tests for get_data_preview ---

test_that("get_data_preview returns correct structure", {
  df <- data.frame(Gene = paste0("G", 1:5), Value = 1:5)
  result <- get_data_preview(df, n = 3)
  expect_true("preview" %in% names(result))
  expect_true("rows" %in% names(result))
  expect_true("cols" %in% names(result))
  expect_equal(result$rows, 5)
  expect_equal(result$cols, 2)
  expect_equal(nrow(result$preview), 3)
})

test_that("get_data_preview limits to 100 rows", {
  df <- data.frame(x = 1:200)
  result <- get_data_preview(df, n = 100)
  expect_equal(nrow(result$preview), 100)
  expect_equal(result$rows, 200)
})

# --- Unit tests for load_demo_data ---

test_that("load_demo_data loads expr_raw from expr_count.rda", {
  data_dir <- file.path(dirname(dirname(dirname(getwd()))), "backend", "data")
  df <- load_demo_data("expr_raw", data_dir)
  expect_true(!is.null(df))
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 1)
})

test_that("load_demo_data loads sampleInfo", {
  data_dir <- file.path(dirname(dirname(dirname(getwd()))), "backend", "data")
  df <- load_demo_data("sampleInfo", data_dir)
  expect_true(!is.null(df))
  expect_true("Sample" %in% colnames(df) | nrow(df) > 0)
})

test_that("load_demo_data errors on unknown type", {
  data_dir <- file.path(dirname(dirname(dirname(getwd()))), "backend", "data")
  expect_error(load_demo_data("unknown", data_dir), "Unknown demo data type")
})

# --- Unit tests for read_uploaded_file ---

test_that("read_uploaded_file reads CSV files", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(A = 1:3, B = c("x", "y", "z")), tmp, row.names = FALSE)
  df <- read_uploaded_file(tmp, "test.csv")
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 2)
  unlink(tmp)
})

test_that("read_uploaded_file errors on unsupported format", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("hello", tmp)
  expect_error(read_uploaded_file(tmp, "test.txt"), "Unsupported file format")
  unlink(tmp)
})
