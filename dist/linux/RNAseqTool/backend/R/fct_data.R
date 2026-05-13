#' Data loading and validation functions for the API
#'
#' @description Functions to handle file uploads and demo data loading
#' @noRd

#' Validate uploaded data file format
#'
#' @param df A data frame to validate
#' @param type The expected data type (expr_raw, expr_norm, sampleInfo, deseq2_result, genelist, trait)
#' @return A list with valid (logical) and message (character)
validate_data <- function(df, type) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(valid = FALSE, message = "Data is empty"))
  }

  switch(type,
    "expr_raw" = {
      # First column should be gene names (non-numeric), rest should be numeric
      if (ncol(df) < 2) {
        return(list(valid = FALSE, message = "Expression matrix must have at least 2 columns (gene + samples)"))
      }
      if (!is.numeric(as.matrix(df[, -1, drop = FALSE]))) {
        return(list(valid = FALSE, message = "Expression values must be numeric"))
      }
      list(valid = TRUE, message = "Valid raw expression matrix")
    },
    "expr_norm" = {
      if (ncol(df) < 2) {
        return(list(valid = FALSE, message = "Expression matrix must have at least 2 columns (gene + samples)"))
      }
      if (!is.numeric(as.matrix(df[, -1, drop = FALSE]))) {
        return(list(valid = FALSE, message = "Normalized expression values must be numeric"))
      }
      list(valid = TRUE, message = "Valid normalized expression matrix")
    },
    "sampleInfo" = {
      required_cols <- c("Sample", "Group")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        return(list(valid = FALSE, message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
      }
      list(valid = TRUE, message = "Valid sample information")
    },
    "deseq2_result" = {
      required_cols <- c("Gene", "log2FoldChange", "padj")
      missing_cols <- setdiff(required_cols, colnames(df))
      if (length(missing_cols) > 0) {
        return(list(valid = FALSE, message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))))
      }
      list(valid = TRUE, message = "Valid DESeq2 result")
    },
    "genelist" = {
      if (ncol(df) < 1) {
        return(list(valid = FALSE, message = "Gene list must have at least 1 column"))
      }
      list(valid = TRUE, message = "Valid gene list")
    },
    "trait" = {
      if (ncol(df) < 2) {
        return(list(valid = FALSE, message = "Trait data must have at least 2 columns"))
      }
      list(valid = TRUE, message = "Valid trait data")
    },
    list(valid = FALSE, message = paste("Unknown data type:", type))
  )
}

#' Get data preview (first N rows as list of lists for JSON serialization)
#'
#' @param df A data frame
#' @param n Number of rows to preview
#' @return A list with preview (list of rows), rows (total row count), cols (total col count)
get_data_preview <- function(df, n = 100) {
  preview_df <- head(df, n)
  list(
    preview = as.data.frame(preview_df),
    rows = nrow(df),
    cols = ncol(df)
  )
}

#' Load demo data from rda files
#'
#' @param type The demo data type to load
#' @param data_dir Path to the data directory
#' @return A data frame with the requested demo data
load_demo_data <- function(type, data_dir) {
  file_map <- c(
    "expr_raw" = "expr_count.rda",
    "expr_norm" = "expr_norm.rda",
    "sampleInfo" = "sampleInfo.rda",
    "deseq2_result" = "DEseq2_res.rda",
    "genelist" = "geneList.rda",
    "trait" = "sampleInfo.rda"
  )

  if (!(type %in% names(file_map))) {
    stop(paste("Unknown demo data type:", type))
  }

  rda_file <- file.path(data_dir, file_map[type])
  if (!file.exists(rda_file)) {
    stop(paste("Demo data file not found:", rda_file))
  }

  # Load the rda file and return the first object
  env <- new.env()
  load(rda_file, envir = env)
  obj_names <- ls(env)
  if (length(obj_names) == 0) {
    stop(paste("No objects found in", rda_file))
  }
  get(obj_names[1], envir = env)
}

#' Read uploaded file (CSV or XLSX)
#'
#' @param file_path Path to the uploaded file
#' @param file_name Original file name (used to determine format)
#' @return A data frame
read_uploaded_file <- function(file_path, file_name) {
  ext <- tolower(tools::file_ext(file_name))

  if (ext == "csv") {
    utils::read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("readxl package is required for xlsx files")
    }
    readxl::read_excel(file_path)
  } else {
    stop(paste("Unsupported file format:", ext, ". Please upload CSV or XLSX files."))
  }
}
