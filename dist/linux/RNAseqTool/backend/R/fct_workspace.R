# Workspace serialization/deserialization functions
WORKSPACE_VERSION <- "0.1.0"

#' Get list of completed analysis steps in workspace
#' @param ws workspace environment
#' @return character vector of completed steps
get_workspace_status <- function(ws) {
  steps <- c()
  if (!is.null(ws$deseq2)) steps <- c(steps, "DESeq2")
  if (!is.null(ws$pca)) steps <- c(steps, "PCA")
  if (!is.null(ws$enrich)) steps <- c(steps, "Enrich")
  if (!is.null(ws$gsea)) steps <- c(steps, "GSEA")
  if (!is.null(ws$genetrend)) steps <- c(steps, "GeneTrend")
  if (!is.null(ws$wgcna)) steps <- c(steps, "WGCNA")
  return(steps)
}

#' Serialize workspace to RDS binary
#' @param ws workspace environment
#' @return raw vector of RDS data
serialize_workspace <- function(ws) {
  # Build list of workspace data with version
  data <- list(
    version = WORKSPACE_VERSION,
    timestamp = Sys.time(),
    deseq2 = ws$deseq2,
    pca = ws$pca,
    enrich = ws$enrich,
    gsea = ws$gsea,
    genetrend = ws$genetrend,
    wgcna = ws$wgcna
  )
  
  # Serialize to temp file then read as raw
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(data, file = tmp)
  raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
  return(raw_bytes)
}

#' Deserialize workspace from RDS binary
#' @param raw_bytes raw vector of RDS data
#' @param ws workspace environment to restore into
#' @return list with status, message, and steps
deserialize_workspace <- function(raw_bytes, ws) {
  # Write raw bytes to temp file
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  writeBin(raw_bytes, tmp)
  
  # Load RDS
  data <- tryCatch({
    readRDS(tmp)
  }, error = function(e) {
    stop("Invalid RDS file: ", e$message)
  })
  
  # Validate structure
  if (is.null(data$version)) {
    stop("Invalid workspace: missing version")
  }
  
  # Version compatibility check
  if (data$version != WORKSPACE_VERSION) {
    stop(paste("Version mismatch: workspace is", data$version, "but current version is", WORKSPACE_VERSION))
  }
  
  # Restore workspace contents
  ws$deseq2 <- data$deseq2
  ws$pca <- data$pca
  ws$enrich <- data$enrich
  ws$gsea <- data$gsea
  ws$genetrend <- data$genetrend
  ws$wgcna <- data$wgcna
  
  # Return status
  steps <- get_workspace_status(ws)
  return(list(
    status = "ok",
    message = paste("Workspace loaded with", length(steps), "analyses"),
    steps = steps
  ))
}
