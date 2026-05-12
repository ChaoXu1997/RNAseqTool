#* @apiTitle RNAseqTool API
#* @apiDescription Backend API for RNAseqTool desktop application

library(plumber)

# Source helper functions
source("R/fct_data.R")
source("R/fct_pca.R")
source("R/fct_deseq2.R")

# Path to data directory
DATA_DIR <- "data"

#* Health check
#* @get /api/health
function() {
  list(status = "ok", version = "0.1.0")
}

#* Upload data file
#* @post /api/data/upload
#* @parser multiPart
#* @serializer json
function(req, res) {
  tryCatch({
    # Get uploaded file
    if (is.null(req$postBody) && length(req$files) == 0) {
      res$status <- 400
      return(list(error = "No file uploaded"))
    }

    # Extract file and type from multipart form
    file_info <- req$files[[1]]
    file_path <- file_info$datapath
    file_name <- file_info$name

    # Get data type from form fields
    type <- req$args$type
    if (is.null(type)) {
      # Try to get from post body
      type <- "expr_raw"
    }

    # Read the uploaded file
    df <- read_uploaded_file(file_path, file_name)

    # Validate the data
    validation <- validate_data(df, type)
    if (!validation$valid) {
      res$status <- 400
      return(list(error = validation$message))
    }

    # Get preview
    preview <- get_data_preview(df, n = 100)

    list(
      status = "ok",
      message = "File uploaded successfully",
      data = list(
        preview = preview$preview,
        rows = preview$rows,
        cols = preview$cols
      )
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Upload failed:", e$message))
  })
}

#* Load demo data
#* @post /api/data/demo
#* @serializer json
function(req, res, type = "expr_raw") {
  tryCatch({
    # Validate type
    valid_types <- c("expr_raw", "expr_norm", "sampleInfo", "deseq2_result", "genelist", "trait")
    if (!(type %in% valid_types)) {
      res$status <- 400
      return(list(error = paste("Invalid data type. Must be one of:", paste(valid_types, collapse = ", "))))
    }

    # Load demo data
    df <- load_demo_data(type, DATA_DIR)

    # Convert to data frame if matrix
    if (is.matrix(df)) {
      df <- as.data.frame(df)
    }

    # Get preview
    preview <- get_data_preview(df, n = 100)

    list(
      status = "ok",
      message = paste("Demo data loaded for", type),
      data = list(
        type = type,
        preview = preview$preview,
        rows = preview$rows,
        cols = preview$cols
      )
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Failed to load demo data:", e$message))
  })
}



# In-memory task store (for async polling)
.task_store <- new.env(parent = emptyenv())

# In-memory workspace store (for downstream modules)
workspace <- new.env(parent = emptyenv())

#* PCA analysis
#* @post /api/analyze/pca
#* @serializer json
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Validate inputs
    if (is.null(body$norm)) {
      res$status <- 400
      return(list(error = "Missing 'norm' expression matrix"))
    }
    if (is.null(body$sampleInfo)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data"))
    }

    # Convert to data frames
    norm_df <- as.data.frame(body$norm)
    sample_df <- as.data.frame(body$sampleInfo)

    # Generate task ID
    taskId <- paste0("pca_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

    # Store initial status
    .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

    # Compute PCA synchronously (R is single-threaded, use task store for polling)
    tryCatch({
      group_list <- sample_df$Group
      pca_result <- compute_pca(norm_df, group_list)

      .task_store[[taskId]] <- list(
        status = "done",
        result = list(
          coordinates = pca_result$coordinates,
          variance_explained = pca_result$variance_explained
        )
      )
    }, error = function(e) {
      .task_store[[taskId]] <- list(
        status = "error",
        error = paste("PCA computation failed:", e$message)
      )
    })

    list(taskId = taskId)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Analysis failed:", e$message))
  })
}

#* Get task status
#* @get /api/task/<taskId>
#* @serializer json
function(taskId, res) {
  if (!exists(taskId, envir = .task_store)) {
    res$status <- 404
    return(list(error = "Task not found"))
  }

  task <- .task_store[[taskId]]

  # Serialize coordinates if present
  result <- task$result
  if (!is.null(result) && !is.null(result$coordinates)) {
    result$coordinates <- as.data.frame(result$coordinates)
  }

  # Convert variance_explained to named list for JSON
  if (!is.null(result) && !is.null(result$variance_explained)) {
    result$variance_explained <- as.list(result$variance_explained)
  }

  list(
    taskId = taskId,
    status = task$status,
    result = result,
    error = task$error
  )
}

#* Generate PCA plot
#* @post /api/plot/pca
#* @serializer json
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$coordinates)) {
      res$status <- 400
      return(list(error = "Missing 'coordinates' data"))
    }

    coords <- as.data.frame(body$coordinates)
    params <- body$params %||% list()
    variance <- body$variance_explained

    # Convert variance from list to named numeric vector
    if (!is.null(variance)) {
      variance <- unlist(variance)
    }

    svg_content <- render_pca_svg(coords, params, variance)
    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Plot generation failed:", e$message))
  })
}

#* DESeq2 analysis
#* @post /api/analyze/deseq2
#* @serializer json
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Validate inputs
    if (is.null(body$raw)) {
      res$status <- 400
      return(list(error = "Missing 'raw' expression matrix"))
    }
    if (is.null(body$sampleInfo)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data"))
    }

    # Convert to data frames
    raw_df <- as.data.frame(body$raw)
    sample_df <- as.data.frame(body$sampleInfo)

    # Get parameters with defaults
    fc <- if (!is.null(body$fc)) as.numeric(body$fc) else 2
    fdr <- if (!is.null(body$fdr)) as.numeric(body$fdr) else 0.05

    # Get contrast pairs (auto-generate if not provided)
    contrast_pairs <- NULL
    if (!is.null(body$contrast_pairs) && length(body$contrast_pairs) > 0) {
      contrast_pairs <- body$contrast_pairs
    }

    # Generate task ID
    taskId <- paste0("deseq2_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

    # Store initial status
    .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

    # Run DESeq2 analysis (synchronously, R is single-threaded)
    tryCatch({
      # Auto-generate contrast pairs if not provided
      if (is.null(contrast_pairs)) {
        contrast_pairs <- get_contrast_pairs(sample_df)
      }

      # Run DESeq2 for all pairs
      deseq2_results <- run_deseq2_all_pairs(
        expr = raw_df,
        sampleInfo = sample_df,
        contrast_pairs = contrast_pairs,
        fc = fc,
        fdr = fdr
      )

      # Store results in workspace for downstream modules
      workspace$deseq2 <- deseq2_results$results

      # Prepare result for client
      # Return first contrast result by default, or all results
      all_results <- list()
      for (pair_name in names(deseq2_results$results)) {
        pair_result <- deseq2_results$results[[pair_name]]
        # Convert to list for JSON serialization
        all_results[[pair_name]] <- as.data.frame(pair_result)
      }

      .task_store[[taskId]] <- list(
        status = "done",
        result = list(
          contrasts = all_results,
          summary = deseq2_results$summary,
          parameters = list(fc = fc, fdr = fdr)
        )
      )
    }, error = function(e) {
      .task_store[[taskId]] <- list(
        status = "error",
        error = paste("DESeq2 computation failed:", e$message)
      )
    })

    list(taskId = taskId)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Analysis failed:", e$message))
  })
}

#* Get available contrast pairs
#* @post /api/deseq2/contrasts
#* @serializer json
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$sampleInfo)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data"))
    }

    sample_df <- as.data.frame(body$sampleInfo)
    pairs <- get_contrast_pairs(sample_df)

    # Convert to list of lists for JSON
    pairs_list <- lapply(pairs, function(p) list(group1 = p[1], group2 = p[2]))

    list(
      status = "ok",
      pairs = pairs_list,
      groups = unique(sample_df$Group)
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Failed to get contrasts:", e$message))
  })
}


#* Save workspace
#* @post /api/workspace/save
function(req, res) {
  # TODO: implement workspace save
  res$status <- 200
  list(status = "ok")
}

#* Load workspace
#* @post /api/workspace/load
#* @parser multiPart
function(req, res) {
  # TODO: implement workspace load
  list(status = "ok")
}

#* Export plot
#* @post /api/export/<format>
function(req, format) {
  # TODO: implement export
  list(status = "ok", format = format)
}
