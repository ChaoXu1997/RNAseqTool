#* @apiTitle RNAseqTool API
#* @apiDescription Backend API for RNAseqTool desktop application

library(plumber)

# Source helper functions
source("R/fct_data.R")

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

#* Get task status
#* @get /api/task/<taskId>
function(taskId) {
  # TODO: implement task status tracking
  list(taskId = taskId, status = "pending")
}

#* PCA analysis
#* @post /api/analyze/pca
function(req) {
  # TODO: implement PCA analysis
  taskId <- paste0("task_", as.integer(Sys.time()))
  list(taskId = taskId)
}

#* DESeq2 analysis
#* @post /api/analyze/deseq2
function(req) {
  # TODO: implement DESeq2 analysis
  taskId <- paste0("task_", as.integer(Sys.time()))
  list(taskId = taskId)
}

#* Generate plot
#* @post /api/plot/<module>
function(req, module) {
  # TODO: implement plot generation
  list(module = module, svg = "<svg></svg>")
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
