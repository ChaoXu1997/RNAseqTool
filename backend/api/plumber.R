#* @apiTitle RNAseqTool API
#* @apiDescription Backend API for RNAseqTool desktop application

library(plumber)

# Helper: convert JSON list-of-objects to data.frame (row-wise, not column-wise)
# as.data.frame(list(obj1, obj2, ...)) binds as columns; we need rows
list_to_df <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.matrix(x)) return(as.data.frame(x))
  if (is.list(x) && length(x) > 0 && is.list(x[[1]])) {
    # list of row-objects -> bind as rows
    return(dplyr::bind_rows(x))
  }
  as.data.frame(x)
}

# Source helper functions - use relative path from api/ to R/
source("../R/fct_data.R")
source("../R/fct_pca.R")
source("../R/fct_deseq2.R")
source("../R/fct_volcano.R")
source("../R/fct_enrich.R")
source("../R/fct_msigdbr.R")
source("../R/fct_gsea.R")
source("../R/fct_genetrend.R")
source("../R/fct_wgcna.R")
source("../R/fct_workspace.R")
source("../R/fct_plot_save.R")

# Path to data directory
DATA_DIR <- "../data"

#* Health check
#* @get /api/health
#* @serializer unboxedJSON
function() {
  list(status = "ok", version = "0.1.0")
}

#* Upload data file
#* @post /api/data/upload
#* @parser multi
#* @serializer unboxedJSON
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
#* @serializer unboxedJSON
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

    # Store in workspace for downstream modules
    workspace[[type]] <- df

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
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get data from request body or workspace
    # Frontend sends DataFile metadata, not actual data - check if it's valid
    norm_data <- NULL
    sample_data <- NULL

    # Only use body data if it has actual content (not just metadata)
    if (!is.null(body$norm) && is.data.frame(body$norm)) {
      norm_data <- body$norm
    }
    if (!is.null(body$sampleInfo) && is.data.frame(body$sampleInfo)) {
      sample_data <- body$sampleInfo
    }

    # Fall back to workspace
    if (is.null(norm_data) && exists("expr_norm", envir = workspace)) {
      norm_data <- workspace[["expr_norm"]]
    }
    if (is.null(sample_data) && exists("sampleInfo", envir = workspace)) {
      sample_data <- workspace[["sampleInfo"]]
    }

    # Validate inputs
    if (is.null(norm_data)) {
      res$status <- 400
      return(list(error = "Missing 'norm' expression matrix. Load demo data first."))
    }
    if (is.null(sample_data)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data. Load demo data first."))
    }

    # Convert to data frames
    norm_df <- list_to_df(norm_data)
    sample_df <- list_to_df(sample_data)

    # Generate task ID
    taskId <- paste0("pca_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

    # Store initial status
    .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

    # Compute PCA synchronously (R is single-threaded, use task store for polling)
    tryCatch({
      group_list <- sample_df$Group
      pca_result <- compute_pca(norm_df, group_list)

      # Store in workspace for plot endpoint
      workspace[["pca_result"]] <- list(
        coordinates = pca_result$coordinates,
        variance_explained = pca_result$variance_explained
      )

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
#* @serializer unboxedJSON
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
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get coordinates from request body or workspace
    # Frontend may send empty object - check if it's valid data
    coords_data <- NULL
    variance <- body$variance_explained

    # Only use body coordinates if it has actual data (not empty object)
    if (!is.null(body$coordinates) && is.list(body$coordinates) && length(body$coordinates) > 0) {
      # Check if it's a valid data frame with rows
      if (is.data.frame(body$coordinates) && nrow(body$coordinates) > 0) {
        coords_data <- body$coordinates
      } else if (length(body$coordinates[[1]]) > 0) {
        # It's a list with data
        coords_data <- body$coordinates
      }
    }

    # Fall back to workspace
    if (is.null(coords_data) && exists("pca_result", envir = workspace)) {
      pca_ws <- workspace[["pca_result"]]
      coords_data <- pca_ws$coordinates
      if (is.null(variance)) {
        variance <- pca_ws$variance_explained
      }
    }

    if (is.null(coords_data)) {
      res$status <- 400
      return(list(error = "Missing 'coordinates' data. Run PCA analysis first."))
    }

    coords <- list_to_df(coords_data)
    params <- body$params %||% list()

    # Convert variance from list to named numeric vector
    if (!is.null(variance)) {
      variance <- unlist(variance)
    }


    svg_content <- render_pca_svg(coords, params, variance)
    # Ensure SVG is a single string, not a vector
    if (is.character(svg_content) && length(svg_content) > 1) {
      svg_content <- paste(svg_content, collapse = "\n")
    }
    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Plot generation failed:", e$message))
  })
}

#* DESeq2 analysis
#* @post /api/analyze/deseq2
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get data from request body or workspace
    raw_data <- NULL
    sample_data <- NULL

    if (!is.null(body$raw) && is.data.frame(body$raw)) {
      raw_data <- body$raw
    }
    if (!is.null(body$sampleInfo) && is.data.frame(body$sampleInfo)) {
      sample_data <- body$sampleInfo
    }

    # Fall back to workspace
    if (is.null(raw_data) && exists("expr_raw", envir = workspace)) {
      raw_data <- workspace[["expr_raw"]]
    }
    if (is.null(sample_data) && exists("sampleInfo", envir = workspace)) {
      sample_data <- workspace[["sampleInfo"]]
    }

    # Validate inputs
    if (is.null(raw_data)) {
      res$status <- 400
      return(list(error = "Missing 'raw' expression matrix. Load demo data first."))
    }
    if (is.null(sample_data)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data. Load demo data first."))
    }

    # Convert to data frames
    raw_df <- list_to_df(raw_data)
    sample_df <- list_to_df(sample_data)

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
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$sampleInfo)) {
      res$status <- 400
      return(list(error = "Missing 'sampleInfo' data"))
    }

    sample_df <- list_to_df(body$sampleInfo)
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


#* Generate Volcano plot
#* @post /api/plot/volcano
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$degs)) {
      res$status <- 400
      return(list(error = "Missing 'degs' data"))
    }

    degs_df <- list_to_df(body$degs)

    # Validate DEGs data
    validation <- validate_deseq2_data(degs_df)
    if (!validation$valid) {
      res$status <- 400
      return(list(error = validation$message))
    }

    params <- body$params %||% list()

    svg_content <- render_volcano_svg(degs_df, params)
    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Volcano plot generation failed:", e$message))
  })
}

#* Save volcano plot as RData
#* @post /api/export/volcano/rdata
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$degs)) {
      res$status <- 400
      return(list(error = "Missing 'degs' data"))
    }

    degs_df <- list_to_df(body$degs)

    # Validate DEGs data
    validation <- validate_deseq2_data(degs_df)
    if (!validation$valid) {
      res$status <- 400
      return(list(error = validation$message))
    }

    params <- body$params %||% list()

    # Build plot object
    title <- params$title %||% "Volcano Plot"
    colr_up <- params$colr_up %||% "#FC4E2A"
    colr_down <- params$colr_down %||% "#4393C3"
    colr_not <- params$colr_not %||% "#00000033"
    xlim <- params$xlim %||% c(-10, 10)
    xbr <- params$xbr %||% 5

    if (is.list(xlim)) {
      xlim <- unlist(xlim)
    }

    p <- volcano_plot_func(
      DEGs = degs_df,
      title = title,
      colr_up = colr_up,
      colr_down = colr_down,
      colr_not = colr_not,
      xlim = xlim,
      xbr = xbr
    )

    # Save to temp file and return
    tmp <- tempfile(fileext = ".rda")
    on.exit(unlink(tmp))
    save_volcano_rdata(p, tmp)

    # Read and encode as base64
    raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
    b64 <- base64enc::base64encode(raw_bytes)

    list(status = "ok", data = b64, filename = "volcano_plot.rda")
  }, error = function(e) {
    res$status <- 500
    list(error = paste("RData export failed:", e$message))
  })
}

#* Enrichment analysis
#* @post /api/analyze/enrich
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get gene list - either from direct input or DESeq2 results
    genes <- NULL
    if (!is.null(body$source) && body$source == "deseq2") {
      # Extract genes from DESeq2 results in workspace
      if (is.null(workspace$deseq2)) {
        res$status <- 400
        return(list(error = "No DESeq2 results in workspace. Run DESeq2 analysis first."))
      }

      contrast <- body$contrast
      if (is.null(contrast)) {
        # Use first contrast
        contrast <- names(workspace$deseq2)[1]
      }

      if (!(contrast %in% names(workspace$deseq2))) {
        res$status <- 400
        return(list(error = paste("Contrast not found:", contrast)))
      }

      direction <- body$direction %||% "Up"
      degs <- workspace$deseq2[[contrast]]
      genes <- extract_genes_from_deseq2(degs, direction = direction)

      if (length(genes) == 0) {
        res$status <- 400
        return(list(error = paste("No", direction, "genes found in contrast", contrast)))
      }
    } else {
      # Direct gene list input
      if (is.null(body$genes) || length(body$genes) == 0) {
        res$status <- 400
        return(list(error = "Missing 'genes' - provide gene list or use source='deseq2'"))
      }
      genes <- as.character(body$genes)
    }

    # Get database parameter
    database <- body$database
    if (is.null(database)) {
      res$status <- 400
      return(list(error = "Missing 'database' parameter"))
    }

    # Get optional parameters
    species <- body$species %||% "Homo sapiens"
    gene_type <- body$gene_type %||% "symbol"
    p_cutoff <- as.numeric(body$p_cutoff %||% 0.05)
    padj_method <- body$padj_method %||% "BH"

    # Fetch database
    db <- msigdbr_func(species = species, database = database)

    # Run enrichment analysis
    enrich_result <- enrich_func(
      gene = genes,
      db = db,
      gene_type = gene_type,
      p = p_cutoff,
      padj_method = padj_method
    )

    if (is.null(enrich_result) || nrow(enrich_result) == 0) {
      return(list(
        status = "ok",
        message = "No significant enrichment found",
        results = list()
      ))
    }

    # Store in workspace for downstream use
    workspace$enrich <- enrich_result

    # Convert to list for JSON serialization
    results_list <- as.list(enrich_result)

    list(
      status = "ok",
      results = results_list,
      gene_count = length(genes),
      database = database,
      species = species
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Enrichment analysis failed:", e$message))
  })
}

#* Generate enrichment plot
#* @post /api/plot/enrich
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$results)) {
      res$status <- 400
      return(list(error = "Missing 'results' data"))
    }

    # Convert results back to data frame
    results_list <- body$results
    eRes <- as.data.frame(results_list)

    if (nrow(eRes) == 0) {
      res$status <- 400
      return(list(error = "Empty enrichment results"))
    }

    params <- body$params %||% list()

    svg_content <- render_enrich_svg(eRes, params)
    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Enrichment plot generation failed:", e$message))
  })
}

#* Get available databases
#* @get /api/databases
#* @serializer unboxedJSON
function(res) {
  tryCatch({
    dbs <- get_msigdbr_databases()
    list(
      status = "ok",
      databases = dbs$databases
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Failed to get databases:", e$message))
  })
}

#* Get available species
#* @get /api/species
#* @serializer unboxedJSON
function(res) {
  tryCatch({
    species <- get_msigdbr_species()
    list(
      status = "ok",
      species = species
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Failed to get species:", e$message))
  })
}

#* Save workspace
#* @post /api/workspace/save
#* @serializer contentType list(type="application/octet-stream")
function(req, res) {
  tryCatch({
    # Serialize workspace
    raw_bytes <- serialize_workspace(workspace)
    
    # Return as binary
    res$setHeader("Content-Disposition", "attachment; filename=workspace.rds")
    return(raw_bytes)
  }, error = function(e) {
    res$status <- 500
    return(list(error = paste("Failed to save workspace:", e$message)))
  })
}

#* Load workspace
#* @post /api/workspace/load
#* @parser multi
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    # Get uploaded file
    if (is.null(req$files) || length(req$files) == 0) {
      res$status <- 400
      return(list(error = "No file uploaded"))
    }
    
    file_info <- req$files[[1]]
    file_path <- file_info$datapath
    
    # Read uploaded file as raw bytes
    raw_bytes <- readBin(file_path, "raw", n = file.info(file_path)$size)
    
    # Deserialize into workspace
    result <- deserialize_workspace(raw_bytes, workspace)
    
    return(result)
  }, error = function(e) {
    res$status <- 500
    return(list(error = paste("Failed to load workspace:", e$message)))
  })
}

#* Get workspace status
#* @get /api/workspace/status
#* @serializer unboxedJSON
function(req, res) {
  steps <- get_workspace_status(workspace)
  list(
    status = "ok",
    steps = steps,
    version = WORKSPACE_VERSION
  )
}

#* GSEA analysis
#* @post /api/analyze/gsea
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get gene list - either from direct input or DESeq2 results
    geneList <- NULL
    if (!is.null(body$source) && body$source == "deseq2") {
      # Extract geneList from DESeq2 results in workspace
      if (is.null(workspace$deseq2)) {
        res$status <- 400
        return(list(error = "No DESeq2 results in workspace. Run DESeq2 analysis first."))
      }

      contrast <- body$contrast
      if (is.null(contrast)) {
        # Use first contrast
        contrast <- names(workspace$deseq2)[1]
      }

      if (!(contrast %in% names(workspace$deseq2))) {
        res$status <- 400
        return(list(error = paste("Contrast not found:", contrast)))
      }

      degs <- workspace$deseq2[[contrast]]
      geneList <- build_genelist_from_deseq2(degs)

      if (length(geneList) == 0) {
        res$status <- 400
        return(list(error = "No valid gene data found in DESeq2 results"))
      }
    } else {
      # Direct gene list input - expect named list with gene -> log2FC
      if (is.null(body$geneList) || length(body$geneList) == 0) {
        res$status <- 400
        return(list(error = "Missing 'geneList' - provide gene list or use source='deseq2'"))
      }
      geneList <- unlist(body$geneList)
      # Sort decreasing
      geneList <- sort(geneList, decreasing = TRUE)
    }

    # Get database parameter
    database <- body$database
    if (is.null(database)) {
      res$status <- 400
      return(list(error = "Missing 'database' parameter"))
    }

    # Get optional parameters
    species <- body$species %||% "Homo sapiens"
    gene_type <- body$gene_type %||% "symbol"
    pvalue <- as.numeric(body$pvalue %||% 0.05)
    pAdjustMethod <- body$pAdjustMethod %||% "BH"

    # Fetch database
    db <- msigdbr_func(species = species, database = database)

    # Run GSEA analysis
    gsea_result <- gsea_func(
      gene = geneList,
      db = db,
      gene_type = gene_type,
      pvalue = pvalue,
      pAdjustMethod = pAdjustMethod
    )

    if (is.null(gsea_result) || nrow(gsea_result@result) == 0) {
      return(list(
        status = "ok",
        message = "No significant GSEA results found",
        result = list()
      ))
    }

    # Store in workspace for downstream use
    workspace$gsea <- gsea_result

    # Convert to list for JSON serialization
    result_df <- as.data.frame(gsea_result@result)

    list(
      status = "ok",
      result = as.list(result_df),
      gene_count = length(geneList),
      database = database,
      species = species,
      parameters = list(
        pvalue = pvalue,
        pAdjustMethod = pAdjustMethod
      )
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("GSEA analysis failed:", e$message))
  })
}

#* Generate GSEA plot
#* @post /api/plot/gsea
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$result)) {
      res$status <- 400
      return(list(error = "Missing 'result' data"))
    }

    if (is.null(body$params) || is.null(body$params$geneSetID)) {
      res$status <- 400
      return(list(error = "Missing 'geneSetID' in params"))
    }

    # Get GSEA result from workspace if available, otherwise reconstruct
    gsea_result <- workspace$gsea

    if (is.null(gsea_result)) {
      # Try to reconstruct from provided result
      result_df <- list_to_df(body$result)
      # Create a mock object for plotting
      gsea_result <- list(result = result_df)
      class(gsea_result) <- "gseaResult"
    }

    params <- body$params

    svg_content <- render_gsea_svg(gsea_result, params)
    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("GSEA plot generation failed:", e$message))
  })
}

#* Export GSEA plot as RData
#* @post /api/export/gsea/rdata
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$result)) {
      res$status <- 400
      return(list(error = "Missing 'result' data"))
    }

    if (is.null(body$params) || is.null(body$params$geneSetID)) {
      res$status <- 400
      return(list(error = "Missing 'geneSetID' in params"))
    }

    # Get GSEA result from workspace
    gsea_result <- workspace$gsea

    if (is.null(gsea_result)) {
      res$status <- 400
      return(list(error = "No GSEA result in workspace. Run GSEA analysis first."))
    }

    params <- body$params

    # Generate plot
    p <- gsea_plot_func(
      object = gsea_result,
      geneSetID = params$geneSetID,
      addGene = params$addGene,
      addPval = params$addPval %||% TRUE,
      base_size = params$base_size %||% 12,
      termWidth = params$termWidth %||% 40,
      subPlot = params$subPlot %||% 3,
      newGsea = params$newGsea %||% FALSE
    )

    if (is.null(p)) {
      res$status <- 500
      return(list(error = "Failed to generate GSEA plot"))
    }

    # Save to temp file and return
    tmp <- tempfile(fileext = ".rda")
    on.exit(unlink(tmp))
    save_gsea_rdata(p, tmp)

    # Read and encode as base64
    raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
    b64 <- base64enc::base64encode(raw_bytes)

    list(status = "ok", data = b64, filename = "gsea_plot.rda")
  }, error = function(e) {
    res$status <- 500
    list(error = paste("RData export failed:", e$message))
  })
}

#* GeneTrend Mfuzz analysis
#* @post /api/analyze/genetrend
#* @serializer unboxedJSON
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
    norm_df <- list_to_df(body$norm)
    sample_df <- list_to_df(body$sampleInfo)

    # Get Mfuzz parameters
    c_value <- if (!is.null(body$c_value)) as.integer(body$c_value) else 4
    filterNA <- if (!is.null(body$filterNA)) as.numeric(body$filterNA) else 0.25
    fillNA <- if (!is.null(body$fillNA)) body$fillNA else "mean"
    filterSD <- if (!is.null(body$filterSD)) as.numeric(body$filterSD) else 0.3

    # Generate task ID
    taskId <- paste0("genetrend_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

    # Store initial status
    .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

    # Run Mfuzz analysis (synchronously, R is single-threaded)
    tryCatch({
      mfuzz_result <- mfuzz_ana(
        data = norm_df,
        groupInfo = sample_df,
        c_value = c_value,
        filterNA = filterNA,
        fillNA = fillNA,
        filterSD = filterSD
      )

      # Extract cluster members
      members <- extract_cluster_members(mfuzz_result$cl)

      # Store in workspace for plotting
      workspace$genetrend <- list(
        eset = mfuzz_result$eset,
        cl = mfuzz_result$cl,
        levels = unique(sample_df$Group)
      )

      .task_store[[taskId]] <- list(
        status = "done",
        result = list(
          clusters = as.list(members),
          n_clusters = c_value,
          n_genes = nrow(members),
          membership = as.data.frame(mfuzz_result$cl$membership)
        )
      )
    }, error = function(e) {
      .task_store[[taskId]] <- list(
        status = "error",
        error = paste("Mfuzz analysis failed:", e$message)
      )
    })

    list(taskId = taskId)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Analysis failed:", e$message))
  })
}

#* Generate GeneTrend all clusters plot
#* @post /api/plot/genetrend
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Check if Mfuzz result exists in workspace
    if (is.null(workspace$genetrend)) {
      res$status <- 400
      return(list(error = "No Mfuzz result in workspace. Run GeneTrend analysis first."))
    }

    gt <- workspace$genetrend
    params <- body$params %||% list()

    plotType <- body$plotType %||% "all"

    if (plotType == "all") {
      svg_content <- render_genetrend_all_svg(
        eset = gt$eset,
        cl = gt$cl,
        levels = gt$levels,
        params = params
      )
    } else {
      # Single cluster plot
      clusterNum <- if (!is.null(body$clusterNum)) as.integer(body$clusterNum) else 1
      svg_content <- render_genetrend_single_svg(
        eset = gt$eset,
        cl = gt$cl,
        plotN = clusterNum,
        levels = gt$levels,
        params = params
      )
    }

    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Plot generation failed:", e$message))
  })
}

#* Export GeneTrend as RData
#* @post /api/export/genetrend/rdata
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    # Check if Mfuzz result exists in workspace
    if (is.null(workspace$genetrend)) {
      res$status <- 400
      return(list(error = "No Mfuzz result in workspace. Run GeneTrend analysis first."))
    }

    gt <- workspace$genetrend

    # Save to temp file and return
    tmp <- tempfile(fileext = ".rda")
    on.exit(unlink(tmp))
    save(gt, file = tmp)

    # Read and encode as base64
    raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
    b64 <- base64enc::base64encode(raw_bytes)

    list(status = "ok", data = b64, filename = "genetrend_mfuzz.rda")
  }, error = function(e) {
    res$status <- 500
    list(error = paste("RData export failed:", e$message))
  })
}

#* WGCNA analysis - multi-step
#* @post /api/analyze/wgcna
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    # Get step parameter
    step <- body$step
    if (is.null(step)) {
      res$status <- 400
      return(list(error = "Missing 'step' parameter"))
    }

    # Generate task ID
    taskId <- paste0("wgcna_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
    .task_store[[taskId]] <- list(status = "pending", created = Sys.time())

    tryCatch({
      if (step == "sampletree") {
        # Step 1: Sample clustering
        if (is.null(body$expr)) {
          res$status <- 400
          return(list(error = "Missing 'expr' expression data"))
        }
        expr_df <- list_to_df(body$expr)
        cutHeight <- if (!is.null(body$cutHeight)) as.numeric(body$cutHeight) else 15

        datExpr0 <- datExpr0_func(expr_df)
        svg <- render_sampletree_svg(datExpr0, cutHeight = cutHeight)

        # Store in workspace
        workspace$wgcna <- list(datExpr0 = datExpr0, cutHeight = cutHeight)

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(
            svg = svg,
            n_samples = nrow(datExpr0),
            n_genes = ncol(datExpr0),
            cutHeight = cutHeight
          )
        )

      } else if (step == "power") {
        # Step 2: Pick soft-thresholding power
        if (is.null(workspace$wgcna) || is.null(workspace$wgcna$datExpr0)) {
          res$status <- 400
          return(list(error = "No preprocessed data in workspace. Run sampletree step first."))
        }
        abline <- if (!is.null(body$abline)) as.numeric(body$abline) else 0.8

        datExpr0 <- workspace$wgcna$datExpr0
        # Apply cutHeight to remove outliers
        cutHeight <- workspace$wgcna$cutHeight
        datExpr <- datExpr_func(datExpr0, cutHeight = cutHeight)

        power_result <- pick_power_func(datExpr, abline = abline)

        # Store datExpr and power
        workspace$wgcna$datExpr <- datExpr
        workspace$wgcna$power <- power_result$recommended_power

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(
            svg = power_result$svg,
            recommended_power = power_result$recommended_power,
            fit_indices = power_result$fit_indices
          )
        )

      } else if (step == "network") {
        # Step 3: Build network
        if (is.null(workspace$wgcna) || is.null(workspace$wgcna$datExpr)) {
          res$status <- 400
          return(list(error = "No expression data in workspace. Run power step first."))
        }

        power <- if (!is.null(body$power)) as.integer(body$power) else workspace$wgcna$power
        mergeCutHeight <- if (!is.null(body$mergeCutHeight)) as.numeric(body$mergeCutHeight) else 0.25

        datExpr <- workspace$wgcna$datExpr
        net <- blockwiseModules_func(datExpr, power = power, mergeCutHeight = mergeCutHeight)

        moduleColors <- labels2colors(net$colors)
        svg <- render_module_dendro_svg(net)

        # Store network result
        workspace$wgcna$net <- net
        workspace$wgcna$moduleColors <- moduleColors
        workspace$wgcna$power <- power

        # Module summary
        module_table <- table(moduleColors)

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(
            svg = svg,
            n_modules = length(unique(moduleColors)) - 1,  # exclude grey
            module_summary = as.list(module_table),
            power = power,
            mergeCutHeight = mergeCutHeight
          )
        )

      } else if (step == "module_trait") {
        # Step 4: Module-trait associations
        if (is.null(workspace$wgcna) || is.null(workspace$wgcna$net)) {
          res$status <- 400
          return(list(error = "No network in workspace. Run network step first."))
        }
        if (is.null(body$trait)) {
          res$status <- 400
          return(list(error = "Missing 'trait' data"))
        }

        trait_df <- list_to_df(body$trait)
        datExpr <- workspace$wgcna$datExpr
        net <- workspace$wgcna$net

        datTraits <- process_trait_func(trait_df, datExpr)
        mt_result <- compute_module_trait(datExpr, net, datTraits)

        svg <- render_module_trait_svg(
          mt_result$moduleTraitCor,
          mt_result$moduleTraitPvalue,
          datTraits,
          mt_result$MEs
        )

        # Store for later use
        workspace$wgcna$datTraits <- datTraits
        workspace$wgcna$mt_result <- mt_result

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(
            svg = svg,
            n_traits = ncol(datTraits),
            n_modules = ncol(mt_result$MEs)
          )
        )

      } else if (step == "mm_gs") {
        # Step 5: Module membership vs gene significance
        if (is.null(workspace$wgcna) || is.null(workspace$wgcna$mt_result)) {
          res$status <- 400
          return(list(error = "No module-trait results. Run module_trait step first."))
        }

        module <- body$module
        trait <- body$trait
        if (is.null(module) || is.null(trait)) {
          res$status <- 400
          return(list(error = "Missing 'module' or 'trait' parameter"))
        }

        datExpr <- workspace$wgcna$datExpr
        datTraits <- workspace$wgcna$datTraits
        net <- workspace$wgcna$net
        mt_result <- workspace$wgcna$mt_result

        mm_gs_result <- compute_mm_gs(datExpr, datTraits, net)
        svg <- render_mm_gs_svg(
          mm_gs_result$geneModuleMembership,
          mm_gs_result$geneTraitSignificance,
          module, trait,
          mm_gs_result$moduleColors,
          mm_gs_result$modNames
        )

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(svg = svg)
        )

      } else if (step == "tom") {
        # Step 6: TOM heatmap
        if (is.null(workspace$wgcna) || is.null(workspace$wgcna$net)) {
          res$status <- 400
          return(list(error = "No network in workspace. Run network step first."))
        }

        nSelect <- if (!is.null(body$nSelect)) as.integer(body$nSelect) else 400
        datExpr <- workspace$wgcna$datExpr
        power <- workspace$wgcna$power
        moduleColors <- workspace$wgcna$moduleColors

        svg <- render_tom_svg(datExpr, power, moduleColors, nSelect = nSelect)

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(svg = svg)
        )

      } else if (step == "export") {
        # Export all WGCNA results
        if (is.null(workspace$wgcna)) {
          res$status <- 400
          return(list(error = "No WGCNA results in workspace"))
        }

        tmp <- tempfile(fileext = ".rda")
        on.exit(unlink(tmp))
        save(workspace$wgcna, file = tmp)
        raw_bytes <- readBin(tmp, "raw", n = file.info(tmp)$size)
        b64 <- base64enc::base64encode(raw_bytes)

        .task_store[[taskId]] <- list(
          status = "done",
          result = list(data = b64, filename = "wgcna_results.rda")
        )

      } else {
        res$status <- 400
        return(list(error = paste("Unknown step:", step)))
      }
    }, error = function(e) {
      .task_store[[taskId]] <<- list(
        status = "error",
        error = paste("WGCNA step", step, "failed:", e$message)
      )
    })

    list(taskId = taskId)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("WGCNA analysis failed:", e$message))
  })
}

#* WGCNA plot
#* @post /api/plot/wgcna
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    step <- body$step
    if (is.null(step)) {
      res$status <- 400
      return(list(error = "Missing 'step' parameter"))
    }

    if (is.null(workspace$wgcna)) {
      res$status <- 400
      return(list(error = "No WGCNA results in workspace. Run WGCNA analysis first."))
    }

    svg_content <- ""
    if (step == "mm_gs") {
      module <- body$module
      trait <- body$trait
      if (is.null(module) || is.null(trait)) {
        res$status <- 400
        return(list(error = "Missing 'module' or 'trait' parameter"))
      }
      datExpr <- workspace$wgcna$datExpr
      datTraits <- workspace$wgcna$datTraits
      net <- workspace$wgcna$net
      mm_gs_result <- compute_mm_gs(datExpr, datTraits, net)
      svg_content <- render_mm_gs_svg(
        mm_gs_result$geneModuleMembership,
        mm_gs_result$geneTraitSignificance,
        module, trait,
        mm_gs_result$moduleColors,
        mm_gs_result$modNames
      )
    } else if (step == "tom") {
      nSelect <- if (!is.null(body$nSelect)) as.integer(body$nSelect) else 400
      svg_content <- render_tom_svg(
        workspace$wgcna$datExpr,
        workspace$wgcna$power,
        workspace$wgcna$moduleColors,
        nSelect
      )
    } else {
      res$status <- 400
      return(list(error = paste("Unknown plot step:", step)))
    }

    list(svg = svg_content)
  }, error = function(e) {
    res$status <- 500
    list(error = paste("WGCNA plot failed:", e$message))
  })
}

#' Save plot as RDS (ggplot object + parameters)
#' @post /api/plot/save
#' @serializer unboxedJSON
function(req, res) {
  tryCatch({
    body <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)

    if (is.null(body$module)) {
      res$status <- 400
      return(list(error = "Missing 'module' parameter"))
    }

    module <- body$module
    params <- body$params %||% list()
    plot_data <- body$plot_data %||% list()

    # Build ggplot object based on module
    plot_obj <- NULL

    if (module == "volcano") {
      if (is.null(plot_data$degs)) {
        res$status <- 400
        return(list(error = "Missing 'degs' data for volcano"))
      }
      degs_df <- list_to_df(plot_data$degs)
      title <- params$title %||% "Volcano Plot"
      colr_up <- params$colr_up %||% "#FC4E2A"
      colr_down <- params$colr_down %||% "#4393C3"
      colr_not <- params$colr_not %||% "#00000033"
      xlim <- params$xlim %||% c(-10, 10)
      xbr <- params$xbr %||% 5
      if (is.list(xlim)) xlim <- unlist(xlim)

      plot_obj <- volcano_plot_func(
        DEGs = degs_df, title = title,
        colr_up = colr_up, colr_down = colr_down, colr_not = colr_not,
        xlim = xlim, xbr = xbr
      )

    } else if (module == "pca") {
      if (is.null(plot_data$coordinates)) {
        res$status <- 400
        return(list(error = "Missing 'coordinates' data for pca"))
      }
      coords <- list_to_df(plot_data$coordinates)
      variance <- plot_data$variance_explained
      if (!is.null(variance)) variance <- unlist(variance)

      title <- params$title %||% "Principal Component Analysis"
      colr <- params$colors %||% c("red", "green", "yellow", "purple")
      addEllipses <- params$ellipse %||% TRUE
      addLabels <- params$labels %||% TRUE
      dotSize <- params$dotSize %||% 2
      xlim <- params$xlim %||% c(-150, 150)
      ylim <- params$ylim %||% c(-150, 150)
      if (is.list(xlim)) xlim <- unlist(xlim)
      if (is.list(ylim)) ylim <- unlist(ylim)

      plot_obj <- pca_plot(
        coordinates = coords, title = title, colr = colr,
        addEllipses = addEllipses, addLabels = addLabels,
        dotSize = dotSize, xlim = xlim, ylim = ylim,
        variance_explained = variance
      )

    } else if (module == "enrich") {
      if (is.null(plot_data$results)) {
        res$status <- 400
        return(list(error = "Missing 'results' data for enrich"))
      }
      eRes <- list_to_df(plot_data$results)
      plot_obj <- enrich_plot_func(eRes,
        showCategory = params$showCategory %||% 10,
        title = params$title %||% "Enrichment Analysis"
      )

    } else if (module == "gsea") {
      # For GSEA, we save params only - plot requires the full GSEA object
      plot_obj <- ggplot2::ggplot() + ggplot2::ggtitle(params$title %||% "GSEA Plot")

    } else if (module == "genetrend") {
      plot_obj <- ggplot2::ggplot() + ggplot2::ggtitle(params$title %||% "GeneTrend Plot")

    } else if (module == "wgcna") {
      plot_obj <- ggplot2::ggplot() + ggplot2::ggtitle(params$title %||% "WGCNA Plot")

    } else {
      res$status <- 400
      return(list(error = paste("Unknown module:", module)))
    }

    # Save to RDS
    raw_bytes <- save_plot_rds(module, plot_obj, params)

    # Encode as base64
    b64 <- base64enc::base64encode(raw_bytes)

    list(status = "ok", data = b64, filename = paste0(module, "_plot.rds"))
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Plot save failed:", e$message))
  })
}

#' Load plot from RDS file
#' @post /api/plot/load
#' @parser multi
#' @serializer unboxedJSON
function(req, res) {
  tryCatch({
    # Get uploaded file
    if (is.null(req$files) || length(req$files) == 0) {
      res$status <- 400
      return(list(error = "No file uploaded"))
    }

    file_info <- req$files[[1]]
    file_path <- file_info$datapath

    # Read uploaded file as raw bytes
    raw_bytes <- readBin(file_path, "raw", n = file.info(file_path)$size)

    # Load the RDS
    result <- load_plot_rds(raw_bytes)

    # Return params and SVG (not the ggplot object itself)
    list(
      status = "ok",
      module = result$module,
      params = result$params,
      svg = result$svg
    )
  }, error = function(e) {
    res$status <- 500
    list(error = paste("Plot load failed:", e$message))
  })
}

#' Export plot
#' @post /api/export/<format>
function(req, format) {
  # TODO: implement export
  list(status = "ok", format = format)
}
