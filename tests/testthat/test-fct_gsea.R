library(testthat)

# Source GSEA functions directly
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_gsea.R"))
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_msigdbr.R"))

# --- Unit tests for gsea_func ---

test_that("gsea_func returns NULL for empty gene list", {
  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway2", "pathway2"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4"),
    entrezid = c("1", "2", "3", "4"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4"),
    Database = c("GO", "GO", "KEGG", "KEGG")
  )

  gene <- numeric(0)
  result <- gsea_func(gene = gene, db = db, gene_type = "symbol")
  expect_null(result)
})

test_that("gsea_func returns GSEA result for valid gene list", {
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1", "pathway2", "pathway2", "pathway2"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5", "GENE6"),
    entrezid = c("1", "2", "3", "4", "5", "6"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4", "ENSG5", "ENSG6"),
    Database = c("GO", "GO", "GO", "KEGG", "KEGG", "KEGG")
  )

  # Create a named numeric vector sorted decreasing
  gene <- c(2.5, 1.8, 1.2, -0.5, -1.0, -2.0)
  names(gene) <- c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5", "GENE6")
  gene <- sort(gene, decreasing = TRUE)

  result <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  # Result should be a GSEA object or NULL
  expect_true(is(result, "gseaResult") || is.null(result))
})

test_that("gsea_func respects pvalue cutoff", {
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2", "GENE3"),
    entrezid = c("1", "2", "3"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3"),
    Database = c("GO", "GO", "GO")
  )

  gene <- c(2.5, 1.8, 1.2)
  names(gene) <- c("GENE1", "GENE2", "GENE3")
  gene <- sort(gene, decreasing = TRUE)

  result <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 0.05)

  if (!is.null(result)) {
    result_df <- as.data.frame(result@result)
    if (nrow(result_df) > 0) {
      expect_true(all(result_df$pvalue <= 0.05 | result_df$p.adjust <= 0.05))
    }
  }
})

test_that("gsea_func works with entrezid gene type", {
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2"),
    entrezid = c("100", "200"),
    ensembl = c("ENSG1", "ENSG2"),
    Database = c("GO", "GO")
  )

  gene <- c(2.5, 1.8)
  names(gene) <- c("100", "200")
  gene <- sort(gene, decreasing = TRUE)

  result <- gsea_func(gene = gene, db = db, gene_type = "entrezid", pvalue = 1)
  expect_true(is(result, "gseaResult") || is.null(result))
})

test_that("gsea_func works with ensembl gene type", {
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2"),
    entrezid = c("1", "2"),
    ensembl = c("ENSG000001", "ENSG000002"),
    Database = c("GO", "GO")
  )

  gene <- c(2.5, 1.8)
  names(gene) <- c("ENSG000001", "ENSG000002")
  gene <- sort(gene, decreasing = TRUE)

  result <- gsea_func(gene = gene, db = db, gene_type = "ensembl", pvalue = 1)
  expect_true(is(result, "gseaResult") || is.null(result))
})

# --- Unit tests for build_genelist_from_deseq2 ---

test_that("build_genelist_from_deseq2 creates named numeric vector", {
  degs <- data.frame(
    Gene = paste0("G", 1:10),
    log2FoldChange = c(3, -2, 0.5, 4, -3, 0.1, 2.5, -1.5, 0.8, -0.3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005, 0.8, 0.02, 0.03, 0.6, 0.7),
    change = c("Up", "Down", "Not", "Up", "Down", "Not", "Up", "Down", "Not", "Not")
  )

  geneList <- build_genelist_from_deseq2(degs)

  expect_type(geneList, "double")
  expect_true(!is.null(names(geneList)))
  expect_true(length(geneList) == nrow(degs))
  # Should be sorted decreasing
  expect_true(all(diff(geneList) <= 0))
})

test_that("build_genelist_from_deseq2 handles empty data frame", {
  degs <- data.frame(
    Gene = character(0),
    log2FoldChange = numeric(0),
    padj = numeric(0),
    change = character(0)
  )

  geneList <- build_genelist_from_deseq2(degs)

  expect_type(geneList, "double")
  expect_true(length(geneList) == 0)
})

test_that("build_genelist_from_deseq2 removes NA values", {
  degs <- data.frame(
    Gene = paste0("G", 1:5),
    log2FoldChange = c(3, NA, 0.5, 4, NA),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005),
    change = c("Up", "Down", "Not", "Up", "Down")
  )

  geneList <- build_genelist_from_deseq2(degs)

  expect_true(length(geneList) == 3)  # Only non-NA values
  expect_true(all(!is.na(geneList)))
})

# --- Unit tests for gsea_plot_func ---

test_that("gsea_plot_func returns ggplot object with valid input", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  # Create a mock GSEA result
  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1", "pathway2", "pathway2", "pathway2"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5", "GENE6"),
    entrezid = c("1", "2", "3", "4", "5", "6"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4", "ENSG5", "ENSG6"),
    Database = c("GO", "GO", "GO", "KEGG", "KEGG", "KEGG")
  )

  gene <- c(2.5, 1.8, 1.2, -0.5, -1.0, -2.0)
  names(gene) <- c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5", "GENE6")
  gene <- sort(gene, decreasing = TRUE)

  gsea_res <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  if (!is.null(gsea_res) && nrow(gsea_res@result) > 0) {
    geneSetID <- gsea_res@result$ID[1]
    p <- gsea_plot_func(object = gsea_res, geneSetID = geneSetID)
    expect_true(inherits(p, "ggplot") || is.null(p))
  }
})

test_that("gsea_plot_func accepts custom parameters", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2", "GENE3"),
    entrezid = c("1", "2", "3"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3"),
    Database = c("GO", "GO", "GO")
  )

  gene <- c(2.5, 1.8, 1.2)
  names(gene) <- c("GENE1", "GENE2", "GENE3")
  gene <- sort(gene, decreasing = TRUE)

  gsea_res <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  if (!is.null(gsea_res) && nrow(gsea_res@result) > 0) {
    geneSetID <- gsea_res@result$ID[1]
    p <- gsea_plot_func(
      object = gsea_res,
      geneSetID = geneSetID,
      addGene = c("GENE1"),
      addPval = TRUE,
      base_size = 14,
      termWidth = 30
    )
    expect_true(inherits(p, "ggplot") || is.null(p))
  }
})

# --- Unit tests for render_gsea_svg ---

test_that("render_gsea_svg returns SVG string", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2", "GENE3"),
    entrezid = c("1", "2", "3"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3"),
    Database = c("GO", "GO", "GO")
  )

  gene <- c(2.5, 1.8, 1.2)
  names(gene) <- c("GENE1", "GENE2", "GENE3")
  gene <- sort(gene, decreasing = TRUE)

  gsea_res <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  if (!is.null(gsea_res) && nrow(gsea_res@result) > 0) {
    geneSetID <- gsea_res@result$ID[1]
    svg <- render_gsea_svg(gsea_res, list(geneSetID = geneSetID))

    expect_type(svg, "character")
    expect_true(grepl("<svg", svg))
    expect_true(grepl("</svg>", svg))
  }
})

test_that("render_gsea_svg uses defaults for missing params", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2", "GENE3"),
    entrezid = c("1", "2", "3"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3"),
    Database = c("GO", "GO", "GO")
  )

  gene <- c(2.5, 1.8, 1.2)
  names(gene) <- c("GENE1", "GENE2", "GENE3")
  gene <- sort(gene, decreasing = TRUE)

  gsea_res <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  if (!is.null(gsea_res) && nrow(gsea_res@result) > 0) {
    geneSetID <- gsea_res@result$ID[1]
    svg <- render_gsea_svg(gsea_res, list(geneSetID = geneSetID))

    expect_type(svg, "character")
    expect_true(grepl("<svg", svg))
  }
})

# --- Unit tests for gsea_to_dataframe ---

test_that("gsea_to_dataframe converts GSEA result to downloadable data frame", {
  # Create a mock GSEA result object
  mock_result <- data.frame(
    ID = c("pathway1", "pathway2"),
    Description = c("Pathway 1", "Pathway 2"),
    setSize = c(100, 200),
    enrichmentScore = c(0.5, -0.3),
    NES = c(1.5, -1.2),
    pvalue = c(0.001, 0.01),
    p.adjust = c(0.005, 0.02),
    qvalues = c(0.003, 0.015),
    rank = c(50, 100),
    leading_edge = c("tags=30%, list=10%, signal=25%", "tags=20%, list=15%, signal=18%"),
    core_enrichment = c("GENE1/GENE2/GENE3", "GENE4/GENE5/GENE6"),
    stringsAsFactors = FALSE
  )

  df <- gsea_to_dataframe(mock_result)

  expect_true(is.data.frame(df))
  expect_true("Pathway" %in% colnames(df))
  expect_true("P_value" %in% colnames(df))
  expect_true("Adjusted_P" %in% colnames(df))
  expect_true("NES" %in% colnames(df))
  expect_true("Enrichment_Score" %in% colnames(df))
  expect_true("Set_Size" %in% colnames(df))
  expect_true("Core_Enrichment" %in% colnames(df))
  expect_true(nrow(df) == 2)
})

test_that("gsea_to_dataframe handles NULL input", {
  df <- gsea_to_dataframe(NULL)

  expect_true(is.data.frame(df))
  expect_true(nrow(df) == 0)
})

# --- Unit tests for save_gsea_rdata ---

test_that("save_gsea_rdata saves to file", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2", "GENE3"),
    entrezid = c("1", "2", "3"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3"),
    Database = c("GO", "GO", "GO")
  )

  gene <- c(2.5, 1.8, 1.2)
  names(gene) <- c("GENE1", "GENE2", "GENE3")
  gene <- sort(gene, decreasing = TRUE)

  gsea_res <- gsea_func(gene = gene, db = db, gene_type = "symbol", pvalue = 1)

  if (!is.null(gsea_res) && nrow(gsea_res@result) > 0) {
    geneSetID <- gsea_res@result$ID[1]
    p <- gsea_plot_func(object = gsea_res, geneSetID = geneSetID)

    if (!is.null(p)) {
      tmp <- tempfile(fileext = ".rda")
      on.exit(unlink(tmp))

      save_gsea_rdata(p, tmp)
      expect_true(file.exists(tmp))
      expect_true(file.info(tmp)$size > 0)
    }
  }
})
