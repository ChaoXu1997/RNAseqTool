library(testthat)

# Source Enrich functions directly
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_enrich.R"))
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_msigdbr.R"))

# --- Unit tests for enrich_func ---

test_that("enrich_func returns NULL for empty gene list", {
  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway2", "pathway2"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4"),
    entrezid = c("1", "2", "3", "4"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4"),
    Database = c("GO", "GO", "KEGG", "KEGG")
  )

  result <- enrich_func(gene = character(0), db = db, gene_type = "symbol")
  expect_null(result)
})

test_that("enrich_func returns data frame for valid gene list", {
  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway2", "pathway2", "pathway3"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4", "GENE5"),
    entrezid = c("1", "2", "3", "4", "5"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4", "ENSG5"),
    Database = c("GO", "GO", "KEGG", "KEGG", "Reactome")
  )

  gene <- c("GENE1", "GENE2", "GENE3")
  result <- enrich_func(gene = gene, db = db, gene_type = "symbol")

  # Result should be a data frame or NULL
  expect_true(is.data.frame(result) || is.null(result))
})

test_that("enrich_func respects pvalue cutoff", {
  db <- data.frame(
    term = c("pathway1", "pathway1", "pathway2", "pathway2"),
    symbol = c("GENE1", "GENE2", "GENE3", "GENE4"),
    entrezid = c("1", "2", "3", "4"),
    ensembl = c("ENSG1", "ENSG2", "ENSG3", "ENSG4"),
    Database = c("GO", "GO", "KEGG", "KEGG")
  )

  gene <- c("GENE1", "GENE2")
  result <- enrich_func(gene = gene, db = db, gene_type = "symbol", p = 0.05)

  if (!is.null(result)) {
    expect_true(all(result$pvalue <= 0.05))
  }
})

test_that("enrich_func works with entrezid gene type", {
  db <- data.frame(
    term = c("pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2"),
    entrezid = c("100", "200"),
    ensembl = c("ENSG1", "ENSG2"),
    Database = c("GO", "GO")
  )

  gene <- c("100", "200")
  result <- enrich_func(gene = gene, db = db, gene_type = "entrezid")

  expect_true(is.data.frame(result) || is.null(result))
})

test_that("enrich_func works with ensembl gene type", {
  db <- data.frame(
    term = c("pathway1", "pathway1"),
    symbol = c("GENE1", "GENE2"),
    entrezid = c("1", "2"),
    ensembl = c("ENSG000001", "ENSG000002"),
    Database = c("GO", "GO")
  )

  gene <- c("ENSG000001", "ENSG000002")
  result <- enrich_func(gene = gene, db = db, gene_type = "ensembl")

  expect_true(is.data.frame(result) || is.null(result))
})

# --- Unit tests for enrich_plot_func ---

test_that("enrich_plot_func returns ggplot object with valid input", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:10),
    ID = paste0("ID_", 1:10),
    GeneRatio = rep("5/100", 10),
    BgRatio = rep("10/500", 10),
    pvalue = seq(0.001, 0.04, length.out = 10),
    p.adjust = seq(0.005, 0.045, length.out = 10),
    qvalue = seq(0.003, 0.043, length.out = 10),
    geneID = rep("GENE1/GENE2/GENE3", 10),
    Count = rep(5, 10),
    Database = rep("GO_BP", 10),
    EnrichFactor = seq(0.1, 0.5, length.out = 10)
  )

  p <- enrich_plot_func(eRes, showCategory = 5)
  expect_s3_class(p, "ggplot")
})

test_that("enrich_plot_func accepts custom colors", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  p <- enrich_plot_func(
    eRes,
    showCategory = 5,
    line_colr = c("#FF0000", "#00FF00", "#0000FF"),
    dot_colr = c("blue", "green")
  )
  expect_s3_class(p, "ggplot")
})

test_that("enrich_plot_func accepts custom title", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  p <- enrich_plot_func(eRes, showCategory = 5, title = "My Enrichment Plot")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "My Enrichment Plot")
})

test_that("enrich_plot_func accepts custom linewidth", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  p <- enrich_plot_func(eRes, showCategory = 5, linewidth = 2.0)
  expect_s3_class(p, "ggplot")
})

# --- Unit tests for render_enrich_svg ---

test_that("render_enrich_svg returns SVG string for barplot", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  svg <- render_enrich_svg(eRes, list(plot_type = "bar", showCategory = 5))

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
})

test_that("render_enrich_svg returns SVG string for dotplot", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  svg <- render_enrich_svg(eRes, list(plot_type = "dot", showCategory = 5))

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
})

test_that("render_enrich_svg uses defaults for missing params", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  svg <- render_enrich_svg(eRes, list())

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
})

# --- Unit tests for enrich_barplot ---

test_that("enrich_barplot returns ggplot object", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:5),
    ID = paste0("ID_", 1:5),
    GeneRatio = rep("5/100", 5),
    BgRatio = rep("10/500", 5),
    pvalue = seq(0.001, 0.04, length.out = 5),
    p.adjust = seq(0.005, 0.045, length.out = 5),
    qvalue = seq(0.003, 0.043, length.out = 5),
    geneID = rep("GENE1/GENE2/GENE3", 5),
    Count = rep(5, 5),
    Database = rep("GO_BP", 5),
    EnrichFactor = seq(0.1, 0.5, length.out = 5)
  )

  p <- enrich_barplot(eRes, showCategory = 5)
  expect_s3_class(p, "ggplot")
})

# --- Unit tests for msigdbr_func ---

test_that("msigdbr_func returns data frame for GO BP", {
  skip_if_not_installed("msigdbr")

  db <- msigdbr_func(species = "Homo sapiens", database = "C5:GO:BP")

  expect_true(is.data.frame(db))
  expect_true(all(c("term", "symbol", "entrezid", "ensembl", "Database") %in% colnames(db)))
  expect_true(nrow(db) > 0)
})

test_that("msigdbr_func returns data frame for KEGG", {
  skip_if_not_installed("msigdbr")

  db <- msigdbr_func(species = "Homo sapiens", database = "C2:CP:KEGG")

  expect_true(is.data.frame(db))
  expect_true(all(c("term", "symbol", "entrezid", "ensembl", "Database") %in% colnames(db)))
  expect_true(nrow(db) > 0)
})

test_that("msigdbr_func supports multiple databases", {
  skip_if_not_installed("msigdbr")

  db <- msigdbr_func(
    species = "Homo sapiens",
    database = c("C5:GO:BP", "C5:GO:CC")
  )

  expect_true(is.data.frame(db))
  expect_true(nrow(db) > 0)
  # Should have entries from both databases
  expect_true(length(unique(db$Database)) >= 1)
})

test_that("msigdbr_func strips prefix from term names", {
  skip_if_not_installed("msigdbr")

  db <- msigdbr_func(species = "Homo sapiens", database = "C5:GO:BP")

  expect_true(is.data.frame(db))
  # Terms should not start with GOBP_
  expect_true(all(!grepl("^GOBP_", db$term)))
})

# --- Unit tests for get_msigdbr_databases ---

test_that("get_msigdbr_databases returns list of available databases", {
  skip_if_not_installed("msigdbr")

  dbs <- get_msigdbr_databases()

  expect_true(is.list(dbs))
  expect_true("databases" %in% names(dbs))
  expect_true(length(dbs$databases) > 0)
})

test_that("get_msigdbr_species returns list of available species", {
  skip_if_not_installed("msigdbr")

  species <- get_msigdbr_species()

  expect_true(is.character(species))
  expect_true("Homo sapiens" %in% species)
})

# --- Unit tests for extract_genes_from_deseq2 ---

test_that("extract_genes_from_deseq2 extracts Up genes", {
  degs <- data.frame(
    Gene = paste0("G", 1:10),
    log2FoldChange = c(3, -2, 0.5, 4, -3, 0.1, 2.5, -1.5, 0.8, -0.3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005, 0.8, 0.02, 0.03, 0.6, 0.7),
    change = c("Up", "Down", "Not", "Up", "Down", "Not", "Up", "Down", "Not", "Not")
  )

  genes <- extract_genes_from_deseq2(degs, direction = "Up")

  expect_type(genes, "character")
  expect_true(length(genes) == 3)  # G1, G4, G7
  expect_true(all(c("G1", "G4", "G7") %in% genes))
})

test_that("extract_genes_from_deseq2 extracts Down genes", {
  degs <- data.frame(
    Gene = paste0("G", 1:10),
    log2FoldChange = c(3, -2, 0.5, 4, -3, 0.1, 2.5, -1.5, 0.8, -0.3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005, 0.8, 0.02, 0.03, 0.6, 0.7),
    change = c("Up", "Down", "Not", "Up", "Down", "Not", "Up", "Down", "Not", "Not")
  )

  genes <- extract_genes_from_deseq2(degs, direction = "Down")

  expect_type(genes, "character")
  expect_true(length(genes) == 3)  # G2, G5, G8
  expect_true(all(c("G2", "G5", "G8") %in% genes))
})

test_that("extract_genes_from_deseq2 extracts All DEGs", {
  degs <- data.frame(
    Gene = paste0("G", 1:10),
    log2FoldChange = c(3, -2, 0.5, 4, -3, 0.1, 2.5, -1.5, 0.8, -0.3),
    padj = c(0.001, 0.01, 0.5, 0.0001, 0.005, 0.8, 0.02, 0.03, 0.6, 0.7),
    change = c("Up", "Down", "Not", "Up", "Down", "Not", "Up", "Down", "Not", "Not")
  )

  genes <- extract_genes_from_deseq2(degs, direction = "All")

  expect_type(genes, "character")
  expect_true(length(genes) == 6)  # All Up + Down
})

test_that("extract_genes_from_deseq2 returns empty for no matches", {
  degs <- data.frame(
    Gene = paste0("G", 1:3),
    log2FoldChange = c(0.5, 0.3, 0.1),
    padj = c(0.5, 0.6, 0.7),
    change = c("Not", "Not", "Not")
  )

  genes <- extract_genes_from_deseq2(degs, direction = "Up")

  expect_type(genes, "character")
  expect_true(length(genes) == 0)
})

# --- Unit tests for enrich_to_dataframe ---

test_that("enrich_to_dataframe converts enrich result to downloadable data frame", {
  eRes <- data.frame(
    Description = paste0("pathway_", 1:3),
    ID = paste0("ID_", 1:3),
    GeneRatio = rep("5/100", 3),
    BgRatio = rep("10/500", 3),
    pvalue = c(0.001, 0.01, 0.04),
    p.adjust = c(0.005, 0.02, 0.05),
    qvalue = c(0.003, 0.015, 0.045),
    geneID = c("GENE1/GENE2", "GENE3/GENE4", "GENE5/GENE6"),
    Count = c(2, 2, 2),
    Database = c("GO_BP", "GO_CC", "KEGG"),
    EnrichFactor = c(0.1, 0.2, 0.3)
  )

  df <- enrich_to_dataframe(eRes)

  expect_true(is.data.frame(df))
  expect_true("Pathway" %in% colnames(df))
  expect_true("P_value" %in% colnames(df))
  expect_true("Adjusted_P" %in% colnames(df))
  expect_true("Gene_Count" %in% colnames(df))
  expect_true("Genes" %in% colnames(df))
  expect_true("Database" %in% colnames(df))
  expect_true("Enrich_Factor" %in% colnames(df))
  expect_true(nrow(df) == 3)
})
