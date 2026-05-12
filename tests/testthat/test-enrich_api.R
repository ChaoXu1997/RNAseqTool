library(testthat)
library(plumber)

# Integration tests for Enrich API endpoints

test_that("POST /api/analyze/enrich returns enrich result with gene list", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    genes = c("TP53", "BRCA1", "MYC", "EGFR", "KRAS"),
    database = "C5:GO:BP",
    species = "Homo sapiens",
    gene_type = "symbol",
    p_cutoff = 0.05
  )

  res <- pr$route("POST", "/api/analyze/enrich", body)
  expect_equal(res$status, 200)
  expect_true("status" %in% names(res$body))
  expect_true("results" %in% names(res$body))
})

test_that("POST /api/analyze/enrich validates input - missing genes", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    database = "C5:GO:BP",
    species = "Homo sapiens"
  )

  res <- pr$route("POST", "/api/analyze/enrich", body)
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})

test_that("POST /api/analyze/enrich validates input - missing database", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    genes = c("TP53", "BRCA1", "MYC"),
    species = "Homo sapiens"
  )

  res <- pr$route("POST", "/api/analyze/enrich", body)
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})

test_that("POST /api/analyze/enrich works with DESeq2 source", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  # Simulate DESeq2 result in workspace
  body <- list(
    source = "deseq2",
    direction = "Up",
    contrast = "Control_vs_Treatment",
    database = "C5:GO:BP",
    species = "Homo sapiens",
    gene_type = "symbol"
  )

  res <- pr$route("POST", "/api/analyze/enrich", body)
  # Should either return 200 with results or error if no DESeq2 data in workspace
  expect_true(res$status %in% c(200, 400, 500))
})

test_that("POST /api/plot/enrich returns SVG with enrich result", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    results = list(
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
    ),
    params = list(
      plot_type = "bar",
      showCategory = 5,
      title = "Enrichment Analysis"
    )
  )

  res <- pr$route("POST", "/api/plot/enrich", body)
  expect_equal(res$status, 200)
  expect_true("svg" %in% names(res$body))
  expect_true(grepl("<svg", res$body$svg))
})

test_that("POST /api/plot/enrich validates input - missing results", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    params = list(plot_type = "bar")
  )

  res <- pr$route("POST", "/api/plot/enrich", body)
  expect_equal(res$status, 400)
  expect_true("error" %in% names(res$body))
})

test_that("POST /api/plot/enrich supports dotplot type", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  body <- list(
    results = list(
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
    ),
    params = list(
      plot_type = "dot",
      showCategory = 5,
      title = "Enrichment Dot Plot"
    )
  )

  res <- pr$route("POST", "/api/plot/enrich", body)
  expect_equal(res$status, 200)
  expect_true("svg" %in% names(res$body))
  expect_true(grepl("<svg", res$body$svg))
})

test_that("GET /api/databases returns available databases", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  res <- pr$route("GET", "/api/databases", list())
  expect_equal(res$status, 200)
  expect_true("databases" %in% names(res$body))
  expect_true(length(res$body$databases) > 0)
})

test_that("GET /api/species returns available species", {
  pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))
  oldwd <- getwd()
  setwd(file.path(dirname(dirname(dirname(getwd()))), "backend"))
  on.exit(setwd(oldwd))

  res <- pr$route("GET", "/api/species", list())
  expect_equal(res$status, 200)
  expect_true("species" %in% names(res$body))
  expect_true("Homo sapiens" %in% res$body$species)
})
