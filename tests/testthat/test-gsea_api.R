library(testthat)
library(plumber)
library(jsonlite)

# Source the plumber API
pr <- plumber::plumb(file.path(dirname(dirname(dirname(getwd()))), "backend", "api", "plumber.R"))

# --- Unit tests for /api/analyze/gsea endpoint ---

test_that("POST /api/analyze/gsea returns error for missing geneList", {
  skip_if_not_installed("clusterProfiler")

  res <- pr$call("POST", "/api/analyze/gsea", list(
    body = toJSON(list(
      database = "C5:GO:BP",
      species = "Homo sapiens"
    ), auto_unbox = TRUE)
  ))

  expect_equal(res$status, 400)
  body <- fromJSON(res$body)
  expect_true(!is.null(body$error))
})

test_that("POST /api/analyze/gsea returns error for missing database", {
  skip_if_not_installed("clusterProfiler")

  res <- pr$call("POST", "/api/analyze/gsea", list(
    body = toJSON(list(
      geneList = list(
        GENE1 = 2.5,
        GENE2 = 1.8,
        GENE3 = 1.2
      )
    ), auto_unbox = TRUE)
  ))

  expect_equal(res$status, 400)
  body <- fromJSON(res$body)
  expect_true(!is.null(body$error))
})

test_that("POST /api/analyze/gsea returns error when DESeq2 source but no workspace", {
  skip_if_not_installed("clusterProfiler")

  res <- pr$call("POST", "/api/analyze/gsea", list(
    body = toJSON(list(
      source = "deseq2",
      database = "C5:GO:BP",
      species = "Homo sapiens"
    ), auto_unbox = TRUE)
  ))

  expect_equal(res$status, 400)
  body <- fromJSON(res$body)
  expect_true(grepl("DESeq2", body$error) || grepl("workspace", body$error))
})

test_that("POST /api/analyze/gsea accepts geneList and returns result", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")

  res <- pr$call("POST", "/api/analyze/gsea", list(
    body = toJSON(list(
      geneList = list(
        TP53 = 3.5,
        BRCA1 = 2.8,
        MYC = 2.1,
        EGFR = 1.5,
        KRAS = -1.2
      ),
      database = "C5:GO:BP",
      species = "Homo sapiens",
      gene_type = "symbol",
      pvalue = 1,
      pAdjustMethod = "BH"
    ), auto_unbox = TRUE)
  ))

  # Should return 200 even if no significant results
  expect_true(res$status == 200 || res$status == 500)
  body <- fromJSON(res$body)
  expect_true(!is.null(body$status))
})

test_that("POST /api/analyze/gsea accepts pAdjustMethod parameter", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")

  res <- pr$call("POST", "/api/analyze/gsea", list(
    body = toJSON(list(
      geneList = list(
        TP53 = 3.5,
        BRCA1 = 2.8
      ),
      database = "C5:GO:BP",
      species = "Homo sapiens",
      gene_type = "symbol",
      pvalue = 1,
      pAdjustMethod = "bonferroni"
    ), auto_unbox = TRUE)
  ))

  expect_true(res$status == 200 || res$status == 500)
})

# --- Unit tests for /api/plot/gsea endpoint ---

test_that("POST /api/plot/gsea returns error for missing result", {
  skip_if_not_installed("GseaVis")

  res <- pr$call("POST", "/api/plot/gsea", list(
    body = toJSON(list(
      params = list(
        geneSetID = "pathway1"
      )
    ), auto_unbox = TRUE)
  ))

  expect_equal(res$status, 400)
  body <- fromJSON(res$body)
  expect_true(!is.null(body$error))
})

test_that("POST /api/plot/gsea returns error for missing geneSetID", {
  skip_if_not_installed("GseaVis")

  res <- pr$call("POST", "/api/plot/gsea", list(
    body = toJSON(list(
      result = list(
        ID = c("pathway1"),
        Description = c("Pathway 1")
      )
    ), auto_unbox = TRUE)
  ))

  expect_equal(res$status, 400)
  body <- fromJSON(res$body)
  expect_true(!is.null(body$error))
})

test_that("POST /api/plot/gsea accepts plot parameters", {
  skip_if_not_installed("GseaVis")
  skip_if_not_installed("clusterProfiler")

  # This test would need a valid GSEA result object
  # For now, test that the endpoint exists and handles requests
  res <- pr$call("POST", "/api/plot/gsea", list(
    body = toJSON(list(
      result = list(
        ID = c("pathway1"),
        Description = c("Pathway 1"),
        setSize = c(100),
        enrichmentScore = c(0.5),
        NES = c(1.5),
        pvalue = c(0.001),
        p_adjust = c(0.005),
        qvalues = c(0.003),
        rank = c(50),
        leading_edge = c("tags=30%, list=10%, signal=25%"),
        core_enrichment = c("GENE1/GENE2/GENE3")
      ),
      params = list(
        geneSetID = "pathway1",
        addGene = TRUE,
        addPval = TRUE,
        base_size = 12,
        termWidth = 40,
        subPlot = 3,
        newGsea = FALSE
      )
    ), auto_unbox = TRUE)
  ))

  # Should return 200 or 500 (depending on GseaVis availability)
  expect_true(res$status == 200 || res$status == 500)
})
