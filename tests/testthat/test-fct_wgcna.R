library(testthat)

# Source WGCNA functions directly
source(file.path(dirname(dirname(dirname(getwd()))), "backend", "R", "fct_wgcna.R"))

# --- Unit tests for datExpr0_func ---

test_that("datExpr0_func processes expression data correctly", {
  skip_if_not_installed("WGCNA")

  # Create a simple expression data frame
  expr <- data.frame(
    Gene = paste0("Gene", 1:20),
    matrix(rnorm(20 * 10), nrow = 20, ncol = 10)
  )
  colnames(expr)[-1] <- paste0("Sample", 1:10)

  result <- datExpr0_func(expr)

  # Should have samples as rows, genes as columns
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 20)
  # Row names should be sample names
  expect_true(all(rownames(result) %in% paste0("Sample", 1:10)))
})

test_that("datExpr0_func removes duplicate genes", {
  skip_if_not_installed("WGCNA")

  expr <- data.frame(
    Gene = c("Gene1", "Gene1", "Gene2"),
    S1 = c(1, 2, 3),
    S2 = c(4, 5, 6)
  )

  result <- datExpr0_func(expr)

  # Should keep only one Gene1
  expect_equal(ncol(result), 2)
})

# --- Unit tests for datExpr_func ---

test_that("datExpr_func removes outlier samples", {
  skip_if_not_installed("WGCNA")

  # Create expression data with one outlier
  set.seed(42)
  expr <- data.frame(matrix(rnorm(20 * 10), nrow = 10, ncol = 20))
  colnames(expr) <- paste0("Gene", 1:20)
  rownames(expr) <- paste0("Sample", 1:10)

  # Make one sample very different
  expr[10, ] <- rnorm(20, mean = 100)

  sampleTree <- hclust(dist(expr), method = "average")
  result <- datExpr_func(expr, cutHeight = 50)

  # The outlier should be removed
  expect_lt(nrow(result), 10)
})

# --- Unit tests for process_trait_func ---

test_that("process_trait_func aligns traits with expression samples", {
  datExpr <- data.frame(
    Gene1 = c(1, 2, 3),
    Gene2 = c(4, 5, 6)
  )
  rownames(datExpr) <- c("S1", "S2", "S3")

  datTrait <- data.frame(
    Sample = c("S1", "S2", "S3", "S4"),
    weight = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )

  result <- process_trait_func(datTrait, datExpr)

  expect_equal(nrow(result), 3)
  expect_true("weight" %in% names(result))
  expect_true(all(rownames(result) %in% c("S1", "S2", "S3")))
})

test_that("process_trait_func converts factor traits to numeric", {
  datExpr <- data.frame(Gene1 = c(1, 2, 3))
  rownames(datExpr) <- c("S1", "S2", "S3")

  datTrait <- data.frame(
    Sample = c("S1", "S2", "S3"),
    category = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )

  result <- process_trait_func(datTrait, datExpr)

  # Category should be converted to numeric
  expect_true(is.numeric(result$category))
})

# --- Unit tests for pick_power_func ---

test_that("pick_power_func returns list with svg and power", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(50 * 30), nrow = 50, ncol = 30))
  colnames(datExpr) <- paste0("Gene", 1:30)

  result <- pick_power_func(datExpr, abline = 0.8)

  expect_type(result, "list")
  expect_true("svg" %in% names(result))
  expect_true("recommended_power" %in% names(result))
  expect_true("fit_indices" %in% names(result))
  expect_type(result$svg, "character")
  expect_true(grepl("<svg", result$svg))
})

test_that("pick_power_func uses default power when none found", {
  skip_if_not_installed("WGCNA")

  # Very small dataset may not find optimal power
  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(10 * 5), nrow = 10, ncol = 5))
  colnames(datExpr) <- paste0("Gene", 1:5)

  result <- pick_power_func(datExpr, abline = 0.99)

  # Should still return valid result
  expect_type(result, "list")
  expect_true(is.numeric(result$recommended_power))
})

# --- Unit tests for blockwiseModules_func ---

test_that("blockwiseModules_func returns network object", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  # Create a dataset with enough genes
  datExpr <- data.frame(matrix(rnorm(100 * 50), nrow = 100, ncol = 50))
  colnames(datExpr) <- paste0("Gene", 1:50)

  net <- blockwiseModules_func(datExpr, power = 6, mergeCutHeight = 0.25)

  expect_true("colors" %in% names(net))
  expect_true("dendrograms" %in% names(net))
  expect_true("blockGenes" %in% names(net))
  expect_equal(length(net$colors), 50)
})

# --- Unit tests for render_module_dendro_svg ---

test_that("render_module_dendro_svg returns SVG string", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(100 * 50), nrow = 100, ncol = 50))
  colnames(datExpr) <- paste0("Gene", 1:50)

  net <- blockwiseModules_func(datExpr, power = 6)
  svg <- render_module_dendro_svg(net)

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
  expect_true(grepl("</svg>", svg))
})

# --- Unit tests for compute_module_trait ---

test_that("compute_module_trait returns correlations and p-values", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(100 * 50), nrow = 100, ncol = 50))
  colnames(datExpr) <- paste0("Gene", 1:50)

  net <- blockwiseModules_func(datExpr, power = 6)

  datTraits <- data.frame(
    weight = rnorm(100),
    age = rnorm(100)
  )
  rownames(datTraits) <- paste0("S", 1:100)

  result <- compute_module_trait(datExpr, net, datTraits)

  expect_true("MEs" %in% names(result))
  expect_true("moduleTraitCor" %in% names(result))
  expect_true("moduleTraitPvalue" %in% names(result))
  expect_true("nSamples" %in% names(result))
  expect_equal(result$nSamples, 100)
})

# --- Unit tests for render_module_trait_svg ---

test_that("render_module_trait_svg returns SVG string", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(100 * 50), nrow = 100, ncol = 50))
  colnames(datExpr) <- paste0("Gene", 1:50)

  net <- blockwiseModules_func(datExpr, power = 6)
  moduleColors <- labels2colors(net$colors)
  MEs0 <- moduleEigengenes(datExpr, moduleColors)$eigengenes
  MEs <- orderMEs(MEs0)

  datTraits <- data.frame(weight = rnorm(100))
  rownames(datTraits) <- paste0("S", 1:100)

  mt_result <- compute_module_trait(datExpr, net, datTraits)
  svg <- render_module_trait_svg(
    mt_result$moduleTraitCor,
    mt_result$moduleTraitPvalue,
    datTraits,
    MEs
  )

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
})

# --- Unit tests for render_tom_svg ---

test_that("render_tom_svg returns SVG string", {
  skip_if_not_installed("WGCNA")

  set.seed(42)
  datExpr <- data.frame(matrix(rnorm(100 * 50), nrow = 100, ncol = 50))
  colnames(datExpr) <- paste0("Gene", 1:50)

  net <- blockwiseModules_func(datExpr, power = 6)
  moduleColors <- labels2colors(net$colors)

  svg <- render_tom_svg(datExpr, power = 6, moduleColors, nSelect = 30)

  expect_type(svg, "character")
  expect_true(grepl("<svg", svg))
})
