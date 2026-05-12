#' WGCNA Co-expression Network Analysis Functions
#'
#' @description Functions for WGCNA analysis: sample tree, power selection,
#' network construction, module-trait associations
#' @noRd
#' @import WGCNA

#' Preprocess expression data for WGCNA
#'
#' @param expr Data frame with Gene column + sample columns
#' @return Data frame with samples as rows, genes as columns
datExpr0_func <- function(expr) {
  femData <- expr %>%
    dplyr::distinct(Gene, .keep_all = TRUE) %>%
    tibble::column_to_rownames("Gene")
  datExpr0 <- as.data.frame(t(femData))

  gsg <- goodSamplesGenes(datExpr0, verbose = 3)
  if (!gsg$allOK) {
    if (sum(!gsg$goodGenes) > 0)
      datExpr0 <- datExpr0[, gsg$goodGenes]
    if (sum(!gsg$goodSamples) > 0)
      datExpr0 <- datExpr0[gsg$goodSamples, ]
  }
  return(datExpr0)
}

#' Remove outlier samples based on clustering
#'
#' @param datExpr0 Preprocessed expression data
#' @param cutHeight Height to cut the dendrogram
#' @return Filtered expression data
datExpr_func <- function(datExpr0, cutHeight = 15) {
  sampleTree <- hclust(dist(datExpr0), method = "average")
  clust <- cutreeStatic(sampleTree, cutHeight = cutHeight, minSize = 10)
  keepSamples <- (clust == 1)
  datExpr <- datExpr0[keepSamples, ]
  return(datExpr)
}

#' Process trait data and align with expression samples
#'
#' @param datTrait Data frame with Sample column + trait columns
#' @param datExpr Expression data (samples as rows)
#' @return Processed trait data frame
process_trait_func <- function(datTrait, datExpr) {
  allTraits <- datTrait %>%
    dplyr::mutate(across(where(is.character) & !Sample, as.factor)) %>%
    dplyr::mutate(across(where(is.factor) & !Sample, as.numeric))
  femaleSamples <- rownames(datExpr)
  traitRows <- match(femaleSamples, allTraits$Sample)
  datTraits <- allTraits[traitRows, -1]
  rownames(datTraits) <- allTraits[traitRows, 1]
  return(datTraits)
}

#' Render sample clustering tree as SVG
#'
#' @param datExpr Expression data
#' @param cutHeight Height for cutting outliers
#' @return SVG string
render_sampletree_svg <- function(datExpr, cutHeight = 15) {
  sampleTree <- hclust(dist(datExpr), method = "average")

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 10, height = 6)
  par(cex = 0.6, mar = c(0, 4, 2, 0))
  plot(
    sampleTree,
    main = "Sample clustering to detect outliers",
    sub = "",
    xlab = "",
    cex.lab = 1.5,
    cex.axis = 1.5,
    cex.main = 2
  )
  abline(h = cutHeight, col = "red")
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Render sample tree with trait heatmap as SVG
#'
#' @param datExpr Expression data
#' @param datTraits Trait data aligned with samples
#' @return SVG string
render_sampletree_trait_svg <- function(datExpr, datTraits) {
  sampleTree2 <- hclust(dist(datExpr), method = "average")
  traitColors <- numbers2colors(datTraits, signed = FALSE)

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 12, height = 9)
  plotDendroAndColors(
    sampleTree2, traitColors,
    groupLabels = names(datTraits),
    main = "Sample dendrogram and trait heatmap"
  )
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Pick soft-thresholding power for WGCNA
#'
#' @param datExpr Expression data
#' @param abline R-squared threshold line
#' @return List with sft object and SVG string
pick_power_func <- function(datExpr, abline = 0.8) {
  powers <- c(c(1:10), seq(from = 12, to = 20, by = 2))
  sft <- pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 10, height = 5)
  par(mfrow = c(1, 2))
  cex1 <- 0.9

  plot(
    sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
    xlab = "Soft Threshold (power)",
    ylab = "Scale Free Topology Model Fit, signed R^2",
    type = "n",
    main = "Scale independence"
  )
  text(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
       labels = powers, cex = cex1, col = "red")
  abline(h = abline, col = "red")

  plot(
    sft$fitIndices[, 1], sft$fitIndices[, 5],
    xlab = "Soft Threshold (power)",
    ylab = "Mean Connectivity",
    type = "n",
    main = "Mean connectivity"
  )
  text(sft$fitIndices[, 1], sft$fitIndices[, 5],
       labels = powers, cex = cex1, col = "red")
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)

  # Find recommended power
  recommended_power <- sft$powerEstimate
  if (is.na(recommended_power)) {
    recommended_power <- 6  # default fallback
  }

  list(
    svg = paste(svg_content, collapse = "\n"),
    recommended_power = recommended_power,
    fit_indices = as.data.frame(sft$fitIndices[, c(1, 2, 3, 5)])
  )
}

#' Build WGCNA network using blockwiseModules
#'
#' @param datExpr Expression data
#' @param power Soft-thresholding power
#' @param mergeCutHeight Height for merging modules
#' @return List with network result
blockwiseModules_func <- function(datExpr, power = 6, mergeCutHeight = 0.25) {
  net <- blockwiseModules(
    datExpr,
    power = power,
    TOMType = "unsigned",
    minModuleSize = 30,
    reassignThreshold = 0,
    mergeCutHeight = mergeCutHeight,
    numericLabels = TRUE,
    pamRespectsDendro = FALSE,
    saveTOMs = FALSE,
    verbose = 3
  )
  return(net)
}

#' Render module dendrogram as SVG
#'
#' @param net Network result from blockwiseModules
#' @return SVG string
render_module_dendro_svg <- function(net) {
  mergedColors <- labels2colors(net$colors)

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 12, height = 9)
  plotDendroAndColors(
    net$dendrograms[[1]],
    mergedColors[net$blockGenes[[1]]],
    "Module colors",
    dendroLabels = FALSE,
    hang = 0.03,
    addGuide = TRUE,
    guideHang = 0.05
  )
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Compute module eigengenes and module-trait correlations
#'
#' @param datExpr Expression data
#' @param net Network result
#' @param datTraits Trait data
#' @return List with MEs, correlations, p-values
compute_module_trait <- function(datExpr, net, datTraits) {
  moduleLabels <- net$colors
  moduleColors <- labels2colors(net$colors)
  MEs0 <- moduleEigengenes(datExpr, moduleColors)$eigengenes
  MEs <- orderMEs(MEs0)

  nSamples <- nrow(datExpr)
  moduleTraitCor <- cor(MEs, datTraits, use = "p")
  moduleTraitPvalue <- corPvalueStudent(moduleTraitCor, nSamples)

  list(
    MEs = MEs,
    moduleColors = moduleColors,
    moduleTraitCor = moduleTraitCor,
    moduleTraitPvalue = moduleTraitPvalue,
    nSamples = nSamples
  )
}

#' Render module-trait heatmap as SVG
#'
#' @param moduleTraitCor Correlation matrix
#' @param moduleTraitPvalue P-value matrix
#' @param datTraits Trait data
#' @param MEs Module eigengenes
#' @return SVG string
render_module_trait_svg <- function(moduleTraitCor, moduleTraitPvalue, datTraits, MEs) {
  textMatrix <- paste(
    signif(moduleTraitCor, 2), "\n(",
    signif(moduleTraitPvalue, 1), ")",
    sep = ""
  )
  dim(textMatrix) <- dim(moduleTraitCor)

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 10, height = 8)
  par(mar = c(6, 8.5, 3, 3))
  labeledHeatmap(
    Matrix = moduleTraitCor,
    xLabels = names(datTraits),
    yLabels = names(MEs),
    ySymbols = names(MEs),
    colorLabels = FALSE,
    colors = greenWhiteRed(50),
    textMatrix = textMatrix,
    setStdMargins = FALSE,
    cex.text = 0.5,
    zlim = c(-1, 1),
    main = "Module-trait relationships"
  )
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Compute gene module membership and gene trait significance
#'
#' @param datExpr Expression data
#' @param datTraits Trait data
#' @param net Network result
#' @return List with geneModuleMembership and geneTraitSignificance
compute_mm_gs <- function(datExpr, datTraits, net) {
  nGenes <- ncol(datExpr)
  nSamples <- nrow(datExpr)
  moduleColors <- labels2colors(net$colors)
  modNames <- substring(names(MEs_func(datExpr, moduleColors)), 3)

  geneModuleMembership <- as.data.frame(cor(datExpr, MEs_func(datExpr, moduleColors), use = "p"))
  MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))
  names(geneModuleMembership) <- paste("MM", modNames, sep = "")
  names(MMPvalue) <- paste("p.MM", modNames, sep = "")

  geneTraitSignificance <- as.data.frame(cor(datExpr, datTraits, use = "p"))
  GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))
  names(geneTraitSignificance) <- paste("GS.", names(datTraits), sep = "")
  names(GSPvalue) <- paste("p.GS.", names(datTraits), sep = "")

  list(
    geneModuleMembership = geneModuleMembership,
    geneTraitSignificance = geneTraitSignificance,
    moduleColors = moduleColors,
    modNames = modNames
  )
}

#' Helper: compute module eigengenes
#' @noRd
MEs_func <- function(datExpr, moduleColors) {
  MEs0 <- moduleEigengenes(datExpr, moduleColors)$eigengenes
  orderMEs(MEs0)
}

#' Render module membership vs gene significance scatter plot as SVG
#'
#' @param geneModuleMembership Module membership data
#' @param geneTraitSignificance Gene significance data
#' @param module Module color
#' @param trait Trait name
#' @param moduleColors Module color assignments
#' @param modNames Module names
#' @return SVG string
render_mm_gs_svg <- function(geneModuleMembership, geneTraitSignificance,
                              module, trait, moduleColors, modNames) {
  column_MM <- match(module, modNames)
  moduleGenes <- moduleColors == module
  column_GS <- match(trait, names(geneTraitSignificance))

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 7, height = 7)
  verboseScatterplot(
    abs(geneModuleMembership[moduleGenes, column_MM]),
    abs(geneTraitSignificance[moduleGenes, column_GS]),
    xlab = paste("Module Membership in", module, "module"),
    ylab = paste("Gene significance for", trait),
    main = paste("Module membership vs. gene significance\n"),
    cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module
  )
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}

#' Render TOM heatmap as SVG
#'
#' @param datExpr Expression data
#' @param power Soft-thresholding power
#' @param moduleColors Module color assignments
#' @param nSelect Number of genes to select for TOM plot
#' @return SVG string
render_tom_svg <- function(datExpr, power, moduleColors, nSelect = 400) {
  nGenes <- ncol(datExpr)
  dissTOM <- 1 - TOMsimilarityFromExpr(datExpr, power = power)

  set.seed(10)
  select <- sample(nGenes, size = min(nSelect, nGenes))
  selectTOM <- dissTOM[select, select]
  selectTree <- hclust(as.dist(selectTOM), method = "average")
  selectColors <- moduleColors[select]
  plotDiss <- selectTOM^7
  diag(plotDiss) <- NA

  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  svg(tmp, width = 9, height = 9)
  TOMplot(
    plotDiss, selectTree, selectColors,
    main = "Network heatmap plot, selected genes"
  )
  dev.off()

  svg_content <- readLines(tmp, warn = FALSE)
  paste(svg_content, collapse = "\n")
}
