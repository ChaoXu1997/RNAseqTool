#' WGCNA
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import WGCNA

datExpr0_func <- function(expr) {
  femData = expr %>%
    dplyr::distinct(Gene, .keep_all = T) %>%
    tibble::column_to_rownames("Gene")
  datExpr0 = as.data.frame(t(femData))
  ## -- good sample ----
  gsg = goodSamplesGenes(datExpr0, verbose = 3)
  gsg$allOK
  if (!gsg$allOK) {
    # Optionally, print the gene and sample names that were removed:
    if (sum(!gsg$goodGenes) > 0)
      printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")))

    if (sum(!gsg$goodSamples) > 0)
      printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")))

    # Remove the offending genes and samples from the data:
    datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
  }
  return(datExpr0)
}


savePlot <- function(func,fileType,width, height){
  #on.exit(dev.off())
  if(fileType == "pdf"){
    outfile <- tempfile(fileext = ".pdf")
    pdf(outfile,width=width, height=height)
    func
    dev.off()
    return(outfile)
  }else if(fileType == "png"){
    outfile <- tempfile(fileext = ".png")
    #outfile <- "./test.png"
    png(outfile,width =width, height =height)
    func
    dev.off()
    return(outfile)
  }
}


# sampleTree = hclust(dist(datExpr0), method = "average")
sampleTree_plot <- function(sampleTree,
                            cutHeight,
                            fileType, width , height) {
  savePlot({
    #on.exit(dev.off())
    par(cex = 0.6)
    par(mar = c(0, 4, 2, 0))
    plot(
      sampleTree,
      main = "Sample clustering to detect outliers",
      sub = "",
      xlab = "",
      cex.lab = 1.5,
      cex.axis = 1.5,
      cex.main = 2
    );
    abline(h = cutHeight, col = "red")
  }, fileType, width = width, height = height)


}
#sampleTree_plot(sampleTree,15,"png")




datExpr_func <-  function(datExpr0,
                          sampleTree,
                          cutHeight = 15) {
  clust = cutreeStatic(sampleTree, cutHeight = cutHeight, minSize = 10)
  # clust 1 contains the samples we want to keep.
  keepSamples = (clust == 1)
  datExpr = datExpr0[keepSamples, ]
  return(datExpr)
}

# library(tidyverse)
# library(WGCNA)
# datTrait <- openxlsx::read.xlsx("./data/WGCNA_ClinicalTraits.xlsx")
# datExpr <- openxlsx::read.xlsx("./data/WGCNA_LiverFemale3600.xlsx")
# datExpr <- datExpr0_func(datExpr)

datTrait <- function(datTrait,datExpr){
  # Loading clinical trait data
  allTraits = datTrait %>%
    dplyr::mutate(across(where(is.character )& !Sample, as.factor)) %>%
    dplyr::mutate(across(where(is.factor )& !Sample, as.numeric))
  # remove columns that hold information we do not need.
  femaleSamples = rownames(datExpr);
  traitRows = match(femaleSamples, allTraits$Sample);
  datTraits = allTraits[traitRows, -1];
  rownames(datTraits) = allTraits[traitRows, 1]
  return(datTraits)

}


#sampleTree2 = hclust(dist(datExpr), method = "average")
sampleTree2_func <- function(sampleTree2,
                             datTraits,
                             fileType, width , height){

  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    # Convert traits to a color representation: white means low, red means high, grey means missing entry
    traitColors = numbers2colors(datTraits, signed = FALSE);
    # Plot the sample dendrogram and the colors underneath.
    plotDendroAndColors(sampleTree2, traitColors,
                        groupLabels = names(datTraits),
                        main = "Sample dendrogram and trait heatmap")
  }, fileType, width = width, height = height)

}


power_func <- function(datExpr,
                       abline=0.8,
                       fileType,
                       width,
                       height ){
  powers = c(c(1:10), seq(from = 12, to=20, by=2))
  # Call the network topology analysis function
  sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    # Plot the results:
    #sizeGrWindow(9, 5)
    par(mfrow = c(1,2));
    cex1 = 0.9;
    # Scale-free topology fit index as a function of the soft-thresholding power
    plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
         xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
         main = paste("Scale independence"));
    text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
         labels=powers,cex=cex1,col="red");
    # this line corresponds to using an R^2 cut-off of h
    abline(h=abline,col="red")
    # Mean connectivity as a function of the soft-thresholding power
    plot(sft$fitIndices[,1], sft$fitIndices[,5],
         xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
         main = paste("Mean connectivity"))
    text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
  }, fileType, width = width, height = height)
}

blockwiseModules_func <- function(datExpr, power = 6,mergeCutHeight=0.25){
  blockwiseModules(datExpr, power = power,
                   TOMType = "unsigned", minModuleSize = 30,
                   reassignThreshold = 0, mergeCutHeight = mergeCutHeight,
                   numericLabels = TRUE, pamRespectsDendro = FALSE,
                   saveTOMs = F,
                   saveTOMFileBase = "femaleMouseTOM",
                   verbose = 3)
}

plotDendroAndColors_plot <- function(net,fileType, width = width, height = height){
  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    # open a graphics window
    #sizeGrWindow(12, 9)
    # Convert labels to colors for plotting
    mergedColors = labels2colors(net$colors)
    # Plot the dendrogram and the module colors underneath
    plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                        "Module colors",
                        dendroLabels = FALSE, hang = 0.03,
                        addGuide = TRUE, guideHang = 0.05)
  }, fileType, width = width, height = height)

}


# moduleLabels = net$colors
# moduleColors = labels2colors(net$colors)
# MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes
# MEs = orderMEs(MEs0)

labeledHeatmap_plot <- function(MEs,datTraits,nSamples,
                                fileType, width = width, height = height){
  moduleTraitCor = cor(MEs, datTraits, use = "p");
  moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)

  sizeGrWindow(10,6)
  # Will display correlations and their p-values
  textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
                     signif(moduleTraitPvalue, 1), ")", sep = "");
  dim(textMatrix) = dim(moduleTraitCor)
  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    par(mar = c(6, 8.5, 3, 3));
    # Display the correlation values within a heatmap plot
    labeledHeatmap(Matrix = moduleTraitCor,
                   xLabels = names(datTraits),
                   yLabels = names(MEs),
                   ySymbols = names(MEs),
                   colorLabels = FALSE,
                   colors = greenWhiteRed(50),
                   textMatrix = textMatrix,
                   setStdMargins = FALSE,
                   cex.text = 0.5,
                   zlim = c(-1,1),
                   main = paste("Module-trait relationships"))
  }, fileType, width = width, height = height)

}


verboseScatterplot_plot <- function(geneModuleMembership,
                                    geneTraitSignificance,
                                    module,
                                    modNames,
                                    moduleColors,
                                    trait,
                                    traitNames,
                                    fileType, width , height ){
  #module = "brown"
  column_MM = match(module, modNames)
  moduleGenes = moduleColors==module;
  #trait = "weight_g"
  column_GS = match(trait, traitNames)#names(datTraits)
  #sizeGrWindow(7, 7);
  #par(mfrow = c(1,1));
  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    verboseScatterplot(abs(geneModuleMembership[moduleGenes, column_MM]),
                       abs(geneTraitSignificance[moduleGenes, column_GS]),
                       xlab = paste("Module Membership in", module, "module"),
                       ylab = paste("Gene significance for",trait),
                       main = paste("Module membership vs. gene significance\n"),
                       cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
  }, fileType, width = width, height = height)

}

# library(tidyverse)
# library(WGCNA)
# datTrait <- openxlsx::read.xlsx("./data/WGCNA_ClinicalTraits.xlsx")
# datExpr <- openxlsx::read.xlsx("./data/WGCNA_LiverFemale3600.xlsx")
# datExpr <- datExpr0_func(datExpr)
# power <- 6
# nGenes <- ncol(datExpr)
# net <- blockwiseModules_func(datExpr)
# moduleLabels = net$colors
# moduleColors = labels2colors(net$colors)

TOMplot_plot <- function(datExpr,power,
                         nSelect = 400,
                         nGenes,
                         moduleColors,
                         fileType, width ,height ){
  # Calculate topological overlap anew: this could be done more efficiently by saving the TOM
  # calculated during module detection, but let us do it again here.
  dissTOM = 1-TOMsimilarityFromExpr(datExpr, power = power);

  #nSelect = nSelects
  # For reproducibility, we set the random seed
  #set.seed(10);
  select = sample(nGenes, size = nSelect);
  selectTOM = dissTOM[select, select];
  # There’s no simple way of restricting a clustering tree to a subset of genes, so we must re-cluster.
  selectTree = hclust(as.dist(selectTOM), method = "average")
  selectColors = moduleColors[select];
  # Taking the dissimilarity to a power, say 10, makes the plot more informative by effectively changing
  # the color palette; setting the diagonal to NA also improves the clarity of the plot
  plotDiss = selectTOM^7;
  diag(plotDiss) = NA
  savePlot({
    on.exit(while (!is.null(dev.list())) dev.off() )
    # Open a graphical window
    #sizeGrWindow(9,9)
    TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected genes")
  }, fileType, width = width, height = height)
}
# MEs = moduleEigengenes(datExpr, moduleColors)$eigengenes
# datTraits <- datTrait[1:135,]
# trait <- names(datTrait)[2]

# plotEigengeneNetworks_plot <- function(MEs,
#                                        datTraits,
#                                        trait,
#                                        fileType, width , height
#                                        ){
#   # Isolate weight from the clinical traits
#   dat = as.data.frame(datTraits[,trait]);
#   names(dat) = trait
#   # Add the weight to existing module eigengenes
#   MET = orderMEs(cbind(MEs, dat))
#   savePlot({
#     on.exit(while (!is.null(dev.list())) dev.off() )
#     # Plot the relationships among the eigengenes and the trait
#     sizeGrWindow(5,7.5);
#     par(cex = 0.9)
#     plotEigengeneNetworks(MET, "", marDendro = c(0,4,1,2),
#                           marHeatmap = c(3,4,1,2), cex.lab = 0.8,
#                           xLabelsAngle = 90)
#   }, fileType, width = width, height = height)
#
#
# }



