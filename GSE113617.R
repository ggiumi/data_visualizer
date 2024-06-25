rm(list = ls())

library(GEOquery)
library(DESeq2)
library(org.Hs.eg.db)
library(ReactomePA)
library(msigdbr)
library(clusterProfiler)
library(ggplot2)
library(pheatmap)
library(RColorBrewer)
library(EnhancedVolcano)
library(reactome.db)
library(gplots)
library(edgeR)
library(viridis)


if (file.exists("stored_objects_GSE113617.RData")) {
  load("stored_objects_GSE113617.RData")
} else {
  geo_ID <- "GSE113617"
  
  geo_dataset <- getGEO(geo_ID)
  
  colData <- pData(phenoData(geo_dataset[[1]]))
  
  # Print the column names of the clinical information
  print(names(colData))
  
  # Specify the column names you want to keep
  columns_to_keep <- c("geo_accession", "characteristics_ch1.1", "characteristics_ch1.2")
  
  # Subset the clinical information data frame to keep only the specified columns
  colData <- colData[, columns_to_keep]
  
  # Print the subsetted clinical information
  print(colData)
  
  # Rename column for convenience
  colnames(colData) <- c("sample_geo_accession", "molecular_subtype", "disease_state")
  
  colData$molecular_subtype <- as.factor(as.character(colData$molecular_subtype))
  colData$disease_state <- as.factor(as.character(colData$disease_state))
  
  colData$disease_state <- sub("^disease state: ", "", colData$disease_state)
  colData$molecular_subtype <- sub("^molecular subtype: ", "", colData$molecular_subtype)
  
  countsData <- read.delim("/home/giulia/GEO/GSE113617_raw_counts_GRCh38.p13_NCBI.tsv", 
                           row.names = 1, check.names = FALSE)
  
  countsData <- countsData[, !duplicated(t(countsData))]
  
  dds <- DESeqDataSetFromMatrix(countData = countsData,
                                colData = colData,
                                design = ~ disease_state) #import data
  keep <- rowSums(counts(dds)) >= 10
  dds <- dds[keep,]
  dds <- DESeq(dds) #we normalize and linear modeling with only one function
  res <- results(dds, contrast = c("disease_state", "HCC", "non-tumor")) #we perform the comparison between the experimental group of samples
  res
  resultsNames(dds)
  resLFC <- lfcShrink(dds, coef="disease_state_non.tumor_vs_HCC", type="apeglm")
  res05 <- results(dds, alpha=0.05)
  
  # create an object to keep colnames to be able to stratify the analysis
  colNames <- colnames(colData)
  
  # Cpm for boxplots
  cpm <- cpm(dds)
  
  # Calculate pairwise distance between samples based on gene expression
  sampleDists <- dist(t(assay(dds)))
  
  # Convert the distance object to a matrix
  sampleDistMatrix <- as.matrix(sampleDists)
  
  # Estrai i dati di conteggio
  counts_matrix <- counts(dds)
  
  # Extract normalized counts
  normalized_counts <- counts(dds, normalized=TRUE)
  
  # Subset colData to include only the desired columns
  selected_colData <- colData(dds)[c("molecular_subtype", "disease_state")]
  
  # Ensure colData has the correct rownames corresponding to the sample names
  rownames(colData) <- colData$sample_geo_accession
  
  # Check if colData and disease_state are correctly defined
  if (!"disease_state" %in% colnames(colData)) {
    stop("The colData dataframe does not contain a 'disease_state' column.")
  }
  
  # Check the unique values in disease_state column
  print("Unique values in disease_state:")
  print(unique(colData$disease_state))
  
  # Define control and case samples based on colData$disease_state
  control <- colData$sample_geo_accession[colData$disease_state == "non-tumor"]
  case <- colData$sample_geo_accession[colData$disease_state == "HCC"]
  
  # # Debugging: Check if the sample names match
  # print("Control samples:")
  # print(control)
  # print("Case samples:")
  # print(case)
  
  # Normalize counts
  normalized_counts <- counts(dds, normalized=TRUE)
  
  # Subset the data to keep only the top 10000 most variable genes
  topVarGenes <- head(order(rowVars(normalized_counts), decreasing=TRUE), 10000)
  data <- normalized_counts[topVarGenes, ]
  
  # # Debugging: Check column names in data
  # print("Data column names:")
  # print(colnames(data))
  
  # Define samples as control or case
  samples <- ifelse(colnames(data) %in% control, "non-tumor", ifelse(colnames(data) %in% case, "HCC", NA))
  
  # # Debugging: Check if all samples are correctly classified
  # print("Samples classified:")
  # print(samples)
  
  # Remove NA samples if there are any
  valid_samples <- !is.na(samples)
  data <- data[, valid_samples]
  samples <- samples[valid_samples]
  
  annotation <- data.frame(SampleType = samples)
  rownames(annotation) <- colnames(data)
  
  # Define annotation colors
  annotation_colors <- list(SampleType = c(`non-tumor` = "green", `HCC` = "red"))
  
  # Define the color scheme for the heatmap
  colorbar <- colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100)
  
  # Save the objects
  save(dds, res, counts_matrix, sampleDists, sampleDistMatrix, colNames, normalized_counts, control, samples, annotation, annotation_colors, colorbar, cpm, data, selected_colData, file = "stored_objects_GSE113617.RData")
}

createPCAPlot <- function(dds) {
  pca_data <- plotPCA(vst(dds), intgroup = "disease_state", returnData = TRUE)
  pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = disease_state)) +
    geom_point() +
    labs(x = "PC1", y = "PC2", title = "PCA Plot") +
    theme_minimal()
  return(pca_plot)
}

pca_plot <- createPCAPlot(dds)
print(pca_plot)

generateHeatmap <- function(data, annotation, annotation_colors, colorbar) {
  pheatmap(data, 
           scale = "row", 
           border_color = NA, 
           clustering_method = "ward.D2", 
           cluster_cols = TRUE, 
           cluster_rows = TRUE, 
           annotation_col = annotation, 
           annotation_colors = annotation_colors, 
           color = colorbar, 
           show_rownames = FALSE, 
           show_colnames = FALSE, 
           main = "Heatmap of 10000 most Variable Genes")
}

library(EnhancedVolcano)

generateVolcanoPlot <- function(res, title = 'Volcano Plot', subtitle = 'Differential Expression', caption = NULL) {
  EnhancedVolcano(
    res,
    lab = rownames(res),
    x = 'log2FoldChange',
    y = 'pvalue',
    xlim = c(-10, 10),
    legendPosition = "right",
    legendIconSize = 3,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}
# Assuming 'res' is your DESeq2 results object
generateVolcanoPlot(res, title = 'My Volcano Plot', subtitle = 'DE Analysis', caption = 'DESeq2 Results')

heatmap_DE <- function(sampleDistMatrix){
  pheatmap(sampleDistMatrix, 
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           col = viridis(100),
           show_rownames = FALSE,  # Do not show row names
           show_colnames = FALSE)  # Do not show column names
}

generateGeneBoxplot <- function(cpm, gene_symbols) {
  # Extract expression values for the specified genes
  gene_expression <- counts(cpm)[gene_symbols, , drop = FALSE]
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Gene = rep(gene_symbols, each = ncol(gene_expression)),
    Condition = rep(colData(cpm)$disease_state, times = length(gene_symbols)),
    Expression = as.vector(t(gene_expression))
  )
  
  # Adjust the number of columns in facet_wrap based on the number of selected genes
  num_cols <- length(gene_symbols)
  
  # Plotting boxplot
  gene_boxplot <- ggplot(plot_data, aes(x = Condition, y = Expression, fill = Condition)) +
    geom_boxplot() +
    facet_wrap(~ Gene, ncol = num_cols, scales = "free_y") + # Separate plots for each gene
    labs(x = "Condition", y = "Expression", title = "Expression of Selected Genes by Condition") +
    theme_minimal() +
    scale_fill_manual(values = c("blue", "red")) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),    # Title font size
      axis.title.x = element_text(size = 14),                 # X-axis title font size
      axis.title.y = element_text(size = 14),                 # Y-axis title font size
      axis.text.x = element_text(size = 12),                  # X-axis text font size
      axis.text.y = element_text(size = 12),                  # Y-axis text font size
      strip.text = element_text(size = 12),                   # Facet labels font size
      legend.title = element_text(size = 14),                 # Legend title font size
      legend.text = element_text(size = 12)                   # Legend text font size
    )
  
  return(gene_boxplot)
}

# # Load required libraries if not already loaded
# library(pheatmap)
# library(RColorBrewer)
# 
# # Create the heatmap with hierarchical clustering
# pheatmap(sampleDistMatrix,
#          clustering_distance_rows = sampleDists,
#          clustering_distance_cols = sampleDists,
#          col = colorRampPalette(rev(brewer.pal(9, 'PuRd')))(170),
#          show_rownames = FALSE,  # Do not show row names
#          show_colnames = FALSE)  # Do not show column names
# 
# # Load necessary libraries
# library(DESeq2)
# library(pheatmap)
# library(viridis)  # Optional for using the viridis color palette
# 
# # Extract normalized counts
# normalized_counts <- counts(dds, normalized=TRUE)
# 
# # Define HCC and non-tumor samples based on your colData
# HCC <- colData$sample_geo_accession[colData$disease_state == "HCC"]
# non_tumor <- colData$sample_geo_accession[colData$disease_state == "non-tumor"]
# 
# # Define samples as HCC or non-tumor
# samples <- ifelse(colnames(normalized_counts) %in% HCC, "HCC", "non-tumor")
# 
# # Create annotation data frame
# annotation <- data.frame(SampleType = samples)
# rownames(annotation) <- colnames(normalized_counts)
# 
# # Define annotation colors with custom colors for HCC and non-tumor
# annotation_colors <- list(SampleType = c(HCC = "red", `non-tumor` = "green"))
# 
# # Define the color scheme for the heatmap using the viridis palette
# colorbar <- viridis(100)
# 
# # Plot the heatmap
# pheatmap(normalized_counts, 
#          scale = "row", 
#          border_color = NA, 
#          clustering_distance_rows = "correlation", 
#          clustering_distance_cols = "correlation", 
#          clustering_method = "complete", 
#          cluster_cols = TRUE, 
#          cluster_rows = FALSE, 
#          annotation_col = annotation, 
#          annotation_colors = annotation_colors, 
#          color = colorbar, 
#          show_rownames = FALSE, 
#          show_colnames = FALSE, 
#          main = "Heatmap of All Genes")