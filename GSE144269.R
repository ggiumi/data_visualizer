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

# Check if objects are already stored
# Load stored objects if they exist
if (file.exists("stored_objects_GSE144269.RData")) {
  load("stored_objects_GSE144269.RData")
} else {
  # Perform the analysis if objects are not stored
  geo_ID <- "GSE144269"
  
  geo_dataset <- getGEO(geo_ID)
  
  colData <- pData(phenoData(geo_dataset[[1]]))
  
  # Print the column names of the clinical information
  print(names(colData))
  
  # Specify the column names you want to keep
  columns_to_keep <- c("geo_accession", "tumor/non-tumor:ch1")
  
  # Subset the clinical information data frame to keep only the specified columns
  colData <- colData[, columns_to_keep]
  
  # Print the subsetted clinical information
  print(colData)
  
  # Rename column for convenience
  colnames(colData) <- c("sample_geo_accession", "tissues_subtype")
  
  # create an object to keep colnames to be able to stratify the analysis
  colNames <- colnames(colData)
  
  cpm <- cpm(dds)
  
  
  # Redefine data types
  colData$tissues_subtype <- as.factor(as.character(colData$tissues_subtype))
  
  # Print the updated data types and column names
  str(colData)
  
  countsData <- read.delim("/home/giulia/GEO/GSE144269_raw_counts_GRCh38.p13_NCBI.tsv", 
                           row.names = 1, check.names = FALSE)
  
  countsData <- countsData[, !duplicated(t(countsData))]
  
  dds <- DESeqDataSetFromMatrix(countData = countsData,
                                colData = colData,
                                design = ~ tissues_subtype) #import data
  keep <- rowSums(counts(dds)) >= 10
  dds <- dds[keep,]
  dds <- DESeq(dds) #we normalize and linear modeling with only one function
  res <- results(dds, contrast = c("tissues_subtype", "tumor", "non-tumor")) #we perform the comparison between the experimental group of samples
  res
  resLFC <- lfcShrink(dds, coef="tissues_subtype_tumor_vs_non.tumor", type="apeglm") #Shrinkage of effect size (LFC estimates) 
  resLFC
  #is useful for visualization and ranking of genes. It is more useful visualize the MA-plot for the shrunken log2 fold changes, 
  #which remove the noise associated with log2 fold changes from low count genes without requiring arbitrary filtering thresholds.
  res05 <- results(dds, alpha=0.05)
  counts_matrix <- counts(dds)
  
  # Calculate pairwise distance between samples based on gene expression
  sampleDists <- dist(t(assay(dds)))
  
  # Convert the distance object to a matrix
  sampleDistMatrix <- as.matrix(sampleDists)
  
  normalized_counts <- counts(dds, normalized=TRUE)
  
  # Subset colData to include only the desired columns
  selected_colData <- colData(dds)[c("sample_geo_accession","tissues_subtype")]
  
  # Ensure colData has the correct rownames corresponding to the sample names
  rownames(colData) <- colData$sample_geo_accession
  
  # Check if colData and tissues_subtype are correctly defined
  if (!"tissues_subtype" %in% colnames(colData)) {
    stop("The colData dataframe does not contain a 'tissues_subtype' column.")
  }
  
  # # Check the unique values in tissues_subtype column
  # print("Unique values in tissues_subtype:")
  # print(unique(colData$tissues_subtype))
  
  # Define control and case samples based on colData$tissues_subtype
  control <- colData$sample_geo_accession[colData$tissues_subtype == "non-tumor"]
  case <- colData$sample_geo_accession[colData$tissues_subtype == "tumor"]
  
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
  
  # Debugging: Check column names in data
  print("Data column names:")
  print(colnames(data))
  
  # Define samples as control or case
  samples <- ifelse(colnames(data) %in% control, "non-tumor", ifelse(colnames(data) %in% case, "tumor", NA))
  
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
  annotation_colors <- list(SampleType = c("non-tumor" = "green", "tumor" = "red"))
  
  # Define the color scheme for the heatmap
  colorbar <- colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100)
  
  
  # Save the objects
  save(dds, res, counts_matrix, sampleDists, sampleDistMatrix, colNames, normalized_counts, control, samples, annotation, annotation_colors, colorbar, cpm, data, selected_colData, file = "stored_objects_GSE144269.RData")
}

# Function to create PCA plot
createPCAPlot <- function(dds) {
  pca_data <- plotPCA(vst(dds), intgroup = "tissues_subtype", returnData = TRUE)
  pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = tissues_subtype)) +
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
    Condition = rep(colData(cpm)$tissues_subtype, times = length(gene_symbols)),
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

# rm(list = ls())
# 
# library(GEOquery)
# library(DESeq2)
# library(org.Hs.eg.db)
# library(ReactomePA)
# library(msigdbr)
# library(clusterProfiler)
# library(ggplot2)
# library(pheatmap)
# library(RColorBrewer)
# library(EnhancedVolcano)
# library(reactome.db)
# library(gplots)
# library(edgeR)
# library(viridis)
# 
# geo_ID <- "GSE144269"
# columns_to_keep <- c("geo_accession", "tumor/non-tumor:ch1")
# raw_counts_path <- "/home/giulia/GEO/GSE144269_raw_counts_GRCh38.p13_NCBI.tsv"
# contrast <- c("tissues_subtype", "tumor", "non-tumor")
# renamed <- c("sample_geo_accession", "tissues_subtype")
# # my_design_variable <- "tissues_subtype"
# # Create the formula dynamically using as.formula
# design_formula <- as.formula(paste("~", "tissues_subtype"))
# selected_columns <- c("sample_geo_accession", "tissues_subtype")
# 
# # Function to load or perform the analysis
# perform_analysis <- function(geo_ID, columns_to_keep, raw_counts_path, contrast, design_formula, selection, selected_columns) {
#   stored_objects_file <- paste0("stored_objects_", geo_ID, ".RData")
# # Check if objects are already stored
# # Load stored objects if they exist
#   if (file.exists(stored_objects_file)) {
#     load(stored_objects_file)
#   } else {
#     geo_dataset <- getGEO(geo_ID)
#     
#     colData <- pData(phenoData(geo_dataset[[1]]))
#     
#     # Print the column names of the clinical information
#     print(names(colData))
#     
#     # # Specify the column names you want to keep
#     # columns_to_keep <- columns_to_keep
#     
#     # Subset the clinical information data frame to keep only the specified columns
#     colData <- colData[, columns_to_keep]
#     
#     # Print the subsetted clinical information
#     print(colData)
#     
#     # Rename column for convenience
#     colnames(colData) <- renamed
#     
#     # create an object to keep colnames to be able to stratify the analysis
#     colNames <- colnames(colData)
#     
#     # Redefine data types
#     colData$tissues_subtype <- as.factor(as.character(colData$tissues_subtype))
#     
#     # Print the updated data types and column names
#     str(colData)
#     
#     countsData <- read.delim(raw_counts_path, 
#                              row.names = 1, check.names = FALSE)
#     
#     countsData <- countsData[, !duplicated(t(countsData))]
#     
#     dds <- DESeqDataSetFromMatrix(countData = countsData,
#                                   colData = colData,
#                                   design = design_formula) #import data
#     keep <- rowSums(counts(dds)) >= 10
#     dds <- dds[keep,]
#     dds <- DESeq(dds) #we normalize and linear modeling with only one function
#     res <- results(dds, contrast = contrast) #we perform the comparison between the experimental group of samples
#     res
#     coef_name <- paste0(contrast[1], "_", contrast[2], "_vs_", gsub("-", ".", contrast[3]))
#     
#     resLFC <- lfcShrink(dds, coef=coef_name, type="apeglm") #Shrinkage of effect size (LFC estimates) 
#     resLFC
#     #is useful for visualization and ranking of genes. It is more useful visualize the MA-plot for the shrunken log2 fold changes, 
#     #which remove the noise associated with log2 fold changes from low count genes without requiring arbitrary filtering thresholds.
#     res05 <- results(dds, alpha=0.05)
#     counts_matrix <- counts(dds)
#     # Assuming 'dds' is your DESeq2 object
#     
#     # Calculate pairwise distance between samples based on gene expression
#     sampleDists <- dist(t(assay(dds)))
#     
#     # Convert the distance object to a matrix
#     sampleDistMatrix <- as.matrix(sampleDists)
#     
#     normalized_counts <- counts(dds, normalized=TRUE)
#     
#     # Subset colData to include only the desired columns
#     selected_colData <- colData(dds)[selected_columns]
#     
#     # Ensure colData has the correct rownames corresponding to the sample names
#     rownames(colData) <- colData$sample_geo_accession
#     
#     # Check if colData and tissues_subtype are correctly defined
#     if (!"tissues_subtype" %in% colnames(colData)) {
#       stop("The colData dataframe does not contain a 'tissues_subtype' column.")
#     }
#     
#     # # Check the unique values in tissues_subtype column
#     # print("Unique values in tissues_subtype:")
#     # print(unique(colData$tissues_subtype))
#     
#     # Define control and case samples based on colData$tissues_subtype
#     control <- colData$sample_geo_accession[colData$tissues_subtype == contrast[3]]
#     case <- colData$sample_geo_accession[colData$tissues_subtype == contrast[2]]
#     
#     # # Debugging: Check if the sample names match
#     # print("Control samples:")
#     # print(control)
#     # print("Case samples:")
#     # print(case)
#     # 
#     # Normalize counts
#     normalized_counts <- counts(dds, normalized=TRUE)
#     
#     # Subset the data to keep only the top 10000 most variable genes
#     topVarGenes <- head(order(rowVars(normalized_counts), decreasing=TRUE), 10000)
#     data <- normalized_counts[topVarGenes, ]
#     # 
#     # # Debugging: Check column names in data
#     # print("Data column names:")
#     # print(colnames(data))
#     # 
#     # Define samples as control or case
#     samples <- ifelse(colnames(data) %in% control, contrast[3], ifelse(colnames(data) %in% case, contrast[2], NA))    
#     
#     # # Debugging: Check if all samples are correctly classified
#     # print("Samples classified:")
#     # print(samples)
#     
#     # Remove NA samples if there are any
#     valid_samples <- !is.na(samples)
#     data <- data[, valid_samples]
#     samples <- samples[valid_samples]
#     
#     annotation <- data.frame(SampleType = samples)
#     rownames(annotation) <- colnames(data)
#     
#     # Define annotation colors
#     annotation_colors <- list(SampleType = c(`non-tumor` = "green", `tumor` = "red"))
#     
#     # Define the color scheme for the heatmap
#     colorbar <- colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100)
#     
#     
#     # Save the objects
#     save(dds, res, counts_matrix, sampleDists, sampleDistMatrix, colNames, normalized_counts, control, samples, annotation, annotation_colors, colorbar, cpm, data, selected_colData, file = "stored_objects_GSE144269.RData")
#   }
# }
# 
# # Function to create PCA plot
# createPCAPlot <- function(dds) {
#   pca_data <- plotPCA(vst(dds), intgroup = "tissues_subtype", returnData = TRUE)
#   pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = tissues_subtype)) +
#     geom_point() +
#     labs(x = "PC1", y = "PC2", title = "PCA Plot") +
#     theme_minimal()
#   return(pca_plot)
# }
# 
# pca_plot <- createPCAPlot(dds)
# print(pca_plot)
# 
# generateHeatmap <- function(data, annotation, annotation_colors, colorbar) {
#   pheatmap(data, 
#            scale = "row", 
#            border_color = NA, 
#            clustering_method = "ward.D2", 
#            cluster_cols = TRUE, 
#            cluster_rows = TRUE, 
#            annotation_col = annotation, 
#            annotation_colors = annotation_colors, 
#            color = colorbar, 
#            show_rownames = FALSE, 
#            show_colnames = FALSE, 
#            main = "Heatmap of 10000 most Variable Genes")
# }
# 
# library(EnhancedVolcano)
# 
# generateVolcanoPlot <- function(res, title = 'Volcano Plot', subtitle = 'Differential Expression', caption = NULL) {
#   EnhancedVolcano(
#     res,
#     lab = rownames(res),
#     x = 'log2FoldChange',
#     y = 'pvalue',
#     xlim = c(-10, 10),
#     legendPosition = "right",
#     legendIconSize = 3,
#     title = title,
#     subtitle = subtitle,
#     caption = caption
#   )
# }
# # Assuming 'res' is your DESeq2 results object
# generateVolcanoPlot(res, title = 'My Volcano Plot', subtitle = 'DE Analysis', caption = 'DESeq2 Results')
# 
# heatmap_DE <- function(sampleDistMatrix){
#   pheatmap(sampleDistMatrix, 
#            clustering_distance_rows = sampleDists,
#            clustering_distance_cols = sampleDists,
#            col = viridis(100),
#            show_rownames = FALSE,  # Do not show row names
#            show_colnames = FALSE)  # Do not show column names
# }
# 
# generateGeneBoxplot <- function(cpm, gene_symbols) {
#   # Extract expression values for the specified genes
#   gene_expression <- counts(cpm)[gene_symbols, , drop = FALSE]
#   
#   # Create a data frame for plotting
#   plot_data <- data.frame(
#     Gene = rep(gene_symbols, each = ncol(gene_expression)),
#     Condition = rep(colData(cpm)$tissues_subtype, times = length(gene_symbols)),
#     Expression = as.vector(t(gene_expression))
#   )
#   
#   # Adjust the number of columns in facet_wrap based on the number of selected genes
#   num_cols <- length(gene_symbols)
#   
#   # Plotting boxplot
#   gene_boxplot <- ggplot(plot_data, aes(x = Condition, y = Expression, fill = Condition)) +
#     geom_boxplot() +
#     facet_wrap(~ Gene, ncol = num_cols, scales = "free_y") + # Separate plots for each gene
#     labs(x = "Condition", y = "Expression", title = "Expression of Selected Genes by Condition") +
#     theme_minimal() +
#     scale_fill_manual(values = c("blue", "red")) +
#     theme(
#       plot.title = element_text(size = 20, face = "bold"),    # Title font size
#       axis.title.x = element_text(size = 14),                 # X-axis title font size
#       axis.title.y = element_text(size = 14),                 # Y-axis title font size
#       axis.text.x = element_text(size = 12),                  # X-axis text font size
#       axis.text.y = element_text(size = 12),                  # Y-axis text font size
#       strip.text = element_text(size = 12),                   # Facet labels font size
#       legend.title = element_text(size = 14),                 # Legend title font size
#       legend.text = element_text(size = 12)                   # Legend text font size
#     )
#   
#   return(gene_boxplot)
# }
# 
# # # Load required libraries if not already loaded
# # library(pheatmap)
# # library(RColorBrewer)
# # 
# # # Create the heatmap with hierarchical clustering
# # pheatmap(sampleDistMatrix,
# #          clustering_distance_rows = sampleDists,
# #          clustering_distance_cols = sampleDists,
# #          col = colorRampPalette(rev(brewer.pal(9, 'PuRd')))(170),
# #          show_rownames = FALSE,  # Do not show row names
# #          show_colnames = FALSE)  # Do not show column names