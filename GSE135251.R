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
# library(heatmaply)
library(viridis)
library(edgeR)

# 
# geo_ID <- "GSE135251"
# 
# geo_dataset <- getGEO(geo_ID)
# 
# colData <- pData(phenoData(geo_dataset[[1]])) #experiment design
# 
# # Print the column names of the clinical information
# print(names(colData))
# 
# columns_to_keep <- c("geo_accession","disease:ch1","fibrosis stage:ch1","group in paper:ch1","nas score:ch1","Stage:ch1", "description")
# 
# # Subset the clinical information data frame to keep only the specified columns
# colData <- colData[, columns_to_keep]
# 
# # Rename column for convenience
# colnames(colData) <- c("Sample_geo_accession","disease","fibrosis_stage","group_in_paper","nas_score","Stage","Sample_description")
# 
# countsData <- read.delim("/home/giulia/GEO/GSE135251_raw_counts_GRCh38.p13_NCBI(pulito).tsv", 
#                          row.names = 1, check.names = FALSE)
# 
# countsData <- countsData[, !duplicated(t(countsData))]
# 
# dds <- DESeqDataSetFromMatrix(countData = countsData,
#                               colData = colData,
#                               design = ~ disease) #import data
# keep <- rowSums(counts(dds)) >= 10
# dds <- dds[keep,]
# dds <- DESeq(dds) #we normalize and linear modeling with only one function
# res <- results(dds, contrast = c("disease", "NAFLD", "Control")) #we perform the comparison between the experimental group of samples
# #res
# resLFC <- lfcShrink(dds, coef="disease_NAFLD_vs_Control", type="apeglm") #Shrinkage of effect size (LFC estimates) 
# #is useful for visualization and ranking of genes. It is more useful visualize the MA-plot for the shrunken log2 fold changes, 
# #which remove the noise associated with log2 fold changes from low count genes without requiring arbitrary filtering thresholds.
# res05 <- results(dds, alpha=0.05)
# summary(res05)
# sum(res05$padj < 0.05, na.rm=TRUE)
# plotMA(res, ylim=c(-6,6))
# plotMA(resLFC, ylim=c(-6,6))
#

# Function to create PCA plot

if (file.exists("stored_objects_GSE135251.RData")) {
  load("stored_objects_GSE135251.RData")
  } else {
    # Define GEO ID and download the dataset
    geo_ID <- "GSE135251"
    geo_dataset <- getGEO(geo_ID)
    
    colData <- pData(phenoData(geo_dataset[[1]]))
    
    # Print the column names of the clinical information
    print(names(colData))
    
    columns_to_keep <- c("geo_accession","disease:ch1","fibrosis stage:ch1","group in paper:ch1","nas score:ch1","Stage:ch1", "description")
    colData <- colData[, columns_to_keep]
    
    colnames(colData) <- c("Sample_geo_accession","disease","fibrosis_stage","group_in_paper","nas_score","Stage","Sample_description")
    
    colData$Sample_description <- gsub("_R[1,2]\\.fastq\\.gz","",colData$Sample_description)
    
    colData$sex <- NA
    all_samples_counts <- read.table("/home/giulia/GEO/GSE135251/GSM3998229_152-Ann-Daly_S3.counts.txt.gz", sep="\t", header=F)
    names(all_samples_counts) <- c("GENE_ID","counts")
    all_samples_counts$counts <- NULL
    
    base_path_govaere <- "/home/giulia/GEO/GSE135251"
    all_samples_counts <- data.frame(GENE_ID = character(0))
    
    for (sample in colData$Sample_geo_accession) {
      current_description <- subset(colData, Sample_geo_accession == sample)$Sample_description
      current_sample_path <- paste0(base_path_govaere,"/",sample,"_", current_description,".counts.txt.gz")
      current_sample_counts <- read.table(current_sample_path, sep="\t", header=FALSE, stringsAsFactors = FALSE)
      names(current_sample_counts) <- c("GENE_ID", sample)
      
      current_SRY <- subset(current_sample_counts, GENE_ID == "ENSG00000183878")
      if (current_SRY[2] == 0) {
        colData[which(colData$Sample_geo_accession == sample), "sex"] <- "female"
      } else {
        colData[which(colData$Sample_geo_accession == sample), "sex"] <- "male"
      }
      
      all_samples_counts <- merge(all_samples_counts, current_sample_counts, by = "GENE_ID", all = TRUE)
    }
    
    all_samples_counts <- all_samples_counts[(grep("^ENSG",all_samples_counts$GENE_ID)),]
    colData$sex <- as.factor(colData$sex)
    
    metaData_govaere <- colData
    
    metaData_govaere$nash_two <- NA
    metaData_govaere$nash_two <- lapply(metaData_govaere$group_in_paper,function(x) {if(x == "NASH_F0-F1" | x == "NASH_F2" ){ "NASH_F0.F1.F2" } else if (x == "NASH_F3" | x == "NASH_F4") {"NASH_F3.F4"} else if (x == 'control' ) {"control"}})
    metaData_govaere$nash_two <- as.factor(as.character(metaData_govaere$nash_two))
    metaData_govaere[metaData_govaere$nash_two == "NULL",]$nash_two <- NA
    metaData_govaere$nash_two <- droplevels(metaData_govaere$nash_two)
    
    metaData_govaere$steato_status <- NA
    metaData_govaere$steato_status <- lapply(metaData_govaere$group_in_paper,function(x) {if(x == "NAFL") { "NAFL" } else if (x == "NASH_F0-F1" | x == "NASH_F2" | x == "NASH_F3" | x == "NASH_F4") {"NASH"} else if (x == 'control' ) {"control"}})
    metaData_govaere$steato_status <- as.factor(as.character(metaData_govaere$steato_status))
    # metaData_govaere[metaData_govaere$steato_status == "NULL",]$steato_status <- NA
    metaData_govaere$steato_status <- droplevels(metaData_govaere$steato_status)

    table(metaData_govaere$steato_status)
    
    rownames(metaData_govaere) <- NULL
    
    selected_colData <- metaData_govaere[c("disease", "fibrosis_stage", "group_in_paper", "nas_score", "Stage", "Sample_description", "sex")]
    
    countsData <- read.delim("/home/giulia/GEO/GSE135251_raw_counts_GRCh38.p13_NCBI(pulito).tsv", 
                             row.names = 1, check.names = FALSE)
    countsData <- countsData[, !duplicated(t(countsData))]
    
    duplicate_rows <- rownames(countsData)[duplicated(rownames(countsData))]
    print(duplicate_rows)
    
    countsData <- countsData[!duplicated(rownames(countsData)), ]
    print(any(duplicated(rownames(countsData))))
    
    rownames(metaData_govaere) <- colnames(countsData)
    
    formula <- "~ sex + fibrosis_stage"
    current_dds <- DESeqDataSetFromMatrix(countData = countsData,
                                          colData = metaData_govaere,
                                          design = as.formula(formula))
    # Run the DESeq pipeline
    dds <- DESeq(current_dds)

    formula2 <- "~ sex + steato_status"
    current_dds2 <- DESeqDataSetFromMatrix(countData = countsData,
                                          colData = metaData_govaere,
                                          design = as.formula(formula2))
    # Run the DESeq pipeline
    dds2 <- DESeq(current_dds2)
    
    # Get the results for the specified contrast without shrinkage
    res <- results(dds2, contrast = c("steato_status", "NASH", "NAFL"))
    
    # Perform LFC shrinkage using the "normal" method
    resLFC <- lfcShrink(dds2, contrast = c("steato_status", "NASH", "NAFL"), type = "normal")
    
    # Filter results for adjusted p-value < 0.05
    res05 <- results(dds2, alpha=0.05)
    
    counts_matrix <- counts(dds2)
    
    # Calculate pairwise distance between samples based on gene expression
    sampleDists <- dist(t(assay(dds2)))
    
    # Convert the distance object to a matrix
    sampleDistMatrix <- as.matrix(sampleDists)
    
    normalized_counts <- counts(dds2, normalized=TRUE)
    
    # Define control and case samples based on group_in_paper
    control <- metaData_govaere$Sample_geo_accession[metaData_govaere$steato_status == "NASH"]
    case <- metaData_govaere$Sample_geo_accession[metaData_govaere$steato_status == "NAFL"]
    
    # # Debugging: Check if the sample names match
    # print("Control samples:")
    # print(control)
    # print("Case samples:")
    # print(case)
    
    # Subset data to keep only top 10000 most variable genes
    topVarGenes <- head(order(rowVars(normalized_counts), decreasing=TRUE), 10000)
    data <- normalized_counts[topVarGenes, ]
    
    # Define samples as control or case
    samples <- ifelse(colnames(data) %in% control, "NASH", ifelse(colnames(data) %in% case, "NAFL", NA))
    
    # Remove NA samples if there are any
    valid_samples <- !is.na(samples)
    data <- data[, valid_samples]
    samples <- samples[valid_samples]
    
    # Create annotation for heatmap
    annotation <- data.frame(SampleType = samples)
    rownames(annotation) <- colnames(data)
    
    # Define annotation colors
    annotation_colors <- list(SampleType = c("NASH" = "green", "NAFL" = "red"))
    
    # Define color scheme for heatmap
    colorbar <- colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100)
    
    cpm <- cpm(dds2)
    
    colNames <- colnames(colData)
    
    # Save objects
    save(dds, res, resLFC, res05, countsData, sampleDists, sampleDistMatrix, normalized_counts, control, case, samples, annotation, annotation_colors, cpm, colNames, colorbar, data, selected_colData, file = "stored_objects_GSE135251.RData")
  
  }

createPCAPlot <- function(dds) {
  # Perform PCA and get the data
  pca_data <- plotPCA(vst(dds), intgroup = c("fibrosis_stage", "sex"), returnData = TRUE)
  percentVar <- round(100 * attr(pca_data, "percentVar"))
  
  # Create the PCA plot
  pca_plot <- ggplot(pca_data, aes(PC1, PC2, color = fibrosis_stage, shape = sex)) +
    geom_point(size = 3) +
    xlab(paste0("PC1: ", percentVar[1], "% variance")) +
    ylab(paste0("PC2: ", percentVar[2], "% variance")) +
    coord_fixed() +
    theme_bw() +
    labs(title = "PCA Plot")
  
  return(list(plot = pca_plot, data = pca_data))  # Return both PCA plot and pca_data
}


pca_plot <- createPCAPlot(dds)
#print(pca_plot)

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

heatmap_DE <- function(sampleDistMatrix){
  pheatmap(sampleDistMatrix, 
           clustering_distance_rows = sampleDists,
           clustering_distance_cols = sampleDists,
           col = viridis(100),
           show_rownames = FALSE,  # Do not show row names
           show_colnames = FALSE)  # Do not show column names
}

generateVolcanoPlot <- function(res, title = 'Volcano Plot', subtitle = 'Differential Expression', caption = NULL) {
  EnhancedVolcano(
    res,
    lab = rownames(res),
    x = 'log2FoldChange',
    y = 'pvalue',
    xlim = c(-5, 5),
    legendPosition = "right",
    legendIconSize = 3,
    title = title,
    subtitle = subtitle,
    caption = caption
  )
}
# Assuming 'res' is your DESeq2 results object
#generateVolcanoPlot(res, title = 'Volcano Plot', subtitle = 'DE Analysis', caption = 'DESeq2 Results')

generateGeneBoxplot <- function(cpm, gene_symbols) {
  # Extract expression values for the specified genes
  gene_expression <- counts(cpm)[gene_symbols, , drop = FALSE]
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Gene = rep(gene_symbols, each = ncol(gene_expression)),
    Condition = rep(colData(cpm)$steato_status, times = length(gene_symbols)),
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
    scale_fill_manual(values = c("blue", "red", "green")) +
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