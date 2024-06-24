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

if (file.exists("stored_objects_GSE135251_female.RData")) {
  load("stored_objects_GSE135251_female.RData")
} else {

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
  
  rownames(metaData_govaere) <- NULL
  
  countsData <- read.delim("/home/giulia/GEO/GSE135251_raw_counts_GRCh38.p13_NCBI(pulito).tsv", 
                           row.names = 1, check.names = FALSE)
  countsData <- countsData[, !duplicated(t(countsData))]
  
  duplicate_rows <- rownames(countsData)[duplicated(rownames(countsData))]
  print(duplicate_rows)
  
  countsData <- countsData[!duplicated(rownames(countsData)), ]
  print(any(duplicated(rownames(countsData))))
  
  rownames(metaData_govaere) <- colnames(countsData)
  
  metaData_male <- subset(metaData_govaere, sex == "male")
  metaData_female <- subset(metaData_govaere, sex == "female")
  
  # Filter out rows with NA in nash_two for males and females and remove controls (?)
  metaData_male <- subset(metaData_male, !is.na(nash_two) & nash_two != "control")
  
  # selected_colData <- metaData_female[c("disease", "fibrosis_stage", "group_in_paper", "nas_score", "Stage", "Sample_description", "sex")]
  
  countsData_male <- countsData[, rownames(metaData_male)]
  
  # Create DESeqDataSet object for males
  dds_male <- DESeqDataSetFromMatrix(countData = countsData_male,
                                     colData = metaData_male,
                                     design = ~ nash_two)
  
  # Run the DESeq2 pipeline for males
  dds <- DESeq(dds_male)
  
  # Extract results for F0F1 versus F3F4 comparison for males
  res <- results(dds, contrast = c("nash_two", "NASH_F3.F4", "NASH_F0.F1.F2"))
  res <- res[order(res$padj),]
  
  vsd_male <- vst(dds, blind = FALSE)
  
  # Generate heatmap of the most variable genes
  topVarGenes <- head(order(rowVars(assay(vsd_male)), decreasing = TRUE), 10000)
  data <- assay(vsd_male)[topVarGenes, ]
  annotation <- as.data.frame(colData(vsd_male)[, "nash_two", drop=FALSE])
  annotation_colors <- list(nash_two = c("NASH_F0.F1.F2" = "green", "NASH_F3.F4" = "red"))
  colorbar <- colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100)
  # generateHeatmap(mat, annotation, annotation_colors, colorbar)
  
  # Generate sample distance heatmap
  sampleDists <- dist(t(assay(vsd_male)))
  sampleDistMatrix <- as.matrix(sampleDists)
  # heatmap_DE(sampleDistMatrix, sampleDists)
  
  counts_matrix <- counts(dds)
  
  normalized_counts <- counts(dds, normalized=TRUE)
  
  colNames <- colnames(metaData_male)
  
  cpm <- cpm(dds)
  
  sampleDists <- dist(t(assay(dds)))
  
  selected_colData <- metaData_female[c("disease", "fibrosis_stage", "group_in_paper", "nas_score", "Stage", "Sample_description", "sex")]
  
  save(dds, res, countsData_male, vsd_male, sampleDistMatrix, sampleDists, normalized_counts, colNames, cpm, counts_matrix, data, annotation, annotation_colors, colorbar, selected_colData, file = "stored_objects_GSE135251_female.RData")
}
  
createPCAPlot <- function(dds) {# Perform variance stabilizing transformation
  vsd_female <- vst(dds, blind = FALSE)
  
  # Plot PCA for females with nash_two grouping (F0F1 vs F3F4)
  pcaData_female <- plotPCA(vsd_female, intgroup = "nash_two", returnData = TRUE)
  percentVar_female <- round(100 * attr(pcaData_female, "percentVar"))
  
  ggplot(pcaData_female, aes(PC1, PC2, color = nash_two)) +
    geom_point(size = 3) +
    xlab(paste0("PC1: ", percentVar_female[1], "% variance")) +
    ylab(paste0("PC2: ", percentVar_female[2], "% variance")) +
    coord_fixed() +
    theme_bw() +
    ggtitle("PCA Plot Female Samples")
}
  
  # Function to generate heatmap
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
  
  # Function to generate volcano plot
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
  
  # Function to generate sample distance heatmap
  heatmap_DE <- function(sampleDistMatrix){
    pheatmap(sampleDistMatrix, 
             clustering_distance_rows = sampleDists,
             clustering_distance_cols = sampleDists,
             col = viridis(100),
             show_rownames = FALSE,  
             show_colnames = FALSE)  
  }
  
  # Function to generate gene expression boxplots
  generateGeneBoxplot <- function(cpm, gene_symbols) {
    gene_expression <- counts(cpm)[gene_symbols, , drop = FALSE]
    plot_data <- data.frame(
      Gene = rep(gene_symbols, each = ncol(gene_expression)),
      Condition = rep(colData(cpm)$nash_two, times = length(gene_symbols)),
      Expression = as.vector(t(gene_expression))
    )
    num_cols <- length(gene_symbols)
    gene_boxplot <- ggplot(plot_data, aes(x = Condition, y = Expression, fill = Condition)) +
      geom_boxplot() +
      facet_wrap(~ Gene, ncol = num_cols, scales = "free_y") +
      labs(x = "Condition", y = "Expression", title = "Expression of Selected Genes by Condition") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red")) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),    
        axis.title.x = element_text(size = 14),                 
        axis.title.y = element_text(size = 14),                 
        axis.text.x = element_text(size = 12),                  
        axis.text.y = element_text(size = 12),                  
        strip.text = element_text(size = 12),                   
        legend.title = element_text(size = 14),                 
        legend.text = element_text(size = 12)                   
      )
    return(gene_boxplot)
  }