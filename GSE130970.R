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


# Load the dataset
geo_ID <- "GSE130970"
geo_dataset <- getGEO(geo_ID)
colData <- pData(phenoData(geo_dataset[[1]]))

# Print the column names of the clinical information
print(names(colData))

# Specify the columns to keep
columns_to_keep <- c("geo_accession", "Sex:ch1", "fibrosis stage:ch1", "nafld activity score:ch1")

# Subset the clinical information data frame to keep only the specified columns
colData <- colData[, columns_to_keep]

# Rename columns for convenience
colnames(colData) <- c("Sample_geo_accession", "sex", "fibrosis_stage", "nafld_activity_score")

# Filter for female samples only
female_colData <- colData[colData$sex == "F", ]

# Subset countsData to include only the columns for female samples
female_countsData <- countsData[, female_colData$Sample_geo_accession]

# Create DESeq2 dataset with female data only
dds <- DESeqDataSetFromMatrix(countData = female_countsData,
                              colData = female_colData,
                              design = ~ nafld_activity_score) 

# Filter low count genes
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]

# Normalize and perform linear modeling
dds <- DESeq(dds)

# Get results (you might need to specify appropriate contrasts based on your new design)
res <- results(dds)

# Shrink log fold changes
# resLFC <- lfcShrink(dds, coef="condition", type="apeglm")

# Calculate counts per million (CPM) for boxplots
cpm <- cpm(dds)

# Calculate pairwise distance between samples based on gene expression
sampleDists <- dist(t(assay(dds)))

# Convert the distance object to a matrix
sampleDistMatrix <- as.matrix(sampleDists)

# Extract count data matrix
counts_matrix <- counts(dds)

# Extract normalized counts
normalized_counts <- counts(dds, normalized=TRUE)

# Print results
print(res)
# print(resLFC)

createPCAPlot <- function(dds) {
  # Perform variance stabilizing transformation
  vsd_female <- vst(dds, blind = FALSE)
  
  # Plot PCA for females with nafld_activity_score grouping
  pcaData_female <- plotPCA(vsd_female, intgroup = "nafld_activity_score", returnData = TRUE)
  percentVar_female <- round(100 * attr(pcaData_female, "percentVar"))
  
  ggplot(pcaData_female, aes(PC1, PC2, color = nafld_activity_score)) +
    geom_point(size = 3) +
    xlab(paste0("PC1: ", percentVar_female[1], "% variance")) +
    ylab(paste0("PC2: ", percentVar_female[2], "% variance")) +
    coord_fixed() +
    theme_bw() +
    ggtitle("PCA Plot Female Samples by NAFLD Activity Score")
}

# Example usage
# Assuming `dds` is already created and filtered for female samples
createPCAPlot(dds)


# Filter results based on log2 fold change and p-value and save to CSV
extractAndSaveDEGs <- function(res, log2FC_threshold = 1, pvalue_threshold = 0.05, output_file = "significant_genes.csv") {
  # Convert results to a data frame
  res_df <- as.data.frame(res)
  
  # Filter for significant genes
  significant_genes <- res_df[which(abs(res_df$log2FoldChange) >= log2FC_threshold & res_df$pvalue <= pvalue_threshold), ]
  
  # Save the significant genes to a CSV file
  write.csv(significant_genes, file = output_file, row.names = TRUE)
  
  return(significant_genes)
}

# You can adjust log2FC_threshold, pvalue_threshold, and output_file as needed
significant_genes <- extractAndSaveDEGs(res, log2FC_threshold = 1, pvalue_threshold = 0.05, output_file = "significant_genes_GSE130970_female.csv")

# Print the significant genes
print(significant_genes)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

 # DOING FOR MALE

# Filter for male samples only
male_colData <- colData[colData$sex == "M", ]

# Subset countsData to include only the columns for male samples
male_countsData <- countsData[, male_colData$Sample_geo_accession]

# Create DESeq2 dataset with male data only
dds <- DESeqDataSetFromMatrix(countData = male_countsData,
                              colData = male_colData,
                              design = ~ nafld_activity_score) # or any other design formula

# Filter low count genes
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]

# Normalize and perform linear modeling
dds <- DESeq(dds)

# Get results (you might need to specify appropriate contrasts based on your new design)
res <- results(dds)

extractAndSaveDEGs <- function(res, log2FC_threshold = 1, pvalue_threshold = 0.05, output_file = "significant_genes_male.csv") {
  # Convert results to a data frame
  res_df <- as.data.frame(res)
  
  # Filter for significant genes
  significant_genes <- res_df[which(abs(res_df$log2FoldChange) >= log2FC_threshold & res_df$pvalue <= pvalue_threshold), ]
  
  # Save the significant genes to a CSV file
  write.csv(significant_genes, file = output_file, row.names = TRUE)
  
  return(significant_genes)
}

# Extract and save significant genes for male samples
significant_genes_male <- extractAndSaveDEGs(res, log2FC_threshold = 1, pvalue_threshold = 0.05, output_file = "significant_genes_male.csv")

# Print the significant genes
print(significant_genes_male)
