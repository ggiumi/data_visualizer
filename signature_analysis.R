library(pheatmap)
library(RColorBrewer)
library(viridis)

generateHeatmap_genes <- function(counts_matrix, genes_of_interest) {
  # Subset the counts matrix to include only the specified genes
  counts_matrix_subset <- counts_matrix[genes_of_interest, , drop = FALSE]
  
  # Generate heatmap using pheatmap
  pheatmap(counts_matrix_subset, 
           scale = "row",
           clustering_method = "ward.D2", 
           cluster_cols = TRUE, 
           cluster_rows = TRUE,
           col = colorRampPalette(colors = c("blue", "blue1", "blue2", "black", "yellow2", "yellow1", "yellow"))(100),
           show_rownames = TRUE,  # Show row names
           show_colnames = FALSE)  # Do not show column names
}