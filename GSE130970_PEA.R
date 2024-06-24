# # Load necessary libraries
# library(dplyr)
# library(readr)
# 
# # Read the enrichment analysis results from the TXT files
# fileM <- "KEGG_202M_Human_table.txt"
# file2 <- "KEGG_202M_Human_table(M).txt"
# 
# enrichmentM <- read_tsv(fileM)
# enrichment2 <- read_tsv(file2)
# 
# # Find common pathways
# common_pathways <- inner_join(enrichment1, enrichment2, by = "Term")
# 
# # View the common pathways
# print(common_pathways)
# 
# # Save the common pathways to a new file
# write_tsv(common_pathways, "common_pathways.txt")


# Load necessary libraries
library(enrichR)
library(dplyr)
library(ggplot2)

# Read gene lists from CSV files
gene_listM <- read.csv("significant_genes_male.csv", header = TRUE, stringsAsFactors = FALSE)[,1]
gene_listF <- read.csv("significant_genes_GSE130970_female.csv", header = TRUE, stringsAsFactors = FALSE)[,1]


# Perform enrichment analysis using Enrichr
dbs <- listEnrichrDbs()$libraryName
kegg_resultsM <- enrichr(gene_listM, "KEGG_2021_Human")[[1]]
kegg_resultsF <- enrichr(gene_listF, "KEGG_2021_Human")[[1]]

# Convert Enrichr results to data frames
kegg_dfM <- as.data.frame(kegg_resultsM)
kegg_dfF <- as.data.frame(kegg_resultsF)

# Find common pathways
common_pathways <- inner_join(kegg_dfM, kegg_dfF, by = "Term")

# View the common pathways
print(common_pathways)

# Save the common pathways to a new file
write.csv(common_pathways, "common_pathways_enrichr.csv")

common_pathways$avg_combined_score <- rowMeans(common_pathways[, c("Combined.Score.x", "Combined.Score.y")])

# Select the top 20 common pathways based on the average combined score
top_common_pathways <- common_pathways %>%
  arrange(desc(avg_combined_score)) %>%
  head(20)

# Create a bar plot for the top 20 common pathways
plot <- ggplot(top_common_pathways, aes(x = reorder(Term, -avg_combined_score), y = avg_combined_score)) +
  geom_bar(stat = "identity", fill = "#d9819c") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Common Pathways (Enrichr)", x = "Pathways", y = "Average Combined Score") +
  theme(axis.text.y = element_text(size = 13))

# Display the plot
print(plot)

# Save the plot to a file
ggsave("common_pathways_plot_enrichr.png", plot)

# # Save the plot to a file
# ggsave("/mnt/data/common_pathways_plot_enrichr.png", plot)


# Find unique pathways for each list
unique_to_listM <- anti_join(kegg_dfM, kegg_dfF, by = "Term")
unique_to_listF <- anti_join(kegg_dfF, kegg_dfM, by = "Term")

# Calculate the combined score for unique pathways
unique_to_listM$combined_score <- unique_to_listM$Combined.Score
unique_to_listF$combined_score <- unique_to_listF$Combined.Score

# Select the top 20 unique pathways for each list
top_unique_to_listM <- unique_to_listM %>%
  arrange(desc(combined_score)) %>%
  head(20)

top_unique_to_listF <- unique_to_listF %>%
  arrange(desc(combined_score)) %>%
  head(20)

# Create a bar plot for the top 20 unique pathways for listM
plot_listM <- ggplot(top_unique_to_listM, aes(x = reorder(Term, -combined_score), y = combined_score)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Unique Pathways Males", x = "Pathways", y = "Combined Score") +
  theme(axis.text.y = element_text(size = 15))

# Display the plot for listM
print(plot_listM)

# Save the plot for listM to a file
ggsave("unique_pathways_plot_listM.png", plot_listM)

# Create a bar plot for the top 20 unique pathways for list2
plot_listF <- ggplot(top_unique_to_listF, aes(x = reorder(Term, -combined_score), y = combined_score)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Unique Pathways Females", x = "Pathways", y = "Combined Score") +
  theme(axis.text.y = element_text(size = 15))

# Display the plot for list2
print(plot_listF)

# Save the plot for list2 to a file
ggsave("unique_pathways_plot_listF.png", plot_listF)
