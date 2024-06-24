# Function to convert a list of genes into a regular expression pattern
genes_to_regex <- function(gene_list) {
  # Split the gene list by commas or spaces
  genes <- unlist(strsplit(gene_list, "[, ]+"))
  # Escape special characters in gene names, except the dot
  genes <- gsub("([\\^$*+?()[{\\\\|])", "\\\\\\1", genes)
  # Create the regular expression pattern
  regex_pattern <- paste0("^", paste(genes, collapse = "$|^"), "$")
  return(regex_pattern)
}

# # Example gene list
# gene_list <- "ILDR2 CTBP2P8 HKDC1 RP11-227H15.5"
# 
# # Convert gene list to regular expression pattern
# regex_pattern <- genes_to_regex(gene_list)
# 
# # Print the regular expression pattern
# print(regex_pattern)