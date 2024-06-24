library(org.Hs.eg.db)
library(dplyr)

#convert gene entrez IDs to symbols
convert_to_symbols <- function(file_path) {
  data <- read.delim(file_path, stringsAsFactors = FALSE)
  #check if 'Symbols' column already exists
  if ("Symbols" %in% colnames(data)) {
    stop("Column 'Symbols' already exists.")
  }
  #conversion of entrez IDs to symbols
  symbols <- mapIds(org.Hs.eg.db, keys = as.character(data[,1]), column = "SYMBOL", keytype = "ENTREZID")
  #insert the new formed 'Symbols' column as the second column
  data <- cbind(GeneID = data[,1], Symbols = symbols, data[, -1])
  #remove NAs
  data <- data[complete.cases(data),]
  #remove eventual duplicated symbols
  data <- data[!duplicated(data$Symbols),]
  return(data)
}

#file path
file_path <- "/home/giulia/GEO/GSE130970_raw_counts_GRCh38.p13_NCBI.tsv"
#file_path <- readline("Enter the path of your TSV matrix: ")
#conversion
updated_data <- convert_to_symbols(file_path)
#overwrite the TSV file
write.table(updated_data, file = file_path, sep = "\t", quote = FALSE, row.names = FALSE)

