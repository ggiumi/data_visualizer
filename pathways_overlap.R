# # Load necessary libraries
# library(dplyr)
# library(readr)
# 
# # Read the enrichment analysis results from the TXT files
# file1 <- "KEGG_2021_Human_table.txt"
# file2 <- "KEGG_2021_Human_table(1).txt"
# 
# enrichment1 <- read_tsv(file1)
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

# Define the gene lists
gene_listM <- c("DCDC2", "CD24", "CDH6", "EPCAM", "BICC1", "COL15A1", "MMP7", "CXCL6", "ANXA3", "PAQR5",
                "PLCXD3", "EHF", "LAMC2", "JAG1", "ITGBL1", "HKDC1", "TACSTD2", "ESRP1", "C1orf116", "VTCN1",
                "PODXL", "CHST4", "LOC105372061", "AP1M2", "CLIC6", "ELOVL7", "CLDN10", "PDGFD", "TRPV6", "AQP1",
                "GRHL2", "EFEMP1", "BMX", "KCNJ15", "SMOC2", "LINC02331", "POF1B", "SPINT1", "RCAN2", "KCNJ16",
                "MUC13", "CFTR", "FAP", "NAV3", "CXCL8", "ITGB8", "KRT80", "C12orf75", "ID4", "CH25H", "B3GNT3",
                "SEMA3G", "SOX4", "SLC35F2", "MYEF2", "RAB25", "CD34", "ALKAL2", "VWF", "FAM3B", "RASL11B", "MGP",
                "GRAMD1B", "FLRT2", "RGS4", "GABRP", "PMEPA1", "AIF1L", "ZPLD1", "ENPP5", "DEPDC1B", "VEPH1", "INAVA",
                "SYT13", "RERGL", "BDKRB2", "MOXD1", "SLC5A1", "F3", "MACC1", "CLDN4", "LUM", "UBD", "SERTAD4",
                "B3GALT5", "TGFB2", "KCNN3", "CFAP221", "SCD5", "APCDD1", "EPHA3", "KRT7", "PLN", "GGT6", "USH1C",
                "BEX2", "STC1", "STMN2", "LOC101929532", "SLC34A2", "MAP1B", "DCLK1", "TPPP3", "STK33", "FRAS1",
                "LOXL4", "TESC", "SOX9", "SLC22A15", "KAAG1", "KRT7-AS", "THBS2", "PRR15L", "SPINT1-AS1", "TMEM125",
                "CAPN6", "LRRC7", "NALCN", "FOXQ1", "MMP28", "PLPP2", "MDFI", "CCDC141", "ENTREP1", "PARM1", "PRSS16",
                "LIF", "DPT", "ITGA3", "FSIP1", "CLCF1", "CACNA1C", "GSN", "C7", "MCOLN3", "VGLL1", "TSPAN2",
                "PLCB4", "SLCO2A1", "LIPH", "TGFB2-OT1", "EDA2R", "PLEKHB1", "CCN3", "TMPRSS3", "LRRC1", "BHLHE22",
                "ITGB6", "CLIC5", "IL20RA", "PRR16", "MST1R", "MEDAG", "ATP13A4", "NCAM2", "ADRA2A", "LOC105378948",
                "SLC28A3", "GPRIN2", "ANXA8", "SYT8", "FOXC1", "SLC52A3", "GJA5", "KLHL29", "PLVAP", "KRT19",
                "ADGRG2", "SEZ6L2", "ANXA8L1", "LOC100288175", "PTK7", "PI16", "PRSS22", "GABRE", "TMEM200A",
                "VSIG2", "DTNA", "MYH11", "ARL14", "CEP126", "EDIL3", "FLNC", "ST8SIA3", "C3orf52", "SCN7A", "CXCL1",
                "CD1E", "THY1", "DPPA4", "SCGN", "SH3TC2", "TMEM178B", "SNAP25", "GULP1", "KLK10", "LOC613266",
                "ANKRD1", "NTS", "SEMA3E", "LOC105378773", "SFRP5", "CDH11", "B3GALT5-AS1", "KRT23", "CASR", "SPHK1",
                "LRRN1", "KCNH8", "RNF212", "A4GNT", "DPY19L2", "OSR1", "PTGDS", "RRAD", "NTRK2", "LOC107985072",
                "CCL21", "PAPPA2", "MGAT3", "FAM83E", "IQGAP3", "ARHGEF38", "PTPRQ", "CD200", "STK32B", "SPINK1",
                "COL28A1", "C1QL1", "TPM2", "FBLN1", "RNA18SN5", "AJAP1", "LOXL1-AS1", "LOC105377685", "POM121L9P",
                "MISP", "APOD", "COL10A1", "OVOL1", "LOXL1", "RNA18SN1", "LOC105375670", "LINC00312", "RAB3B", "LAMC3",
                "LCN2", "STC2", "VWA2", "SCAT8", "LOC105374879", "DKK2", "SLC24A5", "TMEM130", "LOC105370780",
                "CASQ2", "UCA1", "DPP10", "GIRGL", "LINC03039", "GLRB", "PRRX1", "LOC107986475", "ICOS", "PDZK1IP1",
                "AMPD1", "ACKR1", "CA9", "KCNK15", "MFAP2", "LYPD6B", "KLK11", "KRT87P", "LOC102723517", "DNAH7",
                "ATP1A2", "SPON1", "LOC105376042", "POU4F1", "FAT3", "PROM2", "LOC107984960", "MPP2", "LEFTY1",
                "FXYD2", "LCAL1", "EPS8L1", "GDNF", "TENM4", "CSPG4", "GALNT17", "LOC107987039", "MGAT4C",
                "LINC02298", "LOC107984911", "RNA45SN1", "LOC105376526", "SOD3", "CPXM1", "LOC105374179", "CCL20",
                "MRAP2", "RNA28SN2", "PTPRR", "DUSP26", "TEX26", "IQCA1", "CTHRC1", "RNA45SN2", "IGFN1", "RNA45SN3",
                "RNA18SN3", "SFRP4", "GPR83", "OMD", "JPH2", "PTHLH", "APLP1", "CEP55", "RMST", "TMEM132C",
                "LOC105375120", "RNA28SN3", "FAM43B", "RALYL", "C10orf90", "SLC4A3", "TMEM163", "LOC105376105",
                "ROR2", "LOC107984253", "CALN1", "SRRM3", "PCYT1B", "TRPC4", "MYOCD", "MGAM2", "OPCML", "CHMP1B2P",
                "C19orf33", "LINC01205", "CHRDL1", "DIRAS2", "LINC01152", "LOC105377979", "LRRC7-AS1", "HCN1", "HIF3A",
                "PDLIM4", "WFDC2", "DUOX2", "DIPK1C", "SHISA3", "TRPC3", "CCDC187", "CAPN13", "SLITRK4", "GUCA2A",
                "PRDM6-AS1", "LOC101927468", "ACTG2", "OLR1", "NMNAT2", "WNT7B", "LOC107984245", "CASC15", "PRDM16",
                "TDRD1", "C5orf46", "LRRC15", "MBOAT4", "LOC90246", "LOC100127955", "LINC00842", "LOC105375520",
                "PLCH1", "WNK2", "CCAT1", "LOC158434", "TRAV12-2", "SSUH2", "LOC105369455", "KCNK2", "N4BP3", "NRXN3",
                "MYH4", "CNN1", "LOC101927262", "BCAS1", "MYOZ3", "RNA18SN2", "AK5", "PDX1", "NXNL2", "SCN3B", "PRND",
                "C6orf132", "PDE1C", "ITPR3-AS1", "GJB4", "ESM1", "DLGAP1-AS3", "FAM227A", "PAK5", "TMEM52B",
                "AKR1B10", "GDNF-AS1", "LINC00319", "VGLL3", "CCL19", "FHL5", "MAPK15", "MFSD6L", "TRBJ2-7", "GRM6",
                "THORLNC", "SLC6A11", "LOC112268445", "BHLHE22-AS1", "LOC105376041", "PNLDC1", "PSG4", "PRDM16-DT",
                "CASP1P2", "LINC01235", "PGA3", "MT1B", "PKIA", "TRAV23DV6", "OSR2", "BCAN-AS1", "BEND6", "CATSPERB",
                "LOC105376571", "LOC102725231", "DEFA1B", "RNA18SN4", "RBM24", "JPH1", "NHERF4", "LCN10", "DEFA1",
                "SERPINB2", "MYO3A", "HAGLR", "NMRAL2P", "ZSCAN4", "FA2H", "FXYD3", "LOC102723464", "MUC1", "CDKL2",
                "LOC105370598", "GABRD", "HOXD4", "PTX3", "KIAA1549L", "TMC5", "RNA45SN5", "FAXC", "NUP210L", "DEFA3",
                "FOXJ1", "SERTAD4-AS1", "ANO4", "OVOL2", "IGLC6", "SLC6A20", "ADCYAP1R1", "COL11A1", "LOC105373209",
                "C21orf62", "DHH", "LOC100128317", "RORB", "RNA28SN4", "GREB1L-DT", "KIRREL3", "RHOV", "UNC80",
                "LOC107986178", "MYH15", "LOC102724323", "ATP6V1B1", "RNA45SN4", "TRPC6", "CADPS", "LOC105374510",
                "CBLN1", "KCNS1", "BDKRB1", "RNA5-8SN4", "DPP10-AS1", "OR51E1", "MSLN", "LOC107987085", "KCNIP1",
                "EPS8L3", "LOC107985480", "LOC105370092", "LINC00908", "HOXD8", "FAIM2", "LOC107985856", "IL13",
                "ANO2", "COMP", "MYEOV", "MUCL3", "PF4V1", "LINC01914", "SLC28A3-AS1", "FOLR1", "ADAM32", "LOC105371933",
                "SMCO2", "LAMP5", "RIMBP2", "LINC00673", "RNA5-8SN3", "AKR1B15", "RNA28SN5", "NECTIN4", "EPN3", "CNFN",
                "LINC00840", "LOC105379028", "LOC105370121", "LOC105379040", "RNA28SN1", "TNNC2", "DPEP1", "HAS2-AS1",
                "HOXD9", "LOC112267934", "IGSF5", "LDLRAD1", "EGOT", "LINC01186", "FBLL1", "GRHL2-DT", "LMTK3",
                "ACTN2", "OGN", "LCN6", "LOC107986473", "RGS13", "LOC105374975", "LINC02648")

gene_listF <- c("FBLN5", "LINC03040", "SMOC2", "SPON1", "STMN2", "SYT10", "ITGBL1", "EFEMP1", "DPEP1", "DPT",
                "MOXD1", "NAV3", "LTBP2", "KRT23", "NALCN", "GALNT17", "NTRK2", "PDZK1IP1", "CLDN11", "BICC1",
                "CTHRC1", "GSN", "ID4", "CH25H", "C7", "RND2", "NPNT", "CCL21", "CCN5", "SCIRT", "F3", "SOX9",
                "ROR2", "CFAP221", "PTGDS", "DCDC2", "THBS2", "OR51E1", "LOXL1", "LAMC3", "SOX9-AS1", "ZNF541",
                "DPPA4", "MDFI", "RRAD", "PAQR5", "PLCXD3", "MGP", "LOC105376042", "KRT87P", "LOXL4", "LINC02388",
                "BHLHE22", "LOC105372061", "PARM1", "VSIG2", "MYOZ3", "GJA5", "LOC105377896", "FAP", "SYNDIG1",
                "LRRC1", "MEX3B", "WFDC2", "LUM", "ADRA2A", "BMX", "VTCN1", "CXCL8", "CFTR", "CACNA1C", "THY1",
                "CDH6", "CPZ", "CCL19", "MMP7", "ENTREP1", "IER5L", "CLIC6", "PKP1", "INAVA", "CXCL6", "CXCL1",
                "SOD3", "IGDCC4", "ANKRD1", "ITGB6", "CLGN", "TMEM178B", "CETP", "SFRP4", "CCDC80", "CDKN2A",
                "BDKRB2", "AEBP1", "MFAP2", "COL15A1", "FBLN1", "C5AR2", "RASL12", "CABYR", "CADM2", "SSUH2",
                "JAG2", "MUC13", "SLC1A7", "TACSTD2", "POM121L9P", "LRRC15", "NPTX2", "PDX1", "LINC02331", "RNF112",
                "MAGED4B", "LCN10", "RGS4", "SEMA3B", "SERTM2", "LOC107985568", "CA8", "SAXO2", "SCN3B", "FOXS1",
                "LOC107984253", "PTCHD4", "ADGRE1", "KRT7", "NKD2", "NFATC4", "RASL11B", "NPTXR", "GDNF-AS1",
                "LINC02319", "LOC107986414", "TPPP3", "SSTR5-AS1", "LMOD1", "KRT7-AS", "MAP1A", "LOC101929532",
                "DSCAM", "PDZRN3-AS1", "COL7A1", "SCAT8", "STK33", "KRT86", "LINC02648", "ACTG1P20", "ATP8A2",
                "ACKR1", "MADCAM1", "MRAP2", "EDA2R", "DNAH5", "TBILA", "PPP1R14A", "LINC00707", "LOC105378890",
                "LOC105372889", "FOXL1", "HSPB6", "CCN3", "LOC105373883", "SFN", "LOC105377685", "ASCL1", "CDH11",
                "LINC01186", "LOC105378477", "MSX1", "CELF4", "PLCB4", "CILP2", "ASIC5", "SLC22A15", "HAPLN3",
                "PRRX1", "FOXC2", "GAS2L1P2", "SEMA3G", "LOC100996549", "SHC4", "CALML3", "PHEX", "ATP6V1B1",
                "CARMN", "TMEM132C", "MYOCD", "LOC105370777", "SCN7A", "SLC6A11", "RBM46", "OXTR", "VWF", "NMNAT2",
                "LOC105371910", "LINC01605", "HFM1", "CCDC187", "LINC00312", "SRRM3", "PSG4", "MIR514B", "CPXM1",
                "LOC105374309", "CALCA", "CXCL5", "LOC100288175", "LAMA1", "BCAN-AS1", "UCN", "ZPLD1", "C10orf90",
                "ZNF883", "CPNE6", "LOC105378948", "LINC00673", "LINC01205", "LOC105377479", "LINC02519", "PAPPA-AS1",
                "OVOL1", "PODNL1", "RXRG", "PCSK1N", "CREB3L1", "SPOCK3", "SMPX", "IGFN1", "HIF3A", "TIMD4",
                "LOC101927190", "PRR16", "LDB3", "SLC51B", "NCAM2", "ARHGAP40", "LOC112268050", "PCDHAC2", "LOC107987104",
                "PLXNB3", "CBLN2", "LOC105373071", "LOC107985072", "MUC6", "LOC105376127", "TBX1", "OVOL1-AS1",
                "LOC105369306", "LOC102723914", "SPINK1", "C19orf18", "SLC30A2", "OLAH", "ANO2", "GPR158", "PKIA",
                "VSX1", "LOC105377460", "LRRC19", "LOC105369455", "DCX", "SPATA22", "LOC613266", "CATSPERB", "GLDN",
                "PYY2", "SULT1C2", "LCN6", "FER1L4", "LOC105374396")


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
unique_to_list1 <- anti_join(kegg_df1, kegg_df2, by = "Term")
unique_to_list2 <- anti_join(kegg_df2, kegg_df1, by = "Term")

# Calculate the combined score for unique pathways
unique_to_list1$combined_score <- unique_to_list1$Combined.Score
unique_to_list2$combined_score <- unique_to_list2$Combined.Score

# Select the top 20 unique pathways for each list
top_unique_to_list1 <- unique_to_list1 %>%
  arrange(desc(combined_score)) %>%
  head(20)

top_unique_to_list2 <- unique_to_list2 %>%
  arrange(desc(combined_score)) %>%
  head(20)

# Create a bar plot for the top 20 unique pathways for list1
plot_list1 <- ggplot(top_unique_to_list1, aes(x = reorder(Term, -combined_score), y = combined_score)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Unique Pathways Males", x = "Pathways", y = "Combined Score") +
  theme(axis.text.y = element_text(size = 15))

# Display the plot for list1
print(plot_list1)

# Save the plot for list1 to a file
ggsave("unique_pathways_plot_list1.png", plot_list1)

# Create a bar plot for the top 20 unique pathways for list2
plot_list2 <- ggplot(top_unique_to_list2, aes(x = reorder(Term, -combined_score), y = combined_score)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Unique Pathways Females", x = "Pathways", y = "Combined Score") +
  theme(axis.text.y = element_text(size = 15))

# Display the plot for list2
print(plot_list2)

# Save the plot for list2 to a file
ggsave("unique_pathways_plot_list2.png", plot_list2)
