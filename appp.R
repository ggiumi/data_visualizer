library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

# Lista di file con nomi personalizzati
file_list <- list(
  "NAFLD - GSE135251" = "GSE135251.R",
  "LIVER CANCER - GSE144269" = "GSE144269.R",
  "HCC - GSE113617" = "GSE113617.R"
)

signatures_path <- "/home/giulia/script"

signatures_files <- list(
  "ANDREATTA_CD4_NaiveLike" = "signature_ANDREATTA_CD4_NaiveLike.txt",
  "ANDREATTA_CD8_EarlyActivation" = "signature_ANDREATTA_CD8_EarlyActivation.txt",
  "ANDREATTA_CD8_EffectorMemory" = "signature_ANDREATTA_CD8_EffectorMemory.txt",
  "ANDREATTA_TFH" = "signature_ANDREATTA_TFH.txt",
  "ANDREATTA_TH1" = "signature_ANDREATTA_TH1.txt",
  "ANDREATTA_Treg" = "signature_ANDREATTA_Treg.txt",
  "MONTIRONI_inflamed" = "signature_MONTIRONI_inflamed.txt",
  "PFISTER_CD8_Exhaustion" = "signature_PFISTER_CD8_Exhaustion.txt",
  "PFISTER_CD8_Exhaustion_extended" = "signature_PFISTER_CD8_Exhaustion_extended.txt",
  "PFISTER_CD8PD1T_ResidentLike" = "signature_PFISTER_CD8PD1T_ResidentLike.txt"
)

signatures_info <- c(
  "ANDREATTA_CD4_NaiveLike" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: CCR7, SELL, CD40LG, IL7R, TCF7, LEF1, GPR183, KLRB1, LTB, MAL, PASK, AQP3, TRAT1.",
  "ANDREATTA_CD8_EarlyActivation" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: GZMK, FOS, CD69, ZFP36, FOSB, CCL5, GZMM, DUSP2, LYAR, SAMD3, CXCR4, CTSW, CD8A, ANXA1, KLRG1, CD8B, AOAH, TAGAP, KLRD1, IER2, GZMA, CST7, ITM2C, PARP8, BTG2.",
  "ANDREATTA_CD8_EffectorMemory" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: GZMA, GZMK, NKG7, CD8A, CD8B, CTSW, GZMB, CCL5, CST7, PRF1, ABI3, FASLG, ITM2C, C12orf75, EOMES, CHST12, CCR5, HCST, AOAH, HOPX, SLAMF7, CXCR3, OASL, F2R, CXCR6.",
  "ANDREATTA_TFH" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: TOX2, MAF, TBC1D4, TNFRSF4, CD4, UCP2, CORO1B, TIGIT, BATF, NR3C1, ITM2A, TNFRSF18, LIMS1, ICA1, CTSB, SH2D1A, CTLA4, MAGEH1, SESN3, ICOS, NMB, CXCL13, CD200, BTLA, PTPN13.",
  "ANDREATTA_TH1" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: CD40LG, IL7R, KLRB1, CD4, RORA, TNFRSF25, ANXA1, LTB, GPR183, CDKN1A, LMNA, S100A11, S100A4, PLIN2.",
  "ANDREATTA_Treg" = "Andreatta, M., Corria-Osorio, J., Müller, S. et al. Interpretation of T cell states from single-cell transcriptomics data using reference atlases. Nat Commun 12, 2965 (2021). https://doi.org/10.1038/s41467-021-23324-4.
  GENES LIST: TNFRSF4, TNFRSF1B, TNFRSF18, IL2RA, CTLA4, TIGIT, ICOS, GPX1, MAF, FOXP3, BATF, IKZF4, IKZF2, SAT1, TBC1D4, DUSP4, DNPH1, PHLDA1, SNX9, CD4, TYMP, NAMPT, SYNGR2, PBXIP1, GLRX.",
  "MONTIRONI_inflamed" = "Montironi, C., Castet, F., Haber, P. K., Pinyol, R., Torres-Martin, M., Torrens, L., ... Llovet, J. M. (Correspondence Author), & Sia, D. (Correspondence Author). (2023). Inflamed and non-inflamed classes of HCC: a revised immunogenomic classification. Gut, 72(1), 129-140. https://doi.org/10.1136/gutjnl-2021-325918.
  GENES LIST: CCL5, CD2, CD3D, CD48, CD52, CD53, CXCL9, CXCR4, FYB, GZMA, GZMB, GZMK, IGHG1, IGHG3, LAPTM5, LCP2, PTPRC, SLA, TRAC, TRBC2.",
  "PFISTER_CD8_Exhaustion" = "Pfister, D., Núñez, N.G., Pinyol, R. et al. NASH limits anti-tumour surveillance in immunotherapy-treated HCC. Nature 592, 450–456 (2021). https://doi.org/10.1038/s41586-021-03362-0.
  GENES LIST: CD103, CD11b, CD11c, CD206, CD4, CD44, CD62L, CD64, CD69, CD73, CD8, F480, FOXP3, KI67, Ly6C, Ly6G, MERTK, MHCII, NK11, PD1, PDL1, TCRb.",
  "PFISTER_CD8_Exhaustion_extended" = "Pfister, D., Núñez, N.G., Pinyol, R. et al. NASH limits anti-tumour surveillance in immunotherapy-treated HCC. Nature 592, 450–456 (2021). https://doi.org/10.1038/s41586-021-03362-0.
  GENES LIST: BLIMP1, CD103, CD11B, CD11C, CD127, CD206, CD244, CD25, CD39, CD4, CD44, CD62L, CD64, CD69, CD73, CD8, CTLA4, CXCR6, EOMES, F480, FOXP3, GZMB, IFNG, KI67, KLRG1, LAG3, LY6C, LY6G, MERTK, MHCII, NK11, PD1, PDL1, TBET, TCF1, TCRB, TIM3, TNF, TOX.",
  "PFISTER_CD8PD1T_ResidentLike" = "Pfister, D., Núñez, N.G., Pinyol, R. et al. NASH limits anti-tumour surveillance in immunotherapy-treated HCC. Nature 592, 450–456 (2021). https://doi.org/10.1038/s41586-021-03362-0.
  GENES LIST: CCL3, CCL5, CD8A, CXCR3, CXCR6, FASL, GZMA, GZMK, HAVCR2, IL7R, KLF2, LAG3, LAMP1, MKI67, PDCD1, PRF1, RGS1, SELL, STAT1, STAT3, TCF7."
)

dataset_info <- list(
  "NAFLD - GSE135251" = list(
    paragraph1 = "Public on Dec 03, 2020 - TRANSCRIPTOMIC PROFILING ACROSS THE SPECTRUM OF NON-ALCOHOLIC FATTY LIVER DISEASE",
    paragraph2 = "Homo Sapiens",
    paragraph3 = "Expression profiling by high throughput sequencing",
    paragraph4 = "The pathophysiological mechanisms that drive non-alcoholic fatty liver disease (NAFLD) progression remain poorly understood. This multicenter study characterized the transcriptional changes that occur as liver disease progresses. 216 snap frozen liver biopsies, comprising 206 NAFLD cases with different fibrosis stages and 10 controls were studied. Samples underwent high-throughput RNA sequencing. This study provides novel insights into transcriptional changes during liver disease evolution and progression as well as proof of principle that transcriptomic changes reveal potentially tractable biomarkers for NAFLD fibrosis. 216 snap-frozen biopsies were processed for RNA sequencing on the Illumina NextSeq 500 system.",
    paragraph5 = "Govaere O, Cockell S, Tiniakos D, Queen R et al. Transcriptomic profiling across the nonalcoholic fatty liver disease spectrum reveals gene signatures for steatohepatitis and fibrosis. Sci Transl Med 2020 Dec 2;12(572). PMID: 33268509. Pfister D, Núñez NG, Pinyol R, Govaere O et al. NASH limits anti-tumour surveillance in immunotherapy-treated HCC. Nature 2021 Apr;592(7854):450-456. PMID: 33762733."
  ),
  "LIVER CANCER - GSE144269" = list(
    paragraph1 = "Public on Aug 04, 2020 - The genomic landscape of Mongolian hepatocellular carcinoma",
    paragraph2 = "Homo Sapiens",
    paragraph3 = "Expression profiling by high throughput sequencing",
    paragraph4 = "Mongolia has the highest reported incidence of (and mortality from) hepatocellular carcinoma (HCC) in the world. This is the first molecular study aiming to characterize the genomic landscape of Mongolian HCC. Paired tumor/non-tumor liver tissue samples from a cohort of 76 hepatocellular carcinoma patients undergoing surgery between 2015 and 2016 at the National Cancer Center of Mongolia were sequenced with RNA-Seq and Whole Exome Sequencing (WES). This submission consists of 140 RNA-seq samples from 70 HCC tumors and matched nontumor tissue. Due to patient privacy concerns, patient phenotype and WES data has been submitted to dbGaP under accession phs002000.v1.p1.",
    paragraph5 = "Candia J, Bayarsaikhan E, Tandon M, Budhu A et al. The genomic landscape of Mongolian hepatocellular carcinoma. Nat Commun 2020 Sep 1;11(1):4383. PMID: 32873799."
  ),
  "HCC - GSE113617" = list(
    paragraph1 = "Public on Apr 01, 2022 - RNA biotype-associated molecular classification in hepatitis B-related hepatocellular carcinoma",
    paragraph2 = "Homo Sapiens",
    paragraph3 = "Expression profiling by high throughput sequencing",
    paragraph4 = "Recent advance of RNA-seq technology enabled us to profile the diverse variations of RNA transcripts precisely, including the variants from alternative splicing as well as non-coding transcripts. Here, by performing transcriptome profiling including coding and noncoding transcripts, we identify four molecular subtypes of hepatitis B-related hepatocellular carcinoma (HCC) patients that are characterized by enriched expression of the RNA biotypes of noncoding (NC) and immune-related transcripts (IM) (i.e., IM+NC+, IM-NC+, IM+NC-, IM-NC-). The subtype IM+NC+ shows better prognostic outcome, while the subtype IM+NC- shows the worst prognostic outcome, respectively. Further interrogation of the subtypes identifies long noncoding transcripts (i.e., LINC00844, C3P1, and TRPG1-AS1) as well as an alternatively spliced event of USO1 that play pivotal roles in HCC progression. In addition, we report an oncogenic fusion transcript SLC39A14-PIWIL2 that promotes an aggressive phenotype of HCC. Our comprehensive and systematic analysis of HCC transcriptome identify RNA biotype-based molecular classification, revealing novel driver transcriptome variants that can be potential biomarkers and/or therapeutic targets for precision medicine. Transcriptome profiling from 68 cases of HCC tissues and 10 adjacent non-tumoral tissues were performed. Four subtypes: S1: IM+NC+ S2: IM-NC+ S3: IM+NC- S4: IM-NC-."
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 300,
    title = "DATA VISUALIZER"
  ),
  
  dashboardSidebar(
    width = 300,
    
    fluidRow(
      tags$style(
        ".main-sidebar {float:top; margin-top:40px; padding-left:15px; padding-right:15px}"
      ),
      
      selectizeInput(inputId = "datasets", label = "Select dataset(s)", 
                     choices = names(file_list), selected = NULL, multiple = FALSE),
      
      actionButton(inputId = "do", 
                   label = "Run the analysis"),
      
      tags$hr(),
      
      selectInput(inputId = "plotType",
                  label = "Select a Plot",
                  choices = c("Heatmap", "PCA", "Volcano Plot", "Heatmap DE", "Box Plot", "Signatures Heatmap"),
                  selected = NULL),
      
      downloadButton("downloadPlot", "Download Plot"),
      
      tags$hr(),
      
      selectInput(inputId = "selectSignatures",
                  label = "Select a Signature",
                  choices = names(signatures_files)),
      actionButton(inputId = "checkSignatures", 
                   label = "Run the analysis for these signatures"),
      
      actionButton(inputId = "reset_button", label = "Reset"),
      textOutput("signatureInfo")
    )
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("INFO & CUSTOMIZATION",
               fluidRow(
                 column(6, 
                        tags$div(
                          tags$h3("Publication Date and Title"),
                          textOutput("PARAGRAPH1"),
                          tags$h4("Organism"),
                          textOutput("PARAGRAPH2"),
                          tags$h4("Experiment type"),
                          textOutput("PARAGRAPH3"),
                          tags$h4("Summary and Overall design"),
                          textOutput("PARAGRAPH4"),
                          tags$h4("Citations"),
                          textOutput("PARAGRAPH5")
                        )
                 ),
                 column(6,
                        tags$h3("Customization"),
                        uiOutput("dynamicDropdown"),
                        actionButton(inputId = "customize", 
                                     label = "Run the customized analysis")
                 )
               )
      ),
      
      tabPanel("METADATA",
               fluidRow(
                 column(6, plotOutput("PCA_ANALYSIS", width = "100%")),
                 column(6, DTOutput("colData", width = "100%"))
               )
      ),
      
      tabPanel("DE ANALYSIS",
               fluidPage(
                 column(12,
                        textInput("gene_list_input", "Enter Genes List (in capital letters and separated with commas or spaces)")
                 ),
                 column(12,
                        actionButton("search_button", "Search")
                 )
               ),
               fluidRow(
                 column(6, 
                        dataTableOutput("RESULTS"),
                        tags$head(
                          tags$style(
                            HTML(".dataTables_filter { visibility: hidden; }")
                          )
                        )
                 ),
                 column(6, plotOutput("VOLCANO_PLOT_ANALYSIS", width = "100%"))
               )
      ),
      
      tabPanel("BOXPLOTs",
               fluidPage(
                 selectInput("selected_gene", "Select Gene(s):", choices = NULL, multiple = TRUE),
                 plotOutput("GENE_BOXPLOT", width = "100%")
               )
      ),
      
      tabPanel("HEATMAPs",
               fluidRow(
                 column(6, plotOutput("HEATMAP_ANALYSIS", width = "100%")),
                 column(6, plotOutput("HEATMAP", width = "100%"))
               )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  filtered_genes <- reactiveValues()
  
  observeEvent(input$datasets, {
    req(input$datasets)
    
    dataset_info_selected <- dataset_info[[input$datasets]]
    
    output$PARAGRAPH1 <- renderText({ dataset_info_selected$paragraph1 })
    output$PARAGRAPH2 <- renderText({ dataset_info_selected$paragraph2 })
    output$PARAGRAPH3 <- renderText({ dataset_info_selected$paragraph3 })
    output$PARAGRAPH4 <- renderText({ dataset_info_selected$paragraph4 })
    output$PARAGRAPH5 <- renderText({ dataset_info_selected$paragraph5 })
  })
  
  observeEvent(input$do, {
    req(input$datasets)
    
    output$signatureInfo <- renderText({
      selected_signature <- input$selectSignatures
      informative_text <- signatures_info[selected_signature]
      informative_text
    })
    
    withProgress(message = 'Generating PCA plot and heatmap', value = 0, {
      
      # Load the corresponding R script for the selected dataset
      dataset_file <- file_list[[input$datasets]]
      source(dataset_file, local = TRUE)
      
      incProgress(0.1, detail = "Loading data...")
      Sys.sleep(1)  
      
      incProgress(0.3, detail = "Preprocessing data...")
      Sys.sleep(1)
      
      incProgress(0.6, detail = "Creating PCA plot...")
      
      updateSelectInput(session, "dynamicDropdown", choices = colNames)
      
      output$dynamicDropdown <- renderUI({
        selectInput("dynamicDropdown", "Select the Design of your analysis", choices = colNames)
      })
      
      pca_plot <- createPCAPlot(dds)
      
      output$PCA_ANALYSIS <- renderPlot({
        pca_plot
      }, height = 600, width = 600)
      
      heatmap <- generateHeatmap(data, annotation, annotation_colors, colorbar)
      
      output$HEATMAP_ANALYSIS <- renderPlot({
        heatmap
      }, height = 600, width = 600)
      
      volcano_plot <- generateVolcanoPlot(res)
      
      output$VOLCANO_PLOT_ANALYSIS <- renderPlot({
        volcano_plot
      }, height = 600, width = 700)
      
      source("converter.R")
      
      observeEvent(input$search_button, {
        gene_list <- input$gene_list_input
        converted_genes <- genes_to_regex(gene_list)
        
        proxy <- dataTableProxy("RESULTS")
        updateSearch(proxy, keyword = list(global = converted_genes, columns = NULL))
      })
      
      output$RESULTS <- renderDataTable({
        res_df <- data.frame(
          Gene = rownames(res), 
          Log2FC = res$log2FoldChange,
          PValue = res$pvalue
        )
        datatable(res_df, 
                  extensions = 'Buttons',
                  filter = "top",
                  options = list(
                    scrollY = "500px",
                    scrollCollapse = TRUE,
                    paging = FALSE,
                    searching = TRUE,
                    search = list(regex = TRUE, smart = TRUE),
                    dom = 'frtipB',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ))
      }, width = "100%", server = TRUE)
      
      output$colData <- renderDT({
        selected_colData <- as.data.frame(selected_colData)
        datatable(selected_colData,
                  extensions = 'Buttons',
                  filter = "top",
                  options = list(
                    scrollY = "500px",
                    scrollCollapse = TRUE,
                    paging = FALSE,
                    dom = 'frtipB',
                    buttons = c('copy', 'csv', 'excel', 'pdf')
                  ))
      }, width = "100%")
      
      observe({
        updateSelectInput(session, "selected_gene", choices = rownames(dds))
      })
      
      observeEvent(input$selected_gene, {
        req(input$selected_gene)
        gene_boxplot <- generateGeneBoxplot(dds, input$selected_gene)
        
        output$GENE_BOXPLOT <- renderPlot({
          gene_boxplot
        })
      })
      updateSelectizeInput(session, "selected_gene", choices = rownames(res))
      
      heatmap2 <- heatmap_DE(sampleDistMatrix)
      
      output$HEATMAP <- renderPlot({
        heatmap2
      }, height = 600, width = 800)
      
      source("signature_analysis.R")
      
      observeEvent(input$checkSignatures, {
        req(input$selectSignatures)
        # Construct the file path
        # Construct the file path
        selected_signature <- input$selectSignatures
        file_path <- file.path(signatures_path, signatures_files[[selected_signature]])
        
        # Read the gene list from the file
        genes_of_interest <- scan(file_path, what = "", sep = "\n")
        
        # Check if genes are present in the counts matrix
        valid_genes <- genes_of_interest[genes_of_interest %in% rownames(counts_matrix)]
        invalid_genes <- genes_of_interest[!genes_of_interest %in% rownames(counts_matrix)]
        
        if (length(valid_genes) == 0) {
          showNotification("No valid genes found in the counts matrix for the selected signature", type = "error", duration = NULL)
          return(NULL)
        }
        
        if (length(invalid_genes) > 0) {
          showModal(modalDialog(
            title = "Warning",
            paste("The following genes were not found and will be excluded:", paste(invalid_genes, collapse = ", ")),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))    
        }
        heatmap_plot <- generateHeatmap_genes(counts_matrix, valid_genes)
        
        output$HEATMAP_ANALYSIS <- renderPlot({
          heatmap_plot
        }, height = 600, width = 800)
        
        gene_list <- readLines(file_path)
        filtered_genes$list <- gene_list
        converted_genes <- genes_to_regex(paste(gene_list, collapse = ","))
        
        proxy <- dataTableProxy("RESULTS")
        updateSearch(proxy, keyword = list(global = converted_genes, columns = NULL))
      })
      
      observeEvent(input$reset_button, {
        heatmap <- generateHeatmap(data, annotation, annotation_colors, colorbar)
        
        output$HEATMAP_ANALYSIS <- renderPlot({
          heatmap
        }, height = 600, width = 600)
        
        proxy <- dataTableProxy("RESULTS")
        updateSearch(proxy, keyword = list(global = "", columns = NULL))
      })
      
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("plot", input$plotType, ".png", sep = "")
        },
        content = function(file) {
          plot <- switch(input$plotType,
                         "Heatmap" = heatmap,
                         "PCA" = pca_plot,
                         "Volcano Plot" = volcano_plot,
                         "Heatmap DE" = heatmap2,
                         "Box Plot" = generateGeneBoxplot(dds, input$selected_gene),
                         "Signatures Heatmap" = generateHeatmap_genes(counts_matrix, genes_of_interest)
          )
          png(file, width = 1200, height = 800, res = 150)
          print(plot)
          dev.off()
        }
      )
      
      observeEvent(input$customize, {
        if (input$dynamicDropdown == "Stage") {
          source("GSE135251_Stage.R")
          
          pca_plot1 <- createPCAPlot2(dds)
        }
      })
      
      incProgress(0.1, detail = "Done!")
    })
  })
}

shinyApp(ui, server)
