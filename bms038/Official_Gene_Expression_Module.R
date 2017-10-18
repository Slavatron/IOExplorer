require(shiny)
require(DT)
require(ggplot2)
#require(ComplexHeatmap)
require(pheatmap)
require(RColorBrewer)
require(ggplot2)
require(ggthemes)
require(ggpubr)
require(gridExtra)
require(plotly)
library(GSVA)
####################################################################
#
#' Function for collapsing list of named variable vectors into a single character vector
#' @param my_list - a list of named variables structured like: "VariableName" = "Display Name"; meant to be used with lists defined in key_columns.R
Expand_Variable_List = function(my_list) {
  out_vect = c()
  for (i in names(my_list)) {
    n = names(my_list[[i]])
    v = unname(my_list[[i]])
    names(n) = v
    out_vect = c(out_vect, n)
  }
  return(out_vect)
}
# READ IN PATHWAYS ARRAY
source("Parsed_GSEA_Sets.R")
#source("Alexis_Genes_Parsed.R")
#source("Hallmark_Genes.R")
#source("hall_fpkm_parsed.R")

# IMPORT EXPRESSION DATA
#xdat = readRDS("log2.fpkm.with.names.rds") # Log Transformed for plotting
xdat = readRDS("new_xdat.rds")
#fpkm_dat = readRDS("fpkm.BMS_038.dups_removed.rds")
fpkm_dat = readRDS("new_rld.rds")
gene_lookup = readRDS("Symbol_Entrez_Table.rds")
#gene_lookup = readRDS("new_lookup.rds")
fpkm_MAT = as.matrix(fpkm_dat)

# SEPARATE INTO PRE/ON
ons = grep("_on", names(xdat))
preX = xdat[,-ons]

pres = grep("_pre", names(xdat))
onX = xdat[,-pres]

cytdf = read.csv("bms038_data_050917.csv")
cytdf = cytdf[cytdf$Sample %in% names(xdat),c("Sample", "cytscore")]
#aDC = readRDS("fpkm.aDC.gsva.RDS")
Cytoxic_Cells = readRDS("rld.Cytoxic.gsva.rds")
aDC_Set = list("Name" = "Cytoxic", "Hugo" = c("APBA2","APOL3","CTSW","DUSP2","GNLY","GZMA","GZMH","KLRB1","KLRD1","KLRF1","KLRK1","NKG7","RORA","RUNX3","SIGIRR","WHAMMP3","ZBTB16"), "Entrez" = c("10578","1521","1844","22914","2999","3001","321","339005","3820","3824","4818","51348","59307","6095","7704","80833","864"), "GSVA" = Cytoxic_Cells)
#aDC$Sample = rownames(aDC)
#aDC_Set = list("Name" = "aDC", "Hugo" = c("CCL1", "EBI3", "IDO1", "LAMP3", "OAS3"), "Entrez" = c("10148","27074","3620", "4940","6346"), "GSVA" = aDC)


###########################################################################
# FUNCTIONS
###########################################################################

# FUNCTION FOR MAKING HEATMAP PLOT OUT OF EXP MATRIX AND ANNOTATION MATRIX
#' @param exp_mtx - a dataframe of gene X sample expression values
#' @param annot_mtx - a dataframe of sample X annotation values
#' @param title_txt - text shown on top of the heatmap legend
make_heatmap = function(exp_mtx, annot_mtx, title_txt = "mRNA Expression", ann_colors=ann_colors) {
  # create heatmap object
  show_gene <- F
  if(dim(exp_mtx)[1] < 100) {
    show_gene <- T
  }
  pheatmap(exp_mtx, main=title_txt, cluster_rows=F, show_rownames=show_gene, show_colnames=T, cluster_cols=F, annotation_col=annot_mtx, annotation_colors = ann_colors)
}
# DEFINE HEATMAP COLOR PALETTES
ann_colors = list(
  myBOR = c("PRCR" = "green3", "SD" = "blue", "PD" = "red"),
  SampleType = c("on" = "yellow", "pre" = "black"),
  SubtypeEZ = c("BRAF" = "royalblue", "RAS" = "gold", "NF1" = "black", "TripleWt" = "green"),
  cytscore = colorRampPalette(brewer.pal(9, "Reds"))(100),
  log10mut = colorRampPalette(brewer.pal(9, "Blues"))(100),
  thresh.95muts = colorRampPalette(brewer.pal(9, "Greens"))(100)
)


###########################################################################
# END FUNCTIONS
###########################################################################



###########################################################################
# DEFINE MODULES
###########################################################################

GeneExprUI = function(id, choices_list) {
  ns = NS(id)
  tagList(
    sidebarPanel(width = 3,
        h3("Select Genes"),
        textInput(ns("GeneText"), label = "Input Genes as Text", width = '100%'),
        textOutput(ns("BadFeedback")),
        selectInput(ns("Pathways"), "Select Genes by Pathway", choices = c(Type_Names)),
        uiOutput(ns("AddGenesButton")),
        uiOutput(ns("DitchGenesButton")),
#        actionButton(ns("AddGenes"), label = "Add These Genes"),
#        actionButton(ns("DitchGenes"), label = "Remove All Checked Genes"),
        uiOutput(ns("Pathway_Boxes"))
      ),
    mainPanel(width = 9,
      h3("Gene Expression Analysis"),
      "Gene expression for any combination of genes can be examined using the tools found here. Construct your gene sets using the text-input option (with at least one space between each gene) or select genes from known immune pathways. To visualize expression alongside other clinical/genomic features across available patients, use the Heatmap option.",
      p(),
      "Run GSVA on your gene set to create representative variables that can be examined on the 'Correlation' and 'Genomics & Outcome' tabs.",
      p(),
      tabsetPanel(
        tabPanel("Gene Selection",
          textOutput(ns("debugText")),
          fluidRow(
            column(5,
                   textInput(ns("GeneSetName"), "Save Gene Set As:", "My_Gene_Set")),
#                   tableOutput(ns("Selection_Table"))),
            column(7,
                   h3(""),
                   actionButton(ns("RunGSVA"), "Run GSVA Analysis and Save", style = "color: black; background-color: #ffd700"))
#                   actionButton(ns("RunGSVA"), "Run GSVA Analysis and Save", style = "color: white; background-color: #076f30"))
#                   p(),
#                   tableOutput(ns("GSVA_Table"))
          ),
          column(5, tableOutput(ns("Selection_Table"))),
          column(7, tableOutput(ns("GSVA_Table")))
#          column(6,wellPanel(DT::dataTableOutput(ns("Selection_Table")), style = "overflow-x:scroll; max-width: 1000px; overflow-y:scroll; max-height: 600px")),
#          column(6,wellPanel(DT::dataTableOutput(ns("GSVA_Table")), style = "overflow-x:scroll; max-width: 1000px; overflow-y:scroll; max-height: 600px"))
#          actionButton(ns("SaveGeneSet"), "Save Gene Set")        
          ),
#        tabPanel("View GSVA Values",
#          wellPanel(DT::dataTableOutput(ns("GSVA_Table")), style = "overflow-x:scroll; max-width: 1000px; overflow-y:scroll; max-height: 600px")
#        ),
        tabPanel("Heatmap",
#                 ApplyRemoveUI(ns("INNER")),
#                 h3("Choose Heatmap Annotations"),
#                 h2("Patients will be sorted by annotations"),
                 column(3,
                 selectInput(ns("Samples_Shown"), "Samples Shown", choices = c("Pre-Treatment Only", "Pre and On", "On-Treatment Only"))),
                 column(3,
                 uiOutput(ns("Sorting_DropDown"))),
#                 selectInput(ns("Sorting_Method"), "Sort Samples By:", choices = c("Hierarchical Clustering", "Clinical Response", "Annotation A", "Annotation B"))),
                 column(3,
                        selectInput(ns("Anno_A"), "Annotation A", choices = Expand_Variable_List(choices_list))),
#                 selectInput(ns("Anno_A"), "Annotation A", choices = list("UV Signature" = "Signature.7",
#       "Mutation Load (log)" = "log10mut",
#       "Subtype" = "SubtypeEZ",
#       "Clonal Mutation Load" = "thresh95.muts",
#       "Cytolytic Score" = "cytscore"))),
                  column(3,
                   selectInput(ns("Anno_B"), "Annotation B:", choices = c("None" = "None", Expand_Variable_List(choices_list)))),
#                  selectInput(ns("Anno_B"), "Annotation B:", choices = list("None" = "None", "Subtype" = "SubtypeEZ", "Clonal Mutation Load" = "thresh95.muts"))),
##          plotOutput(ns("HeatMap")) 
#          p(),
          uiOutput(ns("Sized_Plot")),
          ApplyRemoveUI(ns("INNER"))
        )
      )
#      ApplyRemoveUI(ns("INNER"))
    )
  )
}

GeneExpr = function(input, output, session, choices_list, filterdata, fulldata) {
  ns = session$ns
#### PROCESS INPUT TEXT
  text_input = reactive({
    # SPLIT INPUT TEXT TO GET A LIST OF GENES
    my_genes = unlist(strsplit(input$GeneText, "\\s+"))
    # CONVERT LOWER CASE LETTERS TO UPPER CASE
    my_genes = toupper(my_genes)
    val_genes = my_genes[my_genes %in% rownames(preX)]
    bad_genes = setdiff(my_genes, val_genes)
    return(list(my_genes, val_genes, bad_genes))
  })
  # Check if genes actually exist and provide feedback if they aren't recognized
  output$BadFeedback = reactive({
    my_bads = text_input()[[3]]
    message = ""
    if (length(my_bads) > 0) {
      bads = paste(my_bads, collapse = " ")
      message = paste("Can't find these genes:", bads)
    }
    return(message)
  })
#### PROCESS INPUT BASED ON PATHWAYS
  # Dynamic UI for selecting genes in a particular pathway
  output$Pathway_Boxes = renderUI({
#    checkboxGroupInput(ns("ChosenPathwayGenes"), "Pathways", choices = sort(unname(SUPP_LIST[[input$Pathways]])), selected = unname(SUPP_LIST[[input$Pathways]]))
    checkboxGroupInput(ns("ChosenPathwayGenes"), "Pathways", choices = sort(unname(Type_List[[input$Pathways]])), selected = unname(Type_List[[input$Pathways]]))
  })
  # Create reactive value for genes selected from pathway checkboxes
  myGenes = reactiveValues(
    Check = c("APBA2", "APOL3", "CTSW", "DUSP2", "GNLY", "GZMA", "GZMH", "KLRB1", "KLRD1", "KLRF1", "KLRK1", "NKG7", "RORA", "RUNX3", "SIGIRR", "WHAMMP3", "ZBTB16")
#    Check = c("CCL1", "EBI3", "IDO1", "LAMP3", "OAS3")
  )
  # Buttons for adding/removing genes
  AddRemoveGenes = reactiveValues(
    Check = "ON"
  )
  # Buttons for Applying and Removing Filters
  output$AddGenesButton = renderUI({
    if (AddRemoveGenes$Check == "ON" & length(setdiff(input$ChosenPathwayGenes, myGenes$Check)) >0) {
      actionButton(ns("AddGenes"), label = "Add These Genes Too?", style = "color: white; background-color: #ff8866")
    } else if (AddRemoveGenes$Check == "ON" & length(setdiff(input$ChosenPathwayGenes, myGenes$Check)) == 0) {
      actionButton(ns("AddGenes"), label = "Genes Added", style = "color: white; background-color: #0000ff")
    } else {
      actionButton(ns("AddGenes"), label = "Add Checked Genes")
    }
    
  })
  output$DitchGenesButton = renderUI({
    if (AddRemoveGenes$Check == "OFF") {
      actionButton(ns("DitchGenes"), label = "Genes Removed", style = "color: white; background-color: #0000ff")
    } else {
      actionButton(ns("DitchGenes"), label = "Remove All Checked Genes")
    }
  })
  observeEvent(input$AddGenes, {
    AddRemoveGenes$Check = "ON"
    myGenes$Check = c(myGenes$Check, input$ChosenPathwayGenes)
  })
  observeEvent(input$DitchGenes, {
    AddRemoveGenes$Check = "OFF"
    myGenes$Check = c()
  })
  
#### DEFINE REACTIVE DATAFRAME OF ALL SELECTED GENES 
  subGenes = reactive({
    text_genes = text_input()[[2]]
    path_genes = myGenes$Check
    all_genes = c(text_genes, path_genes)
    out_df = preX[rownames(preX) %in% all_genes,]
    return(out_df)
  })
  onGenes = reactive({
    text_genes = text_input()[[2]]
    path_genes = myGenes$Check
    all_genes = c(text_genes, path_genes)
    out_df = onX[rownames(onX) %in% all_genes,]
    return(out_df)
  })
  PreOnGenes = reactive({
    text_genes = text_input()[[2]]
    path_genes = myGenes$Check
    all_genes = c(text_genes, path_genes)
    out_df = xdat[rownames(xdat) %in% all_genes,]
    return(out_df)
  })
  # DEFINE AGGREGATE VALUES FOR SELECTED GENES
  AggGenes = reactive({
    num_dat = subGenes()[,grep("Pt",names(subGenes()))]
    out_dat = subGenes()
    out_dat$Mean = apply(num_dat, 1, mean)
#    out_dat$Median = apply(num_dat, 1, median)
    out_dat$Std_Dev = apply(num_dat, 1, sd)
    # Round down to 4 digits 
    out_dat$Mean = round(out_dat$Mean, digits = 4)
#    out_dat$Median = round(out_dat$Median, digits = 4)
    out_dat$Std_Dev = round(out_dat$Std_Dev, digits = 4)
#    out_dat = out_dat[,c("Mean", "Median", "Std_Dev")]
    out_dat = out_dat[,c("Mean", "Std_Dev")]
    # Add annotations indicating how each gene was selected
    if (nrow(out_dat) > 0) {
      out_dat$Select_By = ""
      if (length(text_input()[[2]]) > 0) {
        out_dat[text_input()[[2]],]$Select_By = "Typed"
      }
      if (length(myGenes$Check) > 0) {
        out_dat[myGenes$Check,]$Select_By = "Pathway"
      }
    }
    return(out_dat)
  })
#### DISPLAY TABLE OF SELECTED GENES
  output$Selection_Table = renderTable({
    req(AggGenes)
    AggGenes()
  }, bordered = TRUE, rownames =TRUE)

  # DEFINE CHECKBOX OUTPUT OF ALL CHOSEN GENES
  output$GeneCheckBoxes = renderUI({
    checkboxGroupInput(ns("ChosenGenes"), "Chosen Genes:", choices = rownames(subGenes()) )
  })
  # DEFINE LIST OF GENES FOR ssGSEA  
  GSEA_List = eventReactive(input$RunGSVA, {
    # Define variables for running GSVA
    set_name = input$GeneSetName
    set_name = gsub(" ", "_", set_name)
    Hugo = rownames(AggGenes())
    Entrez = gene_lookup[gene_lookup$Gene %in% Hugo,"Entrez"]
    genelist = list(Entrez)
    # Run GSVA 
    scores <- gsva(fpkm_MAT,genelist,method="ssgsea",rnaseq=T, verbose = T)
    # Define Progress Bar object to let User know this takes a while 
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating GSVA", value = 0.1)
    # Loop to increment bar
    for (i in 1:10) {
      incr = i/10
      perc = incr*100
      # Increment the progress bar, and update the detail text.
      progress$inc(incr, detail = paste(perc, "% done...",sep=""))
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)
    }
    # Process GSVA results
    scores = as.data.frame(t(scores))
    rownames(scores) = gsub("Pre", "pre", rownames(scores))
    rownames(scores) = gsub("On", "on", rownames(scores))
    names(scores)[1] = set_name
#    scores$PT = rownames(scores)
    out_list = list("Name" = set_name, "Hugo" = Hugo, "Entrez" = Entrez, "GSVA" = scores)
    return(out_list)
  })
  # Only present "SaveGeneSet' button once GSVA has been run
  output$SaveGeneSet_Button = renderUI({
    req(GSEA_List())
    actionButton(ns("SaveGeneSet"), "Save Gene Set")
  })
  # List of GSEA_List instances; gets updated with each click of "SaveGeneSet"
  Saved_Gene_Sets = reactiveValues(
    Cytoxic_Cells = list("Name" = "Cytoxic", "Hugo" = c("APBA2","APOL3","CTSW","DUSP2","GNLY","GZMA","GZMH","KLRB1","KLRD1","KLRF1","KLRK1","NKG7","RORA","RUNX3","SIGIRR","WHAMMP3","ZBTB16"), "Entrez" = c("10578","1521","1844","22914","2999","3001","321","339005","3820","3824","4818","51348","59307","6095","7704","80833","864"), "GSVA" = Cytoxic_Cells)
    #aDC = list("Name" = "aDC", "Hugo" = c("CCL1", "EBI3", "IDO1", "LAMP3", "OAS3"), "Entrez" = c("10148","27074","3620", "4940","6346"), "GSVA" = aDC)
  )
  Saved_GSVA_Values = reactive({
    out_data = do.call("rbind", lapply(reactiveValuesToList(Saved_Gene_Sets), function(x) as.data.frame(t(x[[4]]))))
    out_data = as.data.frame(t(out_data))
  })
  observeEvent(input$RunGSVA, {
    req(GSEA_List()) 
    new_name = GSEA_List()$Name
#    Saved_Gene_Sets[[new_name]] = isolate(GSEA_List)
    Saved_Gene_Sets[[new_name]] = GSEA_List()
    # Delete old entries if list is too long DOESN'T WORK
#    if (length(Saved_Gene_Sets) > 3) {
#      Saved_Gene_Sets = Saved_Gene_Sets[-1]
#    }
  })
  output$GSVA_Table = renderTable({
    req(Saved_GSVA_Values)
    Saved_GSVA_Values()
  }, bordered = TRUE, rownames =TRUE)
#  output$GSVA_Table = DT::renderDataTable({
#    req(Saved_GSVA_Values)
#    Saved_GSVA_Values()},
##    as.data.frame(t(Saved_GSVA_Values()))},
##    gsva_table = as.data.frame(t(Saved_GSVA_Values()))},
#    options=list(
#      orderClasses = TRUE,
#      lengthMenu = list(
#        c(-1, 10000, 1000, 100, 10), 
#        c('All','10000', '1000', '100', '10')),
#      style="font-size:25%")
#  )

  output$debugText = renderText({
    req(GSEA_List())
    names(Saved_Gene_Sets)
  })
  
  output$Choose_Gene_Set = renderUI({
    req(GSEA_List)
    my_names = names(Saved_Gene_Sets)
    selectInput(ns("Chosen_Gene_Set"), "Choose a Gene Set", choices = my_names)
  })
  
  output$Aggregate_Values = renderTable({
    req(input$Chosen_Gene_Set)
    my_list = Saved_Gene_Sets[[input$Chosen_Gene_Set]]
    my_names = names(my_list)
#    my_genes = my_list[[2]]
    my_table = my_list[[4]]
    my_table
  })
#### AGGREGATE VALUES TAB
  # FUNCTION FOR GETTING IDS
  getID = reactive({
    my_pos = which(choices_list[[input$genomicSpace]] == input$Sec_Var)
    tid = names(choices_list[[input$genomicSpace]])[my_pos]
    return(tid)
  })
  # 
  output$DebugPlot = renderPlot({
    dp = subGenes()
    plot(1:10, main = paste(rownames(dp)))
  })
  output$Sorting_DropDown = renderUI({
    if (input$Anno_B == "None") {
      selectInput(ns("Sorting_Method"), "Sort Samples By:", choices = c("Hierarchical Clustering", "Clinical Response", "Annotation A"))
    } else {
      selectInput(ns("Sorting_Method"), "Sort Samples By:", choices = c("Hierarchical Clustering", "Clinical Response", "Annotation A", "Annotation B"))
    }
  })

#### HEATMAP...
  # Process input data to either be filtered or not
  my_data = callModule(ApplyRemove, "INNER", filterdata(), fulldata)
  output$HeatMap = renderPlot({
    # Load data for Annotation and Expression
    heatmap_annot <- my_data()[which(my_data()$Sample %in% c(colnames(preX), colnames(onX))), ]
    Exp_Dat = PreOnGenes()
    if (input$Samples_Shown == "Pre-Treatment Only") {
      heatmap_annot <- my_data()[which(my_data()$Sample %in% colnames(preX)), ]
    }
    if (input$Samples_Shown == "On-Treatment Only") {
      heatmap_annot <- my_data()[which(my_data()$Sample %in% colnames(onX)), ]
    }
    rownames(heatmap_annot) <- heatmap_annot$Sample
    # Filter Expression data to match Annotation data
    Exp_Dat = Exp_Dat[,rownames(heatmap_annot)]
    # Define the Annotation data.frame used for plotting 
    this_ant_col = c(input$Anno_A, "myBOR", "SampleType")
    if (input$Anno_B != "None") {
      this_ant_col = c(input$Anno_B, input$Anno_A, "myBOR", "SampleType")
    }
    this_annot <- heatmap_annot[,match(this_ant_col, colnames(heatmap_annot))]
#    this_annot <- this_annot[order(this_annot[,3],this_annot[,2],this_annot[,1],decreasing=T),]
    # Define Heatmap 
    gene_order = c()
    # Scale Expression Matrix before Clustering
#    this_xdat = t(scale(t(this_xdat)))
    Exp_Dat = t(scale(t(Exp_Dat)))
    # Cluster selected genes
    rowclust <- hclust(dist(Exp_Dat))
    colclust <- hclust(dist(as.data.frame(t(Exp_Dat))))
#    if (input$Samples_Shown == "Pre and On") {
#      PreOnGenes = cbind(subGenes(),onGenes())
#      rowclust = hclust(dist(PreOnGenes))
#      colclust <- hclust(dist(as.data.frame(t(PreOnGenes))))
#    }
#    if (input$Samples_Shown == "On-Treatment Only") {
#      rowclust <- hclust(dist(onGenes()))
#      colclust <- hclust(dist(as.data.frame(t(onGenes()))))
#    }
    gene_order = rowclust$labels[rowclust$order]
    # Sort annotation and heatmap based on input$Sorting_Method
    sample_order = c()
    if (input$Sorting_Method == "Hierarchical Clustering") {
      sample_order = colclust$labels[colclust$order]
    }
    if (input$Sorting_Method == "Clinical Response") {
      sample_order = rownames(this_annot[order(this_annot$SampleType, this_annot$myBOR, decreasing=T),])
    }
    if (input$Sorting_Method == "Annotation A") {
      sample_order = rownames(this_annot[order(this_annot[,input$Anno_A], this_annot$SampleType, decreasing=T),])
    }
    if (input$Sorting_Method == "Annotation B") {
      sample_order = rownames(this_annot[order(this_annot[,input$Anno_B],this_annot$SampleType,decreasing=T),])
    }
    # Pre set must be in the same col order with master set, xdat
    # order genes by cluster order; order samples by annotation order 
#    gene_order = rowclust$labels[rowclust$order]
#    sample_order = match(rownames(this_annot), colnames(xdat))
    this_xdat <- xdat[gene_order, sample_order]
#    this_xdat = t(scale(t(this_xdat)))
    # Make sure sorting order is reversed annotation column order

#    # Pre cluster pre sample set genes
#    rowclust <- hclust(dist(subGenes()))
#    # Pre set must be in the same col order with master set, xdat
#    # order genes by cluster order; order samples by annotation order 
#    gene_order = rowclust$labels[rowclust$order]
#    sample_order = match(rownames(this_annot), colnames(xdat))
#    this_xdat <- xdat[gene_order, sample_order]
#    this_xdat = t(scale(t(this_xdat)))
    # HeatMap
#    j = names(this_annot)
    j = dim(my_data())
#    j = dim(onGenes())
    j = paste(j, collapse = ";")
#    make_heatmap(this_xdat, this_annot, j, ann_colors)
    make_heatmap(this_xdat, this_annot, 'Gene Expression', ann_colors)
  }, width = 1000, height = 700, res = 90)

  # Make Heatmap plot a UI element so it's size is known to Shiny...
  output$Sized_Plot = renderUI({
    plotOutput(ns("HeatMap"), width = 1000, height = 800)
  })
  # RETURN REACTIVE VALUE FOR USE BY OTHER MODULES
  return(Saved_Gene_Sets)
}

