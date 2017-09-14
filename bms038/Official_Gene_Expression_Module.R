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
# 

# READ IN PATHWAYS ARRAY
source("Parsed_GSEA_Sets.R")
#source("Alexis_Genes_Parsed.R")
#source("Hallmark_Genes.R")
#source("hall_fpkm_parsed.R")

# IMPORT EXPRESSION DATA
xdat = readRDS("log2.fpkm.with.names.rds") # Log Transformed for plotting
fpkm_dat = readRDS("fpkm.BMS_038.dups_removed.rds")
gene_lookup = readRDS("Symbol_Entrez_Table.rds")
fpkm_MAT = as.matrix(fpkm_dat)

# SEPARATE INTO PRE/ON
ons = grep("_on", names(xdat))
preX = xdat[,-ons]

pres = grep("_pre", names(xdat))
onX = xdat[,-pres]

# VECTOR FOR ALPHABETICAL SELECTION
letters_vector = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "Numeric")



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



###########################################################################
# END FUNCTIONS
###########################################################################


GeneExprUI = function(id, choices_list) {
  ns = NS(id)
  tagList(
    sidebarPanel(width = 3,
      tabsetPanel(
        tabPanel("Select by Genes Pathway",
#                 selectInput(ns("Pathways"), "Pathways", choices = c(SUPP_NAMES)),
                 selectInput(ns("Pathways"), "Pathways", choices = c(Type_Names)),
                 actionButton(ns("AddGenes"), label = "Add These Genes"),
                 actionButton(ns("DitchGenes"), label = "Remove All Checked Genes"),
                 uiOutput(ns("Pathway_Boxes"))
                 ),
        tabPanel("Select Genes by Typing",
                 textInput(ns("GeneText"), label = "Type Gene Names", width = '100%'),
                 textOutput(ns("BadFeedback"))
                 ),
        tabPanel("Select Genes Alphabetically",
                 selectInput(ns("Letter"), "Alphabetical", choices = letters_vector),
                 actionButton(ns("AddGenes_AZ"), label = "Add These Genes"),
                 actionButton(ns("DitchGenes_AZ"), label = "Remove All Checked Genes"),
                 uiOutput(ns("AZ_Boxes"))
                 )
      ),
      p(), # NEED MORE SEPARATION HERE
      
      "Run GSVA if you want to save gene sets...",
      textInput(ns("GeneSetName"), "Save Gene Set As:", "My_Gene_Set"),
      actionButton(ns("RunGSVA"), "Run GSVA Analysis"),
      uiOutput(ns("SaveGeneSet_Button"))
      
    ),
    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Gene Selection",
          textOutput(ns("debugText")),
          wellPanel(DT::dataTableOutput(ns("Selection_Table")), style = "overflow-x:scroll; max-width: 1000px; overflow-y:scroll; max-height: 600px")
#          actionButton(ns("SaveGeneSet"), "Save Gene Set")        
          ),
        tabPanel("Aggregate Expression Value Plot",
          wellPanel(DT::dataTableOutput(ns("GSVA_Table")), style = "overflow-x:scroll; max-width: 1000px; overflow-y:scroll; max-height: 600px")
#          uiOutput(ns("Choose_Gene_Set")),
#          tableOutput(ns("Aggregate_Values"))
        ),
        tabPanel("Heatmap",
                 h3("Choose Heatmap Annotations"),
                 h2("Patients will be sorted by annotations"),
                 selectInput(ns("First_Anno"), "First Annotation", choices = list("Response" = "myBOR",
       "Mutation Load (log)" = "log10mut",
       "UV Signature" = "Signature.7",
       "Subtype" = "SubtypeEZ",
       "Clonal Mutation Load" = "thresh95.muts",
       "Cytolytic Score" = "cytscore")),
                 selectInput(ns("Second_Anno"), "Second Annotation", choices = list("Subtype" = "SubtypeEZ",
         "Mutation Load (log)" = "log10mut",
         "Response" = "myBOR",
         "UV Signature" = "Signature.7",
         "Clonal Mutation Load" = "thresh95.muts",
         "Cytolytic Score" = "cytscore")),
                 selectInput(ns("Third_Anno"), "Third Annotation", choices = list("UV Signature" = "Signature.7",
       "Mutation Load (log)" = "log10mut",
       "Response" = "myBOR",
       "Subtype" = "SubtypeEZ",
       "Clonal Mutation Load" = "thresh95.muts",
       "Cytolytic Score" = "cytscore")),
          plotOutput(ns("HeatMap"))
          
        )
      )
    )
  )
}

GeneExpr = function(input, output, session, choices_list, my_data) {
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
  #### PROCESS INPUT BASED ON ALPHABETICAL SELECTION
  # Reactive Object listing all genes for a given letter
  AZ_Gene_Vector = reactive({
    my_cols = grep("^\\d", rownames(preX))
    if (input$Letter != "Numeric") {
      my_pat = paste("^", input$Letter, sep = "")
      my_cols = grep(my_pat, rownames(preX))
    }
    my_vect = sort(rownames(preX)[my_cols])
    return(my_vect)
  })
  # Dynamic UI for selecting genes in a particular pathway
  output$AZ_Boxes = renderUI({
    checkboxGroupInput(ns("Chosen_AZ_Genes"), "Genes", choices = AZ_Gene_Vector())
  })
  # Create reactive value for genes selected from alphabetical checkboxes
  myAZGenes = reactiveValues(
    Check = c()
  )
  observeEvent(input$AddGenes_AZ, {
    myAZGenes$Check = c(myAZGenes$Check, input$Chosen_AZ_Genes)
  })
  observeEvent(input$DitchGenes_AZ, {
    myAZGenes$Check = c()
  })
#### PROCESS INPUT BASED ON PATHWAYS
  # Dynamic UI for selecting genes in a particular pathway
  output$Pathway_Boxes = renderUI({
#    checkboxGroupInput(ns("ChosenPathwayGenes"), "Pathways", choices = sort(unname(SUPP_LIST[[input$Pathways]])), selected = unname(SUPP_LIST[[input$Pathways]]))
    checkboxGroupInput(ns("ChosenPathwayGenes"), "Pathways", choices = sort(unname(Type_List[[input$Pathways]])), selected = unname(Type_List[[input$Pathways]]))
  })
  # Create reactive value for genes selected from pathway checkboxes
  myGenes = reactiveValues(
    Check = c()
  )
  observeEvent(input$AddGenes, {
    myGenes$Check = c(myGenes$Check, input$ChosenPathwayGenes)
  })
  observeEvent(input$DitchGenes, {
    myGenes$Check = c()
  })
  
#### DEFINE REACTIVE DATAFRAME OF ALL SELECTED GENES 
  subGenes = reactive({
    text_genes = text_input()[[2]]
    path_genes = myGenes$Check
    az_genes = myAZGenes$Check
    all_genes = c(text_genes, path_genes, az_genes)
    out_df = preX[rownames(preX) %in% all_genes,]
    return(out_df)
  })
  onGenes = reactive({
    text_genes = text_input()[[2]]
    path_genes = myGenes$Check
    az_genes = myAZGenes$Check
    all_genes = c(text_genes, path_genes, az_genes)
    out_df = onX[rownames(onX) %in% all_genes,]
    return(out_df)
  })
  # DEFINE AGGREGATE VALUES FOR SELECTED GENES
  AggGenes = reactive({
    num_dat = subGenes()[,grep("Pt",names(subGenes()))]
    out_dat = subGenes()
    out_dat$Mean = apply(num_dat, 1, mean)
    out_dat$Median = apply(num_dat, 1, median)
    out_dat$Standard_Deviation = apply(num_dat, 1, sd)
    # Round down to 4 digits 
    out_dat$Mean = round(out_dat$Mean, digits = 4)
    out_dat$Median = round(out_dat$Median, digits = 4)
    out_dat$Standard_Deviation = round(out_dat$Standard_Deviation, digits = 4)
    out_dat = out_dat[,c("Mean", "Median", "Standard_Deviation")]
    # Add annotations indicating how each gene was selected
    if (nrow(out_dat) > 0) {
      out_dat$Selection_Method = ""
      if (length(text_input()[[2]]) > 0) {
        out_dat[text_input()[[2]],]$Selection_Method = "Typed"
      }
      if (length(myGenes$Check) > 0) {
        out_dat[myGenes$Check,]$Selection_Method = "Pathway"
      }
      if (length(myAZGenes$Check) > 0) {
        out_dat[myAZGenes$Check,]$Selection_Method = "Alphabetical"
      }
    }
    return(out_dat)
  })
#### DISPLAY TABLE OF SELECTED GENES
  output$Selection_Table = DT::renderDataTable({
    AggGenes()},
    options=list(
      orderClasses = TRUE,
      lengthMenu = list(
        c(-1, 10000, 1000, 100, 10), 
        c('All','10000', '1000', '100', '10')),
      style="font-size:25%")
  )
  
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
  Saved_Gene_Sets = reactiveValues()
  Saved_GSVA_Values = reactive({
    # Start with an empty data.frame
    out_data = data.frame()
    # Condense GSVA values from Saved_Gene_Sets when "SaveGeneSet" button is clicked
    if (input$SaveGeneSet) {
      out_data = do.call("rbind", lapply(reactiveValuesToList(Saved_Gene_Sets), function(x) as.data.frame(t(x[[4]]))))
    }
    return(out_data)
  })
  observeEvent(input$SaveGeneSet, {
    req(GSEA_List()) 
    new_name = GSEA_List()$Name
#    Saved_Gene_Sets[[new_name]] = isolate(GSEA_List)
    Saved_Gene_Sets[[new_name]] = GSEA_List()
    # Delete old entries if list is too long DOESN'T WORK
#    if (length(Saved_Gene_Sets) > 3) {
#      Saved_Gene_Sets = Saved_Gene_Sets[-1]
#    }
  })
  output$GSVA_Table = DT::renderDataTable({
    as.data.frame(t(Saved_GSVA_Values()))},
#    gsva_table = as.data.frame(t(Saved_GSVA_Values()))},
    options=list(
      orderClasses = TRUE,
      lengthMenu = list(
        c(-1, 10000, 1000, 100, 10), 
        c('All','10000', '1000', '100', '10')),
      style="font-size:25%")
  )

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
  # CALCULATE AGGREGATE VALUE FOR BOX

#  agg_df = reactive({
#    x = make_Agg_Exp(subGenes(), input$AggMethod)
#    return(x)
#  })

  # PRODUCE BOXPLOT USING AGGREGATE VALUE
#  output$AggBoxPlot = renderPlot({
#    make_Agg_boxplot(agg_df(), my_data())
#  })

#  output$AggPlot = renderPlot({
#    temp_dat = agg_df()
#    temp_dat = merge(temp_dat, my_data(), by = "Sample", all.x = TRUE, all.y = FALSE)
#    plot.data(temp_dat, x = getID(), y = "AggFPKM", plot.type = "scatter plot")
#  })

  
  output$DebugPlot = renderPlot({
    dp = subGenes()
    plot(1:10, main = paste(rownames(dp)))
  })

#### HEATMAP...
  output$HeatMap = renderPlot({
    ###########################################################################
    # HEAT MAP STUFF
    ###########################################################################
    # Annotation...
    heatmap_annot <- my_data()[which(my_data()$Sample %in% c(colnames(preX), colnames(onX))), ]
    rownames(heatmap_annot) <- heatmap_annot$Sample
    
    # DEFINE HEATMAP COLOR PALETTES
    ann_colors = list(
      #  Response = c("PRCR" = "green3", "SD" = "blue", "PD" = "red")
      myBOR = c("PRCR" = "green3", "SD" = "blue", "PD" = "red"),
      SampleType = c("on" = "yellow", "pre" = "black"),
      SubtypeEZ = c("BRAF" = "royalblue", "RAS" = "gold", "NF1" = "black", "TripleWt" = "green"),
      cytscore = colorRampPalette(brewer.pal(9, "Reds"))(100),
      log10mut = colorRampPalette(brewer.pal(9, "Blues"))(100),
      thresh.95muts = colorRampPalette(brewer.pal(9, "Greens"))(100)
      
    )
    
    ###########################################################################
    # END OF HEATMAP STUFF
    ###########################################################################
    
    # Annotation data sorting
    # Make sure sorting order is reversed annotation column order
    this_ant_col <- c(input$Third_Anno, input$Second_Anno, input$First_Anno, 'SampleType')
    this_annot <- heatmap_annot[,match(this_ant_col, colnames(heatmap_annot))]
    this_annot <- this_annot[order(this_annot[,4],this_annot[,3],this_annot[,2],this_annot[,1],decreasing=T),]

    # Pre cluster pre sample set genes
    rowclust <- hclust(dist(subGenes()))

    # Pre set must be in the same col order with master set, xdat
    # order genes by cluster order; order samples by annotation order 
    gene_order = rowclust$labels[rowclust$order]
    sample_order = match(rownames(this_annot), colnames(xdat))
    this_xdat <- xdat[gene_order, sample_order]
#    this_xdat <- xdat[rowclust$labels[order(rowclust$order)], match(rownames(this_annot), colnames(xdat))]
    
    this_xdat = t(scale(t(this_xdat)))
    
    # HeatMap

    make_heatmap(this_xdat, this_annot, 'Pre-On Treatment Sample Gene Expression', ann_colors)
  }, width = 1000, height = 1000, res = 90)  }

