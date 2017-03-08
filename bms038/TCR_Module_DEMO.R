# SCRIPT CONTAINS MODULE FOR PRODUCING 'Frequency Distribution' and 'Distribution of TCR Components' TABS OF THE TCR ANALYSIS WEBSITE
# To Test this module:
# 1. Open and R session
# 2. Load Shiny with command: library(shiny)
# 3. Run this script with: runApp("PATH/TO/THIS/SCRIPT.R/")
# NOTE: This scripts functionality depends upon having patient's folders available in the same directory from which the script is run.

# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STILL TO DO:
# 1. FIGURE OUT HOW TO ACCESS input$sample_name EXPLICITLY FOR THINGS LIKE NAMING DOWNLOADED FILES (probably has to do with reactivity)
# 2. MAKE PLOTS DOWNLOADABLE
# # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(shiny)
require(xlsx)
require(ggplot2)
library(ggplot2)
require(gridExtra)
require(scales)
library(DT)

# DEFINE LISTS PAIRING SAMPLE NAMES WITH FILENAMES
bms038_samples = list("Pt3_on" = "Pt3_on_S0089498",
                      "Pt3_pre" = "Pt3_pre_S0089487",
                      "Pt5_on" = "Pt5_on_S0089494",
                      "Pt5_pre" = "Pt5_pre_S0089505")

# NOTE THAT SOME OF THESE SAMPLES DON'T ACTUALLY HAVE THE NECESSARY DATA YET
cumc_gbm_samples = list("L04_PBMC_A" = "TCR22A.BC1",
                        "L04_PBMC_B" = "TCR23B.BC2",
                        "L04_TIL_A" = "TCR23A.BC3",
                        "L04_TIL_B" = "TCR22B.BC4",
                        "G13_PBMC_A" = "TCR111A.BC1",
                        "G13_PBMC_B" = "TCR106B.BC2",
                        "G13_TIL_A" = "TCR106A.BC1",
                        "G13_TIL_B" = "TCR105B.BC2")

# AGGREGATE LISTS INTO A LIST OF LISTS
study_samples_list = list("bms_038" = unlist(bms038_samples),
                          "cumc_gbm" = unlist(cumc_gbm_samples))

# MODULE: Genomics_Outcome()
# SUMMARY: Module creates 'Frequency Distribution' tab for TCR analysis.
TCR_Freq_DistUI = function(id) {
  ns = NS(id)
  tagList(
    sidebarPanel(
      # SELECT STUDY FROM WHICH TO SELECT A SAMPLE
      # NOTE THAT ALL INPUT/OUTPUT VARIABLES MUST BE CALLED USING ns()
      selectInput(ns("study"),"STUDY:",
                  c("Riaz - Melanoma" = "bms_038",
                    "Sims - Glioma" = "cumc_gbm")
      ),
      # SAMPLE CHOICES DEPENDENT UPON ns("study") SELECTION
      uiOutput(ns("Sample_Choice"))
    ),
    mainPanel(
      wellPanel(width = 9,style = "background-color: #ffffff;",
                tabsetPanel(type = "tabs",
                            tabPanel("Frequency Distribution",
                                     fluidRow(
                                       column(width = 3,align = "left",
                                              h4(textOutput(ns("display_sample_name"))),
                                              hr(),
                                              selectInput(ns("rep_type"), label=NULL,
                                                          list("VJcombos", 
                                                               "aaCDR3", 
                                                               "totCDR3",
                                                               "ntCDR3"))
                                       ),
                                       column(width = 3,
                                              # WIDGETS FOR CONTROLLING DISTRIBUTION PLOT
                                              selectInput(ns("plot_type"),label=NULL,
                                                          list("Distribution", 
                                                               "Histogram")),
                                              radioButtons(ns("Ylog"), label=NULL,
                                                           c("Log" = "log",
                                                             "Linear" = "lin"),width='150px',inline=TRUE),
                                              downloadButton(ns("downloadPlot"), "Download Plot",plotOutput("distplot"))
                                       ),
                                       column(width = 3,
                                              
                                              # THIS PART GETS EVALUATED BY JAVASCRIPT SO THE SYNTAX NEEDS TO BE DIFFERENT
                                              # conditionalPanel(sprintf("input['%s'] != 'Histogram'", ns("plot_type")),
                                              conditionalPanel(paste0("input['", ns("plot_type"), "'] == 'Histogram'"),                                              sliderInput(ns("n"),"Number of Bins", min = 10, max = 30, value = 20)
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(width = 9,
                                              # PRODUCE WHICHEVER DISTRIBUTION PLOT WAS CHOSEN ABOVE
                                              plotOutput(ns("plot1")))
                                     ),
                                     hr(),
                                     fluidRow(
                                       # PRINT DESCRIPTIVE STATISTICS FROM THE SUMMARY FILE
                                       column(width = 2,
                                              "Reads:",textOutput(ns("TotalReads"))),
                                       column(width = 2,
                                              "Unique:",textOutput(ns("num"))),
                                       column(width = 1,
                                              "N50:",textOutput(ns("N50"))),
                                       column(width = 1,
                                              "N90:",textOutput(ns("N90"))),
                                       column(width = 2,
                                              "Entropy:",textOutput(ns("ent"))),
                                       column(width = 2,
                                              "Clonality:",textOutput(ns("clon")))
                                     ),
                            # WIDGET FOR PRESENINT A TUTORIAL
                            selectInput(ns("tutorial"),label=NULL,
                                        list("Hide Tutorial" = "hide",
                                             "Show Tutorial" = "show")),
                            conditionalPanel(paste0("input['", ns("tutorial"), "'] == 'show'"),
                                             #      conditionalPanel(ns("input.tutorial == 'show'"),
                                             wellPanel(width = 9,style = "background-color: #ffffff;",style = "overflow-x:scroll; max-height: 300px",
                                                       imageOutput(ns("tutorial_img"),width = "800px",height = "200px"),
                                                       textOutput(ns("tutorial_text"))
                                             )
                            ),
                            # DOWNLOAD THE TABLE
                            downloadButton(ns("downloadTable"), "Download Table"),
                            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                      DT::dataTableOutput(ns("table")))
                            
                            
                                     ),
######################   #   #   #   #   #   ##########################   #   #   #   #   #   #########################
######################   #   #   #   #   #   ##########################   #   #   #   #   #   #########################
######################   #   #   #   #   #   ##########################   #   #   #   #   #   #########################
                            tabPanel("Distribution of TCR Components",
                                     wellPanel(width = 9,style = "background-color: #ffffff;",
                                               fluidRow(
                                                 column(width = 3,align = "left",
                                                        h4("Sample Name"),
                                                        selectInput(ns("component_type"), label=NULL,
                                                                    list("AAperVJ", 
                                                                         "VJperAA"))
                                                 ),
                                                 column(width = 3,align = "center",
                                                        selectInput(ns("Hdist_type"),label=NULL,
                                                                    list("DotPlot",
                                                                         "Density"))
                                                 )
                                               ),
                                               #                    h3(textOutput("component_type"))),
                                               fluidRow(
                                                 column(width = 9,align="center",
                                                        #plotOutput(ns("what")))
                                                        plotOutput(ns("component_Hdist"),width="500px",height="300px"),
                                                        "Median number: ",textOutput(ns("med_T2perT1"),inline=TRUE))
                                               ),
                                               hr(),
                                               fluidRow(
                                                 column(width = 6,
                                                        #plotOutput(ns("component_Hhist"))),
                                                        plotOutput(ns("component_Hhist"),width="350px",height="300px")),
                                                 column(width = 3,
                                                        sliderInput(ns("n2"),"Number of Bins",
                                                                    min = 10, max = 30, value = 20),
                                                        "Mean entropy: ",textOutput(ns("mean_HT2perT1"),inline=TRUE)
#                                                        downloadButton(ns("downloadPlot2"), "Download Plot",plotOutput("component_Hhist"))
                                                 ))),
                                     downloadButton(ns("downloadTable2"), "Download Table"),
                                     wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                               DT::dataTableOutput(ns("table2")))
                                     
                            )
                            
                        )
                    )
    )
    
  )
}


# SERVER COMPONENT OF THE MODULE
TCR_Freq_Dist = function(input, output, session) {
  ns = session$ns
  output$what = renderPlot({
    plot(1:10, main = paste(input$sample_name))
  })
  # CREATE UI FOR CHOOSING SAMPLES BASED ON "study" SELECTION
  # Note that the choices are stored in a list-object called "study_samples_list" with "input$study" as the indexing value
  output$Sample_Choice = renderUI({
    radioButtons(ns("sample_name"), "SAMPLE ID:", choices = unname(study_samples_list[[input$study]]))
  })
  # DISPLAY SAMPLE NAME ON PAGE
  output$display_sample_name <- renderText({
    input$sample_name
  })
  #### DEBUGGING EXPERIMENT TO TRY AND RESOLVE THE 'input$sample_name == NULL' ISSUE
  ####  my_file_path = reactive({
  ####    paste(input$sample_name,"/",input$sample_name,"_H_summary.tsv",sep="")
  ####  })
  summary_data <- reactive({
    read.csv(paste("../TCR/", input$sample_name,"/",input$sample_name,"_H_summary.tsv",sep=""),sep="\t",as.is = 1,header = TRUE)
    ####    read.csv(my_file_path(),sep="\t",as.is = 1,header = TRUE)
  })
  # CREATE TUTORIAL IMAGE AND TEXT
  output$tutorial_img <- renderImage({
    if (input$rep_type == "VJcombos"){
      return(list(
        src = "TCR_VJ.png",
        contentType = "image/png",
        width = 800,
        height = 200,
        alt = "Number of VJ combinations"
      )) }
    else if (input$rep_type == "aaCDR3") {
      return(list(
        src = "TCR_aaCDR3.png",
        contentType = "image/png",
        width = 800,
        height = 200,
        alt = "Number of aaCDR3"
      )) }
    else if (input$rep_type == "totCDR3") {
      return(list(
        src = "TCR_totCDR3.png",
        contentType = "image/png",
        width = 800,
        height = 200,
        alt = "Number of clonotypes"
      )) }  
    else if (input$rep_type == "ntCDR3") {
      return(list(
        src = "TCR_ntCDR3.png",
        contentType = "image/png",
        width = 800,
        height = 200,
        alt = "Number of unique nucleotide sequences"
      )) }  
  },deleteFile = FALSE)
  
  output$tutorial_text <- reactive({
    if (input$rep_type == "VJcombos") {
      paste("Number of V-J cassette combinations (VJ). The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of combinations of V and J cassettes detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "aaCDR3") {
      paste("Number of amino acid CDR3 motifs (aaCDR3).  The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of unique aaCDR3 motifs detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "totCDR3") {
      paste("Number of clonotypes (totCDR3).  The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of unique V-J combination/aaCDR3s detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "ntCDR3") {
      paste("Number of nucleotide CDR3 sequences (ntCDR3).  The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of unique nucleotide CDR3 sequences detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "H_vj") {
      paste("Number of V-J cassette combinations (VJ). The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of combinations of V and J cassettes detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "H_aa") {
      paste("Number of V-J cassette combinations (VJ). The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of combinations of V and J cassettes detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "H_tot") {
      paste("Number of V-J cassette combinations (VJ). The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of combinations of V and J cassettes detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
    else if (input$rep_type == "H_nt") {
      paste("Number of V-J cassette combinations (VJ). The complementarity determining region 3 (CDR3) of the T cell receptor is encoded by a nucleotide sequence incorporating bases from a V cassette, a J cassette, (in the case of TCRb) an intervening D cassette, and randomly added and subtracted nucleotides at the junctions between these cassettes.  These nucleotides encode amino acid residues (yellow), which comprise the antigen binding domain, the CDR3, of the final TCRa or TCRb protein (left).  The total number of combinations of V and J cassettes detected (right, x-axis) and their distribution of abundance in the repertoire (right, y-axis) were quantified by TCRseq of the sample.")
    }
    
  })
  # COMPILE SUMMARY DATA
  output$TotalReads <- renderText({
    summaryStats <- summary_data()
    paste(summaryStats[c(2)])
  })
  
  output$num <- renderText({
    summaryStats <- summary_data()
    if (input$rep_type == "VJcombos"){
      paste(summaryStats[c(4)])
    }
    else if (input$rep_type == "aaCDR3"){
      paste(summaryStats[c(5)])
    }  
    else if (input$rep_type == "totCDR3"){
      paste(summaryStats[c(6)])
    }
    else if (input$rep_type == "ntCDR3"){
      paste(summaryStats[c(7)])
    }
  })
  
  output$N50 <- renderText({
    summaryStats <- summary_data()
    if (input$rep_type == "VJcombos"){
      paste(summaryStats[c(10)])
    }
    else if (input$rep_type == "aaCDR3"){
      paste(summaryStats[c(11)])
    }  
    else if (input$rep_type == "totCDR3"){
      paste(summaryStats[c(12)])
    }
    else if (input$rep_type == "ntCDR3"){
      paste(summaryStats[c(13)])
    }
  })
  
  output$N90 <- renderText({
    summaryStats <- summary_data()
    if (input$rep_type == "VJcombos"){
      paste(summaryStats[c(14)])
    }
    else if (input$rep_type == "aaCDR3"){
      paste(summaryStats[c(15)])
    }  
    else if (input$rep_type == "totCDR3"){
      paste(summaryStats[c(16)])
    }
    else if (input$rep_type == "ntCDR3"){
      paste(summaryStats[c(17)])
    }
  })
  
  output$ent <- renderText({
    summaryStats <- summary_data()
    if (input$rep_type == "VJcombos"){
      paste(summaryStats[c(18)])
    }
    else if (input$rep_type == "aaCDR3"){
      paste(summaryStats[c(19)])
    }  
    else if (input$rep_type == "totCDR3"){
      paste(summaryStats[c(20)])
    }
    else if (input$rep_type == "ntCDR3"){
      paste(summaryStats[c(21)])
    }
  })
  
  output$clon <- renderText({
    summaryStats <- summary_data()
    if (input$rep_type == "VJcombos"){
      paste(1-(summaryStats[c(18)]/summaryStats[c(25)]))
    }
    else if (input$rep_type == "aaCDR3"){
      paste(1-(summaryStats[c(19)]/summaryStats[c(26)]))
    }  
    else if (input$rep_type == "totCDR3"){
      paste(1-(summaryStats[c(20)]/summaryStats[c(27)]))
    }
    else if (input$rep_type == "ntCDR3"){
      paste(1-(summaryStats[c(21)]/summaryStats[c(28)]))
    }
  })
  
  output$med_T2perT1 <- renderText({
    summaryStats <- summary_data()
    if (input$component_type == "AAperVJ"){
      paste(summaryStats[c(8)])}
    else if(input$component_type == "VJperAA"){
      paste(summaryStats[c(9)])}
  })
  
  output$mean_HT2perT1 <- renderText({
    summaryStats <- summary_data()
    if (input$component_type == "AAperVJ"){
      paste(summaryStats[c(23)])}
    else if(input$component_type == "VJperAA"){
      paste(summaryStats[c(24)])}
  })
  
  file_name <- reactive({
    req(input$sample_name)
    if (input$rep_type == "VJcombos"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_VJ.txt",sep="")
    }
    else if (input$rep_type == "aaCDR3"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_aaCDR3.txt",sep="")
    }  
    else if (input$rep_type == "totCDR3"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_totCDR3.txt",sep="")
    }
    else if (input$rep_type == "ntCDR3"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_ntCDR3.txt",sep="")
    }
  })
  rep_data <- reactive({
    read.csv(file_name(),sep="\t",as.is=1,col.names = c(input$rep_type,"Reads","Frequency"))
  })

  # MAKE THE DATA TABLE
  output$table <- DT::renderDataTable({
    rep_data()},options=list(
      orderClasses=TRUE,
      lengthMenu = list(c(-1, 10000, 1000, 100, 10), c('All','10000', '1000', '100', '10')),
      style="font-size:50%")
  )

  # DOWNLOAD TABLE 
  output$downloadTable <- downloadHandler(
    # (doesn't work) CREATE SENSIBLE FILENAME (doesn't work)
    #filename = paste(input$sample_name, "_", input$rep_type, ".xlsx", sep = ""),
    filename = "RenameThisTable.xlsx",
    content = function(file) {
      write.xlsx(rep_data(), file)
    }
  )
  # PLOT - FREQ DIST (for the TCR repertoires)
  distplot <- reactive({
    df <- rep_data()
    dat2 <- data.frame(tcrs = rownames(df),df,row.names = NULL)
    dat2$tcrs <- factor(dat2$tcrs, levels = dat2$tcrs[rev(order(dat2$Frequency))])
    if (input$Ylog == "log"){
      dat2$Frequency <- log10(dat2$Frequency)
    }
    else if(input$Ylog == "lin"){
      dat2$Frequency <- dat2$Frequency
    }
    ggplot(dat2,aes(x = tcrs,y = Frequency)) + geom_point() + scale_x_discrete(input$rep_type) + scale_y_continuous("log10 Frequency") + theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  })

  # PLOT - FREQ HIST
  histplot <- reactive({
    df <- rep_data()
    dat3 <- data.frame(tcrs = rownames(df),df,row.names = NULL)
    if (input$Ylog == "log"){
      dat3$Frequency <- log10(dat3$Frequency)
    }
    else if(input$Ylog == "lin"){
      dat3$Frequency <- dat3$Frequency
    }
    bins <- seq(min(dat3$Frequency),max(dat3$Frequency),length.out = input$n + 1)
    hist(dat3$Frequency, breaks = bins, col = 'darkgreen', border = 'white',main = paste("Histogram of ",input$rep_type," frequencies",sep=""),xlab = paste("Frequency of ",input$rep_type," in repertoire (log10)",sep = ""),ylab = paste("Count (number of ",input$rep_type," in population with frequency)",sep = ""))
  })
  
  output$plot1 <- renderPlot({
    #  reactive({
    if (input$plot_type == "Distribution"){
      distplot()}
    else if (input$plot_type == "Histogram"){
      histplot()}
    #  })  # closes reactive
  })    # closes renderPlot

  # (doesn't work) DOWNLOAD WHATEVER plot1 IS (doesn't work)
  output$downloadPlot <- downloadHandler(
    filename = "RenameThisPlot.png",
    content = function(file) {
      ggsave(file, plot1())
    }
  )
  
  
  output$test3 <- renderText({      # this is just here to see whether the headers for rep_data() came out OK
    heads <- colnames(rep_data())
    #  paste(heads[c(1)])
    paste(heads)
    #  paste(rep_data())
  })

  #  return(list(reactive({input$sample_name}), summary_data, my_file_path))
  #  return(list(reactive({input$sample_name}), summary_data))
  ################################### DISTRIBUTION TAB #############################
  output$component_type <- renderText(input$component_type)

  file2_name <-reactive({
    if (input$component_type == "AAperVJ"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_AAperVJ.tsv",sep="")
    }
    else if (input$component_type == "VJperAA"){
      paste("../TCR/", input$sample_name,"/",input$sample_name,"_VJperAA.tsv",sep="")
    }
  })

  rep2_data <- reactive({
    t2 <- substr(input$component_type,1,2)
    t1 <- substr(input$component_type,6,7)
    read.csv(file2_name(),sep="\t",as.is=1,col.names = c(paste(t1),paste("Num",t2,sep="_"),paste("H",t2,sep="_"),paste("Hnorm",t2,sep="_"),paste(t2,"IDs",sep="_"),paste(t2,"Reads",sep="_")))
  })

  # PLOT - HISTOGRAM OF Thing2 NUMBER per Thing1
  H_hist <- reactive({
    t2 <- substr(input$component_type,1,2)
    t1 <- substr(input$component_type,6,7)
    df <- rep2_data()
    ent_dat <- c(as.numeric(df[,3]))
    bins <- seq(min(ent_dat),max(ent_dat),length.out = input$n2 + 1)
    hist(ent_dat, breaks = bins, col = 'darkcyan', border = 'white',main = paste("Histogram of ",t2," entropy per ",t1,sep=""),xlab = paste("H(",t2,") per ",t1,sep = ""),ylab = paste("Count (number of ",t1," with H(",t2,"))",sep = ""))
  })
  output$component_Hhist <- renderPlot(H_hist(),width=350,height=300)

  # DOWNLOAD H HISTOGRAM
  output$downloadPlot2 <- downloadHandler(
    filename = "RenameThisPlot.png",
    content = function(file) {
      ggsave(file, component_Hhist())
    }
  )
  
  H_dist <- reactive({
    t2 <- substr(input$component_type,1,2)
    t1 <- substr(input$component_type,6,7)
    df <- rep2_data()
    num_dat <- c(as.numeric(df[,2]))
    entnorm_dat <- c(as.numeric(df[,4]))
    if (input$Hdist_type == "DotPlot"){
      ggplot(cbind.data.frame(num_dat,entnorm_dat),aes(x = num_dat,y = entnorm_dat)) + geom_point() + scale_x_continuous(paste("Number of ",t2," per ",t1,sep=""))+ scale_y_continuous(paste("Evenness of ",t2," per ",t1,sep="")) + ggtitle(paste("Number and Evenness of ",t2," per ",t1,sep="")) + theme(panel.border = element_rect(color = "black", fill = NA, size = 1), plot.title = element_text(face="bold",size = 16,hjust = 0.5))
    }
    else if (input$Hdist_type == "Density"){
      ggplot(cbind.data.frame(num_dat,entnorm_dat),aes(x = num_dat,y = entnorm_dat)) + geom_density_2d() + stat_density_2d(aes(fill = ..level..),geom="polygon") + scale_fill_gradient(low="blue", high="yellow") + scale_x_continuous(paste("Number of ",t2," per ",t1,sep=""))+ scale_y_continuous(paste("Evenness of ",t2," per ",t1,sep="")) + ggtitle(paste("Number and Evenness of ",t2," per ",t1,sep="")) + theme(panel.border = element_rect(color = "black", fill = NA, size = 1), plot.title = element_text(face="bold",size = 16,hjust = 0.5))
    }    
  })
  output$component_Hdist <- renderPlot(H_dist(),width=500,height=300)
  
  # (doesn't work) DOWNLOAD H DISTRIBUTION (doesn't work)
  output$downloadPlot3 <- downloadHandler(
    filename = "RenameThisPlot.png",
    content = function(file) {
      ggsave(file, component_Hdist())
    }
  )
  
  # MAKE THE T2 PER T1 DATA TABLE
  output$table2 <- DT::renderDataTable({
    rep2_data()},options=list(
      orderClasses=TRUE,
      lengthMenu = list(c(-1, 10000, 1000, 100, 10), c('All','10000', '1000', '100', '10'))
    )
  )
  
  # DOWNLOAD T2 PER T1 DATA TABLE
  output$downloadTable2 <- downloadHandler(
    filename = "RenameThisTable.xlsx",
    content = function(file) {
      write.xlsx(rep2_data(), file)
    }
  )
  return(list(reactive({input$sample_name}), summary_data))
  
}

# DEFINE UI AND SERVER FUNCTIONS AS NOTHING MORE THAN A PAIR OF FUNCTION CALLS WITH MATCHING NAMESPACE
ui = fluidPage(
  TCR_Freq_DistUI("Test")
)

server = function(input, output) {
  callModule(TCR_Freq_Dist, "Test")
}

shinyApp(ui, server)
