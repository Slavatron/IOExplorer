library(shiny)
require(xlsx)
require(ggplot2)
library(ggplot2)
require(gridExtra)
require(scales)
library(DT)

# Define server logic 
function(input, output) {

# DEFINE HEADER BITS ############## DO NOT ALTER ####
output$header_image <- renderImage({
    list(
      src = "logo_only.png",
      contentType = "image/png",
      width = 100,
      height = 100
    )
  },deleteFile=FALSE)  ###### REALLY DON'T ALTER THIS, I CANNOT BELIEVE deleteFile=TRUE IS THE DEFAULT I HATE R #####



###### CALLING THE SUMMARY SHEET GIVES YOU QUICK ACCESS TO LOTS OF VARIABLES YOU NEED ###

output$sample_name <- renderText({input$sample_name})

summary_data <- reactive({
  read.csv(paste(input$sample_name,"/",input$sample_name,"_H_summary.tsv",sep=""),sep="\t",as.is = 1,header = TRUE)
})

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

#H_delta(21) Hmax_vj(24)	Hmax_aa	Hmax_tot	Hmax_nt	FiltUnprodAA	FiltNotransAA	FiltUnresolvedVJ	FiltAmbigNT
  
###### FOR THE FREQUENCY DISTRIBUTION TAB #################
# DEFINING INPUT FILE AND DATA TABLE FOR FREQ DIST

file_name <- reactive({
  req(input$sample_name)
  if (input$rep_type == "VJcombos"){
    paste(input$sample_name,"/",input$sample_name,"_VJ.txt",sep="")
  }
  else if (input$rep_type == "aaCDR3"){
    paste(input$sample_name,"/",input$sample_name,"_aaCDR3.txt",sep="")
  }  
  else if (input$rep_type == "totCDR3"){
    paste(input$sample_name,"/",input$sample_name,"_totCDR3.txt",sep="")
  }
  else if (input$rep_type == "ntCDR3"){
    paste(input$sample_name,"/",input$sample_name,"_ntCDR3.txt",sep="")
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
  filename = "RenameThisTable.xlsx",
    content = function(file) {
    write.xlsx(rep_data(), file)
  }
)

output$test3 <- renderText({      # this is just here to see whether the headers for rep_data() came out OK
  heads <- colnames(rep_data())
#  paste(heads[c(1)])
  paste(heads)
#  paste(rep_data())
})

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

# Here we define what type plot1 is by what rep_type we read in. The plots themselves are defined reactively below.
output$plot1 <- renderPlot({
  #  reactive({
  if (input$plot_type == "Distribution"){
    distplot()}
  else if (input$plot_type == "Histogram"){
    histplot()}
#  })  # closes reactive
})    # closes renderPlot

# DOWNLOAD WHATEVER plot1 IS
output$downloadPlot <- downloadHandler(
  filename = "RenameThisPlot.png",
  content = function(file) {
    ggsave(file, plot1())
  }
)

################################# FOR THE TCR COMPONENTS TAB ###########################

# THIS IS WHERE YOU GET THE TITLE FOR THE PANEL FROM
output$component_type <- renderText(input$component_type)

file2_name <-reactive({
  if (input$component_type == "AAperVJ"){
    paste(input$sample_name,"/",input$sample_name,"_AAperVJ.tsv",sep="")
  }
  else if (input$component_type == "VJperAA"){
    paste(input$sample_name,"/",input$sample_name,"_VJperAA.tsv",sep="")
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

# DOWNLOAD H DISTRIBUTION
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

############# FOR THE COMPONENT USAGE TAB #######################

vjtab_data <- reactive({
  read.csv(paste(input$sample_name,"/",input$sample_name,"_VJtable.tsv",sep=""),sep="\t",as.is = 1,header = TRUE)
})


# MAKE THE VJ DATA TABLE
output$vj_table <- DT::renderDataTable({
  if(input$cassette_col == "jcass"){
    data.frame(t(vjtab_data()),row.names = NULL)}
  else if(input$cassette_col == "vcass"){
    data.frame(vjtab_data(),row.names = NULL)}
  },
  options=list(
    orderClasses=TRUE,
    lengthMenu = list(c(-1,5), c('All',"Top 5"))
  )
)

#output$vj_table <- renderTable({vjtab_data()})

# DOWNLOAD VJ DATA TABLE
output$download_vjtable <- downloadHandler(
  filename = "RenameThisTable.xlsx",
  content = function(file) {
    write.xlsx(vjtab_data(), file)
  }
)
#####################################################

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
#  else if (input$Y_Axis == "H_vj") {
#    return(list(
#      src = "TCR_Hvj.png",
#      contentType = "image/png",
#      width = 720,
#      height = 180,
#      alt = "Entropy of VJ combo repertoire"
#    )) }  
#  else if (input$Y_Axis == "H_aaCDR3") {
#    return(list(
#      src = "TCR_Haa.png",
#      contentType = "image/png",
#      width = 720,
#      height = 180,
#      alt = "Entropy of aaCDR3 repertoire"
#    )) }
#  else if (input$Y_Axis == "H_totCDR3") {
#    return(list(
#      src = "TCR_Htot.png",
#      contentType = "image/png",
#      width = 720,
#      height = 180,
#      alt = "Entropy of clonotype repertoire"
#    )) }
#  else if (input$Y_Axis == "H_ntCDR3") {
#    return(list(
#      src = "TCR_Hnt.png",
#      contentType = "image/png",
#      width = 720,
#      height = 180,
#      alt = "Entropy of nucleotide repertoire"
#    )) }    
  
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

}