# MODULE FOR CROSS-CORRELATION TAB  
require(shiny)
require(ggplot2)
require(plotly)
require(grid)
require(gridExtra)
require(survival)
require(GGally)
require(Cairo)

source("key_columns.R")

scatter_plotly = function(df, x, y, t, c) {
  my_fit = lm(df[,y] ~ df[,x])
#  my_cor = cor(df[,x], df[,y], use = "complete.obs", method = "pearson")
  caption_text = paste("Pearson Correlation:", round(my_cor, digits = 4))
  s_plot = ggplot(df, aes_string(x = x, y = y, text = t, color = c)) +
    geom_abline(intercept = my_fit$coefficients[1], slope = my_fit$coefficients[2]) +
    geom_point() +
    scale_colour_manual(name = "", 
                        labels = c("PRCR","SD","PD"), 
                        values = c("green", "orange","red")) +
    ggtitle(paste(y, " vs ", x)) + 
    #ggtitle(tit_txt) +
    xlab(x) + ylab(y) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))

  #s_plot = arrangeGrob(s_plot, sub = textGrob(caption_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)), heights = c(0.8,0.2))
    
  p = ggplotly(s_plot, tooltip = c("text","color","x","y"))
#  p_out = arrangeGrob(p, sub = textGrob(caption_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)), heights = c(0.8,0.2))
  return(p)
#  return(s_plot)
}

predat = read.csv("bms038_data_122016.csv")
predat = predat[predat$SampleType == "pre",]
predat$myBOR <- factor(predat$myBOR, levels=c("PRCR","SD","PD"), ordered = TRUE)
preondat = read.csv("bms038_preon_122016.csv")


CrossTabUI <- function(id, choices_list) {
  ns = NS(id)
  # NOT SURE WHAT TO DO WITH THIS, varList_copy
#  varList_copy <- names(pre_choice1)
  ### Layout for Cross-Correlation analysis
  tagList(
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          selectInput(ns("genomicSpace_Y"), "Y-Axis Genomic Space:", choices = names(choices_list)),
          uiOutput(ns("cross_Second")),
          selectInput(ns("genomicSpace_X"), "X-Axis Genomic Space:", choices = names(choices_list)),
          uiOutput(ns("cross_Second_2"))
        )
      ),
      
      # Show the caption and plot of the requested variable against mpg
      mainPanel(
        h3("Correlation between Genomic Features"),
        "Descriptive text here",
#        plotlyOutput(ns("Debug_Plot")),
        plotlyOutput(ns("cross_Scatter_plot")),
        p(),
        "More descriptive text on correlation plot",
        plotOutput(ns("corr_plot"))
      ) 
    )
  )
}


# NEED TO SEE IF THIS IS FUNCTIONALLY DIFFERENT FROM getID() IN OTHER MODULES
# Utility function (need to document)
getCrossID <- function(genomicSpace, feature) {
  if ( is.null(genomicSpace) || genomicSpace == "") {
    return(-1)
  }
  if (is.null(feature) || feature == ""  ) {
    return(-1)
  }
  tid <- which(pre_choice1[[genomicSpace]] == feature)
  tid <- names(pre_choice1[[genomicSpace]])[tid]
  return(tid)
}


CrossTab <- function(input, output, session, choices_list, my_data) {
  # INTERACTIVE UI FOR VARIABLE SELECTION DROP-DOWN MENUS
  ns = session$ns
  output$cross_Second = renderUI( {
    selectInput(ns("cross_sec_var"), "Y-Axis Feature", choices = unname(choices_list[[input$genomicSpace_Y]]))
  })
  
  output$cross_Second_2 = renderUI( {
    selectInput(ns("cross_sec_var_2"), "X-Axis Feature", choices = unname(choices_list[[input$genomicSpace_X]]))
  })
  # GET IDENTIY OF SELECTED VARIABLES
  getID_Y = reactive({
    my_pos = which(choices_list[[input$genomicSpace_Y]] == input$cross_sec_var)
    tid = names(choices_list[[input$genomicSpace_Y]])[my_pos]
    return(tid)
  })
  getID_X = reactive({
    my_pos = which(choices_list[[input$genomicSpace_X]] == input$cross_sec_var_2)
    tid = names(choices_list[[input$genomicSpace_X]])[my_pos]
    return(tid)
  })
  output$Debug_Plot = renderPlotly({
#    plot(1:10, main = paste(getID_X()))
    y_vars = unname(choices_list[[input$genomicSpace_Y]])
#    plot(1:10, main = paste(getCrossID(input$genomicSpace_Y, y_vars)))
#    plot(1:10, main = paste(class(getCrossID(input$genomicSpace_Y, y_vars))))
#    plot(1:10, main = paste(class(y_vars)))
    tid_x = getID_X()
    tid_y = getID_Y()
#    plot(1:10, main = paste(class(tid_x)))
#    s_plot = ggplot(my_data, aes_string(tid_x, tid_y)) + geom_point()
#    s_plot = ggplot(my_data, aes_string(x = tid_x, y = tid_y)) + geom_point(aes_string(x = tid_y, y = tid_y))
    scatter_plotly(my_data, tid_x, tid_y, t = my_data[,"PatientID.x"], c = my_data[,"myBOR"])
  })
  # CROSS-CORRELATION PLOT
  output$corr_plot <- renderPlot({
    y_vars = unname(choices_list[[input$genomicSpace_Y]])
    x_vars = unname(choices_list[[input$genomicSpace_X]])
    clean_vars = c()
    for (i in y_vars) {
      cur_var = getCrossID(input$genomicSpace_Y, i)
#      cur_var = getID_Y()
      clean_vars = c(clean_vars, cur_var)
    }
    for (i in x_vars) {
      cur_var = getCrossID(input$genomicSpace_X, i)
      clean_vars = c(clean_vars, cur_var)
    }
    clean_vars = unique(clean_vars)
    
    cc_dat = my_data[,c(clean_vars)]
    ggcorr(cc_dat)
    
    #  plot(1:10, main = paste(names(my_data)[90:93]))
    #  plot(1:10, main = paste(clean_vars))
    #  my_cors = c(input$cross_sec_var, input$cross_sec_var_2)
    #  tid_x <- getCrossID(input$genomicSpace, input$cross_sec_var)
    #  tid_y <- getCrossID(input$genomicSpace, input$cross_sec_var_2)
    #  cc_dat = my_data[,c(tid_x, tid_y)]
    #  plot(my_data[,tid_x], my_data[,tid_y])
    #  ggcorr(cc_dat, layout.exp = 1.5, hjust = 0.75)
    #  plot(1:10, main = paste(class(tid_x)))
  })
  
  # SCATTERPLOT
  output$cross_Scatter_plot <- renderPlotly ({
    req(input$cross_sec_var, input$cross_sec_var_2)
#    tid_x <- getCrossID(input$genomicSpace_X, input$cross_sec_var_2)
#    tid_y <- getCrossID(input$genomicSpace_Y, input$cross_sec_var)  
    tid_x = getID_X()
    tid_y = getID_Y()
    pdf(NULL);
    scatter_plotly(my_data, tid_x, tid_y, t = my_data[,"PatientID.x"], c = my_data[,"myBOR"])
  })
#    if (tid_x == -1 || tid_y == -1) { return }
    
    #pdf(NULL);
    
    # note ordering here on scale_colour_manual works, because myBOR is a factor ordered appropritely
    #tit_txt <- paste(input$cross_sec_var, " vs ", input$cross_sec_var_2)
    #s_plot = ggplot(my_data, aes_string(x = tid_x, y = tid_y, text = my_data$PatientID.x, color = my_data$myBOR)) +
#    s_plot = ggplot(my_data, aes_string(x = tid_x, y = tid_y)) +
#    s_plot = ggplot(my_data, aes(x = tid_x, y = tid_y)) +
      #geom_point() +
      #scale_colour_manual(name = "", 
      #                    labels = c("PR/CR","SD","PD"), 
      #                    values = c("green", "orange","red")) +
      ##ggtitle(tit_txt) +
      #xlab(input$cross_sec_var_2) + ylab(input$cross_sec_var) +
      #theme_minimal() +
      #theme(plot.title = element_text(hjust = 0.5)) +
      #theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
    
    
    #p = ggplotly(s_plot, tooltip = c("text","color","x","y"))
#    return(p)
    #return(s_plot)
  #})
  
  
}


ui = navbarPage("BMS038",
                navbarMenu("Genomics & Outcome",
                           tabPanel("Pre-Therapy",
                                    CrossTabUI("PRE", pre_choice1)
                           ),
                           tabPanel("Change on therapy",
                                    CrossTabUI("DIFF", diff_choice1)
                           ),
                           # THIS TAB ONLY EXISTS TO DEMOSTRATE THAT VALUES CREATE BY A MODULE CAN BE RE-USED ELSEWHER
                           tabPanel("Blank Space",
                                    h3("...and I'll write your name"),
                                    plotOutput("diffplot")
                           )
                )
)
# SERVER CODE FOR ENTIRE DEMONSTRATION APP
server = function(input, output) {
  # callModule's arguments: 
  # 1. Name of server-side function
  # 2. Namespace (should match namespace used in corresponding UI call)
  # 3. List of variables defined in file, key_columns.R
  # 4. Dataframe used to produce plots
  callModule(CrossTab, "PRE", pre_choice1, predat)
  # ASSIGN callModule TO A VALUE IF YOU WANT TO ACCESS THE VALUES IN A DIFFERENT PAGE
  Diff_Selections = callModule(CrossTab, "DIFF", diff_choice1, preondat)
  output$diffplot = renderPlot({
    hist(preondat[,Diff_Selections[[1]]()])
  })
}

shinyApp(ui, server)