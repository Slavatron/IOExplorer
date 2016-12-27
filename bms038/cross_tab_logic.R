# Included from server.R
# Logic for guiding Cross-Correlation Tab
source("key_columns.R")


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


createCrossCorrTab <- function(input,output, predat) {
  # INTERACTIVE UI FOR VARIABLE SELECTION DROP-DOWN MENUS

  output$cross_Second = renderUI( {
    selectInput("cross_sec_var", "Y-Axis Feature", choices = unname(pre_choice1[[input$genomicSpace_Y]]))
  })

  output$cross_Second_2 = renderUI( {
    selectInput("cross_sec_var_2", "X-Axis Feature", choices = unname(pre_choice1[[input$genomicSpace_X]]))
  })

# CROSS-CORRELATION PLOT
  output$corr_plot <- renderPlot({
    y_vars = unname(pre_choice1[[input$genomicSpace_Y]])
    x_vars = unname(pre_choice1[[input$genomicSpace_X]])
    clean_vars = c()
    for (i in y_vars) {
      cur_var = getCrossID(input$genomicSpace_Y, i)
      clean_vars = c(clean_vars, cur_var)
    }
    for (i in x_vars) {
      cur_var = getCrossID(input$genomicSpace_X, i)
      clean_vars = c(clean_vars, cur_var)
    }
    clean_vars = unique(clean_vars)
  
    cc_dat = predat[,c(clean_vars)]
    ggcorr(cc_dat)
    
  #  plot(1:10, main = paste(names(predat)[90:93]))
  #  plot(1:10, main = paste(clean_vars))
  #  my_cors = c(input$cross_sec_var, input$cross_sec_var_2)
  #  tid_x <- getCrossID(input$genomicSpace, input$cross_sec_var)
  #  tid_y <- getCrossID(input$genomicSpace, input$cross_sec_var_2)
  #  cc_dat = predat[,c(tid_x, tid_y)]
  #  plot(predat[,tid_x], predat[,tid_y])
  #  ggcorr(cc_dat, layout.exp = 1.5, hjust = 0.75)
  #  plot(1:10, main = paste(class(tid_x)))
  })

# SCATTERPLOT
  output$cross_Scatter_plot <- renderPlotly ({
    tid_x <- getCrossID(input$genomicSpace_X, input$cross_sec_var_2)
    tid_y <- getCrossID(input$genomicSpace_Y, input$cross_sec_var)  
  
    if (tid_x == -1 || tid_y == -1) { return }
    
    # note ordering here on scale_colour_manual works, because myBOR is a factor ordered appropritely
    tit_txt <- paste(input$cross_sec_var, " vs ", input$cross_sec_var_2)
    s_plot = ggplot(predat, aes_string(x = tid_x, y = tid_y, text = predat$PatientID.x, color = predat$myBOR)) +
      geom_point() +
      scale_colour_manual(name = "", 
                        labels = c("PR/CR","SD","PD"), 
                        values = c("green", "orange","red")) +
      #ggtitle(tit_txt) +
      xlab(input$cross_sec_var_2) + ylab(input$cross_sec_var) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
    
    
    p = ggplotly(s_plot, tooltip = c("text","color","x","y"))
    return(p)
  })


}
