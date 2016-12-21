# Included from server.R
# Logic for guiding Cross-Correlation Tab

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

  formulaText <- reactive({
    paste("Survival ~", input$cross_sec_var)
  })
  
output$cross_caption <- renderText( { formulaText() } )


# INTERACTIVE UI FOR VARIABLE SELECTION DROP-DOWN MENUS
varList_copy = names(pre_choice1)
output$cross_Y_var = renderUI( {
  selectInput("genomicSpace_Y", "Y-Axis Genomic Space:", varList_copy)
})

output$cross_Second = renderUI( {
  selectInput("cross_sec_var", "Y-Axis Feature", choices = unname(pre_choice1[[input$genomicSpace_Y]]))
})

output$cross_X_var = renderUI( {
  selectInput("genomicSpace_X", "X-Axis Genomic Space:", varList_copy)
})

output$cross_Second_2 = renderUI( {
  selectInput("cross_sec_var_2", "X-Axis Feature", choices = unname(pre_choice1[[input$genomicSpace_X]]))
})

# SLIDER FOR DEFINING CUT-POINT
output$cross_Slider = renderUI({
  # Figure out which variable to look up in raw data
  tid <- getCrossID(input$genomicSpace_Y, input$cross_sec_var)
  if ( tid == -1 ) { return() }
  print(sprintf("cross_slider %s %s", input$genomicSpace, input$cross_sec_var))
  medv <- median(predat[,tid], na.rm = TRUE)
  minv <- min(predat[,tid], na.rm = TRUE)
  maxv <- max(predat[,tid], na.rm = TRUE)
  print(sprintf("min = %.2f max = %.2f med = %.2f", minv, maxv, medv))
  sliderInput("cross_slide1", label = h3(paste(input$cross_sec_var)), min = minv, max = maxv, value = medv, round = FALSE)
})

# SLIDER-CONTROLLED HISTOGRAM
output$cross_slide_hist <- renderPlot({
  # Figure out which variable to look up in raw data
  tid <- getCrossID(input$genomicSpace_Y, input$cross_sec_var)
  if ( tid == -1 ) { return() }
  niceHist(predat[,tid], input$cross_sec_var, cutpoint=input$cross_slide1)
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
  s_plot = ggplot(predat, aes_string(x = tid_x, y = tid_y)) +
    ggtitle("interACTIVE") +
    geom_point(aes_string(x = tid_x, y = tid_y, text = predat$PatientID.x, color = predat$myBOR))
#  p = ggplotly(s_plot, tooltip = "text")
  p = ggplotly(s_plot, tooltip = c(s_plot, "text", "color"))
  p
#  ggplotly(s_plot)
})


#output$CHOOSE_SURVIVAL_TYPE = renderUI( {
#  radioButtons("Survival_Type", label = h3("Choose Survival Type for Analysis"),choices = list("Overall Survival" = 1, "Progression Free Survival" = 2), selected = 1)
#})
#output$survival_plot <- renderPlot({
#  #print("Trying to render OS")
#  tid <- getCrossId(input$genomicSpace, input$cross_sec_var)
#        
#  survival_plot = clever_gg_surv(predat, "OSWK", "OS_event", tid, input$pre_slide1, "Overall Survival")
#  switch = input$Survival_Type
#  if (switch == 2 ) {
#    survival_plot = clever_gg_surv(predat, "PFSWK", "PFS_event", tid, input$pre_slide1, "Progression Free Survival")
#  }
#  survival_plot
#})


# SUMMARY TABLE
#output$pre_my_table <- renderTable({
#  mini_table = predat[,c("Sample", input$sec_var, "PFS", "PFS_SOR", "OS", "OS_SOR", "myBOR" )]
#  mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
#  mini_table
#  })

}
