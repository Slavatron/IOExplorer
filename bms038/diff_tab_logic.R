# Included from server.R
# Logic for guiding Change on therapy  Tab
source("key_columns.R")

getDiffId <- function(genomicSpace, feature) {
  if ( is.null(genomicSpace) || genomicSpace == "") {
    return(-1)
  }
  if (is.null(feature) || feature == ""  ) {
    return(-1)
  }
  tid <- which(diff_choice1[[genomicSpace]] == feature)
  tid <- names(diff_choice1[[genomicSpace]])[tid]
  return(tid)
}

createDiffTab <- function(input,output, preondat) {

  output$diff_Second = renderUI( {
    selectInput("diff_sec_var", "Feature", choices = unname(diff_choice1[[input$diff_genomicSpace]]))
  })

  # SLIDER FOR DEFINING CUT-POINT
  output$diff_Slider = renderUI({
    # Figure out which variable to look up in raw data
    tid <- getDiffId(input$diff_genomicSpace, input$diff_sec_var)
    if ( tid == -1 ) { return() }
    print(sprintf("diff_slider %s %s", input$diff_genomicSpace, input$diff_sec_var))
    medv <- median(preondat[,tid], na.rm = TRUE)
    minv <- min(preondat[,tid], na.rm = TRUE)
    maxv <- max(preondat[,tid], na.rm = TRUE)
    print(sprintf("min = %.2f max = %.2f med = %.2f", minv, maxv, medv))
    sliderInput("diff_slide1", label = h3(paste(input$diff_sec_var)), min = minv, max = maxv, value = medv, round = FALSE)
  })

  # SLIDER-CONTROLLED HISTOGRAM
  output$diff_slide_hist <- renderPlot({
    # Figure out which variable to look up in raw data
    tid <- getDiffId(input$diff_genomicSpace, input$diff_sec_var)
    if ( tid == -1 ) { return() }
    print(sprintf("Make histogram, tid='%s' genomicSpace='%s', feature='%s'", input$tid, input$diff_genomicSpace, input$diff_sec_var))
    niceHist(preondat[,tid], input$diff_sec_var, input$diff_slide1)
  })

  ## Survival Plots
  output$diff_choose_survival_type = renderUI( {
    radioButtons("diff_Survival_Type", label = "Outcome to Analyze: ",choices = list("Overall Survival (OS)" = 1, "Progression Free Survival (PFS)" = 2), inline=TRUE, selected = 1)
  })
  

  output$diff_survival_plot <- renderPlot({
    tid <- getDiffId(input$diff_genomicSpace, input$diff_sec_var)
    
    switch = input$diff_Survival_Type
    if (switch == 1 ) {
      mytit = paste("OS by", input$diff_sec_var)
      survival_plot = clever_gg_surv(preondat, "OSWK", "OS_event", tid, input$diff_slide1, mytit)
    } else {
      mytit = paste("PFS by", input$diff_sec_var)
      survival_plot = clever_gg_surv(preondat, "PFSWK", "PFS_event", tid, input$diff_slide1, mytit)
    }
    return(grid.draw(survival_plot))
  })
  
# SUMMARY TABLE
#output$diff_my_table <- renderTable({
#  mini_table = predat[,c("Sample", input$sec_var, "PFS", "PFS_SOR", "OS", "OS_SOR", "myBOR" )]
#  mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
#  mini_table
#  })

}