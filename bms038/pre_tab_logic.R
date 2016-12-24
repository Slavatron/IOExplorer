# Included from server.R
# Logic for guiding Pre Treatment Tab

# Utility function (need to document)
getPreId <- function(genomicSpace, feature) {
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


createPreTab <- function(input,output, predat) {

  output$pre_Second = renderUI( {
    selectInput("pre_sec_var", "Feature", choices = unname(pre_choice1[[input$pre_genomicSpace]]))
  })

  # SLIDER FOR DEFINING CUT-POINT
  output$pre_Slider = renderUI({
    # Figure out which variable to look up in raw data
    tid <- getPreId(input$pre_genomicSpace, input$pre_sec_var)
    if ( tid == -1 ) { return() }
    #print(sprintf("pre_slider %s %s", input$pre_genomicSpace, input$pre_sec_var))
    medv <- median(predat[,tid], na.rm = TRUE)
    minv <- min(predat[,tid], na.rm = TRUE)
    maxv <- max(predat[,tid], na.rm = TRUE)
    #print(sprintf("min = %.2f max = %.2f med = %.2f", minv, maxv, medv))
    sliderInput("pre_slide1", label = h3(paste(input$pre_sec_var)), min = minv, max = maxv, value = medv, round = FALSE)
  })

  # SLIDER-CONTROLLED HISTOGRAM 
  output$pre_slide_hist <- renderPlot({
    # Figure out which variable to look up in raw data
    tid <- getPreId(input$pre_genomicSpace, input$pre_sec_var)
    if ( tid == -1 ) { return() }
    niceHist(predat[,tid], input$pre_sec_var, cutpoint=input$pre_slide1)
  })

  # SURVIVAL PLOTS
  output$pre_choose_survival_type = renderUI( {
    radioButtons("Survival_Type", label = "Outcome to Analyze: ",choices = list("Overall Survival" = 1, "Progression Free Survival" = 2), inline=TRUE, selected = 1)
  })
  
  output$pre_survival_plot <- renderPlot({
    #print("Trying to render OS")
    tid <- getPreId(input$pre_genomicSpace, input$pre_sec_var)
        
    survival_plot = clever_gg_surv(predat, "OSWK", "OS_event", tid, input$pre_slide1, "Overall Survival")
    switch = input$Survival_Type
    if (switch == 2 ) {
      survival_plot = clever_gg_surv(predat, "PFSWK", "PFS_event", tid, input$pre_slide1, "Progression Free Survival")
    }
    survival_plot
  })


  # SUMMARY TABLE
  #output$pre_my_table <- renderTable({
  #  mini_table = predat[,c("Sample", input$sec_var, "PFS", "PFS_SOR", "OS", "OS_SOR", "myBOR" )]
  #  mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
  #  mini_table
  #  })

}
