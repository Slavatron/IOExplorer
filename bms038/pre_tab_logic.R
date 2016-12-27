# Included from server.R
# Logic for guiding Pre Treatment Tab
source("key_columns.R")


createPreTab <- function(input,output, predat) {

  output$pre_Second = renderUI( {
    selectInput("pre_sec_var", "Feature", choices = unname(pre_choice1[[input$pre_genomicSpace]]))
  })

  # SLIDER FOR DEFINING CUT-POINT
  output$pre_Slider = renderUI({
    # Figure out which variable to look up in raw data
    tid <- getPreId()
    if ( tid == -1 ) { return() }
    #print(sprintf("pre_slider %s %s", input$pre_genomicSpace, input$pre_sec_var))
    medv <- median(predat[,tid], na.rm = TRUE)
    minv <- min(predat[,tid], na.rm = TRUE)
    maxv <- round(max(predat[,tid], na.rm = TRUE), digits=3)
    #print(sprintf("min = %.2f max = %.2f med = %.2f", minv, maxv, medv))
    sliderInput("pre_slide1", label = h3(paste(input$pre_sec_var)), min = minv, max = maxv, value = medv, round = -2)
  })

  # SLIDER-CONTROLLED HISTOGRAM 
  output$pre_slide_hist <- renderPlot({
    # Figure out which variable to look up in raw data
    tid <- getPreId()
    # make sure key variables defined already
    if (tid == -1 || is.null(input$pre_slide1) ) { return(NULL) }
    niceHist(predat[,tid], input$pre_sec_var, cutpoint=input$pre_slide1)
  })

  # SURVIVAL PLOTS
  output$pre_choose_survival_type = renderUI( {
    radioButtons("pre_Survival_Type", label = "Outcome to Analyze: ",choices = list("Overall Survival (OS)" = 1, "Progression Free Survival (PFS)" = 2), inline=TRUE, selected = 1)
  })
  
  output$pre_survival_plot <- renderPlot({
    tid <- getPreId()
    if (tid == -1 || is.null(input$pre_slide1) ) { return(NULL) }
        
    switch = input$pre_Survival_Type
    if (switch == 1 ) {
      mytit = paste("OS by", input$pre_sec_var)
      survival_plot = clever_gg_surv(predat, "OSWK", "OS_event", tid, input$pre_slide1, mytit)
    } else {
      mytit = paste("PFS by", input$pre_sec_var)
      survival_plot = clever_gg_surv(predat, "PFSWK", "PFS_event", tid, input$pre_slide1, mytit)
    }
    return(grid.draw(survival_plot))
  })


  # Utility function (need to document)
  getPreId <- reactive({
    print("in getPreId")
    genomicSpace <- input$pre_genomicSpace
    feature <- input$pre_sec_var
    if ( is.null(genomicSpace) || genomicSpace == "") {
      print("genomic space undefined")
      return(-1)
    }
    if (is.null(feature) || feature == ""  ) {
      print("feature undefined")
      return(-1)
    }
    print(sprintf("Requesting preid genomicSpace='%s', feature ='%s'", genomicSpace, feature))
    tid <- which(pre_choice1[[genomicSpace]] == feature)
    tid <- names(pre_choice1[[genomicSpace]])[tid]
    return(tid)
  })
  
  # SUMMARY TABLE
  #output$pre_my_table <- renderTable({
  #  mini_table = predat[,c("Sample", input$sec_var, "PFS", "PFS_SOR", "OS", "OS_SOR", "myBOR" )]
  #  mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
  #  mini_table
  #  })

}
