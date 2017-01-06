# Included from server.R
# Logic for guiding Patient Selection Tab
source("key_columns.R")

createSelectorTab <- function(input,output, predat) {

  output$selector_Second = renderUI( {
    selectInput("selector_sec_var", "Feature", choices = unname(selector_choice1[[input$selector_genomicSpace]]))
  })

  # SLIDER FOR DEFINING CUT-POINT
  output$selector_Slider = renderUI({
    # Figure out which variable to look up in raw data
    tid <- getselectorId()
    if ( tid == -1 ) { return(NULL) }
    print(sprintf("selector_slider %s %s", input$selector_genomicSpace, input$selector_sec_var))
    medv <- median(predat[,tid], na.rm = TRUE)
    minv <- round(min(predat[,tid], na.rm = TRUE), digits=3)
    maxv <- round(max(predat[,tid], na.rm = TRUE), digits=3)
    print(sprintf("min = %.2f max = %.2f med = %.2f", minv, maxv, medv))
    sliderInput("selector_slide1", label = h3(paste(input$selector_sec_var)), min = minv, max = maxv, value = medv, round = -2)
  })

  # DEFINE RANGES OF VALUES
  output$selector_Ranges = renderUI({
    # Figure out which variable to look up in raw data
    tid <- getselectorId()
    if ( tid == -1 ) { return(NULL) }
    varClass = class(predat[,tid])
    my_choices = sort(unique(predat[,tid]),na.last = FALSE)
    numericInput("selector_range", label = h3("Selector Range"), value = 1)
#    checkboxGroupInput("selector_range", label = h3("Checkbox"), choices = my_choices, selected = NULL)
  })

  # APPLY FILTERS
  output$apply_Filters = renderUI({
    actionButton("set_filter", label = "Apply Filter")
  })

  # RESET FILTERS
  output$reset_Filters = renderUI({
    actionButton("clear_filter", label = "Reset All Filters")
  })

  # USED TO BE SLIDER-CONTROLLED HISTOGRAM
  # NOW USED TO TRY AND DEBUG SUPPOSEDLY REACTIVE VALUE, PREDAT
  output$selector_slide_hist <- renderPlot({
    plot(1:10, main = paste(class(predat)))
#    # Figure out which variable to look up in raw data
#    tid <- getselectorId()
#    if ( tid == -1 ) { return(NULL) }
#    print(sprintf("Make histogram, tid='%s' genomicSpace='%s', feature='%s'", input$tid, input$selector_genomicSpace, input$selector_sec_var))
#    niceHist(predat[,tid], input$selector_sec_var, input$selector_slide1)
  })

 # SUMMARY TABLE
 # SHOULD CHANGE IN SIZE WHEN FILTERS ARE APPLIED
  output$selector_my_table <- renderTable({
    mini_table = predat[,c("Sample", "myBOR" )]
 # EXPERIMENT TO TRY PUTTING REACTIVITY WITHIN THE renderTable() FUNCTION
    observeEvent(input$set_filter, {
      mini_table = mini_table[1:10,]
    })
#    mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
    mini_table
  })
 


  ## Survival Plots (NOT USED)
  output$selector_choose_survival_type = renderUI( {
    radioButtons("selector_Survival_Type", label = "Outcome to Analyze: ",choices = list("Overall Survival (OS)" = 1, "Progression Free Survival (PFS)" = 2), inline=TRUE, selected = 1)
  })
  
  output$selector_survival_plot <- renderPlot({
    tid <- getselectorId()
    if ( tid == -1 ) { return(NULL) }
    
    switch = input$selector_Survival_Type
    if (switch == 1 ) {
      mytit = paste("OS by", input$selector_sec_var)
      survival_plot = clever_gg_surv(predat, "OSWK", "OS_event", tid, input$selector_slide1, mytit)
    } else {
      mytit = paste("PFS by", input$selector_sec_var)
      survival_plot = clever_gg_surv(predat, "PFSWK", "PFS_event", tid, input$selector_slide1, mytit)
    }
    return(grid.draw(survival_plot))
  })
  
  getselectorId <- reactive({
    genomicSpace <- input$selector_genomicSpace
    feature <- input$selector_sec_var
    if ( is.null(genomicSpace) || genomicSpace == "") {
      return(-1)
    }
    if (is.null(feature) || feature == ""  ) {
      return(-1)
    }
    tid <- which(selector_choice1[[genomicSpace]] == feature)
    tid <- names(selector_choice1[[genomicSpace]])[tid]
    return(tid)
  })
  
# SUMMARY TABLE
#output$selector_my_table <- renderTable({
#  mini_table = predat[,c("Sample", input$sec_var, "PFS", "PFS_SOR", "OS", "OS_SOR", "myBOR" )]
#  mini_table = mini_table[order(mini_table[,input$sec_var], decreasing = FALSE),]
#  mini_table
#  })

}
