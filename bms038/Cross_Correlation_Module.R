# MODULE FOR CROSS-CORRELATION TAB  

# FUNCTION: scatter_plotly()
# SUMMARY:
# Creates interactive scatterplot with additional data available by hovering the mouse over individual points.
#' @param df - dataframe containing data to be plotted
#' @param x - variable in df for the X axis
#' @param y - variable in df for the Y axis
#' @param x_lab - label for the X axis; defaults to x
#' @param y_lab - label for the Y axis; defaults to y
#' @param t - variable in df to be displayed in hover-over text
#' @param c - variable in df to define colors; should always be "Response" in BMS Portal
scatter_plotly = function(df, x, y, x_lab = x, y_lab = y, t, c) {
  my_fit = lm(df[[y]] ~ df[[x]])
  s_plot = ggplot(df, aes(x = !!sym(x), y = !!sym(y), text = !!sym(t), color = !!sym(c))) +
    geom_abline(intercept = my_fit$coefficients[1], slope = my_fit$coefficients[2], size = 0.25) +
    geom_point() +
    scale_colour_manual(name = "", 
                        labels = c("PRCR","SD","PD"), 
                        values = c("green", "orange","red")) +
#    ggtitle(paste(y, " vs ", x)) + 
    #ggtitle(tit_txt) +
    xlab(x_lab) + ylab(y_lab) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
  
  p = ggplotly(s_plot, tooltip = c("text","color","x","y"))
  return(p)
}

# MODULE: CrossTabUI()
# SUMMARY:
# Creates cross-correlation page of BMS Portal. Includes variable-selection widgets and 2 plots
#' @param id - namespace id for coordinating calls in ui.R and server.R
#' @param choices_list - list of lists defined in key_columns.R
CrossTabUI <- function(id, choices_list) {
  ns = NS(id)
  ### Layout for Cross-Correlation analysis
  tagList(
    sidebarLayout(
      sidebarPanel(
#        textOutput(ns("debugText")),
        wellPanel(
#          selectInput(ns("genomicSpace_Y"), "Y-Axis Genomic Space:", choices = names(choices_list)),
          uiOutput(ns("cross_First")),
          uiOutput(ns("cross_Second")),
#          selectInput(ns("genomicSpace_X"), "X-Axis Genomic Space:", choices = names(choices_list)),
          uiOutput(ns("cross_First_2")),
          uiOutput(ns("cross_Second_2"))
        )
      ),
      
      # Show the caption and plot of the requested variable against mpg
      mainPanel(
        h3("Correlation between Genomic Features"),
        tabsetPanel(
          tabPanel("Scatterplot",
                   #        "Descriptive text here",
                   "Here you can explore the relationships between genomic features on a patient-by-patient basis. The drop-down menus allow you to choose different groups of variables to place on the Y and X axes. You can identify individual patient's and their exact measurements for the variables selected rolling your mouse over individual points on the graph.",
                   plotlyOutput(ns("cross_Scatter_plot")),
                   p()
                   ),
          tabPanel("Correlation Plot",
                   #        "More descriptive text on correlation plot",
                   "This graphic displays the relative correlations between the full set of variables selected using the drop-down menus.",
                   plotOutput(ns("corr_plot"))
                   )
        ),
        ApplyRemoveUI(ns("INNER"))
      ) 
    )
  )
}


# Utility function (need to document)
# ALSO NEED TO GENERALIZE THIS FUNCTION
getCrossID <- function(genomicSpace, feature, choices_list) {
  if ( is.null(genomicSpace) || genomicSpace == "") {
    return(-1)
  }
  if (is.null(feature) || feature == ""  ) {
    return(-1)
  }
  tid <- which(choices_list[[genomicSpace]] == feature)
  tid <- names(choices_list[[genomicSpace]])[tid]
  return(tid)
}

# MODULE: CrossTab()
# SUMMARY:
# Creates cross-correlation page of BMS Portal. Includes variable-selection widgets and 2 plots
#' @param choices_list - list of lists defined in key_columns.R
#' @param filt_data - reactive object containing a data frame
#' @param full_data - normal data frame (unfiltered version of my_data)
# STILL TO DO:
# 1. ELIMINATE HARD-CODED ELEMENTS
# 2. GENERALIZE CODE SO IT WORKS FOR PRE-ON DATA JUST AS WELL AS FOR PRE-DATA
CrossTab <- function(input, output, session, choices_list_raw, filt_data, full_data, my_gsva) {
  ns = session$ns
  # RE-DEFINE REACTIVE INPUTS SO THEY CAN BE MODIFIED IF GSVA VALUES GET INTRODUCED
  Saved_GSVA_Values = reactive({
    # Start with an empty data.frame
    out_data = data.frame()
    # Condense GSVA values from my_gsva when there are named entries in it
    out_data = do.call("rbind", lapply(reactiveValuesToList(my_gsva), function(x) as.data.frame(t(x[[4]]))))
    out_data = as.data.frame(t(out_data))
    # Process Delta values if necessary
    if (nrow(out_data) > 0 & nrow(full_data) == 62) {
      out_data = make_diff_gsva(out_data)
    }
    return(out_data)
  })
  choices_list = reactive({
    out_list = choices_list_raw
    if (length(names(my_gsva)) > 0) {
      # Define new list of variables based on Saved_GSVA_Values
      new_vals = names(Saved_GSVA_Values())
      new_list = list(new_vals)
      names(new_list[[1]]) = new_vals
      # Append choices_list with these values
      list_pos = length(out_list)+1
      out_list[[list_pos]] = unlist(new_list)
      names(out_list)[list_pos] = "User-Defined GSVA"
    }
    return(out_list)
  })
  # USE ApplyRemoveModule TO SWITCH BETWEEN FILTERED/UNFILTERED DATA
  #my_data = callModule(ApplyRemove, "INNER", filt_data(), full_data)
  
  my_data_raw = callModule(ApplyRemove, "INNER", filt_data(), full_data)
  my_data = reactive({
    out_data = my_data_raw()
    gsva_data = Saved_GSVA_Values()
    # check column names...
    # Merge as by 'PatientIDx' if you're working with DIFF samples
    if (nrow(full_data) == 62) {
      gsva_data$PatientID.x = rownames(gsva_data)
      out_data = merge(out_data, gsva_data, by = "PatientID.x", all.x = TRUE)
    } else {
      gsva_data$Sample = rownames(gsva_data)
      out_data = merge(out_data, gsva_data, by = "Sample", all.x = TRUE)
    }
    return(out_data)
  })
#  output$debugText = renderText({
#    class(choices_list)
#  })
  # INTERACTIVE UI FOR VARIABLE SELECTION DROP-DOWN MENUS
  output$cross_First = renderUI({
    selectInput(ns("genomicSpace_Y"), "Y-Axis Genomic Space:", choices = names(choices_list()))
  })
  output$cross_Second = renderUI( {
    req(input$genomicSpace_Y)
    radioButtons(ns("cross_sec_var"), "Y-Axis Feature", choices = unname(choices_list()[[input$genomicSpace_Y]]))
  })
  
  output$cross_First_2 = renderUI({
    selectInput(ns("genomicSpace_X"), "X-Axis Genomic Space:", choices = names(choices_list()), selected = names(choices_list())[2])
  })
  output$cross_Second_2 = renderUI( {
    req(input$genomicSpace_X)
    radioButtons(ns("cross_sec_var_2"), "X-Axis Feature", choices = unname(choices_list()[[input$genomicSpace_X]]))
  })
  # GET IDENTIIES OF SELECTED VARIABLES
  getID_Y = reactive({
    my_pos = which(choices_list()[[input$genomicSpace_Y]] == input$cross_sec_var)
    tid = names(choices_list()[[input$genomicSpace_Y]])[my_pos]
    return(tid)
  })
  getID_X = reactive({
    my_pos = which(choices_list()[[input$genomicSpace_X]] == input$cross_sec_var_2)
    tid = names(choices_list()[[input$genomicSpace_X]])[my_pos]
    return(tid)
  })
  # CROSS-CORRELATION PLOT
  output$corr_plot <- renderPlot({
    y_vars = unname(choices_list()[[input$genomicSpace_Y]])
    x_vars = unname(choices_list()[[input$genomicSpace_X]])
    clean_vars = c()
    for (i in y_vars) {
      cur_var = getCrossID(input$genomicSpace_Y, i, choices_list())
      clean_vars = c(clean_vars, cur_var)
    }
    for (i in x_vars) {
      cur_var = getCrossID(input$genomicSpace_X, i, choices_list())
      clean_vars = c(clean_vars, cur_var)
    }
    clean_vars = unique(clean_vars)
    
    cc_dat = my_data()[,c(clean_vars)]
    # RENAME DATAFRAME COLUMNS TO MAKE CORR-PLOT LOOK BETTER (needs work)
    dirty_vars = c(y_vars, x_vars)
    dirty_vars = unique(dirty_vars)
    names(cc_dat) = dirty_vars
    ggcorr(cc_dat)
    
  })
  
  # SCATTERPLOT 
  output$cross_Scatter_plot <- renderPlotly ({
    req(input$cross_sec_var, input$cross_sec_var_2)
    tid_x = getID_X()
    tid_y = getID_Y()
    pdf(NULL);
    scatter_plotly(my_data(), tid_x, tid_y, y_lab = input$cross_sec_var, x_lab = input$cross_sec_var_2, t = "PatientID.x", c = "Response") #t = my_data()[,"PatientID.x"], c = my_data()[,"Response"])
  })

}
