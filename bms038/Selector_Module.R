library(shiny)
#source("key_columns.R")

# LIST OF VARIABLES TO DISPLAY AS CHECK-BOX OPTIONS FOR FILTERING
my_factors = list("PatientID.x" = "Patient", 
                  "Cohort" = "Cohort",
                  "SubtypeEZ" = "Subtype",
                  "NonSynMut" = "Mutation Load")

DEMO_LIST = list("Factors" = unlist(my_factors))

# UI-SIDE FUNCTION
#' Selection_ModulerUI()
#' SUMMARY:
#' Module for filtering data. The output of this module is a Reactive DataFrame that can be used by other tabs for the analyses that follow.
#' @param id - namespace identifier
#' @param choices_list - list of lists
#' STILL TO DO:
#' 1. Create the possibility for data to be filtered by more than one parameter at a time
#' 
Selection_ModuleUI = function(id, choices_list) {
  ns = NS(id)
  tagList(
    sidebarPanel(
      wellPanel(
        selectInput(ns("selector_genomicSpace"), "Genomic Space:", choices = names(choices_list)),
        uiOutput(ns("selector_Second")),
        uiOutput(ns("apply_Filters")),
        p(),
        uiOutput(ns("Filter_Box"))
        #      uiOutput(ns("selector_Ranges")),
        #      uiOutput(ns("apply_Filters")),
        #      uiOutput(ns("reset_Filters")),
        #      uiOutput(ns("selector_Slider"))
      )
    ),
    mainPanel(
      # THIS TABLE SHOULD REALLY BE ONE OF THOSE DT TABLES JENNY PUT ON THE TCR PAGE
      tableOutput(ns("selector_table"))
    )
  )
}


Selection_Module = function(input, output, session, choices_list, my_data) {
  ns = session$ns
  
  output$selector_Second = renderUI({
    selectInput(ns("Sec_Var"), "Variable to Filter By:", choices = unname(choices_list[[input$selector_genomicSpace]]))
  })
  # REACTIVE VARIABLE FOR GETTING input$Sec_Var IN A MALIABLE FORM
  getID = reactive({
    my_pos = which(choices_list[[input$selector_genomicSpace]] == input$Sec_Var)
    tid = names(choices_list[[input$selector_genomicSpace]])[my_pos]
    return(tid)
  })
  
  my_boxes = reactive({
    b = unique(my_data[,getID()])
    b = sort(b)
    return(b)
  })
  
  # CHECK BOX FOR SELECTING CATEGORICAL VARIABLES
  output$Filter_Box = renderUI({
    checkboxGroupInput(ns("my_cats"), "Select What You Wanna Keep", choices = my_boxes())
  })
  
  # CHECK BOX FOR WHETHER OR NOT FILTERS SHOULD BE APPLIED
  output$apply_Filters = renderUI({
    checkboxInput(ns("Filt"), "Click This To Apply Filters", value = FALSE)
  })

  filt_data = reactive({
    temp_dat = my_data
    if (input$Filt) {
      temp_dat$Filt = temp_dat[,getID()]
      temp_dat = temp_dat[temp_dat$Filt %in% input$my_cats,]
    }
    return(temp_dat)
  })
  
  output$selector_table = renderTable({
    filt_data()[,c(1:6)]
  })
  return(filt_data)
}
