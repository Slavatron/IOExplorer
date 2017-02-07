library(shiny)
library
#source("key_columns.R")

Selection_ModuleUI = function(id, choices_list) {
  ns = NS(id)
  tagList(
    sidebarPanel(width = 3,
                 h3("Filter The Dataset"),
                 uiOutput(ns("apply_Filters")),
                 wellPanel(
                   checkboxGroupInput(ns("Cohort"), "Treatment Cohort", choices = c("NIV3-NAIVE", "NIV3-PROG")),
                   checkboxGroupInput(ns("Subtype"), "Melanoma Subtype", choices = c("BRAF" = "1", "RAS" = "2", "NF1" = "3", "TripleWt" = "4")),
                   checkboxGroupInput(ns("Pre_Exome"), "Pre-Treatment Exome Sequenced", choices = c("Yes" = "1", "No" = "0")),
                   checkboxGroupInput(ns("On_Exome"), "On-Treatment Exome Sequenced", choices = c("Yes" = "1", "No" = "0")),
                   checkboxGroupInput(ns("Pre_RNA"), "Pre-Treatment RNA Sequenced", choices = c("Yes" = "1", "No" = "0")),
                   checkboxGroupInput(ns("On_RNA"), "On-Treatment RNA Sequenced", choices = c("Yes" = "1", "No" = "0")),
                   checkboxGroupInput(ns("Has_TCR"), "TCR Data Available", choices = c("Yes" = "1", "No" = "0")),
                   checkboxGroupInput(ns("Response"), "Response", choices = c("PRCR", "SD", "PD"))
                 )
    ),
    mainPanel(width = 7,
              h3("Summary Table"),
              "Compilation of clinical and genomic data for all 73 patients in BMS-038 dataset. The set of patients included in this table can be filtered using the checkboxes on the left side of the screen. To perform analyses using a filtered subset of the full dataset you must Apply the selected filters using either the checkbox or the radiobuttons. To modify the set of datatypes displayed in this table, use the checkboxes to the right of the table; note that all datatypes are available for relevant analyses regardless of whether they are displayed in this table",
              # IF USER SELECTS ANY ROWS IN THE DISPLAY TABLE, GIVE THEM THE OPTION TO FILTER BASED ON THAT
              # (note that this conditional statement uses javascript syntax)
              conditionalPanel(paste0("input['", ns("table_rows_selected"), "'].length > '0'"),
              # CHECK BOX TO ALLOW FURTHER FILTRATION BY INDIVIDUAL ROWS
              # (note that this filtering mechanism might fuck up if the table has been re-ordered)
              uiOutput(ns("apply_table_filters"))),
              #      dataTableOutput(DT::dataTableOutput(ns("table")), width = "25%")
              #      dataTableOutput(ns("table"), width = "25%")
              wellPanel(DT::dataTableOutput(ns("table")), style = "overflow-x:scroll; max-width: 800px; overflow-y:scroll; max-height: 600px")
    ),
    # CHECKBOX FOR COLUMNS TO DISPLAY
    sidebarPanel(width = 2,
#                 # plot used for debugging purposes
#                 plotOutput(ns("Debug_Plot")),
                 h3("Add Columns to the Display Table"),
                 wellPanel(
                   #                 plotOutput(ns("Debug_Plot")),
                   checkboxGroupInput(ns("Display_Columns"), 
                                      "Columns to Display", 
                                      choices = c("NonSynMut",
                                                  "log10mut",
                                                  "NACnt",
                                                  "log10na",
                                                  "PreTreat_Exome",
                                                  "OnTreat_Exome",
                                                  "TCR_Data",
                                                  "RNASeq_Pre",
                                                  "RNASeq_On",
                                                  "Cohort",
                                                  "NACnt2",
                                                  "NAFrac",
                                                  "oncosnp_Purity",
                                                  "oncosnp_Ploidy",
                                                  "facets_Purity",
                                                  "facets_Ploidy")),
                   "This list will eventually include every variable we have, it will be organized, and it will have better names"
                   
                 )
    )
  )
}


Selection_Module = function(input, output, session, choices_list, my_data) {
  ns = session$ns
  # CHECK BOX FOR WHETHER OR NOT FILTERS SHOULD BE APPLIED TO OTHER ANALYSES
  output$apply_Filters = renderUI({
    checkboxInput(ns("Filt"), "(Click Box To Apply Filters to Analyses)", value = FALSE)
  })
  # FILTER INPUT TABLE
  filt_data = reactive({
    temp_dat = my_data
    # APPLY EACH CHECK BOX FILTER TO temp_data
    if (!is.null(input$Cohort)) {
      temp_dat = temp_dat[temp_dat$Cohort %in% input$Cohort,]
    }
    if (!is.null(input$Subtype)) {
      #      temp_dat = temp_dat[temp_dat$Subtype %in% input$Subtype,]
      # INCLUDE UN-TYPED SAMPLES BY DEFAULT
      temp_dat = temp_dat[temp_dat$Subtype %in% c(input$Subtype, NA),]
    }
    if (!is.null(input$Pre_Exome)) {
      temp_dat = temp_dat[temp_dat$PreTreat_Exome %in% input$Pre_Exome,]
    }
    if (!is.null(input$On_Exome)) {
      temp_dat = temp_dat[temp_dat$OnTreat_Exome %in% input$On_Exome,]
    }
    if (!is.null(input$Pre_RNA)) {
      temp_dat = temp_dat[temp_dat$RNASeq_Pre %in% input$Pre_RNA,]
    }
    if (!is.null(input$On_RNA)) {
      temp_dat = temp_dat[temp_dat$RNASeq_On %in% input$On_RNA,]
    }
    if (!is.null(input$Has_TCR)) {
      temp_dat = temp_dat[temp_dat$TCR_Data %in% input$Has_TCR,]
    }
    if (!is.null(input$Response)) {
      temp_dat = temp_dat[temp_dat$myBOR %in% input$Response,]
    }
    return(temp_dat)
  })
  # SUBSET FILTERED TABLE'S COLUMNS FOR DISPLAY
  Display_Table = reactive({
    disp_data = filt_data()
    row.names(disp_data) = disp_data$PatientID.x
    disp_data = disp_data[,c("Cohort", "SubtypeEZ", input$Display_Columns)]
    names(disp_data)[2] = "Subtype"
    return(disp_data)
  })

  output$table = DT::renderDataTable({
    Display_Table()},
    
    options=list(
      orderClasses = TRUE,
      lengthMenu = list(
        c(-1, 10000, 1000, 100, 10), 
        c('All','10000', '1000', '100', '10')),
      style="font-size:25%")
  )
  # UI FOR FILTERING EVEN FURTHER BY CLICKING ON ROWS
  # simple checkbox version
#  output$apply_table_filters = renderUI({
#    checkboxInput(ns("Row_Filt"), "(Click To Filter by Selected Rows)", value = FALSE)
#  })
  # more flexible radiobutton version (DOESN'T WORK YET)
  output$apply_table_filters = renderUI({
    radioButtons(ns("Radio_Row_Filt"), "Apply Filters by Row Selection",
                 choices = list("Remove Selected Rows" = "1",
                                                "Keep Only Selected Rows" = "2",
                                                "Do Not Filter by Selected Rows" = "3"),
                 selected = "3")
  })
  # APPLY FILTERS TO OUTPUT TABLE IF CHECKBOX IS CLICK
  out_data = reactive({
    sub_data = my_data
    if (input$Filt) {
      sub_data = filt_data()
    } 
    # simple checkbox version
#    if (input$Row_Filt) {
#      sub_data = sub_data[input$table_rows_selected,]
#    }
    # more flexible radiobutton version
    if (!input$Radio_Row_Filt == "3") {
      if (input$Radio_Row_Filt == "1") {
        sub_data = sub_data[-input$table_rows_selected,]
      } else if (input$Radio_Row_Filt == "2") {
        sub_data = sub_data[input$table_rows_selected,]
      } 
    }
    return(sub_data)
  })
  # FOR DEBUGGING PURPOSES
  output$Debug_Plot = renderPlot({
    #    plot(1:10, main = paste(input$Has_Exome))
#    plot(1:10, main = paste(dim(out_data())))
    plot(1:10, main = paste(out_data()[1,2]))
#    plot(1:10, main = paste(input$table_rows_selected))
#    plot(1:10, main = paste(class(input$table_rows_selected)))
#    plot(1:10, main = paste(class(out_data())))
#    plot(1:10, main = paste(input$Filt))
  })
  return(out_data)
}
