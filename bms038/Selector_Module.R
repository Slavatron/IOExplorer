library(shiny)
library(htmltools)
library(bsplus)
#source("key_columns.R")

Selection_ModuleUI = function(id, choices_list) {
  ns = NS(id)
  tagList(
    sidebarPanel(width = 3,
                 tabsetPanel(
                   tabPanel("Add Features",
                            "Select clinical/genomic features to display",
                            bs_accordion(id = "Display") %>%
                              bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
                              bs_append(title = "Exome", 
                                        content = checkboxGroupInput(ns("Display_Exome"), label = NULL, choices = unname(choices_list[["Exome"]]))) %>%
                              bs_append(title = "RNASeq",
                                        content = checkboxGroupInput(ns("Display_RNA"), label=NULL, choices=unname(choices_list[["RNASeq"]]))) %>%
                              bs_append(title = "TCR Features",
                                        content = checkboxGroupInput(ns("Display_TCR"), label = NULL, choices = unname(choices_list[["TCR"]]))) %>%
                              bs_append(title = "Immune Deconvolution",
                                        content = checkboxGroupInput(ns("Display_ImmDec"), label = NULL, choices = unname(choices_list[["Immune Deconvolution"]]))) %>%
                              bs_append(title = "Immunohistochemistry Features",
                                        content = checkboxGroupInput(ns("Display_IHC"), label = NULL, choices = unname(choices_list[["IHC"]])))
                            ),
                   tabPanel("Define Filters",
                            "Select filters to subset data for analysis",
                            bs_accordion(id = "beatles") %>%
                              bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
                              bs_append(title = "Cohort", 
                                        content = checkboxGroupInput(ns("Cohort"), label = NULL, choices = c("NIV3-NAIVE", "NIV3-PROG"))) %>%
                              bs_append(title = "Melanoma Subtype", content = checkboxGroupInput(ns("Subtype"), label = NULL, choices = c("BRAF" = "1", "RAS" = "2", "NF1" = "3", "TripleWt" = "4"))) %>%
                              bs_append(title = "Pre-Treatment Exome Sequenced", content = checkboxGroupInput(ns("Pre_Exome"), label=NULL, choices = c("Yes" = "1", "No" = "0"))) %>%
                              bs_append(title = "On-Treatment Exome Sequenced", content = checkboxGroupInput(ns("On_Exome"), label=NULL, choices = c("Yes" = "1", "No" = "0"))) %>%
                              bs_append(title = "Pre-Treatment RNA Sequenced", content = checkboxGroupInput(ns("Pre_RNA"), label=NULL, choices = c("Yes" = "1", "No" = "0"))) %>%
                              bs_append(title = "On-Treatment RNA Sequenced", content = checkboxGroupInput(ns("On_RNA"), label=NULL, choices = c("Yes" = "1", "No" = "0"))) %>%
                              bs_append(title = "TCR Data Available", content = checkboxGroupInput(ns("Has_TCR"), label=NULL, choices = c("Yes" = "1", "No" = "0"))) %>%
                              bs_append(title = "Response", content = checkboxGroupInput(ns("Response"), label=NULL, choices = c("PRCR", "SD", "PD")))
                            
                            )
                 )
    ),
    mainPanel(width = 9,
              h3("Summary Table"),
              "Compilation of clinical and genomic data for all 68 patients in BMS-038 dataset. Use the checkboxes on the left side of the screen to display additional features or to define a filtered subset of patients for comparative analysis on other tabs of this portal. Filters defined on this tab can be turned On/Off on the 'Genomics & Outcome', 'Correlation', and 'Gene Expr' tabs.",
              #To perform analyses using a filtered subset of the full dataset you must Apply the selected filters using either the checkbox or the radiobuttons. To modify the set of datatypes displayed in this table, use the checkboxes to the right of the table; note that all datatypes are available for relevant analyses regardless of whether they are displayed in this table
              wellPanel(DT::dataTableOutput(ns("table")), style = "overflow-x:scroll; overflow-y:scroll; max-height: 600px"),
              # IF USER SELECTS ANY ROWS IN THE DISPLAY TABLE, GIVE THEM THE OPTION TO FILTER BASED ON THAT
              # (note that this conditional statement uses javascript syntax)
              conditionalPanel(paste0("input['", ns("table_rows_selected"), "'].length > '0'"),
                               # CHECK BOX TO ALLOW FURTHER FILTRATION BY INDIVIDUAL ROWS
                               # (note that this filtering mechanism might fuck up if the table has been re-ordered)
                               uiOutput(ns("apply_table_filters")))

    )
  )
}


Selection_Module = function(input, output, session, choices_list, my_data) {
  ns = session$ns
  # Reactive Variable for Applying/Removing filters
  Filters_OnOff = reactiveValues(
    Check = "On"
  )
  # Buttons for Applying and Removing Filters
  output$Filters_On_Button = renderUI({
    if (Filters_OnOff$Check == "On") {
      actionButton(ns("Filters_On"), label = "Filters On", style = "color: white; 
                     background-color: #0000ff")
    } else {
      actionButton(ns("Filters_On"), label = "Apply Filters")
    }  
    })
  output$Filters_Off_Button = renderUI({
    if (Filters_OnOff$Check == "Off") {
      actionButton(ns("Filters_Off"), label = "Filters Off", style = "color: white; 
                     background-color: #0000ff")
    } else {
      actionButton(ns("Filters_Off"), label = "Remove Filters")
    }
  })
  observeEvent(input$Filters_On, {
    Filters_OnOff$Check = "On"
  })
  observeEvent(input$Filters_Off, {
    Filters_OnOff$Check = "Off"
  })
#  # CHECK BOX FOR WHETHER OR NOT FILTERS SHOULD BE APPLIED TO OTHER ANALYSES
#  output$apply_Filters = renderUI({
#    checkboxInput(ns("Filt"), "(Click Box To Apply Filters to Analyses)", value = FALSE)
#  })
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
  ExomeID = reactive({
    my_pos = which(choices_list[["Exome"]] %in% input$Display_Exome)
    tid = names(choices_list[["Exome"]])[my_pos]
    return(tid)
  })
  RNAID = reactive({
    my_pos = which(choices_list[["RNASeq"]] %in% input$Display_RNA)
    tid = names(choices_list[["RNASeq"]])[my_pos]
    return(tid)
  })
  IHCID = reactive({
    my_pos = which(choices_list[["IHC"]] %in% input$Display_IHC)
    tid = names(choices_list[["IHC"]])[my_pos]
    return(tid)
  })
  TCRID = reactive({
    my_pos = which(choices_list[["TCR"]] %in% input$Display_TCR)
    tid = names(choices_list[["TCR"]])[my_pos]
    return(tid)
  })
  ImmDecID = reactive({
    my_pos = which(choices_list[["Immune Deconvolution"]] %in% input$Display_ImmDec)
    tid = names(choices_list[["Immune Deconvolution"]])[my_pos]
    return(tid)
  })
  # SUBSET FILTERED TABLE'S COLUMNS FOR DISPLAY
  Display_Table = reactive({
    disp_data = filt_data()
    row.names(disp_data) = disp_data$PatientID.x
    extra_cols = c(ExomeID(), IHCID(), RNAID(), TCRID(), ImmDecID())
    if (length(extra_cols) == 0 ) {
      disp_data = disp_data[,c("Cohort", "SubtypeEZ")]
    }
    if (length(extra_cols) > 0) {
      disp_data = disp_data[,c("Cohort", "SubtypeEZ", extra_cols)]
    }
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
    if (Filters_OnOff$Check == "On") {
      sub_data = filt_data()
    }
#    if (input$Filt) {
#      sub_data = filt_data()
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
