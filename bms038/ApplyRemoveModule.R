

ApplyRemoveUI = function(id) {
  ns <- NS(id)
  # Buttons for Applying and Removing Filters
  tagList(
#    actionButton(ns("Filters_On"), label = "Apply Filters"),
#    actionButton(ns("Filters_Off"), label = "Remove Filters"),
    wellPanel(
      h4("Filter Patients"),
      "Define filters on 'Select Patients' page to view analyses performed on a subset of the available data. By default, no filters are defined and these buttons have no effect. To define a set of filters, visit the 'Select Patients' page.", 
      fluidRow(
        column(3,    uiOutput(ns("Filters_On_Button"))),
        column(3,    uiOutput(ns("Filters_Off_Button"))),
        column(3, textOutput(ns("Filter_Count")))
      ),
      "NOTE: the number of patients displayed graphically may be reduced further depending on the availability of your selected variables."
    )
#    fluidRow(
#      column(6,    uiOutput(ns("Filters_On_Button"))),
#      column(6,    uiOutput(ns("Filters_Off_Button")))
#    ),
#    textOutput(ns("Filter_Count"))
#    fluidRow(
#      column(6, textOutput(ns("Filter_Status"))),
#      column(6, textOutput(ns("Filter_Count")))
#    )
#    uiOutput(ns("Filters_On_Button")),
#    uiOutput(ns("Filters_Off_Button")),
#    textOutput(ns("Filter_Status"))
  )

}
ApplyRemove = function(input, output, session, filt_dat, full_dat) {
  ns = session$ns
  Filters_OnOff = reactiveValues(
    Check = "ON"
  )
  # Buttons for Applying and Removing Filters
  output$Filters_On_Button = renderUI({
    if (Filters_OnOff$Check == "ON") {
      actionButton(ns("Filters_On"), label = "Filters On", style = "color: white; 
                     background-color: #0000ff")
    } else {
      actionButton(ns("Filters_On"), label = "Apply Filters")
    }

  })
  output$Filters_Off_Button = renderUI({
    if (Filters_OnOff$Check == "OFF") {
      actionButton(ns("Filters_Off"), label = "Filters Off", style = "color: white; 
                     background-color: #0000ff")
    } else {
      actionButton(ns("Filters_Off"), label = "Remove Filters")
    }
  })
  observeEvent(input$Filters_On, {
    Filters_OnOff$Check = "ON"
  })
  observeEvent(input$Filters_Off, {
    Filters_OnOff$Check = "OFF"
  })
  TheDat = reactive({
    out_dat = full_dat
    if (Filters_OnOff$Check == "ON") {
      out_dat = filt_dat
    }
    return(out_dat)
  })
  output$Filter_Status = renderText({
#    my_dim = nrow(TheDat())
#    paste("Filters are", Filters_OnOff$Check, "   (n =", my_dim, "patients)")
    paste("Filters are", Filters_OnOff$Check)
  })
  output$Filter_Count = renderText({
#    my_dim = nrow(TheDat())
    my_dim = length(unique(TheDat()$PatientID.x))
    paste("(n =", my_dim, "patients)")
  })
  return(TheDat)
#  return(Filters_OnOff)
}

#ui = fluidPage(
#  ApplyRemoveUI("YO")
#)

#server = function(input, output) {
#  callModule(ApplyRemove, "YO")
#}

#shinyApp(ui, server)