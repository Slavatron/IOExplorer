

ApplyRemoveUI = function(id) {
  ns <- NS(id)
  # Buttons for Applying and Removing Filters
  tagList(
#    actionButton(ns("Filters_On"), label = "Apply Filters"),
#    actionButton(ns("Filters_Off"), label = "Remove Filters"),
    fluidRow(
      column(6,    uiOutput(ns("Filters_On_Button"))),
      column(6,    uiOutput(ns("Filters_Off_Button")))
    ),
#    uiOutput(ns("Filters_On_Button")),
#    uiOutput(ns("Filters_Off_Button")),
    textOutput(ns("Filter_Status"))
  )

}
ApplyRemove = function(input, output, session, filt_dat, full_dat) {
  ns = session$ns
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
  TheDat = reactive({
    out_dat = full_dat
    if (Filters_OnOff$Check == "On") {
      out_dat = filt_dat
    }
    return(out_dat)
  })
  output$Filter_Status = renderText({
    my_dim = nrow(TheDat())
#    my_dim = nrow(full_dat)
#    if (Filters_OnOff$Check == "On") {
#      my_dim = nrow(filt_dat)
#    }
#    paste("Filters are", Filters_OnOff$Check, "\nUsing", my_dim, "patients")
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