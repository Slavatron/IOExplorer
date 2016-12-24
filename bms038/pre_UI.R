# Code to create UI interface for pretab panel
# included from ui.R

source("key_columns.R")

createPreTabUI <- function() {
  varList <- names(pre_choice1)
  
  pre_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("genomicSpace", "Genomic Space:", varList),
        uiOutput("pre_Second"),
        uiOutput("pre_Slider")
      ),
      plotOutput("pre_slide_hist")
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(textOutput("pre_caption")),
      uiOutput("CHOOSE_SURVIVAL_TYPE"),
      plotOutput("survival_plot"),
      tableOutput("pre_my_table")
    ) 
  )
  
  return(pre_ui)
}