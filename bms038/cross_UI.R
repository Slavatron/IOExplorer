# Create Cross Correlation TAB UI
source("key_columns.R")

createCrossTabUI <- function() {
  varList_copy <- names(pre_choice1)
  ### Layout for Cross-Correlation analysis
  cross_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("genomicSpace_Y", "Y-Axis Genomic Space:", varList_copy),
        uiOutput("cross_Second"),
        selectInput("genomicSpace_X", "X-Axis Genomic Space:", varList_copy),
        uiOutput("cross_Second_2")
      )
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3("Correlation between Genomic Features"),
      "Descriptive text here",
      plotlyOutput("cross_Scatter_plot"),
      p(),
      "More descriptive text on correlation plot",
      plotOutput("corr_plot")
    ) 
  )
  
  return(cross_ui)
}