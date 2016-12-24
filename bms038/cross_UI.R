# Create Cross Correlation TAB UI

createCrossTabUI <- function() {
  ### Layout for Cross-Correlation analysis
  cross_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        uiOutput("cross_Y_var"),
        uiOutput("cross_Second"),
        uiOutput("cross_X_var"),
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