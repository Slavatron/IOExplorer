# Create Cross Correlation TAB UI

createCrossTabUI <- function() {
  ### Layout for Cross-Correlation analysis
  cross_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        #      selectInput("genomicSpace", "Genomic Space:", varList),
        uiOutput("cross_Y_var"),
        #      uiOutput("cross_Second"),
        uiOutput("cross_X_var"),
        #      uiOutput("cross_Second_2"),
        uiOutput("cross_Slider")
      ),
      plotOutput("cross_slide_hist")
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(textOutput("cross_caption")),
      plotOutput("corr_plot"),
      uiOutput("cross_Second"),
      uiOutput("cross_Second_2"),
      plotlyOutput("cross_Scatter_plot")
      #    plotOutput("pre_os_plot"),
      #    plotOutput("pre_pfs_plot"),
      #    tableOutput("pre_my_table")
    ) 
  )
  
  return(cross_ui)
}