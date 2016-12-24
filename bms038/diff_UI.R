source("key_columns.R")

createDiffTabUI <- function() {
  varList <- names(pre_choice1)
  
  diff_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("diff_genomicSpace", "Genomic Space:", varList),
        uiOutput("diff_Second"),
        uiOutput("diff_Slider")
      ),
      plotOutput("diff_slide_hist")
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3(textOutput("diff_caption")),
      plotOutput("diff_os_plot"),
      plotOutput("diff_pfs_plot"),
      tableOutput("diff_my_table")
    ) 
  )
  
  return(diff_ui)
}