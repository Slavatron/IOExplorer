# Code to create UI interface for pretab panel
# included from ui.R

source("key_columns.R")

createPreTabUI <- function() {
  varList <- names(pre_choice1)
  
  pre_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("pre_genomicSpace", "Genomic Space:", varList),
        uiOutput("pre_Second"),
        uiOutput("pre_Slider")
      ),
      plotOutput("pre_slide_hist")
    ),
    
    # Show the caption and plot of the requested variable against mpg
    mainPanel(
      h3("Pre-treatment Genomic Predictors & Outcomes"),
      "Pre-treatment genomic predictors and their relationship with Overall Survival (OS), Progression Free Survival (PFS), 
      and respone can be examined here. First select the type of genomic data you would like to look at \
      on the left; then select the particular genomic feature from the menu below. For example, one can select exome \
      and then pick mutation load to look at the relationship of mutation load and survival. Different cutpoints on mutation \
      load can be reviewed by using the slider on the left \
      ",
      p(),
      p(),
      uiOutput("pre_choose_survival_type"),
      p(),
      plotOutput("pre_survival_plot"),
      tableOutput("pre_my_table")
    ) 
  )
  
  return(pre_ui)
}