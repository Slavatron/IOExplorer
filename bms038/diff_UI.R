source("key_columns.R")

createDiffTabUI <- function() {
  varList <- names(diff_choice1)
  
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
      h3("Change in Genomic Predictors on Therapy & Outcomes"),
      "The change in a genomic predictor on immunotherapy (increase or decrease) and its relationship with Overall Survival (OS), Progression Free Survival (PFS), 
      and respone can be examined here. First select the type of genomic data you would like to look at \
      on the left; then select the particular genomic feature from the menu below. For example, one can select exome \
      and then pick change in mutation load to look at the relationship of mutation load and survival.
      ",
      p(),
      p(),
      uiOutput("diff_choose_survival_type"),
      p(),
      plotOutput("diff_survival_plot"),
      tableOutput("diff_my_table")
    ) 
  )
  
  return(diff_ui)
}