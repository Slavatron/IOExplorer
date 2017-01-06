source("key_columns.R")

createSelectorTabUI <- function() {
  varList <- names(selector_choice1)
  
  selector_ui <- sidebarLayout(
    sidebarPanel(
      wellPanel(
        selectInput("selector_genomicSpace", "Genomic Space:", varList),
        uiOutput("selector_Second"),
	uiOutput("selector_Ranges"),
	uiOutput("apply_Filters"),
	uiOutput("reset_Filters"),
        uiOutput("selector_Slider")
      ),
      plotOutput("selector_slide_hist")
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
      tableOutput("selector_my_table"),
      uiOutput("selector_choose_survival_type"),
      p(),
      plotOutput("selector_survival_plot")
#      tableOutput("selector_my_table")
    ) 
  )
  
  return(selector_ui)
}
