library(shiny)
source("key_columns.R")

### Layout for Pre-treatment analysis
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
#    plotOutput("pre_os_plot"),
#    plotOutput("pre_pfs_plot"),
    tableOutput("pre_my_table")
  ) 
)
######################################################


### Layout for On-Pre Analysis ###

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

######################################################

#diff_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
clone_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
gene_exp_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
gene_clus_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
tcr_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
cross_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))


tabui <- tabsetPanel(
    tabPanel("Pre Rx & Outcome",pre_ui),
    tabPanel("Clonality Analysis", clone_ui),
    tabPanel("Change on Rx & Outcome", diff_ui),
    tabPanel("Gene Expression & Outcome", gene_exp_ui),
    tabPanel("TCR Analysis", tcr_ui),
    tabPanel("Cross Correlation", cross_ui)
)


ui <- fluidPage(
  # HEADER
  headerPanel(
    list(
      fluidRow(
        column(2,
               imageOutput("header_image",height=100,width=100)),
        column(10,h2("BMS-038 Companion Website",br()),h4("MSKCC/IPOP in collaboration with cBio"))),
      hr())
  ),
  tabui
)

# Define UI for miles per gallon application
shinyUI(ui)
                  
