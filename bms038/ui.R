library(shiny)
library(plotly)

source("pre_UI.R")
source("diff_UI.R")
source("cross_UI.R")



pre_ui <- createPreTabUI()
diff_ui <- createDiffTabUI()
cross_ui <- createCrossTabUI()
clone_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
gene_exp_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
gene_clus_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
tcr_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))
#cross_ui <- mainPanel(headerPanel("Nothing here yet", "junk"))


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
                  
