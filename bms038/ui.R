compiler::enableJIT(0)
library(shiny)
library(plotly)
library(Cairo)

#source("pre_UI.R")
source("Modules.R")
source("diff_UI.R")
source("cross_UI.R")
#source("selector_UI.R")
source("Selector_Module.R")
source("Cross_Correlation_Module.R")
source("Clonality_Module.R")
source("TCR_Module.R")
source("Official_Gene_Expression_Module.R")

options(bitmapType='cairo')


#pre_ui <- createPreTabUI()
diff_ui <- createDiffTabUI()
#cross_ui <- createCrossTabUI()
#cross_diff_ui <- mainPanel(headerPanel("On therapy correlations"), "This will be similiar to pre-therapy correlation tab but with differences instead")
#clone_ui <- mainPanel(h3("Tumor Clonality Analysis"), "analyze tumor clonal changes")
gene_exp_ui <- mainPanel(h3("Gene expression analysis"), "Under development...")
#tcr_ui <- mainPanel(h3("TCR Analysis"), "per Jenny..")
neoantigen_ui <- mainPanel(h3("Neo-antigen Analysis"), "per Vlad...")
#selector_ui <- createSelectorTabUI()
#selector_ui <- mainPanel(h3("Select patients"), "Under development... select patients for analysis")

# HEADER
hPanel <- headerPanel(
  list(fluidRow(
      column(width=2, style='padding-right:0px;',img(src="logo_only.png",height=100, width=100)),
             #imageOutput("header_image",height=100,width=100)),
      column(width=10,offset =0, style='padding:0px;', h2("BMS-038 Companion Website",br()),h4("MSKCC/IPOP in collaboration with cBio"))),
    hr())
)

#tabui <- tabsetPanel(tabPanel("Pre Rx & Outcome",pre_ui),
#    tabPanel("Clonality Analysis", clone_ui),
#    tabPanel("Change on Rx & Outcome", diff_ui),
#    tabPanel("Gene Expression", gene_exp_ui),
#    tabPanel("TCR Analysis", tcr_ui),
#    tabPanel("Cross Correlation", cross_ui),
#    tabPanel("Neo-antigen Analysis", neoantigen_ui)
#)
#ui <- fluidPage(hPanel, tabui)

ui<-navbarPage("BMS038", 
# PATIENT SELECTION MIGHT NEED TO BE THE FIRST TAB IF OTHER TABS USE ITS OUTPUT
           tabPanel("Select Patients", Selection_ModuleUI("GLOBAL", pre_choice1)),

           navbarMenu("Genomics & Outcome", 
#                      tabPanel("Pre-therapy",pre_ui),
                      tabPanel("Pre-therapy", Genomics_OutcomeUI("PRE", pre_choice1)),
#                      tabPanel("Change on therapy", diff_ui)),
                      tabPanel("Change on therapy", Genomics_OutcomeUI("DIFF", diff_choice1))),
#           tabPanel("Clonality", clone_ui),
           tabPanel("Clonality", ClonalityUI("CLONE")),
           navbarMenu("Correlation",
#                         tabPanel("Pre-therapy", cross_ui),
                         tabPanel("Pre-therapy", CrossTabUI("CROSS", pre_choice1)),
#                         tabPanel("Change on therapy", cross_diff_ui)),
                         tabPanel("Change on therapy", CrossTabUI("CROSSDIFF", diff_choice1))),
#           tabPanel("Gene Expr", gene_exp_ui),
	   tabPanel("Gene Expr", GeneExprUI("EXPR", pre_choice1)),	
           tabPanel("TCR", TCR_Freq_DistUI("Test")),
#           tabPanel("Neo-antigens", neoantigen_ui),
#           tabPanel("Select Patients", Selection_ModuleUI("GLOBAL", DEMO_LIST)),
           header=hPanel
           )
  
  



# Define UI for BMS-038 Companion website
shinyUI(ui)
                  
