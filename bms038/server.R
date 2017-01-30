library(shiny)
require(ggplot2)
require(plotly)
require(survival)
require(GGally)
require(grid)
require(gridExtra)
require(plotly)

source("key_columns.R")
source("graphs.R")
#source("pre_tab_logic.R")
source("Modules.R")
source("diff_tab_logic.R")
source("cross_tab_logic.R")
#source("selector_tab_logic.R")
source("Selector_Module.R")

# READ IN FILE
predat = read.csv("bms038_data_122016.csv")
# REMOVE ON-TREATMENT SAMPLES
predat = predat[predat$SampleType == "pre",]
predat$myBOR <- factor(predat$myBOR, levels=c("PRCR","SD","PD"), ordered = TRUE)
full_dat = predat
half_dat = predat[1:40,]
preondat <- read.csv("bms038_preon_122016.csv")

# DEFINE SERVER LOGIC
shinyServer(function(input, output) {

# CREATE REACTIVE DATA FRAME TO TRACK PATIENT SELECTION
  Filtered_Pre_Data = callModule(Selection_Module, "GLOBAL", DEMO_LIST, predat)

# PULL SAMPLE LIST OUT OF FILTERED DATA TO USE
  Filt_Preon = reactive({
    my_samples = Filtered_Pre_Data()[,"PatientID.x"]
    my_dat = preondat[preondat$id %in% my_samples,]
    return(my_dat)
  })

# FEED REACTIVE DATA FRAME INTO OTHER TABS AS A FUNCTION CALL
  callModule(Genomics_Outcome, "PRE", pre_choice1, Filtered_Pre_Data)
#  callModule(Genomics_Outcome, "PRE", pre_choice1, predat)


#  createDiffTab(input, output, preondat)
  callModule(Genomics_Outcome, "PREON", diff_choice1, Filt_Preon)

 createCrossCorrTab(input, output, predat)
#  createSelectorTab(input, output, values$predat_1)
})
