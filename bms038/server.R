library(shiny)
require(ggplot2)
require(plotly)
require(survival)
require(GGally)
require(grid)
require(scales)
require(DT)
require(gridExtra)
require(plotly)
require(Cairo)
require(RColorBrewer)
require(ks)


source("key_columns.R")
source("graphs.R")
#source("pre_tab_logic.R")
source("Modules.R")
source("diff_tab_logic.R")
source("cross_tab_logic.R")
#source("selector_tab_logic.R")
source("Selector_Module.R")
source("Cross_Correlation_Module.R")
source("Clonality_Graphs.R")
source("Clonality_Module.R")
source("TCR_Module.R")


# READ IN FILE
#predat = read.csv("bms038_data_122016.csv")
predat = read.csv("bms038_data_050917.csv")
predat$myBOR <- factor(predat$myBOR, levels=c("PRCR","SD","PD"), ordered = TRUE)
full_dat = predat
# REMOVE ON-TREATMENT SAMPLES
predat = predat[predat$SampleType == "pre",]
####    ####   ####    ####    ####    ####    ####    ####    ####    ####    ####
# Create special data.frame that contains all pre samples plus "only-on" samples
on_only_names = setdiff(full_dat$PatientID.x,predat$PatientID.x)
on_only_dat = rbind(predat,full_dat[full_dat$PatientID.x %in% on_only_names,])
####    ####    ####    ####    ####    ####    ####    ####    ####    ####    ####
#half_dat = predat[1:40,]
#preondat <- read.csv("bms038_preon_122016.csv")
preondat = read.csv("bms038_preon_050917.csv")
# CHANGE ID COLUMN TO MATCH PREDAT
#names(preondat)[2] = "PatientID.x"
preondat$PatientID.x = preondat$id
preondat$myBOR <- factor(preondat$myBOR, levels=c("PRCR","SD","PD"), ordered = TRUE)
preondat$Sample = paste(preondat$PatientID.x, "on", sep="_")
# IMPORT CLONALITY DATASET
dd = read.table("clonality.data.table.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# ADD CLONAL GROWTH COLUMN
dd$Clonal_Growth = 0
# DEFINE CLINICAL TABLE FOR CLONALITY
clin = predat
clin$Clonal_Growth = 0

# DEFINE SERVER LOGIC
shinyServer(function(input, output) {

# CREATE REACTIVE DATA FRAME TO TRACK PATIENT SELECTION
  ####    ####    ####    ####    ####    ####    ####    ####    ####    ####    ####
#  Filtered_Pre_Data = callModule(Selection_Module, "GLOBAL", pre_choice1, predat)
  Filtered_Data = callModule(Selection_Module, "GLOBAL", pre_choice1, on_only_dat)
  Filtered_Pre_Data = reactive({
    my_dat = Filtered_Data()
    my_dat = my_dat[my_dat$SampleType == "pre",]
  })
  ####    ####    ####    ####    ####    ####    ####    ####    ####    ####    ####
# PULL SAMPLE LIST OUT OF FILTERED DATA TO USE
  Filt_Preon = reactive({
    my_samples = Filtered_Pre_Data()[,"PatientID.x"]
    my_dat = preondat[preondat$id %in% my_samples,]
    return(my_dat)
  })

  Filt_Full = reactive({
#    my_samples = Filtered_Pre_Data()[,"PatientID.x"]
    ####    ####    ####    ####    ####    ####    ####
    my_samples = Filtered_Data()[,"PatientID.x"]
    ####    ####    ####    ####    ####    ####    ####
    my_dat = full_dat[full_dat$PatientID.x %in% my_samples,]
    return(my_dat)
   
  })

# FEED REACTIVE DATA FRAME INTO OTHER TABS AS A FUNCTION CALL
  callModule(Genomics_Outcome, "PRE", pre_choice1, Filtered_Pre_Data, myGSVA)
#  callModule(Genomics_Outcome, "PRE", pre_choice1, predat)


#  createDiffTab(input, output, preondat)
  callModule(Genomics_Outcome, "DIFF", diff_choice1, Filt_Preon, myGSVA)

# createCrossCorrTab(input, output, predat)
  callModule(CrossTab, "CROSS", pre_choice1, Filtered_Pre_Data)
  callModule(CrossTab, "CROSSDIFF", diff_choice1, Filt_Preon)
#  createSelectorTab(input, output, values$predat_1)
  
  callModule(Clonality, "CLONE", dd, clin)

  callModule(TCR_Freq_Dist, "Test")

  myGSVA = callModule(GeneExpr, "EXPR", pre_choice1, Filt_Full)

})
