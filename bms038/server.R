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
source("pre_tab_logic.R")
source("diff_tab_logic.R")
source("cross_tab_logic.R")
source("selector_tab_logic.R")

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
  values = reactiveValues(predat_1 = predat)
  observeEvent(input$set_filter, {
    temp1 = half_dat
    values$predat_1 = temp1
 })
  observeEvent(input$clear_filter, {
    temp2 = full_dat
    values$predat_1 = temp2
 })
  createPreTab(input, output,predat)
  createDiffTab(input, output, preondat)
  createCrossCorrTab(input, output, predat)
  createSelectorTab(input, output, values$predat_1)
})
