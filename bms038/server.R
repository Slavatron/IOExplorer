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

# READ IN FILE
predat = read.csv("bms038_data_122016.csv")
# REMOVE ON-TREATMENT SAMPLES
predat = predat[predat$SampleType == "pre",]
predat$myBOR <- factor(predat$myBOR, levels=c("PRCR","SD","PD"), ordered = TRUE)

preondat <- read.csv("bms038_preon_122016.csv")

# DEFINE SERVER LOGIC
shinyServer(function(input, output) {
  createPreTab(input, output,predat)
  createDiffTab(input, output, preondat)
  createCrossCorrTab(input, output, predat)

})
