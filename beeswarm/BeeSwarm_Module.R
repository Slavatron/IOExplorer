# Beeswarm Module
library(shiny)
require(beeswarm)
require(ggplot2)
require(xlsx)
require(grid)
require(gridExtra)

#' Function 'SmartSwarm' for making plots and calculating descriptive statistics
#' @param xdat - data.frame containing data to analyze
#' @param Y_Var - numeric variable from xdat used as y-axis
#' @param X_Var - categorical variable from xdat used as x-axis
#' @param Color_Var - categorical variable used for coloring points
#' @param adjust_p_method = optional vector for adjusting p-values
#' @param X_Labels - optional vector of group names for the x-axis
SmartSwarm <- function(xdat, Y_Var, X_Var, Color_Var, X_Labels = unique(xdat[,X_Var]), adjust_p_methods = "none") {
  # DEAL WITH NA VALUES
  xdat = xdat[!is.na(xdat[,X_Var]),]
  X_Labels = unique(xdat[,X_Var])
  # DEFINE BEESWARM OBJECT
  B_Swarm = beeswarm(xdat[,Y_Var] ~ xdat[,X_Var], data = xdat, pwcol = xdat[,Color_Var], do.plot = F )
  # RENAME COLOR-CODING VARIABLE
  names(B_Swarm)[4] = Color_Var
  # CALCULATE DESCRIPTIVE STATS FOR PLOT
  d_tab = as.data.frame(table(xdat[,X_Var]))
  names(d_tab) = c("Var", "N")
  d_tab$Mean = 0
  d_tab$Median = 0
  for (i in unique(xdat[,X_Var])) {
    temp_dat = xdat[which(xdat[,X_Var] == i),]
    m = mean(temp_dat[,Y_Var], na.rm = T)
    med = median(temp_dat[,Y_Var], na.rm = T)
    d_tab[d_tab$Var == i,"Mean"] = round(m, digits = 4)
    d_tab[d_tab$Var == i, "Median"] = round(med, digits = 4)
  }
  Willy = pairwise.wilcox.test(xdat[,Y_Var], xdat[,X_Var], p.adjust.method = adjust_p_methods)
  Willy_P = round(Willy$p.value, digits = 4)
  # CREATE PLOT
  swarm_plot = ggplot(B_Swarm, aes(x,y)) + 
    
    geom_point(aes_string(colour = Color_Var, fill = Color_Var)) + 
    #		labs(title = input$Color) +
    xlab("Groups") +
    ylab(paste(Y_Var)) +
    scale_x_continuous(breaks = c(1:length(unique(B_Swarm$x.orig))), labels = X_Labels) +
    theme(text = element_text(size = 16)) +
    theme_bw() 
#    ggtitle(paste("Pre-Treatment Mean = ", mean_pre, "\nOn-Treatment Mean =", mean_on, "\nWilcoxon Rank-Sum p-value =", wilcox_p ))
  return(list(swarm_plot, d_tab, Willy_P))
#  return(swarm_plot)
}
###################
# LOAD SUMMARY DATA
zig = read.table("PRE_H_summary.tsv", sep = "\t", header = T)
zag = read.table("ON_H_summary.tsv", sep = "\t", header = T)
hdat = rbind(zig, zag)
# LOAD DATA USED IN MAIN PORTAL
clinical_data = read.csv("../bms038/bms038_data_101017.csv")
# COMBINE
cdat = merge(clinical_data, hdat, by.x = "sample_name", by.y = "SampleName")

###################################
# Define list of Grouping Variables
VAR_NAMES = list("Pre/On Treatment" = "SampleType", "Subtype" = "SubtypeEZ", "Cohort" = "Cohort", "Sex" = "SEX.y", "Response" = "myBOR")
GROUP_NAMES = list()
for (i in 1:length(VAR_NAMES)) {
  N = names(VAR_NAMES)[i]
  Val = VAR_NAMES[[i]]
  Group_Vals = as.character(unique(clinical_data[,Val]))
  Group_Vals = Group_Vals[!is.na(Group_Vals)]
  GROUP_NAMES[[N]] = Group_Vals
}

#############################
### MODULE
BeeSwarmerUI = function(id) {
  ns = NS(id)
  tagList(
    sidebarPanel(
      selectInput(ns("Group_Groups"), label = "Choose Groups", choices = names(VAR_NAMES)),
      conditionalPanel(
        condition = "input$"
      ),
      uiOutput(ns("Group")),
      selectInput(ns("Color_Groups"), label = "Color-Coding Variable:", choices = names(VAR_NAMES)),
#      uiOutput(ns("Color")),
      selectInput(ns("Y_Axis"), "Y-Axis Variable:",
                  list("VJcombos", 
                       "aaCDR3", 
                       "totCDR3",
                       "ntCDR3",
                       "Median_AAperVJ",
                       "Median_VJperAA",
                       "H_vj",
                       "H_aa",
                       "H_tot",
                       "H_nt",
                       "H_delta",
                       "Mean_Haa_perVJ",
                       "Mean_Hvj_perAA")),
#    downloadButton(ns("downloadTable"), "Download Table"),
    downloadButton(ns("downloadPlot"), "Download Plot")
      
    ),
    mainPanel(
#      textOutput(ns("DebugText")),
      fluidRow(
        column(6, 
               h3("Descriptive Statistics"),
               tableOutput(ns("D_Stats"))
               ),
        column(6,
               h3("Pairwise Wilcoxon Test P-Values"),
               tableOutput(ns("P_Vals"))
               )
      ),
      plotOutput(ns("Bees")),
      tableOutput(ns("my_table"))
    )
  )
}
#' Server component
#' @param Merged_Data - merged table containing clinical data points as well as *H_Summary.tsv data 
BeeSwarmer = function(input, output, session, Merged_Data) {
  ns = session$ns
  output$N_Groups = 
  output$Group = renderUI({
    checkboxGroupInput(ns("Group_By"), label = "Select AT LEAST two groups", choices = GROUP_NAMES[[input$Group_Groups]], selected = GROUP_NAMES[[input$Group_Groups]])
  })
  output$Color = renderUI({
    checkboxGroupInput(ns("Color_By"), label = "Color By:", choices = GROUP_NAMES[[input$Color_Groups]], selected = GROUP_NAMES[[input$Color_Groups]])
  })
  GroupID = reactive({
    req(input$Group_Groups)
    my_pos = which(names(VAR_NAMES) == input$Group_Groups)
    my_id = VAR_NAMES[[my_pos]]
    return(my_id)
  })
  ColorID = reactive({
    req(input$Color_Groups)
    my_pos = which(names(VAR_NAMES) == input$Color_Groups)
    my_id = VAR_NAMES[[my_pos]]
    return(my_id)
  })

  # Create filtered version of Merged_Data based on selected groups
  mdat = reactive({
    x = Merged_Data[which(Merged_Data[,GroupID()] %in% input$Group_By),]
    x[,GroupID()] = factor(x[,GroupID()], levels = unique(x[,GroupID()]))
    return(x)
  })
  Swarm_Object = reactive({
    req(GroupID(), ColorID(), mdat())
#    swarm_object = SmartSwarm(cdat, input$Y_Axis, GroupID(), ColorID())
    swarm_object = SmartSwarm(mdat(), input$Y_Axis, GroupID(), ColorID())
    return(swarm_object)
  })
  output$downloadPlot <- downloadHandler(
    filename = "test.png",
    content = function(file) {
      ggsave(file, Swarm_Object()[[1]])
#      ggsave(file, grid.arrange(textGrob(Swarm_Object()[[2]]), textGrob(Swarm_Object()[[3]]), Swarm_Object()[[1]]))
    }
  )
  output$D_Stats = renderTable({
    req(Swarm_Object())
    Swarm_Object()[[2]]
  })
  output$P_Vals = renderTable({
    req(Swarm_Object())
    Swarm_Object()[[3]]
  }, rownames = T)
  output$Bees = renderPlot({
    Swarm_Object()[[1]]
  })
  output$my_table = renderTable({
    Merged_Data[,c("Sample", GroupID(), input$Y_Axis, ColorID())]
  })
  output$DebugText = renderText({
    ColorID()
  })
}


ui = fluidPage(
  BeeSwarmerUI("Test")
)
server = function(input, output) {
  callModule(BeeSwarmer, "Test", cdat)
}

shinyApp(ui, server)