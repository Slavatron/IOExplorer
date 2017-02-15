# IMPORT DATA
dd = read.table("clonality.data.table.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# ADD CLONAL GROWTH COLUMN
dd$Clonal_Growth = 0
dd[dd$clonal2 == "positive selection",]$Clonal_Growth = 1
dd[dd$clonal2 == "negative selection",]$Clonal_Growth = -1



############################
####### SHINY MODULE #######
ClonalityUI = function(id) {
  ns = NS(id)
  tagList(
    sidebarPanel(width = 3,
                 wellPanel(
                   radioButtons(ns("plot_type"), "Plot Type", choices = c("Clonal" = "clonal", "Selection" = "selection")),
                   radioButtons(ns("ccf_type"), "CCF Method", choices = c("Absolute" = "absolute", "Pyclone" = "pyclone")),
                   checkboxInput(ns("CI"), "Include Confidence Interval"),
                   sliderInput(ns("threshold_slider"), label = "Threshold", min = 0.5, max = 0.99, value = 0.95, round = -2)
                 )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Clonality Barplot",
                 radioButtons(ns("bar_type"), label = "", choices = list("Number of Variants" = "1", "Relative Frequencies" = "2")),
                 plotOutput(ns("clone_bar"))
        ),
        tabPanel("Clonality Boxplot",
                 plotOutput(ns("clone_box"))
        ),
        tabPanel("Waterfall Plot",
                 plotOutput(ns("Waterfall_Plot"))
        ),
        tabPanel("Survival",
                 radioButtons(ns("Survival_Type"), label = "Outcome to Analyze", choices = list("Overal Survival (OS)" = 1, "Progression Free Survival (PFS)" = 2), inline = TRUE, selected = 1),
                 plotOutput(ns("Survival_Plot"))
        ),
        tabPanel("Density Plot",
                 "<Explain how to read interpret these plots>",
                 plotOutput(ns("Density_Plot")),
                 wellPanel(
                   uiOutput(ns("density_patient_selection"))
                 )
        )
      )
    )
  )
}

Clonality = function(input, output, session, my_data, clin) {
  ns = session$ns
#  # ...
#  brpct = clin[, c('Sample', 'BRPCT')]
#  brpct$Sample = gsub('_.*', '', brpct$Sample)
#  brpct = unique(brpct)
#  rownames(brpct) = brpct[,1]
#  
#  #response class per sample
#  my_response = unique(data.frame(gsub('_.*', '', clin$Sample), clin$myBOR))
#  my_class = as.character(my_response[,2])
#  names(my_class) = my_response[,1]
  my_response = clin[,c("PatientID.x", "myBOR")]
  my_class = as.character(my_response[,2])
  names(my_class) = my_response[,1]
  
  
  # DEFINE COLOR SCHEME FOR CLONALITY BARPLOTS
#  reds = brewer.pal(6, 'PuRd')
#  greens = brewer.pal(6, 'Blues')
#  yellows = brewer.pal(7, 'Greys')
#  
#  cc2 = c(reds[4], yellows[3], greens[4])
#  cc1 = c(reds[4:2], yellows[3:2], greens[3:1])
  
  
  # CLONALITY BARPLOT
  output$clone_bar = renderPlot({
#    plot(1:10, main = paste(length(my_class)))
    if (input$bar_type == "2") {
      make.barplot(my_data, plot.type = input$plot_type, ccf.type = input$ccf_type, threshold = input$threshold_slider, my_class = my_class, relative = T)
    } else {
      make.barplot(my_data, plot.type = input$plot_type, ccf.type = input$ccf_type, threshold = input$threshold_slider, my_class = my_class)
    }
    
  })
  # CLONALITY BOXPLOTS
  output$clone_box = renderPlot({
    make.boxplot(dd, plot.type = input$plot_type, ccf.type = input$ccf_type, threshold = input$threshold_slider, ci = input$CI, my_class = my_class)
  })
  # SURVIVAL PLOTS
  output$Survival_Plot = renderPlot({
    alt.survival.plot(dd, clin, plot.type = input$plot_type, ccf.type = input$ccf_type, threshold = input$threshold_slider, ci = input$CI, surv_type = input$Survival_Type)
  })
  # WATERFALL PLOTS
  output$Waterfall_Plot = renderPlot({
#    plot(1:10, main = paste(names(clin)[ncol(clin)]))
    clonality_waterfall(dd, clin, ccf.type = input$ccf_type, threshold = input$threshold_slider, ci = input$CI)
  })
  # REACTIVE LIST OF AVAILABLE PATIENTS
  pt_list = reactive({
    unique(my_data[,"sample"])
  })
  # CHECKBOX FOR DENSITY PLOT PATIENT SELECTION 
  output$density_patient_selection = renderUI({
    #    radioButtons(ns("dense_pt"), "Select Patient", choices = c("Pt10", "Pt100"))
    #    radioButtons(ns("dense_pt"), "Select Patient", choices = c(pt_list()))
    selectInput(ns("dense_pt"), "Select Patient", choices = c(pt_list()))
  })
  # DENSITY PLOT
  output$Density_Plot = renderPlot({
    #    plot(1:10, main = paste(class(pt_list())))
    plot.density(dd, sample = input$dense_pt, ccf.type = input$ccf_type, threshold = input$threshold_slider, ci = input$CI, my_class = my_class) 
  })
}

