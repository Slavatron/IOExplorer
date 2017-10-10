# MODULE: Genomics_Outcome()
source("ForestFire.R")
source("ApplyRemoveModule.R")
# SUMMARY:
# Module creates the page "Genomics and Outcome" tab of the BMS038 companion website.
# STILL TO DO:
# - incorporate boxplots showing effect on Response
# - implement patient-selection functionality
# UI COMPONENT OF THE Genomics_Outcome MODULE
#' @param id - namespace identifier for communication between module's UI and server elements
#' @param choices_list_raw - list of lists formatted like what's in key_columns.R
Genomics_OutcomeUI = function(id, choices_list_raw) {
  # DEFINE NAMESPACE TO DISTINGUISH INDIVIDUAL FUNCTION CALLS
  ns = NS(id)
  # USE tagList TO RETURN MULTIPLE UI ELEMENTS
  tagList(
    sidebarPanel(    
      wellPanel(
        # input/output VARIABLES CREATED IN MODULE'S SERVER FUNCTION MUST BE ACCESSED VIA NAMESPACE OBJECT, ns()
#        selectInput(ns("genomicSpace"), "Select Genomic Space:", choices = names(choices_list_raw)),
#        ApplyRemoveUI(ns("INNER")),
        uiOutput(ns("First_Choice")),
        uiOutput(ns("Second_Choice")),
        uiOutput(ns("Hist_Slider"))
      ),
      plotOutput(ns("Cutpoint_Hist"))
    ),
    mainPanel(
      # CAPTION TEXT
      h3(Subtitles_List[[id]]),
      Caption_List[[id]],
      p(),
#      ApplyRemoveUI(ns("INNER")),
      tabsetPanel(
        tabPanel("Survival", 
                 textOutput(ns("debug_Text")),
                 uiOutput(ns("Choose_Survival_Type")),
                 p(),
                 plotOutput(ns("Survival_Plot")),
                 tableOutput(ns("my_Table"))
        ),
        tabPanel("Response",
                 plotOutput(ns("Response_Plot"))
#                 p(),
#                 plotOutput(ns("Response_Plot_2"))
        ),
        tabPanel("Build Your Own Survival Model",
                 uiOutput(ns("Choose_Survival_Type_2")),
#                 uiOutput(ns("Choose_BYO_Vars")),
                 column(4,
                        checkboxGroupInput(ns("Display_Exome"), 
                                           "Exome Variables", 
                                           choices = unname(choices_list_raw[["Exome"]])
                        ),
                        checkboxGroupInput(ns("Display_RNA"), 
                                           "RNASeq Variables", 
                                           choices = unname(choices_list_raw[["RNASeq"]])
                        )
                 ),
                 column(4,
                        checkboxGroupInput(ns("Display_TCR"), 
                                           "TCR Variables", 
                                           choices = unname(choices_list_raw[["TCR"]])
                        ),
                        checkboxGroupInput(ns("Display_ImmDec"), 
                                           "Immune Deconvolution Variables", 
                                           choices = unname(choices_list_raw[["Immune Deconvolution"]])
                        )
                 ),
                 column(4,
                        checkboxGroupInput(ns("Display_IHC"), 
                                           "IHC Variables", 
                                           choices = unname(choices_list_raw[["IHC"]])
                        )
                 ),
                        
#                 checkboxGroupInput(ns("BYO_Vars"), label = h3("Select Predictive Variables"),
#                                    choices = list("Mutation Load" = "log10mut", 
#                                                   "Subtype" = "Subtype",
#                                                   "B.cells.naive" = "B.cells.naive",
#                                                   "B.cells.memory" = "B.cells.memory"), selected = "log10mut"),
                 plotOutput(ns("byoForestPlot"))
#                 plotOutput(ns("byoSurv_Plot"))
                 )
        ),
        ApplyRemoveUI(ns("INNER"))
      )
    )
}
# SERVER COMPONENT OF THE MODULE
#' @param input - required for all Shiny modules
#' @param output - required for all Shiny modules
#' @param session - required for all Shiny modules
#' @param choices_list - list of lists formatted like what's in key_columns.R
#' @param my_data_raw - dataframe containing all data used in module
#' @param my_gsva - reactive value containing results from user-run GSVA analyses
Genomics_Outcome = function(input, output, session, choices_list_raw, filterdata, predat, my_gsva) {
  # DEFINE NAMESPACE OBJECT FROM session OBJECT 
  ns = session$ns
  # RE-DEFINE REACTIVE INPUTS SO THEY CAN BE MODIFIED IF GSVA VALUES GET INTRODUCED
  Saved_GSVA_Values = reactive({
    # Start with an empty data.frame
    out_data = data.frame()
    # Condense GSVA values from my_gsva when there are named entries in it
    if (names(my_gsva) > 0) {
      out_data = do.call("rbind", lapply(reactiveValuesToList(my_gsva), function(x) as.data.frame(t(x[[4]]))))
    }
    out_data = as.data.frame(t(out_data))
    return(out_data)
  })
  choices_list = reactive({
    out_list = choices_list_raw
    if (length(names(my_gsva)) > 0) {
      # Define new list of variables based on Saved_GSVA_Values
      new_vals = names(Saved_GSVA_Values())
      new_list = list(new_vals)
      names(new_list[[1]]) = new_vals
      # Append choices_list with these values
      list_pos = length(out_list)+1
      out_list[[list_pos]] = unlist(new_list)
      names(out_list)[list_pos] = "User-Defined GSVA"
    }
    return(out_list)
  })
  my_data_raw = callModule(ApplyRemove, "INNER", filterdata(), predat)
  my_data = reactive({
    out_data = my_data_raw()
    if (length(names(my_gsva)) > 0) {
      gsva_data = Saved_GSVA_Values()
      # check column names...
      gsva_data$Sample = rownames(gsva_data)
      out_data = merge(out_data, gsva_data, by = "Sample", all.x = TRUE)
    }
    return(out_data)
  })
  output$debug_Text = renderText({
    huh = names(my_gsva)
    length(huh)
#    class(Saved_GSVA_Values())
  })
  output$First_Choice = renderUI({
    selectInput(ns("genomicSpace"), "Select Genomic Space:", choices = names(choices_list()))
  })
  output$Second_Choice = renderUI({
    req(input$genomicSpace)
    # input/output VARIABLES MUSE BE CALLED USING ns()
    radioButtons(ns("Sec_Var"), "Choose Feature", choices = unname(choices_list()[[input$genomicSpace]]))
  })
  # GET IDENTITY OF SELECTED VARIABLE AS COLUMN-NAME IN DATAFRAME
  # Note that reactive variables are functions that return a value
  # So to get this dataframe you must call it with:  getID()
  getID = reactive({
    req(input$genomicSpace, input$Sec_Var)
    my_pos = which(choices_list()[[input$genomicSpace]] == input$Sec_Var)
    tid = names(choices_list()[[input$genomicSpace]])[my_pos]
    return(tid)
  })
  # SLIDER FOR DEFINING CUT-POINT
  output$Hist_Slider = renderUI({
    medv = median(my_data()[,getID()], na.rm = TRUE)
    meanv = mean(my_data()[,getID()], na.rm = TRUE)
    minv = min(my_data()[,getID()], na.rm = TRUE)
    maxv = round(max(my_data()[,getID()], na.rm = TRUE), digits=3)
#    sliderInput(ns("slider_value"), label = h3(paste(input$Sec_Var)), min = minv, max = maxv, value = c(medv, maxv), round = -2)
    # COMPUTE MIDDLE VALUE AND MAKE SURE ITS THE SAME AS THE MINIMUM VALUE
    middle_val = medv
    if (medv == minv) {
      middle_val = meanv
    }
    # set min on range to 0 if no negative #'s
    if (minv > 0 ) { minv <- 0 }
    sliderInput(ns("slider_value"), label = h4("Bisect Patients into Two Groups for Analysis"), min = minv, max = maxv, value = c(minv, middle_val), round = -5)
  })
  # SLIDER-CONTROLLED HISTOGRAM
  output$Cutpoint_Hist = renderPlot({
    req(input$Sec_Var, input$slider_value)
    #    plot(1:10, main = paste(class(my_data())))
    niceHist(my_data()[,getID()], input$Sec_Var, cutpoint1 = input$slider_value[1], cutpoint2 = input$slider_value[2])
  })
  # RADIO BUTTON FOR SURVIVAL TYPE
  output$Choose_Survival_Type = renderUI({
    radioButtons(ns("Survival_Type"), label = "Outcome to Analyze:", choices = list("Overall Survival (OS)" = 1, "Progression Free Survival (PFS)" = 2), inline=TRUE, selected = 1)
  })
  # RADIO BUTTON FOR BUILD-YOUR-OWN SURVIVAL
  output$Choose_Survival_Type_2 = renderUI({
    radioButtons(ns("Survival_Type_2"), label = "Outcome to Analyze:", choices = list("Overall Survival (OS)" = "OS", "Progression Free Survival (PFS)" = "PFS"), inline=TRUE, selected = "OS")
  })
  # CONDENSE VARIABLES INTO MODEL
  ExomeID = reactive({
    my_pos = which(choices_list()[["Exome"]] %in% input$Display_Exome)
    tid = names(choices_list()[["Exome"]])[my_pos]
    return(tid)
  })
  RNAID = reactive({
    my_pos = which(choices_list()[["RNASeq"]] %in% input$Display_RNA)
    tid = names(choices_list()[["RNASeq"]])[my_pos]
    return(tid)
  })
  IHCID = reactive({
    my_pos = which(choices_list()[["IHC"]] %in% input$Display_IHC)
    tid = names(choices_list()[["IHC"]])[my_pos]
    return(tid)
  })
  TCRID = reactive({
    my_pos = which(choices_list()[["TCR"]] %in% input$Display_TCR)
    tid = names(choices_list()[["TCR"]])[my_pos]
    return(tid)
  })
  ImmDecID = reactive({
    my_pos = which(choices_list()[["Immune Deconvolution"]] %in% input$Display_ImmDec)
    tid = names(choices_list()[["Immune Deconvolution"]])[my_pos]
    return(tid)
  })
  BYO_Vars = reactive({
    extra_cols = c(getID(),ExomeID(), IHCID(), RNAID(), TCRID(), ImmDecID())
    extra_cols = unique(extra_cols)
  })
  # BUILD-YOUR-OWN SURVIVAL PLOT
  output$byoForestPlot = renderPlot({
#    req(input$BYO_Vars, input$Survival_Type_2)
    req(BYO_Vars(), input$Survival_Type_2)
    n_vars = length(BYO_Vars())
    if (n_vars == 1) {
      forestfire(my_data(), s_type = input$Survival_Type_2, p1 = BYO_Vars())
    }
    if (n_vars > 1) {
      forestfire(my_data(), s_type = input$Survival_Type_2, p1 = BYO_Vars()[1], BYO_Vars()[2:length(BYO_Vars())])
    }
  })
  # PRIMARY SURVIVAL PLOT
  output$Survival_Plot = renderPlot({
    req(input$Survival_Type, input$Sec_Var, input$slider_value)
    p_switch = input$Survival_Type
    if (p_switch == 1) {
      my_title = paste("OS by", input$Sec_Var)
      my_survival_plot = clever_gg_surv(my_data(), "OSWK", "OS_event", getID(), cut1 = input$slider_value[1], cut2 = input$slider_value[2], my_title)
    } else {
      my_title = paste("PFS by", input$Sec_Var)
      my_survival_plot = clever_gg_surv(my_data(), "PFSWK", "PFS_event", getID(), cut1 = input$slider_value[1], cut2 = input$slider_value[2], my_title)
    }
    return(grid.draw(my_survival_plot))
  })
  # PRODUCE BOXPLOT COMPARING RESPONSE GROUPS TO CONTINUOUS VARIABLE
  output$Response_Plot = renderPlot({
    my_obj = clever_gg_boxplot(my_data(), "myBOR", getID(), title = paste(input$Sec_Var, "vs. Response"))
    # PREPARE P-VALUE CAPTION
    pv_text = paste("Pairwise T-Test P-value =", round(my_obj[[3]]$p.value, digits = 4), "\nPairwise Wilcoxon Test P-value =", round(my_obj[[4]]$p.value, digits = 4), "\nCut-point =", input$slider_value)
    # HARD-CODE COLORS FOR RESPONSE PLOT
    my_plot = my_obj[[1]] +
      scale_fill_manual(values = c("PRCR" = "green3", "SD" = "dodgerblue", "PD" = "red3"))
    resp_box = arrangeGrob(my_plot, sub = textGrob(pv_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)),heights=c(0.8, 0.2))
    return(grid.draw(resp_box))
  })
  # PRODUCE BARPLOTS COMPARING RESPONDERS ABOVE/BELOW CUT-POINT
  output$Response_Plot_2 = renderPlot({
    cut1 = input$slider_value[1]
    cut2 = input$slider_value[2]
    my_var = getID()
    #    Group_Label = as.character(input$Sec_Var)
    temp_data = my_data()
    temp_data$Var = temp_data[,my_var]
    temp_data = temp_data[!is.na(temp_data$Var),]
    
    temp_data$Group = "Not Selected"
    temp_data[temp_data$Var >= cut1 & temp_data$Var <= cut2,"Group"] = "Selected Range"
    temp_data$Group = factor(temp_data$Group)
    
    # PREPARE PLOT/ANALYSIS OBJECT
    obj_2 = Count_Data_Barplot(temp_data, "myBOR", "Group", x_lab = input$Sec_Var, g_lab = "Response", title = "Distribution of Responders Relative Within Selected Range")
    # PREPARE P-VALUES TEXT
    pv_text = paste("Chi-Squared P-value =", round(obj_2[[4]], digits = 4), "\nFisher Test P-value =", round(obj_2[[6]], digits = 4), "\nCut-point =", input$slider_value)
    # HARD-CODE COLORS FOR RESPONSE PLOT
    plot_2 = obj_2[[1]] +
      scale_fill_manual(values = c("PRCR" = "green3", "SD" = "dodgerblue", "PD" = "red3"))
    resp_bar <- arrangeGrob(plot_2, sub = textGrob(pv_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)),heights=c(0.8, 0.2))
    return(grid.draw(resp_bar))
    
  })
  
  # RETURN LIST OF VALUES CREATED BY UI WIDGETS
  return(list(getID, reactive({input$genomicSpace}), reactive({input$Sec_Var})))
}
# LIST OF SUBTITLES FOR EACH PAGE:
# Note: names in this list are meant to mirror the namespace used for each module's calls
Subtitles_List = list(
  "PRE" = "Pre-treatment Genomic Predictors & Outcomes",
  "DIFF" = "Change in Genomic Predictors on Therapy & Outcomes"
)

# LIST OF CAPTIONS FOR EACH PAGE:
Caption_List = list(
  "PRE" = "Pre-treatment genomic predictors and their relationship with Overall Survival (OS), Progression Free Survival (PFS), 
  and respone can be examined here. First select the type of genomic data you would like to look at \
  on the left; then select the particular genomic feature from the menu below. For example, one can select exome \
  and then pick mutation load to look at the relationship of mutation load and survival. Different cutpoints on mutation \
  load can be reviewed by using the slider on the left \
  ",
  "DIFF" = "The change in a genomic predictor on immunotherapy (increase or decrease) and its relationship with Overall Survival (OS), Progression Free Survival (PFS), 
  and respone can be examined here. First select the type of genomic data you would like to look at \
  on the left; then select the particular genomic feature from the menu below. For example, one can select exome \
  and then pick change in mutation load to look at the relationship of mutation load and survival.
  "
)

