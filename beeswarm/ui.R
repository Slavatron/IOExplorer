library(shiny)

# Define UI 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Covered in Bees"),

  # SELECT GROUPS FOR COMPARISON
  sidebarPanel(
  checkboxGroupInput("Group", 
			label = h3("Choose Groups"),
			choices = list("Pre-Treatment" = 1,
					"On-Treatment" = 2),
			selected = c(1,2)),

  # Sidebar with controls to select the variable to plot on Y-axis
    selectInput("Y_Axis", "Y-Axis Variable:",
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
  # Dropdown menu for color-coded variables
  selectInput("Color", "Color-Coded Variable:",
		list("Subtype",
			"Cohort",
			"Sex",
			"Response")),
  # Download Buttons
  downloadButton('downloadTable', 'Download Table'),
  downloadButton('downloadPlot', 'Download Plot')
#    uiOutput("Second"),
#    uiOutput("Slider"),
#    plotOutput("slide_hist")
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput("caption")),

    plotOutput("swarm_plot"),


    tableOutput("my_table")
  )
))
