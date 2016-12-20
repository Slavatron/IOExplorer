library(shiny)
require(beeswarm)
require(ggplot2)

# DEFINE OPTIONS FOR SUBSEQUENT DROP-DOWN MENUS
GROUP_FILES <- c("PRE_H_summary.tsv", "ON_H_summary.tsv")
GROUP_NAMES <- c("Pre-Treatment", "On-Treatment")

COLOR_VARS <- c("Subtype" = "SubtypeEZ", "Cohort" = "Cohort", "Sex" = "SEX", "Response" = "myBOR")


# READ IN FILES
PRE_H = read.table("PRE_H_summary.tsv", header = TRUE, sep = "\t")
ON_H = read.table("ON_H_summary.tsv", header = TRUE, sep = "\t")

# DEFINE SERVER LOGIC
shinyServer(function(input, output) {

  fileName <- reactive({
    paste(input$Group, "_summary.tsv", sep = "")
  }) 

  formulaText <- reactive({
   paste("Beeswarm Plot Comparing", input$Y_Axis)
  })
	
  # RETURN FORMULA TEXT AS A CAPTION
  output$caption <- renderText({
    formulaText()
  })

##### NOT YET FUNCTIONAL !!!
  # SLIDER FOR DEFINING CUT-POINT
  output$Slider = renderUI({
	sliderInput("slide1", label = h3(paste(input$sec_var)), min = min(xdat[,input$sec_var], na.rm = TRUE), max = max(xdat[,input$sec_var], na.rm = TRUE), value = median(xdat[,input$sec_var], na.rm = TRUE), round = FALSE)
  })

##### NOT YET FUNCTIONAL !!!
  # SLIDER-CONTROLLED HISTOGRAM
  output$slide_hist <- renderPlot({
	
	hist(xdat[,input$sec_var], main = paste(input$sec_var))
	abline(v = input$slide1, col = "red")
  })

 
  # BEESWARM
  output$swarm_plot <- renderPlot({
	# READ FILES DEFINED BY GROUP AND BIND INTO A SINGLE TABLE
	clin_dat = read.csv("bms038_data_102316.csv")
	Color_Var = COLOR_VARS[input$Color]
	clin_dat = clin_dat[,c("Sample", Color_Var)]
	xdat = data.frame()
	Reactive_Groups = c()
	for (i in input$Group) {
		I = as.numeric(i)
		infile = GROUP_FILES[I]
#		FILENAME = GROUP_FILES[I]
		Reactive_Groups = c(Reactive_Groups, GROUP_NAMES[I])
#		infile = paste("/Users/kendalls/Jenny/", FILENAME, sep = "")
		i_dat = read.table(infile, header = TRUE, sep = "\t")
		# PARSE 'SampleName'
		split_names = strsplit(as.character(i_dat$SampleName), "_")
		i_dat$PatientID.x = sapply(split_names,`[`,1)
		i_dat$SampleType = sapply(split_names,`[`,2)
		i_dat$Sample = paste(i_dat$PatientID.x, i_dat$SampleType, sep = "_")
		i_dat$Alt_ID = split_names[[1]][3]
		i_dat$Group = i
		xdat = rbind(i_dat, xdat)
#		test = paste(test, infile)
	}
	# CONVERT GROUP VARIABLE TO FACTOR
	xdat$Group = factor(xdat$Group, levels = input$Group)	

	# ADD CLINICAL VARIABLE
	xdat = merge(xdat, clin_dat, by = "Sample")
	# EXPORT CLEAN TABLE
#	output$my_table = renderTable({
#		out_table = xdat[,c("Sample", input$Y_Axis)]
#		out_table
#	})

	# DEFINE BEESWARM OBJECT
	B_Swarm = beeswarm(xdat[,input$Y_Axis] ~ xdat$Group, data = xdat, pwcol = xdat[,Color_Var] )
	# RENAME COLOR-CODING VARIABLE
	names(B_Swarm)[4] = Color_Var
	# CALCULATE DESCRIPTIVE STATS FOR PLOT
	mean_pre = mean(B_Swarm[B_Swarm$x.orig == 1,]$y)
	mean_pre = round(mean_pre, digits = 0)
	mean_on = mean(B_Swarm[B_Swarm$x.orig == 2,]$y)
	mean_on = round(mean_on, digits = 0)
	Wilcoxon_Pre_vs_On = wilcox.test(B_Swarm[B_Swarm$x.orig == 1,]$y, B_Swarm[B_Swarm$x.orig == 2,]$y)
	wilcox_p = round(Wilcoxon_Pre_vs_On$p.value, digits = 4)
	# CREATE PLOT
	ggplot(B_Swarm, aes(x,y)) + 

		geom_point(aes_string(colour = Color_Var, fill = Color_Var)) + 
#		labs(title = input$Color) +
		xlab("Groups") +
		ylab(paste(input$Y_Axis)) +
		scale_x_continuous(breaks = c(1:length(unique(B_Swarm$x.orig))), labels = Reactive_Groups) +
		theme(text = element_text(size = 16)) +
		theme_bw() +
		ggtitle(paste("Pre-Treatment Mean = ", mean_pre, "\nOn-Treatment Mean =", mean_on, "\nWilcoxon Rank-Sum p-value =", wilcox_p ))
#		ggtitle(paste(length(Reactive_Groups), length(unique(B_Swarm$x.orig))))
#		ggtitle(paste(class(B_Swarm$x), class(B_Swarm$y), class(xdat$Group)))
  })

  # RENDER SUMMARY TABLE
  output$my_table <- renderTable({
	# READ FILES DEFINED BY GROUP AND BIND INTO A SINGLE TABLE
	clin_dat = read.csv("bms038_data_102316.csv")
	Color_Var = COLOR_VARS[input$Color]
	clin_dat = clin_dat[,c("Sample", Color_Var)]
	xdat = data.frame()
	Reactive_Groups = c()
	for (i in input$Group) {
		I = as.numeric(i)
		infile = GROUP_FILES[I]
#		FILENAME = GROUP_FILES[I]
		Reactive_Groups = c(Reactive_Groups, GROUP_NAMES[I])
#		infile = paste("/Users/kendalls/Jenny/", FILENAME, sep = "")
		i_dat = read.table(infile, header = TRUE, sep = "\t")
		# PARSE 'SampleName'
		split_names = strsplit(as.character(i_dat$SampleName), "_")
		i_dat$PatientID.x = sapply(split_names,`[`,1)
		i_dat$SampleType = sapply(split_names,`[`,2)
		i_dat$Sample = paste(i_dat$PatientID.x, i_dat$SampleType, sep = "_")
		i_dat$Alt_ID = split_names[[1]][3]
		i_dat$Group = i
		xdat = rbind(i_dat, xdat)
	}
	# CONVERT GROUP VARIABLE TO FACTOR
	xdat$Group = factor(xdat$Group, levels = input$Group)	

	# ADD CLINICAL VARIABLE
	xdat = merge(xdat, clin_dat, by = "Sample")
	# EXPORT CLEAN TABLE
	out_table = xdat[,c("Sample", "Group", input$Y_Axis, Color_Var)]
	out_table
	})

})
