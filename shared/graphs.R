
# FUNCTION: clever_gg_surv()
# SUMMARY: 
# Creates a plot summarizing survival analysis based on a single predictive variable. Predictive variable is treated as both a continuous predictor and as a binary variable based on a cut-point. 
# 
#' @param df name of dataframe
#' @param t quoted name of column in dataframe containing survival time as a number, NOT a date
#' @param e quoted name of column in dataframe containing censorship data as 1's and 0's
#' @param v quoted name of column containing variable being used to predict survival; coxph p-value will be based on this value
#' @param cut cut-point within the range of value, v; samples above and below this cut-point will be used to produce separate K-M curves and to calculate a log-rank p-value comparing the two curves
#' @param tit Title of graph (default = Survival)
#' @param ylab name of the variable, v, to display on the y-axis (default = Survival)
#' @param xlab label to display on the x-axis (default = "Weeks")
#' 
#' @return a survival plot
clever_gg_surv = function(df, t, e, v, cut, tit="Survival", ylabT="Fraction Surviving", xlabT="Weeks") {

  temp_dat = df[,c(t,e,v)]
  # REMOVE NA VALUES FROM VARIABLE
  temp_dat = temp_dat[! is.na(temp_dat[,v]),]
  # DEFINE CUT-POINT AS MEDIAN IF UNDEFINED
  if(missing(cut)) {
    cut = median(temp_dat[,v])
  }
  #print("Here in gg_surv 2 (start)")
  temp_dat$Var = temp_dat[,v]
  temp_dat$Bin = 0
  temp_dat[temp_dat$Var <= cut,]$Bin = 1
  
  # COUNT SAMPLES IN EACH GROUP
  n1 = sum(temp_dat$Bin)
  n0 = nrow(temp_dat) - n1
  
  # FIT CURVE
  #print("Here in gg_surv 2 (fitting)")
  fit1 = survfit(Surv(temp_dat[,t], temp_dat[,e]) ~ temp_dat$Bin)
  log_rank = survdiff(Surv(temp_dat[,t], temp_dat[,e]) ~ temp_dat$Bin)
  pval = 1 - pchisq(log_rank$chisq, 1)
  cox_obj = coxph(Surv(df[,t], df[,e]) ~ df[,v])
  cox_pval = summary(cox_obj)$logtest[3]
  
  pv_text <- paste( "\nLog-Rank P-value =", round(pval, digits = 3), 
                    "\nHazard Ratio (HR) =", round(cox_pval, digits = 2),
                    "\nCut-Point: ", cut)
  
  print(pv_text)

  fig <- ggsurv(fit1) + 
    ylab(ylabT) + 
    xlab(xlabT) + 
    ggtitle(tit) + 
    guides(linetype = F) +
    # fix issues with axis starting point
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits=c(0,1)) +
#    scale_colour_discrete(name = paste(v), breaks = c(0,1), labels = c(paste("Below (n =", n0, ")" ), paste("Above (n =", n1, ")"))) +
    scale_colour_manual(name = "", breaks = c(0,1), 
                        labels = c(paste("Above (n =", n0, ")"), paste("Below (n =", n1, ")")), 
                        values = c("blue", "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
    
  
  #print('HERE in gg_Surv 2 (near bot')
  fig2 <- arrangeGrob(fig, sub = textGrob(pv_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)),
                    heights=c(0.8, 0.2))
  
  return(fig2)
}

# FUNCTION: niceHist()
# SUMMARY: 
# Creates a histogram with a vertical line bisecting the data
#' @param data to be plotted as histogram
#' @param title for the plot
#' @param cutpoint within the range of data to be displayed as a vertical line
niceHist <- function(data, title, cutpoint) {
  my_hist = qplot(data) +
    geom_histogram(fill = "forestgreen") +
    geom_vline(xintercept = cutpoint) +
    ggtitle(paste(title)) +
    theme_minimal() +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
  my_hist
}

# FUNCTION: Count_Data_Barplot()
# SUMMARY: 
# Compares two categorical variables. 
# Returns a plot, contingency table, Chi-Square/Fisher Test results and Chi-Square/Fisher Test p-values
#' @param df name of dataframe
#' @param x categorical variable displayed on x-axis
#' @param g categorical variable displayed as groups
#' @param x_lab optional input for labelling x-axis
#' @param g_lab optional input for labelling groups
#' @param title optional input for plot's title
Count_Data_Barplot = function(df, x, g, x_lab, g_lab, title) {
  # ENSURE ARGUMENTS ARE VALID
  if (!class(df) == "data.frame") { stop("df should be a data.frame")}
  if (!x %in% names(df)) { stop("x is not a variable in df")}
  if (!class(df[,x]) %in% c("logical", "factor", "integer")) { stop("x should be a categorical variable")}
  if (!g %in% names(df)) { stop("g is not a variable in df")}
  if (!class(df[,g]) %in% c("logical", "factor", "integer")) { stop("g should be a categorical variable")}
  # ASSIGN TITLE AND LABEL NAMES IF NOT PROVIDED
  if (missing(title)) {title = ""}
  if (missing(x_lab)) {x_lab = x}
  if (missing(g_lab)) {g_lab = "Group"}
  # REMOVE ROWS LACKING DATA
  df = df[!is.na(df[,x]),]
  df = df[!is.na(df[,g]),]
  # MAKE SURE THERE AREN'T TOO MANY VALUES OF EITHER VARIABLE
  # (CONTINGENCY TABLES LARGER THAN 4 x 3 HAVEN'T WORKED)
  if (length(unique(df[,x])) > 3) { stop("x should have 3 or fewer values")}
  if (length(unique(df[,g])) > 4) { stop("g should have 4 or fewer values")}
  # CREATE CONTINGENCY TABLE
  C_table = table(df[,x], df[,g])
  # EXECUTE CHI-SQUARE TEST
  Chi_Obj = chisq.test(C_table)
  Chi_P = Chi_Obj$p.value
  # EXECUTE FISHER TEST
  Fish_Obj = fisher.test(C_table)
  Fish_P = Fish_Obj$p.value
  # CREATE PLOTTING TABLE
  P_table = data.frame(Count = as.numeric(C_table), Group = rep(c(dimnames(C_table)[[1]]), ncol(C_table)), Var = rep(c(dimnames(C_table)[[2]]), each = nrow(C_table)))  
  # RENAME 'GROUP' COLUMN TO MATCH LABEL
  names(P_table)[2] = g_lab
  # CREATE GGPLOT OBJECT
  p = ggplot(P_table, aes(x = Var, y = Count)) + 
    geom_bar(aes_string(x = "Var", y = "Count", fill = g_lab), position = "dodge", stat = "identity") + 
    ggtitle(paste(title)) +
    xlab(paste(x_lab)) + 
    theme_minimal() +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
  # PUT ALL OUTPUTS INTO A LIST OBJECT
  out_list = list("ggplot Object" = p, "Contingency Table" = C_table, "Chi-Squared Test Results" = Chi_Obj, "Chi-Squared Test P-Value" = Chi_P, "Fisher Test Results" = Fish_Obj, "Fisher Test P-Value" = Fish_P)
}

# FUNCTION: clever_gg_boxplot()
# SUMMARY: 
# Compares continuous and categorical variables.
# Returns ggplot object and T-Test/Wilcoxon Rank-Sum test results, 
#' @param df name of dataframe
#' @param x categorical variable
#' @param y continuous variable displayed 
#' @param x_lab optional input for labelling x-axis
#' @param y_lab optional input for labelling y-axis
#' @param title optional input for plot's title
clever_gg_boxplot = function(df, x, y, x_lab, y_lab, title) {
  # ENSURE ARGUMENTS ARE VALID
  if (!class(df) == "data.frame") { stop("df should be a data.frame")}
  if (!x %in% names(df)) { stop("x is not a variable in df")}
  if (!class(df[,x]) %in% c("logical", "factor", "integer")) { stop("x should be a categorical variable")}
  if (!y %in% names(df)) { stop("y is not a variable in df")}
  if (!class(df[,y]) %in% c("numeric", "integer")) { stop("x should be a continuous variable")}
  # ASSIGN TITLE AND LABEL NAMES IF NOT PROVIDED
  if (missing(title)) {title = ""}
  if (missing(x_lab)) {x_lab = ""}
  if (missing(y_lab)) {y_lab = y}
  # REMOVE ROWS LACKING DATA
  df = df[!is.na(df[,x]),]
  df = df[!is.na(df[,y]),]
  
  # maybe do some other quality control...?
  
  # GET SUMMARY TABLE FOR CATEGORICAL VARIABLE
  C_table = table(df[,x])
  # COMBINE COUNTS AND NAMES INTO CHARACTER VECTOR
  C_names = as.character(unlist(dimnames(C_table)))
  C_nums = as.numeric(C_table)
  C_labels = paste(C_names, "\n(n = ", C_nums, ")", sep="")
  # RUN PAIR-WISE TESTS:
  T_obj = pairwise.t.test(df[,y], df[,x])
  W_obj = pairwise.wilcox.test(df[,y], df[,x])
  # CREATE GGPLOT OBJECT
  p = ggplot(df, aes_string(x = x, y = y)) +
    geom_boxplot(aes_string(fill = x)) +
    ggtitle(paste(title)) +
    xlab(paste(x_lab)) + 
    scale_x_discrete(labels = C_labels) +
    ylab(paste(y_lab)) +
    theme_minimal() +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5), legend.position = "none")
  out_list = list("ggplot Object" = p, "Group Count Table" = C_table, "T-Test Object" = T_obj, "Wilcoxon Rank Sum Object" = W_obj)
}

# FUNCTION FOR PRODUCING BOX PLOTS WITH P VALUES
Fisher_P_Box <- function(df) {
  
}