
# CLEVER SURVIVAL FUNCTION
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
  print("Here in gg_surv 2 (start)")
  temp_dat$Var = temp_dat[,v]
  temp_dat$Bin = 0
  temp_dat[temp_dat$Var <= cut,]$Bin = 1
  
  # COUNT SAMPLES IN EACH GROUP
  n1 = sum(temp_dat$Bin)
  n0 = nrow(temp_dat) - n1
  
  # FIT CURVE
  print("Here in gg_surv 2 (fitting)")
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
#    scale_colour_discrete(name = paste(v), breaks = c(0,1), labels = c(paste("Below (n =", n0, ")" ), paste("Above (n =", n1, ")"))) +
    scale_colour_manual(name = "", breaks = c(0,1), 
                        labels = c(paste("Above (n =", n0, ")"), paste("Below (n =", n1, ")")), 
                        values = c("blue", "red")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
  
  print('HERE in gg_Surv 2 (near bot')
  fig2 <- arrangeGrob(fig, sub = textGrob(pv_text, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 14)),
                    heights=c(0.8, 0.2))
  
  return(fig2)
}

niceHist <- function(data, title, cutpoint) {
  my_hist = qplot(data) +
    geom_histogram(fill = "forestgreen") +
    geom_vline(xintercept = cutpoint) +
    ggtitle(paste(title)) +
    theme_minimal() +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5))
  my_hist
}

# FUNCTION FOR PRODUCING BOX PLOTS WITH P VALUES
Fisher_P_Box <- function(df) {
  
}