
# DEFINE SURVIVAL FUNCTION
# DOCUMENT the parameters!%$!@$!$
clever_gg_surv = function(df, t, e, v, cut, ylab) {
  temp_dat = df[,c(t,e,v)]
  # REMOVE NA VALUES FROM VARIABLE
  temp_dat = temp_dat[! is.na(temp_dat[,v]),]
  # DEFINE CUT-POINT AS MEDIAN IF UNDEFINED
  if(missing(cut)) {
    cut = median(temp_dat[,v])
  }
  print("Here in gg_surv 2")
  temp_dat$Var = temp_dat[,v]
  temp_dat$Bin = 0
  temp_dat[temp_dat$Var <= cut,]$Bin = 1
  # COUNT SAMPLES IN EACH GROUP
  n1 = sum(temp_dat$Bin)
  n0 = nrow(temp_dat) - n1
  # FIT CURVE
  #  fit1 = survfit(Surv(df[,t], df[,e]) ~ df[,v])
  fit1 = survfit(Surv(temp_dat[,t], temp_dat[,e]) ~ temp_dat$Bin)
  log_rank = survdiff(Surv(temp_dat[,t], temp_dat[,e]) ~ temp_dat$Bin)
  pval = 1 - pchisq(log_rank$chisq, 1)
  cox_obj = coxph(Surv(df[,t], df[,e]) ~ df[,v])
  cox_pval = summary(cox_obj)$logtest[3]
  fig = ggsurv(fit1) +
    ylab(paste(ylab)) +
    xlab("Days") +
    ggtitle(paste("Cut-Point:", v, "=", cut, "\nCox Proportional Hazard Ratio =", round(cox_pval, digits = 4), "\nLog-Rank P-Value=", round(pval, digits = 4))) +
    guides(linetype = F) +
    scale_colour_discrete(name = paste(v), breaks = c(0,1), labels = c(paste("Below (n =", n0, ")" ), paste("Above (n =", n1, ")")))
  #  out = list(fig, pval, log_rank)
  return(fig)
}

niceHist <- function(data, title, cutpoint) {
  hist(data, main = title, xlab="", col="purple")
  abline(v = cutpoint, col = "red")
}


