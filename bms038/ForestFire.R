library(forestplot)
#' Function for calculating coxPH for a set of values and producing a forest plot
#' @param df dataframe containing all data to be used
#' @param s_type survival type can be either "OS" or "PFS"
#' @param p1 predictive variable for survival model
#' @param ... additional variables to use as predictors
forestfire = function(df, s_type = "OS", p1, ...) {
  # DEFINE SURVIVAL TYPE STRICTLY TO AVOID PROBLEMS
  if (! s_type %in% c("OS", "PFS")) {
    s_type = "OS"
  }
  s_time = paste(s_type, "WK", sep = "")
  s_event = paste(s_type, "event", sep = "_")
  # COUNT AND REMOVE NA VALUES
  text_table = data.frame("Pred" = NA, "NA_Count" = NA)
  for (i in c(p1, ...)) {
    n = nrow(df[is.na(df[,i]),])
    text_table[i, "Pred"] = i
    text_table[i,"NA_Count"] = n
  }
  text_table = text_table[-1,]
  temp_dat = df[,c("PatientID.x", s_time, s_event, p1, ...)]
  temp_dat = temp_dat[complete.cases(temp_dat),]
  # COUNT NUMBER OF PATIENTS ACTUALLY USED
  n_used = nrow(temp_dat)
  # DEFINE FORMULA
  # Count arguments supplied to define formula appropriately
  my_nargs = nargs()
  if (my_nargs == 3) {
#    display_formula = paste(p1, ..., sep = " + ")
    display_formula = paste(s_type, p1, sep = " ~ ")
#    pred_formula = paste(p1, ..., sep = " + temp_dat$")
    pred_formula = paste("Surv(temp_dat[,s_time], temp_dat[,s_event]) ~ temp_dat$", p1, sep = "")
  }
  if (my_nargs > 3) {
    display_formula = paste(..., collapse = " + ")
    display_formula = paste(p1, display_formula, sep = " + ")
    display_formula = paste(s_type, display_formula, sep = " ~ ")
    pred_formula = paste(..., collapse = " + temp_dat$")
    pred_formula = paste(p1, pred_formula, sep = " + temp_dat$")
    pred_formula = paste("Surv(temp_dat[,s_time], temp_dat[,s_event]) ~ temp_dat$", pred_formula, sep = "")
  }
  print(display_formula)
  print(my_nargs)
  print(class(my_nargs))
  print(pred_formula)
  print(".....................................................")
  # FIT COX-PH MODEL
  cox_obj = coxph(as.formula(pred_formula))
  # EXTRACT HAZARD RATIO AND OTHER COEFFICIENTS 
  cox_summary = summary(cox_obj)
  cox_coef = as.data.frame(cox_summary$coefficients)
  text_table$Pval = round(cox_coef[,5], digits = 5)
  tabletext <- cbind(c("Predictor",text_table$Pred), 
#                     c("Patients Lacking Data",text_table$NA_Count), 
                     c("P Value",text_table$Pval))
  cox_conf.int = as.data.frame(cox_summary$conf.int)
  names(cox_conf.int) = c("HR", "negHR", "LCI", "UCI")
  cox_conf.int$Pval = cox_coef[,5]
  pred_names = list(list(p1, ...))
  # Produce Forest Plot
  fplot = forestplot(labeltext = tabletext,
                     mean = c(NA, as.numeric(cox_conf.int$HR)), 
                     lower = c(NA, as.numeric(cox_conf.int$LCI)), 
                     upper = c(NA, as.numeric(cox_conf.int$UCI)),
                     title = paste(display_formula),
                     graph.pos = 3,
                     txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                    ticks=gpar(cex=1.1),
                                    xlab=gpar(cex = 1.2),
                                    title=gpar(cex = 1.2)),
                     col=fpColors(box="black", lines="black", zero = "gray50"),
                     zero=1, 
                     cex=0.9, 
                     lineheight = "auto", 
                     xlog = TRUE,
                     boxsize=0.3, 
                     colgap=unit(6,"mm"),
                     lwd.ci=2, 
                     ci.vertices=TRUE, 
                     ci.vertices.height = 0.3
                     )
  return(fplot)
}

