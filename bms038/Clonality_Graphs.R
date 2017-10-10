# DEFINE COLOR SCHEME FOR CLONALITY BARPLOTS
reds = brewer.pal(6, 'PuRd')
greens = brewer.pal(6, 'Blues')
yellows = brewer.pal(7, 'Greys')

cc2 = c(reds[4], yellows[3], greens[4])
cc1 = c(reds[4:2], yellows[3:2], greens[3:1])

# FUNCTION FOR UPDATING CLONAL GROWTH
# SUMMARY:
# Updates the Clonal_Growth column by assigning values of -1,0,1 to negative selection, no selection, positive selection
#' @param dd clonality-specific dataframe
#' @param clin clinical dataframe with an extra column for Clonal_Growth
update.clonal_growth = function(dd, clin) {
  my_idx = (!is.na(dd$pre.ccf)) & (!is.na(dd$on.ccf))
  dd = dd[my_idx,]
  dd$Clonal_Growth = 0
  dd[dd$clonal2 == "no selection",]$Clonal_Growth = 1
  dd[dd$clonal2 == "negative selection",]$Clonal_Growth = -1

  for (pt in unique(dd$sample)) {
    temp_dd = dd[dd$sample == pt,]

    #  growth_sum = sum(temp_dd$Clonal_Growth)
    growth_sum = (sum(temp_dd$Clonal_Growth)/nrow(temp_dd))
    clin[clin$PatientID.x == pt,]$Clonal_Growth = growth_sum
  }
  return(clin)
}

# FUNCTION FOR WATERFALL PLOTS
# SUMMARY:
# Produces waterfall plot using Clonal_Growth metric with response-colored bars
#' @param dd - clonality-specific dataframe
#' @param clin - clinical dataframe with Clonal_Growth column
#' @param ccf.type - defines which Cancer Cell Fraction measure to use (choose "pyclone" or "absolute")
#' @param threshold - number between 0-1 that sets cut-off for some clonality measuremet...
#' @param ci - boolean value indicating whether or not to use confidence intervals (MIGHT NO MATTER FOR THIS FUNCTION...)
clonality_waterfall = function(dd, clin, ccf.type = "pyclone", threshold = 0.9, ci=F) {
  dd = update.ccf(dd, ccf.type, threshold, ci)
  clin = update.clonal_growth(dd, clin)

  temp_dat = clin[,c("PatientID.x", "Clonal_Growth", "myBOR")]
  temp_dat = temp_dat[temp_dat$PatientID.x %in% dd$sample,]
  temp_dat = temp_dat[order(temp_dat$Clonal_Growth),]
  #wf = data.frame(Pos = 1:nrow(temp_dat), Patient = temp_dat$PatientID.x, Clonal_Growth = temp_dat$Clonal_Growth, Response = temp_dat$myBOR)
  wf = data.frame(Pos = 1:nrow(temp_dat), Patient = as.character(temp_dat$PatientID.x), Clonal_Growth = temp_dat$Clonal_Growth, Response = temp_dat$myBOR)
  pt_names = as.character(wf$Patient)
  wf$Patient = factor(wf$Patient, levels = pt_names)
  w_plot = ggplot(wf, aes(x = Patient, y = Clonal_Growth)) +
    geom_bar(stat = "identity", aes(x = Patient, y = Clonal_Growth, fill = Response)) +
    ggtitle("Genomic Changes (Contraction vs Persistence)") +
    ylab("Net Size Change") +
    xlab("") +
    scale_fill_manual(values = c("PRCR" = "darkgreen", "SD" = "orange", "PD" = "red")) +
#    scale_fill_manual(name = "",
#                        labels = c("PRCR","SD","PD"),
#                        values = c("green", "orange","red")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(text = element_text(size = 16), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5)) +
    theme(axis.text.x = element_text(angle = 90))
  return(w_plot)
}

# DEFINE CLONALITY FUNCTIONS
# SUMMARY:
# Updates clonality-specific dataframe, dd
# ask Nils for details...
update.clonality = function(dd, threshold = .95) {
        dd$clonal = rep(0, nrow(dd))
        dd$clonal[is.na(dd$pre.ccf) | is.na(dd$on.ccf)] = NA
        dd$clonal[dd$on.ccf > threshold & dd$pre.ccf > threshold] = 'clonal both'
        dd$clonal[dd$pre.ccf == 0 & dd$on.ccf > threshold] = 'novel clonal in on'
        dd$clonal[dd$pre.ccf == 0 & dd$on.ccf <= threshold] = 'novel subclonal in on'
        dd$clonal[dd$on.ccf == 0 & dd$pre.ccf <= threshold] = 'lost subclonal pre2on'
        dd$clonal[dd$on.ccf == 0 & dd$pre.ccf > threshold] = 'lost clonal pre2on'
        dd$clonal[dd$clonal == 0] = 'subclonal both'
        dd$clonal[dd$clonal == 'subclonal both'& dd$on.ccf > threshold & dd$pre.ccf <= threshold] = 'increased pre2on'
        dd$clonal[dd$clonal == 'subclonal both'& dd$on.ccf <= threshold & dd$pre.ccf > threshold] = 'decreased pre2on'
        dd$clonal = as.factor(dd$clonal)

        dd$clonal2 = rep(NA, nrow(dd))
        dd$clonal2[dd$clonal == 'clonal both'] = 'no selection'
        dd$clonal2[dd$clonal == 'novel clonal in on'] = 'positive selection'
        dd$clonal2[dd$clonal == 'novel subclonal in on'] = 'positive selection'
        dd$clonal2[dd$clonal == 'lost subclonal pre2on'] = 'negative selection'
        dd$clonal2[dd$clonal == 'lost clonal pre2on'] = 'negative selection'
        dd$clonal2[dd$clonal == 'subclonal both'] = 'no selection'
        dd$clonal2[dd$clonal == 'increased pre2on'] = 'positive selection'
        dd$clonal2[dd$clonal == 'decreased pre2on'] = 'negative selection'
        dd
}


# FUNCTION FOR UPDATING CCF
# Changes which column gets used to store CCF values
update.ccf = function(dd, ccf.type, threshold = .95, ci = T) {
        if(ccf.type == 'absolute' ) {
                dd$on.ccf = dd$on.ccf.abs
                dd$pre.ccf = dd$pre.ccf.abs
        }
        else if (ccf.type == 'pyclone') {
                if(ci == T) {
                        dd$on.ccf = dd$on.ccf.pyc.ci
                        dd$pre.ccf = dd$pre.ccf.pyc.ci
                }
                else{
                        dd$on.ccf = dd$on.ccf.pyc
                        dd$pre.ccf = dd$pre.ccf.pyc
                }
        }
        dd = update.clonality(dd, threshold)
}

# ask Nils...
update.plot.data = function(dd, plot.type) {
  if(plot.type == 'clonal') {
    data = sapply(split(as.factor(dd$clonal), dd$sample), table)
    data = data[type.order, idx]
  }
  else if(plot.type == 'selection') {
    data = sapply(split(as.factor(dd$clonal2), dd$sample), table)
  }
  data
}

# BARPLOT FUNCTION
# SUMMARY
# Produces three barplots
#' @param dd - clonality-specific dataframe
#' @param plot.type - defines what type of plot to make (choose either 'clonal' or 'selection')
#' @param ccf.type - defines whether to use 'pyclone' or 'absolute' CCF values
#' @param threshold - define what threshold to use
#' @param ci - boolean defining whether or not to use confidence intervals
#' @param relative - boolean defining whether or not to put Y-axis on a relative scale from 0-1
#' @param my_class - object needed to include patient response data (HACK)
make.barplot = function(dd, plot.type, ccf.type, threshold, ci = F, relative = F, my_class) {
  dd = update.ccf(dd, ccf.type, threshold, ci)
  type.order = c('novel clonal in on', 'novel subclonal in on', 'increased pre2on', 'clonal both', 'subclonal both', 'lost clonal pre2on', 'lost subclonal pre2on', 'decreased pre2on')
  samples = as.character(unique(dd$sample))
  sample.count = table(dd$sample)[samples]
  samples = paste(my_class[samples], samples)
  # samples = paste(class(samples), samples)
  idx = order(substr(samples, 1, 2), sample.count)

  if(plot.type == 'clonal') {
    data = sapply(split(as.factor(dd$clonal), dd$sample), table)
    data = data[type.order, idx]
    if(relative) data = t(t(data)/colSums(data))
    barplot(data, beside = F, col = cc1, las = 2, border = NA)
    legend('topleft', legend = rev(rownames(data[type.order,])), fill = rev(cc1))
  }
  if(plot.type == 'selection') {
    data = sapply(split(as.factor(dd$clonal2), dd$sample), table)
    if(relative) data = t(t(data)/colSums(data))
    barplot(data[c('positive selection', 'no selection', 'negative selection'), idx], beside = F, col = cc2, las = 2, border = NA)
    legend('topleft', legend = rev(rownames(data[c('positive selection', 'no selection', 'negative selection'),])), fill = rev(cc2))
  }
}

# BOXPLOT FUNCTION
# SUMMARY:
# Creates three sets of boxplots comparing Selection, Response and on-treatment CCF
#' @param dd - clonality-specific dataframe
#' @param plot.type - defines what type of plot to make (choose either 'clonal' or 'selection')
#' @param ccf.type - defines whether to use 'pyclone' or 'absolute' CCF values
#' @param threshold - define what threshold to use
#' @param ci - boolean defining whether or not to use confidence intervals
#' @param my_class - object needed to include patient response data (HACK)
make.boxplot = function(dd, plot.type, ccf.type, threshold, ci = F, my_class) {
  dd = update.ccf(dd, ccf.type, threshold, ci)
  type.order = c('novel clonal in on', 'novel subclonal in on', 'increased pre2on', 'clonal both', 'subclonal both', 'lost clonal pre2on', 'lost subclonal pre2on', 'decreased pre2on')
  samples = as.character(unique(dd$sample))
  sample.count = table(dd$sample)[samples]
  samples = paste(my_class[samples], samples)
  idx = order(substr(samples, 1, 2), sample.count)

  data = sapply(split(as.factor(dd$clonal2), dd$sample), table)
  data = t(t(data)/colSums(data))
  colnames(data) = paste(my_class[colnames(data)], colnames(data))
  cl = gsub(' .*', '', colnames(data))
  par(mfrow = c(1,3))


  plot_list = list()
  caption_list = list()
  plot_count = 0
  for (i in rownames(data)) {
    y = split(data[i,], cl)
    pv1 = round(t.test(y$PD, y$SD, alternative = 'greater')$p.value, digits = 5);
    pv2 = round(t.test(y$PD, y$SD, alternative = 'less')$p.value, digits = 5);
    label = c('contraction', 'persistence', 'expansion')
    names(label) = c('negative selection', 'no selection', 'positive selection')
    p = boxplot(list(PRCR = y$PRCR, SD = y$SD, PD = y$PD), border = c('darkgreen', 'orange', 'red'), outline = F, ylim = c(0,1), main = paste(label[i], ', P_greater: ', round(pv1, digits=2),', P_less: ', round(pv2, digits=2)))
#    p = boxplot(list(PRCR = y$PRCR, SD = y$SD, PD = y$PD), border = c('red', 'orange', 'darkgreen'), outline = F, ylim = c(0,1), main = paste(i, ', p.vals = ', pv1, pv2))
    #    p = boxplot(list(PRCR = y$PRCR, SD = y$SD, PD = y$PD), border = c('red', 'orange', 'darkgreen'), outline = F, ylim = c(0,1))
    #    cap = paste(i, ', p.vals =', pv1, pv2)
    plot_count = plot_count + 1
    plot_list[[plot_count]] = p
    #    caption_list[[plot_count]] = cap
  }
  for (i in 1:plot_count) {
    print(plot_list[[i]])
  }
  #  grid.arrange(grobs = plot_list, nrow = 3, ncol = 1)
}

# DENSITY PLOT FUNCTION
# SUMMARY:
# Creates density plot of pre-vs-on CCF values
#' @param dd - clonality-specific dataframe
#' @param sample - individual sample to produce plot of their data
#' @param plot.type - defines what type of plot to make (choose either 'clonal' or 'selection')
#' @param ccf.type - defines whether to use 'pyclone' or 'absolute' CCF values
#' @param threshold - define what threshold to use
#' @param ci - boolean defining whether or not to use confidence intervals
#' @param my_class - object needed to include patient response data (HACK)
plot.density = function(dd, sample, ccf.type, threshold, ci = F, my_class) {
  dd = update.ccf(dd, ccf.type, threshold, ci)
  idx = as.character(dd$sample) == sample
  data = cbind(dd$pre.ccf[idx], dd$on.ccf[idx])
  data = data[rowSums(is.na(data)) == 0,]
  data[,1] = jitter(data[,1])
  data[,2] = jitter(data[,2])
  fhat = kde(x=as.data.frame(data));
  plot(fhat, main = paste(my_class[sample], sample), xlim=c(-.3, 1.3), ylim = c(-.3, 1.3), display='filled.contour2', cont = seq(0,100,1),xlab = 'pre ccf', ylab = 'on ccf')
  # plot(fhat, main = paste(sample), xlim=c(-.3, 1.3), ylim = c(-.3, 1.3), display='filled.contour2', cont = seq(0,100,1),xlab = 'pre ccf', ylab = 'on ccf')
  points(data, cex=.2, pch=16)
}

# SURVIVAL PLOT FUNCTION
# SUMMARY:
# Produces survival plot comparing patiens with net size change above or below 0
#' @param dd - clonality-specific dataframe
#' @param clin - clinical data with Clonal_Growth column added
#' @param plot.type - defines what type of plot to make (choose either 'clonal' or 'selection')
#' @param ccf.type - defines whether to use 'pyclone' or 'absolute' CCF values
#' @param threshold - define what threshold to use
#' @param ci - boolean defining whether or not to use confidence intervals
alt.survival.plot = function(dd, clin, plot.type, ccf.type, threshold, ci=F, surv_type = 1) {
  dd = update.ccf(dd, ccf.type, threshold, ci)

  samples = as.character(unique(dd$sample))

  # LOOP THROUGH SAMPLES TO CLASSIFY EACH AS GROWING OR SHRINKING
  clin = update.clonal_growth(dd, clin)
  # CREATE MINI CLINICAL TABLE
  my_clin = clin[clin$PatientID.x %in% samples,c("PatientID.x", "SampleType", "OSWK", "OS_event", "PFSWK", "PFS_event", "Clonal_Growth")]

  if (surv_type == 1) {
    surv_obj = clever_gg_surv(my_clin, "OSWK", "OS_event", "Clonal_Growth", cut = 0)
  } else {
    surv_obj = clever_gg_surv(my_clin, "PFSWK", "PFS_event", "Clonal_Growth", cut = 0)
  }

  return(grid.draw(surv_obj))
}
