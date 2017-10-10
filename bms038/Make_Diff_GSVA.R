# FUNCTION FOR CONVERTING DATA.FRAME, myGSVA, TO A TABLE OF DIFFERENCES
#' @param df - data.frame containing user-defined GSVA results
make_diff_gsva = function(df) {
  paired_pt = c("Pt1","Pt10","Pt101","Pt103","Pt106","Pt11","Pt17","Pt18","Pt2","Pt23","Pt26","Pt27","Pt28","Pt3","Pt30","Pt31","Pt34","Pt36","Pt37","Pt38","Pt4","Pt44","Pt46","Pt47","Pt48","Pt49","Pt5","Pt52","Pt62","Pt65","Pt67","Pt77","Pt78","Pt79","Pt8","Pt82","Pt84","Pt85","Pt89","Pt9","Pt92","Pt94","Pt98")
  out_dat = data.frame(PatientID.x = paired_pt)
  for (c in names(df)) {
    new_col = paste("delt",c,sep = ".")
    out_dat[,new_col] = NA
    for (i in paired_pt) {
      pre = paste(i, "pre",sep="_")
      on = paste(i, "on", sep="_")
      out_dat[out_dat$PatientID.x == i,new_col] = df[on,c]-df[pre,c]
    }
  }
  # Clean up output so it's formatted like the input
  my_rows = out_dat$PatientID.x
  if (ncol(out_dat)==2) {
    out_dat = as.data.frame(out_dat[,2])
    rownames(out_dat) = my_rows
    names(out_dat) = paste("delt",names(df), sep=".")
  } else {
    out_dat = out_dat[,-1]
  }
  return(out_dat)
}
