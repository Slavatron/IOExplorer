plot.a.scatter <- function(data,x,y,Stats=T){
  col <- tableau_color_pal('tableau10')(10)[4]
  colors <- tableau_color_pal('tableau10')(10)[2]
  mdf <- data.frame("arg1" = as.numeric(data[,x]),
                    "arg2" = as.numeric(data[,y]))
  
  #Ttest of number of mutation in High score vs Low score
  res<-(summary(lm(arg1~arg2,data = mdf))$r.squared)
  pv <- sprintf("R-square = %.2e", res)
  maxVal <- max(mdf[,"arg2"], na.rm=TRUE)
  if(Stats==T){
  ggplot(data = mdf,aes(x = arg1,y = arg2,colour=arg1))+
    geom_point()+
  stat_smooth(method = "lm", col = col)+
    labs(x=x,y=y)+
    theme_minimal(base_size = 20)+
    theme(legend.position = "none")+
    annotate("text",size=10, label=pv, x=median(mdf[,"arg1"]), y=maxVal*1.05)
  }else{  ggplot(data = mdf,aes(x = arg1,y = arg2,colour=arg1))+
      geom_point()+
      stat_smooth(method = "lm", col = col)+
      labs(x=x,y=y)+
      theme_minimal(base_size = 20)+
      theme(legend.position = "none")}
  
}
plot.a.boxplot <- function(data,x,y,Stats=T,levels=data[,x]){
  mdf <- data.frame("arg1" = as.factor(data[,x]),
                   "arg2" = as.numeric(data[,y]))
  col <- tableau_color_pal('tableau10')(10)
  if(Stats == T){
    res<-t.test(mdf[mdf[,"arg1"]==levels[1],"arg2"], mdf[mdf[,"arg1"]==levels[2],"arg2"])
  resw <- wilcox.test(mdf[mdf[,"arg1"]==levels[1],"arg2"], mdf[mdf[,"arg1"]==levels[2],"arg2"]);
  pv <- paste(paste("Comparing",levels[1],"vs",levels[2],":",sep=" "),
              sprintf("t-test = %.2e wilcox = %.2e", res$p.value, resw["p.value"]),sep="\n")
  maxVal <- max(mdf[,"arg2"], na.rm=TRUE)
  ggboxplot(data = mdf,add.params = list(size=0.5),
            x = "arg1", width = 0.8,
            y = "arg2",
            color = "arg1",
            palette=col,
            add="dotplot")+labs(x=x,y=y)+
    annotate("text", label=pv, x=1.5, size=5, y=maxVal*1.05)+theme(legend.position = "none")
  }else{  ggboxplot(data = mdf,
                    x = "arg1",
                    y = "arg2",
                    color = "arg1",
                    palette=col,
                    add="jitter")+labs(x=x,y=y)}
}
plot.data <- function(mydata,x,y,plot.type = c("boxplot","scatter plot"),...){
  if(!class(mydata[,y])=="numeric")
    stop(paste0("WTF are you trying to plot? Dude..."))
  if(!length(plot.type)==1)
    stop(paste0("\tHuuuu, maybe you could be a little more specific about the type of plot you want...\n\tYou think I'm some sort of psychic?"))
  if(!(plot.type %in% c("boxplot","scatter plot")))
    stop(paste0("\tDude, that's not even a type of plot ! Jesus..."))
  
  if(plot.type == "boxplot"){
    optionalParamNames <-c("Stats","levels")
    params <- list(...)
    unusedParams <- setdiff(names(params),optionalParamNames)
    unusedParams <- setdiff(names(params),optionalParamNames)
    if(length(unusedParams))
      stop(paste0("WTF does '",unusedParams,"' even means here?? "))
    
    plot.a.boxplot(data = mydata,x = x,y = y,...)
  }else{ 
    optionalParamNames <-c("Stats")
    params <- list(...)
    unusedParams <- setdiff(names(params),optionalParamNames)
    unusedParams <- setdiff(names(params),optionalParamNames)
    if(length(unusedParams))
      stop(paste0("WTF does '",unusedParams,"' even means here?? "))
    plot.a.scatter(data = mydata,x = x,y = y,...)}
}


