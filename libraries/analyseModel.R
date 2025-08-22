analyseModel=function(ds, observed, model, modelName) {
  
  
  ds$observed=as.numeric(as.character(observed)) 
  #it was my crazy mistake! as.factor(0) converted as first category and became as.numeric(1)!
  ds$predicted=predict(model, newdata = ds, type="response")

  library(pROC)
  rocCurve=roc(ds$observed, ds$predicted)
  aucValue=auc(rocCurve)
  print(modelName)
  print (aucValue)
  # print(paste("AUC:", aucValue))
  plot(rocCurve, col="blue", main="ROC")
 # ds$residuals=residuals(model, newdata = ds, type="pearson") #I want to see normality, variability, outliers 
  ds$residuals = ds$observed
  ds$residuals = ds$observed - ds$predicted
  print(paste("AUC:", aucValue))
  print  (shapiro.test(ds$residuals) )
  
  mean_exp=mean(ds$residuals, na.rm=TRUE)  # Среднее значение
  sd_exp=sd(ds$residuals, na.rm=TRUE)      # Стандартное отклонение
  xname=paste("Disribution of residuals of ",modelName)
  gg=ggplot(ds, aes(residuals)) +
    geom_density(alpha=0.4) + 
    geom_vline(aes(xintercept=median(residuals)), color="darkgrey", linetype="dashed") +
    geom_vline(aes(xintercept=mean(residuals)), color="darkgrey", linetype="solid") +
    stat_function(fun=dnorm, args=list(mean=mean_exp, sd=sd_exp), 
                  color="darkgrey", linetype="solid", size=1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title="Density plot with Expected Normal Distribution",
      subtitle=xname,
      x=xname,
      y="Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-density-residuals-pearson.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/images","/",modelName, "-density-residuals-pearson.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()

  
  observed=as.vector(as.numeric(as.character(ds$observed)))
  predicted=as.vector(ds$predicted)
  # divide data to 10 bins
  data <- data.frame(observed, predicted)
  data <- data %>%
    mutate(bin = cut(predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(mean_predicted = mean(predicted),
              observed_proportion = mean(observed))
  
  gg=ggplot(data, aes(x = mean_predicted, y = observed_proportion)) +
    geom_point(size = 3) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(x = "Mean Predicted Probability",
         y = "Observed Proportion",
         title = "Calibration Curve") +
    theme_minimal()
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-calibration.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/images","/",modelName, "-calibration.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
  gg=ggplot(ds, aes(as.factor(observed),predicted,fill= as.factor(observed)))+
    geom_boxplot(alpha=0.7) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
    geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
    theme_minimal()+
    labs(title="Boxplot: Predicted vs Observed", 
         subtitle=modelName,
         x="Observed", 
         y="Predicted", 
         
         fill="Observed",
         shape="")#,
  plot(gg) 
  
  plotfilename=paste(dir_plots,"/",modelName,"-boxplot-predicted-vs-observed.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/images","/",modelName, "-boxplot-predicted-vs-observed.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  

}#
ADVanalyseModel=function(ds, observed, model, modelName) {# зкаомментить
#
  
    library(ggpubr) #для stat_cor
  library(RColorBrewer)
  gg=ggplot(ds)+ 
    aes(x=observed,y=residuals)+# , shape=factor(Sex)) + 
    geom_count(show.legend=TRUE) +
    theme_minimal()+
    scale_color_brewer(palette="Set1") +
    labs(title="Scatterplot: DC_outcome vs model1Residuals", 
         subtitle =modelName,
         x="Observed", 
         y="Pearson Residuals", )  
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName,"-observed-vs-residuals.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/images","/",modelName, "-observed-vs-residuals.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
  gg=ggplot(ds, aes(observed,predicted,fill= observed))+
    geom_boxplot(alpha=0.7) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
    geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
    theme_minimal()+
    labs(title="Boxplot: Predicted vs Observed", 
         subtitle=modelName,
         x="Observed", 
         y="Predicted", 
        
         fill="Observed",
         shape="")#,
  plot(gg) 
  
  plotfilename=paste(dir_plots,"/",modelName,"-boxplot-predicted-vs-observed.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/images","/",modelName, "-boxplot-predicted-vs-observed.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
 
  
}


