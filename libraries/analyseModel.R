analyseModel=function(ds, observed, model, modelName) {
  
  
  ds$observed=observed
  ds$predicted=predict(model, newdata = ds, type="response")
 
  library(pROC)
  rocCurve=roc(ds$observed, ds$predicted)
  aucValue=auc(rocCurve)
  print(modelName)
  print (aucValue)
  # print(paste("AUC:", aucValue))
  plot(rocCurve, col="blue", main="ROC")
  ds$residuals=residuals(model, newdata = ds, type="pearson") #I want to see normality, variability, outliers 
  print(paste("AUC:", aucValue))
  print  (shapiro.test(ds$residuals) )
  
  mean_exp=mean(ds$residuals, na.rm=TRUE)  # Среднее значение
  sd_exp=sd(ds$residuals, na.rm=TRUE)      # Стандартное отклонение
  xname=paste("Disribution of Pearson residuals of ",modelName)
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
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
 
  
}


