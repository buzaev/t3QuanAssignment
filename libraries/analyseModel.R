analyseModel=function(ds, observed, model, modelName) {
  
  
  ds$observed=as.numeric(as.character(observed)) 
  #it was my crazy mistake! as.factor(0) converted as first category and became as.numeric(1)!
  ds$predicted=predict(model, newdata = ds, type="response")

  library(pROC)
  rocCurve=roc(ds$observed, ds$predicted)
  aucValue=auc(rocCurve)
  print(modelName)
  plot(rocCurve, col="blue", main="ROC")
  ds$residuals = ds$observed
  ds$residuals = ds$observed - ds$predicted

  print(paste("AUC:", aucValue))
  print  (shapiro.test(ds$residuals) )
  
  mean_exp=mean(ds$residuals, na.rm=TRUE) 
  sd_exp=sd(ds$residuals, na.rm=TRUE)      
  xname=paste(modelName)
  gg=ggplot(ds, aes(residuals)) +
    geom_density(alpha=0.4, fill=3) + 
    geom_vline(aes(xintercept=median(residuals)), color="darkgrey", linetype="dashed") +
    geom_vline(aes(xintercept=mean(residuals)), color="darkgrey", linetype="solid") +
    stat_function(fun=dnorm, args=list(mean=mean_exp, sd=sd_exp), 
                  color="darkgrey", linetype="solid", size=1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title="Density plot: distribution of residuals",
      subtitle=xname,
   #   x=xname,
      y="Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-density-residuals-pearson.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-density-residuals-pearson.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()

  
  ##### Calibration Curve #####
  observed=as.vector(as.numeric(as.character(ds$observed)))
  predicted=as.vector(ds$predicted)
  # we divide predicted probabilities into 10 bins
  # then we make a table data which contain mean of predicted probability and number_of_positives/all_predictions 
  # in the left bins there are predicted 0, in the right predicted 1, y - rate of positives 
  # better model has diagonal graph 
  
  data = data.frame(observed, predicted)
  data = data %>%
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
         title = "Calibration Curve",
         subtitle=xname) +
    theme_minimal()
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-calibration.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-calibration.pdf}", sep=""))
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
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-boxplot-predicted-vs-observed.pdf}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
  
  mean_exp=mean(ds$residuals, na.rm=TRUE) 
  sd_exp=sd(ds$residuals, na.rm=TRUE)      
  xname=modelName
  gg=ggplot(ds, aes(residuals)) +
    geom_density(alpha = 0.4, aes(fill = Profession)) + 
    geom_vline(aes(xintercept=median(residuals)), color="darkgrey", linetype="dashed") +
    geom_vline(aes(xintercept=mean(residuals)), color="darkgrey", linetype="solid") +
    stat_function(fun=dnorm, args=list(mean=mean_exp, sd=sd_exp), 
                  color="darkgrey", linetype="solid", size=1, alpha=0.5) + 
    theme_minimal() +
    labs(
      title="Density plot: distribution of residuals (by Profession)",
      subtitle=xname,
 #     x=xname,
      y="Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-density-residuals-profession.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-density-residuals-profession}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  mean_exp=mean(ds$residuals, na.rm=TRUE) 
  sd_exp=sd(ds$residuals, na.rm=TRUE)      
  xname=modelName
  gg=ggplot(ds, aes(residuals)) +
    geom_density(alpha = 0.4, aes(fill = DC_outcome)) + 
    geom_vline(aes(xintercept=median(residuals)), color="darkgrey", linetype="dashed") +
    geom_vline(aes(xintercept=mean(residuals)), color="darkgrey", linetype="solid") +
    stat_function(fun=dnorm, args=list(mean=mean_exp, sd=sd_exp), 
                  color="darkgrey", linetype="solid", size=1, alpha=0.5) + 
    theme_minimal() +
    labs(
      title="Density plot: distribution of residuals (by DC_outcome)",
      subtitle=xname,
   #   x=xname,
      y="Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-density-residuals-DC_o.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-density-residuals-DC_o}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  
  
  mean_exp=mean(ds$residuals, na.rm=TRUE) 
  sd_exp=sd(ds$residuals, na.rm=TRUE)      
  xname=modelName
  gg=ggplot(ds, aes(residuals)) +
    geom_density(alpha = 0.4, aes(fill = Sex)) + 
    geom_vline(aes(xintercept=median(residuals)), color="darkgrey", linetype="dashed") +
    geom_vline(aes(xintercept=mean(residuals)), color="darkgrey", linetype="solid") +
    stat_function(fun=dnorm, args=list(mean=mean_exp, sd=sd_exp), 
                  color="darkgrey", linetype="solid", size=1, alpha=0.5) + 
    theme_minimal() +
    labs(
      title="Density plot: distribution of residuals (by Sex)",
      subtitle=xname,
      x=xname,
      y="Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/",modelName, "-density-residuals-sex.pdf", sep="")
  print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-density-residuals-sex}", sep=""))
  pdf(plotfilename,6,4)
  print(gg, newpage=FALSE)
  dev.off()
  
  ########### ODDS RATIOS ###################
  
  #exp(cbind(coef(model), confint(model)))
  #cbind( 	exp(coef(model)), 	exp(summary(model)$coefficients[,1] - 1.96*summary(model)$coefficients[,2]), 	exp(summary(model)$coefficients[,1] + 1.96*summary(model)$coefficients[,2]) )
  #anova() function on the model to analyze the table of deviance
  #anova(model, test="Chisq")
  #The difference between the null deviance and the residual deviance shows how our model is doing against the null model (a model with only the intercept). The wider this gap, the better. 
  #Если профессия больше всего влияет, то связан ли вообще техностресс вообще с DC?
  #outputFile=file(paste(dir_output_prefix,"/exports/", modelName,".txt",sep=""), open="wt")

  sink(paste(dir_output_prefix,"/exports/", modelName,".txt",sep=""))
  print (modelName)
  print ("################### summary")
  print(summary(model))
  print("Odds ratios")
  print ("################### exp(coef(model))")
  print (cbind( 	exp(coef(model)), 	exp(summary(model)$coefficients[,1] - 1.96*summary(model)$coefficients[,2]), 	exp(summary(model)$coefficients[,1] + 1.96*summary(model)$coefficients[,2]) ))
  print ("################## anova() function on the model to analyze the table of deviance")
  print (anova(model, test="Chisq"))
  print ("################## ROC area under curve")
  print(paste("AUC =",aucValue))
  
  #close(outputFile)
  print ("\n\n############ beta, OR and p-values")
  a= data.frame(cbind(rownames(summary(model)$coefficients), round(coef(model),2), 	 round(exp(coef(model)),2), round(summary(model)$coefficients[,4],3), round(	exp(summary(model)$coefficients[,1] - 1.96*summary(model)$coefficients[,2]),2), 	round(exp(summary(model)$coefficients[,1] + 1.96*summary(model)$coefficients[,2]),2) ))
  colnames(a) <- c("Variable","beta", "OR", "p-value", "OR CI -0.95", "OR CI +0.95")
  print (a)
  
  cat(convertToLatex(a, paste("Short model summary. ", modelName)))
  
  sink()
  
  colnames(a) <- c("variable","beta", "or", "p-value", "lowCI", "highCI")
  a$or=as.numeric(a$or)
  a$lowCI=as.numeric(a$lowCI)
  a$highCI=as.numeric(a$highCI)
  
  variable=a$variable
  gg = ggplot(a, aes(x = or, y = variable)) +
   geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh (aes(xmax = highCI, xmin = lowCI), size = .5, height = .2, color = "black") +
    geom_point(size = 3.5, color = "black") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_discrete (labels = variable) +
    scale_x_continuous(breaks = c(0.2,0.25,0.33,0.5,0,1,2,3,4,5), limits = c(min(a$lowCI), max(a$highCI)))+
    coord_trans(x = "log10") +
    ylab("") +
    xlab("Odds ratio") +
    ggtitle(modelName)
   plot(gg)
   plotfilename=paste(dir_plots,"/",modelName, "-treeplot.pdf", sep="")
   print (paste("\\includegraphics[width=1\\linewidth]{t3-quan-assessment/R-script/outputs/plots","/",modelName, "-treeplot}", sep=""))
   pdf(plotfilename,6,4)
   print(gg, newpage=FALSE)
   dev.off()
  
}


