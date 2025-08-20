# clear workspace 
rm(list = ls()) 
library(mise)
mise()
library(gdata) #trim

setwd ("/Users/garrybear/Documents/#UCL-DBA-thesis/overleaf/t3-quan-assessment/R-script")

source ("libraries/my_procedures.R") 


#this is redirect console to a file console.txt
dir_output_prefix=paste("outputs/",Sys.Date(), sep="")
dir_output_prefix=("outputs/")

dir.create(dir_output_prefix)
plotfilenum=0
dir_plots=(paste(dir_output_prefix, "/", "plots",sep=""))
dir.create(dir_plots)
dir_exports=(paste(dir_output_prefix, "/", "exports",sep=""))
dir.create(dir_exports)
dir_data=paste("data/","", sep="")
subsetName="all"
filename="simulatedData.csv"

zz <- file(paste(dir_output_prefix, "/", "exports/console.txt",sep=""), open="wt")
sink(zz,  split=TRUE)


#load and prepare ALL patients table
dsSource <- read.csv(paste(dir_data,'/',filename, sep=""), stringsAsFactors = TRUE)
ds=dsSource

ds$Profession=as.factor(ds$Profession)
ds$Sex=as.factor(ds$Sex)
ds$TS_outcome=as.factor(ds$TS_outcome)
ds$DC_outcome=as.factor(ds$DC_outcome)
ds$Age=as.numeric(ds$Age)
ds$YearsWorking=as.numeric(ds$YearsWorking)

export_data = cbind(ds)
write.csv(export_data,paste(dir_exports,"/d.csv", sep=""))

source("libraries/convertToJournal.R")
source("libraries/getDescriptives.R")
source("libraries/convertToLatex.R")

descriptives=getDescriptives(ds)
latex=convertToLatex(descriptives,"Descriptive Statistics")
cat(latex) # без косячных переводов каретки

source("libraries/getFactorTables.R")
tableFactors=getFactorTables (ds)
latex=convertToLatex(tableFactors,"Factors")
cat(latex) # без косячных переводов каретки

source("libraries/getAllCorrelations.R")
correlationsAll=getAllCorrelations(ds)
correlationsSelected=getSelectedCorrelations(ds)
latex=createSpearmanLatexTable(ds)
cat(latex)
createCorrelationPlots(ds)

source("libraries/densityPlots.R")
ds$startedCareer=ds$Age-ds$YearsWorking
exportDensityPlots(ds, filename,subsetName)



#Table 2x2 DC_outcome vsTS_outcome
tableOutcome <- table(ds$DC_outcome, ds$TS_outcome)
print(tableOutcome)
latex=convertToLatex(tableOutcome,"2x2 Table")
cat(latex)
chi_square_test <- chisq.test(table_outcome)
print(chi_square_test)


#DC vs individual
#library(dummies)
#ds <- dummy.data.frame(ds, names = "Profession", sep = "") #R language glm does automatically
#logistic regression 

model1 <- glm(DC_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
summary(model1)

ds$model1Residuals=residuals(model1, type="pearson") #I want to see normality, variability, outliers 
shapiro.test(ds$model1Residuals) 

mean_exp <- mean(ds$model1Residuals, na.rm = TRUE)  # Среднее значение
sd_exp <- sd(ds$model1Residuals, na.rm = TRUE)      # Стандартное отклонение
xname="Disribution of residuals of model 1"
gg <- ggplot(ds, aes(model1Residuals)) +
  geom_density(alpha = 0.4) + 
  geom_vline(aes(xintercept = median(model1Residuals)), color = "darkgrey", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(model1Residuals)), color = "darkgrey", linetype = "solid") +
  stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
  theme_minimal() +
  labs(
    title = "Density plot with Expected Normal Distribution",
    subtitle = xname,
    x = xname,
    y = "Density"
  )
plot(gg)
plotfilename=paste(dir_plots,"/","densityDrModel1.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage = FALSE)
dev.off()


  library(ggpubr) #для stat_cor
  library(RColorBrewer)
  gg=ggplot(ds)+ 
    aes(x=DC_outcome,y=model1Residuals)+# , shape=factor(Sex)) + 
    geom_count(show.legend=TRUE) +
    theme_minimal()+
    scale_color_brewer(palette = "Set1") +
    labs(subtitle="DC_outcome vs model1Residuals", 
         x="DC_outcome", 
         y="model1Residuals", 
         title="Scatterplot", 
         caption = paste( "Subset:",subsetName, "from file:", filename ))
  plot(gg)
  plotfilename=paste(dir_plots,"/","DC_outcome-vs-model1Residuals.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()


  
  
  #TS vs individual
  #library(dummies)
  #ds <- dummy.data.frame(ds, names = "Profession", sep = "") #R language glm does automatically
  #logistic regression 
  model2 <- glm(TS_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
  summary(model2)
  
  ds$model2Residuals=residuals(model2, type="pearson") #I want to see normality, variability, outliers 
  shapiro.test(ds$model2Residuals) 
  
  mean_exp <- mean(ds$model2Residuals, na.rm = TRUE)  # Среднее значение
  sd_exp <- sd(ds$model2Residuals, na.rm = TRUE)      # Стандартное отклонение
  xname="Disribution of residuals of model 2"
  gg <- ggplot(ds, aes(model2Residuals)) +
    geom_density(alpha = 0.4) + 
    geom_vline(aes(xintercept = median(model1Residuals)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(model1Residuals)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      x = xname,
      y = "Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","densityDrModel2.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()

  
  
  
  # model 3
  
  model3 <- glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, family = binomial)
  summary(model3)

  predictedProbs <- predict(model3, type = "response")

  library(pROC)
  rocCurve <- roc(ds$DC_outcome, predictedProbs)
  aucValue <- auc(rocCurve)
  print(paste("AUC:", aucValue))
  plot(rocCurve, col = "blue", main = "ROC")
  
  
  ds$model3Residuals=residuals(model3, type="deviance") #I want to see normality, variability, outliers 
  shapiro.test(ds$model3Residuals) 
  
  mean_exp <- mean(ds$model3Residuals, na.rm = TRUE)  # Среднее значение
  sd_exp <- sd(ds$model3Residuals, na.rm = TRUE)      # Стандартное отклонение
  xname="Disribution of residuals of model 3"
  gg <- ggplot(ds, aes(model2Residuals)) +
    geom_density(alpha = 0.4) + 
    geom_vline(aes(xintercept = median(model1Residuals)), color = "darkgrey", linetype = "dashed") +
    geom_vline(aes(xintercept = mean(model1Residuals)), color = "darkgrey", linetype = "solid") +
    stat_function(fun = dnorm, args = list(mean = mean_exp, sd = sd_exp), 
                  color = "darkgrey", linetype = "solid", size = 1, alpha=0.5) +  # Нормальное распределение
    theme_minimal() +
    labs(
      title = "Density plot with Expected Normal Distribution",
      subtitle = xname,
      x = xname,
      y = "Density"
    )
  plot(gg)
  plotfilename=paste(dir_plots,"/","densityDrModel3.pdf", sep="")
  pdf(plotfilename,6,4)
  print(gg, newpage = FALSE)
  dev.off()
  print(paste("AUC:", aucValue))
  
  gg=ggplot(ds, aes(ds$DC_outcome,model3Residuals,fill=DC_outcome))+
    geom_boxplot(alpha=0.7) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
    geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
    theme_minimal()+
    labs(subtitle="Возраст мужчин и женщин", 
         x="Пол", 
         y="Возраст (лет)", 
         title="Диаграмма", 
         fill="Пол",
         shape="",
         caption = paste( "Выборка: ",subsetName, "из файла", filename ))
  plot(gg) 
  