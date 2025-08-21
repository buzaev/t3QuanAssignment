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


  
  
  #TS vs individual
  #library(dummies)
  #ds <- dummy.data.frame(ds, names = "Profession", sep = "") #R language glm does automatically
  #logistic regression 
  model2 <- glm(TS_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
  summary(model2)
  
  ds$model2Residuals=residuals(model2, type="pearson") #I want to see normality, variability, outliers 


  
  
  # model 3
  
  model3 <- glm(TS_outcome ~Profession*YearsWorking+ Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, binomial(link = 'logit'))
  summary(model3)
  source("libraries/analyseModel.R")
  analyseModel(ds,ds$TS_outcome, model3,"(Model 3. TS_outcome ~ Personal + DC_outcome)")
  
  