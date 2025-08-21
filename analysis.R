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

zz = file(paste(dir_output_prefix, "/", "exports/console.txt",sep=""), open="wt")
sink(zz,  split=TRUE)


#load and prepare ALL patients table
dsSource = read.csv(paste(dir_data,'/',filename, sep=""), stringsAsFactors = TRUE)
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


#Table 2x2 DC_outcome Profession
tableOutcome = table(ds$DC_outcome, ds$Profession)
print(tableOutcome)
latex=convertToLatex(tableOutcome,"2x2 Table")
cat(latex)
ChiST = chisq.test(tableOutcome)
print(ChiST)

library(tidyverse)
dataPlot=as.data.frame(tableOutcome)
dataPlot$Group <- factor(data$Var1, labels = c("0", "1"))
dataPlot =dataPlot %>% group_by(Var2) %>% mutate(Proportion = Freq / sum(Freq) * 100)

gg=# Построение графика
  ggplot(data, aes(x = Var2, y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.7) +
  labs(
    title = "Distribution of DC_outcome in groups by profession",
    x = "Category",
    y = "Proportion (%)",
    fill = "DC_outcome"
  ) +
  theme_minimal()
plot(gg)


ggplot(ds, aes(x = YearsWorking, y = TS_outcome, color = Profession)) +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(title = "Interaction between Profession and Experience",
       x = "Experience (years)",
       y = "Probability of TechnoStress")



#Table 2x2 DC_outcome vsTS_outcome
tableOutcome = table(ds$DC_outcome, ds$TS_outcome)
print(tableOutcome)
latex=convertToLatex(tableOutcome,"2x2 Table")
cat(latex)
ChiST = chisq.test(tableOutcome)
print(ChiST)

kruskal.test(ds$Age, ds$DC_outcome)


#DC vs individual
#library(dummies)
#ds = dummy.data.frame(ds, names = "Profession", sep = "") #R language glm does automatically
#logistic regression 

model1 = glm(DC_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
summary(model1)
source("libraries/analyseModel.R")
analyseModel(ds, ds$TS_outcome, model1, "Model 1. DC_outcome = Profession +Sex + Age+ YearsWorking")

  
  
  #TS vs individual
  model2 = glm(TS_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
  summary(model2)
  summary(model1)
  source("libraries/analyseModel.R")
  analyseModel(ds, ds$TS_outcome, model1, "Model 2. TS_outcome = Profession +Sex + Age+ YearsWorking")
  

  
  
  # model 3
  model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, family=binomial)
  #model3 = glm(TS_outcome ~Profession:DC_outcome+Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, binomial(link = 'logit'))
  #model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, binomial(link = 'logit'))
  
  
  model3 = glm(TS_outcome ~
                  Profession+ 
                  Sex +     
                  Age+
                  YearsWorking+ 
                  DC_outcome+
               
        #         Profession*YearsWorking#+ 
        #         Profession*DC_outcome#+
        #         Profession*Age+
        #         Profession*Sex+

        #         Sex*Age+ 
        #         Sex*YearsWorking+
        #         Sex*DC_outcome +
 
        #         Age*YearsWorking+
        #         Age*DC_outcome +
  
        #         YearsWorking*DC_outcome
 
               
               ,data = ds, family =binomial)
  
  
  summary(model3)
  source("libraries/analyseModel.R")
  analyseModel(ds, ds$TS_outcome, model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  
  
  set.seed(28467) # random seed
  idx = sample(2,nrow(ds),replace=TRUE,prob=c(.8,.2)) # data split
  table(idx)
  model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds[idx==1,], family=binomial)
  analyseModel(ds[idx==1,], ds$TS_outcome[idx==1], model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  analyseModel(ds[idx==2,], ds$TS_outcome[idx==2], model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  

  