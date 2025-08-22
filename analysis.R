# clear workspace 
rm(list = ls()) 
library(mise)
mise()
library(gdata) #trim


############## Loading and Cleaning Data ##################
setwd ("/Users/garrybear/Documents/#UCL-DBA-thesis/overleaf/t3-quan-assessment/R-script")
print (R.version)
writeLines(toBibtex(citation()), "outputs/R_citation.bib")

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
ds = read.csv(paste(dir_data,'/',filename, sep=""), stringsAsFactors = TRUE)
ds$Profession=as.factor(ds$Profession)
ds$Sex=as.factor(ds$Sex)
ds$TS_outcome=as.factor(ds$TS_outcome)
ds$DC_outcome=as.factor(ds$DC_outcome)
ds$Age=as.numeric(ds$Age)
ds$YearsWorking=as.numeric(ds$YearsWorking)

# finished data loading

export_data = cbind(ds)
write.csv(export_data,paste(dir_exports,"/d.csv", sep=""))


####################### Exploring Data #####################

source("libraries/convertToJournal.R") # my function to write median[q1;q3] nice.
source("libraries/convertToLatex.R") # my function to convert dataframe to latex table


#Descriptive stats
source("libraries/getDescriptives.R") # my function returns table with descriptives for as.numeric
descriptives=getDescriptives(ds) 
latex=convertToLatex(descriptives,"Descriptive Statistics")
cat(latex) # latex without flawed line breaks.

#Frequency tables
source("libraries/getFactorTables.R") #my function returns frequency tables for all factors
tableFactors=getFactorTables (ds)
latex=convertToLatex(tableFactors,"Factors")
cat(latex) 

source("libraries/getAllCorrelations.R") #my function returns all correlations between all is.numeric
correlationsAll=getAllCorrelations(ds)
correlationsSelected=getSelectedCorrelations(ds)
latex=createSpearmanLatexTable(ds)
cat(latex)
createCorrelationPlots(ds)

source("libraries/densityPlots.R") #my function generates density plots for numeric variables
ds$startedCareer=ds$Age-ds$YearsWorking
exportDensityPlots(ds, filename,subsetName)

################# Analyse ###################


############## Question 2.1.  Digital competence and technostress  ###########
#Table 2x2 DC_outcome Profession
tableOutcome = table(ds$DC_outcome, ds$Profession)
print(tableOutcome)
latex=convertToLatex(tableOutcome,"2x2 Table")
cat(latex)
ChiST = chisq.test(tableOutcome)
print(ChiST)

library(tidyverse)
dataPlot=as.data.frame(tableOutcome)
dataPlot$Group = factor(dataPlot$Var1, labels = c("0", "1"))
dataPlot =dataPlot %>% group_by(Var2) %>% mutate(Proportion = Freq / sum(Freq) * 100)

gg=ggplot(dataPlot, aes(x = Var2, y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", alpha=0.7) +
  labs(
    title = "Distribution of DC_outcome in groups by profession",
    x = "Category",
    y = "Proportion (%)",
    fill = "DC_outcome"
  ) +
  theme_minimal()
plot(gg)

kruskal.test(ds$Age, ds$DC_outcome)

gg=ggplot(ds, aes(DC_outcome,Age,fill= DC_outcome))+
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ 
  stat_compare_means(method = "wilcox")+
    geom_jitter(shape=16, position=position_jitter(0.2), alpha=0.5)+
  theme_minimal()+
  labs(title="Boxplot: Digital Competence vs Age", 
       x="Digital Competence", 
       y="Age", 
       fill="Age",
       shape="")#,
plot(gg) 
plotfilename=paste(dir_plots,"/","-boxplot-DC-Age.pdf", sep="")
pdf(plotfilename,6,4)
print(gg, newpage=FALSE)
dev.off()


############## Question 2.2.Digital competence and the individualised characteristics of health professionals.
#library(dummies)
#ds = dummy.data.frame(ds, names = "Profession", sep = "") 
#R language glm does automatically
#logistic regression because dc_outcome is binary

model1 = glm(DC_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, binomial(link = 'logit'))
summary(model1)
source("libraries/analyseModel.R")
analyseModel(ds, ds$TS_outcome, model1, "Model 1. DC_outcome = Profession +Sex + Age+ YearsWorking")

  
  
  #TS vs individual
  model2 = glm(TS_outcome ~ Profession +Sex + Age+ YearsWorking, data = ds, family = binomial)
  summary(model2)
  source("libraries/analyseModel.R")
  analyseModel(ds, ds$TS_outcome, model2, "Model 2. TS_outcome = Profession +Sex + Age+ YearsWorking")
  

  
  
  # model 3
  model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, family=binomial)
  #model3 = glm(TS_outcome ~Profession:DC_outcome+Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, binomial(link = 'logit'))
  #model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds, binomial(link = 'logit'))
  
  
  model3 = glm(TS_outcome ~
                  Profession+ 
                  Sex +     
                  Age+
                  YearsWorking+ 
                  DC_outcome# +
               
         #        Profession*YearsWorking#+ 
         #         Profession*DC_outcome#+
        #         Profession*Age+
        #         Profession*Sex+

        #         Sex*Age+ 
        #         Sex*YearsWorking +
        #         Sex*DC_outcome +
 
        #         Age*YearsWorking+
        #         Age*DC_outcome +
  
        #         YearsWorking*DC_outcome
 
               
               ,data = ds, binomial(link = 'logit'))
  
  
  summary(model3)
  source("libraries/analyseModel.R")
  analyseModel(ds, ds$TS_outcome, model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  
  
  set.seed(28467) # random seed
  idx = sample(2,nrow(ds),replace=TRUE,prob=c(.8,.2)) # data split
  table(idx)
  model3 = glm(TS_outcome ~Profession+ Sex + Age+ YearsWorking+ DC_outcome, data = ds[idx==1,], family=binomial)
  analyseModel(ds[idx==1,], ds$TS_outcome[idx==1], model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  analyseModel(ds[idx==2,], ds$TS_outcome[idx==2], model3, "Model 3. TS_outcome = Personal + DC_outcome.")
  

  ds$DCrequired <- ifelse(ds$Profession %in% c("MTP", "Nurse", "Phys"), 1, 0)
  model3 = glm(TS_outcome ~
                 DCrequired+ 
                 Sex +     
                 Age+
                 YearsWorking+ 
                 DC_outcome +
                 
               #         Profession*YearsWorking#+ 
                        DCrequired*DC_outcome#+
               #         Profession*Age+
               #         Profession*Sex+
               
               #         Sex*Age+ 
               #         Sex*YearsWorking +
               #         Sex*DC_outcome +
               
               #         Age*YearsWorking+
               #         Age*DC_outcome +
               
               #         YearsWorking*DC_outcome
               
               
               ,data = ds, binomial(link = 'logit'))
  
  
  summary(model3)
  source("libraries/analyseModel.R")
  analyseModel(ds, ds$TS_outcome, model3, "Model 3. TS_outcome = DCrequired Personal + DC_outcome.")
  
  
  
  
  
  
################# 
  library(brms)
  model_brms <- brm(
    TS_outcome ~ Sex + Age + YearsWorking + DC_outcome + (1 | Profession),
    data = ds,
    family = bernoulli()
  )
  summary(model_brms)
  analyseModel(ds, ds$TS_outcome, model_brms, "Model 3. Bayes. TS_outcome = DCrequired Personal + DC_outcome.")
  predicted=fitted(model_brms, newdata = ds, re_formula = ~0)
  observed=as.vector(as.numeric(as.character(ds$TS_outcome)))
  library(pROC)
  rocCurve=roc(observed, predicted)
  aucValue=auc(rocCurve)
  print(paste("AUC:", aucValue))
  plot(rocCurve, col="blue", main="ROC")
  
  
  
  
  