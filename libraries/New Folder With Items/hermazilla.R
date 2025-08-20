Hermazilla <- function(ds, plotfilenum, subsetName){
  
  
  
  
  #Hermazilla
  set.seed(177) # random seed
  idx = sample(2,nrow(ds),replace=TRUE,prob=c(.7,.3)) # data split
  table(idx) # confirm 70/30 split
  #ratio of 0:1 is 6636/23364 = 0.28
  
  lm1 <- lm(
  innovativeBehaviorInventory~
    sex +
    age + 
    experience+
    degree+ 
#    talentUseFull+
#    jobSatisfaction + 
    talentUseWish+ 
    zeroSumMindset+
#    talentUseOpportunity+
#    satisfyValueSefRespect+
#    satisfyValueSafety+
#    satisfyValueWarmRelations+
#    satisfyValueFulfillment+
#    satisfyValueAccomplishment+
#    satisfyValueBeingRespected+
#    satisfyValueBelonging+
#    satisfyValueJoy+
#    wantValueMore+
    job+
    supportManagerial+
    supportOrganizational
   # ideaGen+
  # ideaSearch+
   # ideaCommunication+
  #  ideaImplementationStart+
  #  ideaInvolvingOthers+
  #  overcomingObstacles+
   # innovationOutput+
  #  innovationSupportInventory
  , data=ds) #[idx==1,]) # "lm1" contains all relevant estimation results

  summary(lm1) # view estimation results

  lm1_pred = predict(lm1,newdata=ds)
  plot(ds$time_op_sec[idx==1],lm1_pred[idx==1])
  postResample(pred = lm1_pred[idx==1], obs = ds$time_op_sec[idx==1]) # in-sample
  plot(ds$time_op_sec[idx==2],lm1_pred[idx==2])
  postResample(pred = lm1_pred[idx==2], obs = ds$time_op_sec[idx==2]) # out-of-sample
  
  #difftime("2020-5-16", "2020-1-15", units = "days")
  
  
  
  
  
  
  
  
  #Hermazilla
  set.seed(177) # random seed
  idx = sample(2,nrow(ds),replace=TRUE,prob=c(.7,.3)) # data split
  table(idx) # confirm 70/30 split
  #ratio of 0:1 is 6636/23364 = 0.28
  
  glm1 <- glm(
    as.numeric(zeroSumMindsetBool)~
      sex +
      age + 
      experience+
      degree+ 
      #    talentUseFull+
      #    jobSatisfaction + 
      talentUseWish+ 
     # zeroSumMindsetBool+
          talentUseOpportunity+
     satisfyValueSefRespect+
          satisfyValueSafety+
          satisfyValueWarmRelations+
          satisfyValueFulfillment+
          satisfyValueAccomplishment+
          satisfyValueBeingRespected+
          satisfyValueBelonging+
          satisfyValueJoy+
          wantValueMore+
      job+
      supportManagerial+
      supportOrganizational
    # ideaGen+
    # ideaSearch+
    # ideaCommunication+
    #  ideaImplementationStart+
    #  ideaInvolvingOthers+
    #  overcomingObstacles+
    # innovationOutput+
    #  innovationSupportInventory
    , data=ds) #[idx==1,]) # "lm1" contains all relevant estimation results
  
  summary(glm1) # view estimation results
  
  lm1_pred = predict(lm1,newdata=ds)
  plot(ds$time_op_sec[idx==1],lm1_pred[idx==1])
  postResample(pred = lm1_pred[idx==1], obs = ds$time_op_sec[idx==1]) # in-sample
  plot(ds$time_op_sec[idx==2],lm1_pred[idx==2])
  postResample(pred = lm1_pred[idx==2], obs = ds$time_op_sec[idx==2]) # out-of-sample
  
  #difftime("2020-5-16", "2020-1-15", units = "days")
}