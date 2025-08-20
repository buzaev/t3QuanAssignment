convert_ds <- function(ds) {
  colnames <- read.csv(paste(dir_data,'/colnames.csv', sep=""), stringsAsFactors = FALSE)
  colnames(ds)=colnames$newColName
  
  ds$job=ds$jobChief
  ds$job[ds$jobOther!=""]="jobOther"  
  ds$job[ds$jobNurse!=""]="jobNurse"  
  ds$job[ds$jobDoctor!=""]="jobDoctor"  
  ds$job[ds$jobChief!=""]="jobChief"  
  ds$job=as.factor(ds$job)
  
  ds$jobOther=NULL
  ds$jobNurse=NULL
  ds$jobDoctor=NULL
  ds$jobChief=NULL
  
  ds$firstTime[ds$firstTime=="Да"]=1
  ds$firstTime[ds$firstTime=="Нет"]=0
  ds$firstTime=as.factor(ds$firstTime)
  
  ds$sex[ds$sex=="Женский"]="Female"
  ds$sex[ds$sex!="Female"]="Male"
  ds$sex=as.factor(ds$sex)
  
  ds$degree[ds$degree=="Нет"]="Spec"
  ds$degree[ds$degree=="Кандидат наук"]="PhD"
  ds$degree[ds$degree=="Доктор наук"]="DSc"
  
#  ds$dateSurvey<-strptime(ds$dateSurvey,format="%Y-%m-%d")  #%H%:M:%S") 
#  ds$dateSurvey=as.Date(ds$dateSurvey)
  ds$dateSurvey <- as.POSIXct(ds$dateSurvey, format = "%Y-%m-%d %H:%M:%S")
#  typeof(ds$dateSurvey)
  
  ds$talentUseFull[ds$talentUseFull=="Да"]="1"
  ds$talentUseFull[ds$talentUseFull!="1"]="0"
  ds$talentUseFull=as.numeric(ds$talentUseFull)
 ds$talentUseFull=as.factor(ds$talentUseFull)
 
 ds$talentUseWish[ds$talentUseWish=="Да"]="1"
 ds$talentUseWish[ds$talentUseWish!="1"]="0"
 ds$talentUseWish=as.numeric(ds$talentUseWish)
 ds$talentUseWish=as.factor(ds$talentUseWish)
 
 
 ds$talentUseOpportunity[ds$talentUseOpportunity=="Да"]="1"
 ds$talentUseOpportunity[ds$talentUseOpportunity!="1"]="0"
 ds$talentUseOpportunity=as.numeric(ds$talentUseOpportunity)
 ds$talentUseOpportunity=as.factor(ds$talentUseOpportunity)
 
ds$wantValueMore[ds$wantValueMore=="Чувства самореализации"]="satisfyValueFulfillment"
ds$wantValueMore[ds$wantValueMore=="Уважение коллег за рабочие качества"]="satisfyValueBeingRespected"
ds$wantValueMore[ds$wantValueMore=="Безопасности и денег"                 ]="satisfyValueSafety"
ds$wantValueMore[ds$wantValueMore=="Теплых отношений с отдельными людьми (дружба, взаимная поддержка и т.д.)"]="satisfyValueWarmRelations"
ds$wantValueMore[ds$wantValueMore=="Радости достижений"]="satisfyValueJoyProcessAndAccomplishment"
ds$wantValueMore[ds$wantValueMore=="Самоуважения"]="satisfyValueSefRespect"
ds$wantValueMore[ds$wantValueMore=="Быть принятым командой не только за рабочие качества"]="satisfyValueBelonging"
          
ds$ideaGen=ds$ideaGen1
ds$ideaGen=(ds$ideaGen1+ds$ideaGen2+ds$ideaGen3)/3

ds$ideaSearch=ds$ideaSearch1
ds$ideaSearch=(ds$ideaSearch1+ds$ideaSearch2+ds$ideaSearch3)/3

ds$ideaCommunication=ds$ideaCommunication1
ds$ideaCommunication=(ds$ideaCommunication1+ds$ideaCommunication2+ds$ideaCommunication3+ds$ideaCommunication4)/4

ds$ideaImplementationStart=ds$ideaImplementationStart1
ds$ideaImplementationStart=(ds$ideaImplementationStart1+ds$ideaImplementationStart2+ds$ideaImplementationStart3)/3

ds$ideaInvolvingOthers=ds$ideaInvolvingOthers1
ds$ideaInvolvingOthers=(ds$ideaInvolvingOthers1+ds$ideaInvolvingOthers2+ds$ideaInvolvingOthers3)/3

ds$overcomingObstacles=ds$overcomingObstacles1
ds$overcomingObstacles1=(ds$overcomingObstacles1+ds$overcomingObstacles2+ds$overcomingObstacles3+ds$overcomingObstacles4)/4
ds$innovationOutput=ds$innovationOutput1
ds$innovationOutput=(ds$innovationOutput1+ds$innovationOutput2+ds$innovationOutput3)/3

ds$supportManagerial=ds$supportManagerial1
ds$supportManagerial=(ds$supportManagerial1+ds$supportManagerial2+ds$supportManagerial3+ds$supportManagerial4+ds$supportManagerial5)/5

ds$supportOrganizational=ds$supportOrganizational1
ds$supportOrganizational=(ds$supportOrganizational1+ds$supportOrganizational2+ds$supportOrganizational3)/3

       


ds$innovativeBehaviorInventory=ds$supportOrganizational1
ds$innovativeBehaviorInventory=( 
ds$ideaGen1+
ds$ideaGen2+
ds$ideaGen3+
ds$ideaSearch1+
ds$ideaSearch2+
ds$ideaSearch3+
ds$ideaCommunication1+
ds$ideaCommunication2+
ds$ideaCommunication3+
ds$ideaCommunication4+
ds$ideaImplementationStart1+
ds$ideaImplementationStart2+
ds$ideaImplementationStart3+
ds$ideaInvolvingOthers1+
ds$ideaInvolvingOthers2+
ds$ideaInvolvingOthers3+
ds$overcomingObstacles1+
ds$overcomingObstacles2+
ds$overcomingObstacles3+
ds$overcomingObstacles4+
ds$innovationOutput1+
ds$innovationOutput2+
ds$innovationOutput3)/23


ds$innovationSupportInventory=ds$innovativeBehaviorInventory
ds$innovationSupportInventory=(
  ds$supportManagerial1+
  ds$supportManagerial2+
  ds$supportManagerial3+
  ds$supportManagerial4+
  ds$supportManagerial5+
  ds$supportOrganizational1+
  ds$supportOrganizational2+
  ds$supportOrganizational3
)/8






#ds$birthdate<-strptime(ds$birthdate,format="%d.%m.%Y") #defining what is the original format of your date
#ds$birthdate<-as.Date(ds$birthdate)
#ds$first_sonication_time=format(ds$first_sonication_time,digits = 14)
#ds$last_sonication_time=format(ds$last_sonication_time,digits = 14)
#fst<-strptime(ds$first_sonication_time,format="%Y%m%d%H%M%S") #defining what is the original format of your date
#lst<-strptime(ds$last_sonication_time,format="%Y%m%d%H%M%S") #
#ds$time_op_sec=as.numeric(difftime(lst,fst,units ="secs"))
#ds$time_op_min=as.numeric(difftime(lst,fst,units ="mins"))
#ds$first_sonication_time<-fst
#ds$last_sonication_time<-lst

#ds$follow_days=as.numeric(difftime(ds$date_last_follow, ds$date_op, units="days"))
#ds$fstd<-as.Time(fst)
#ds$date_op<-strptime(ds$date_op,format="%d.%m.%Y") #defining what is the original format of your date
#ds$date_op<-as.Date(ds$date_op)
#ds$date_last_follow<-strptime(ds$date_last_follow,format="%d.%m.%Y") #defining what is the original format of your date
#ds$date_last_follow<-as.Date(ds$date_last_follow)
#ds$diagnosis=as.factor(ds$diagnosis)
#ds$tremor_duration=as.numeric(ds$tremor_duration)
#ds$edema_maxsize_immediate=gsub(",", ".", ds$edema_maxsize_immediate, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

#ds$follow_days=as.numeric(difftime(ds$date_last_follow, ds$date_op, units="days"))
#ds_et_subs=ds[which(ds$diagnosis=="ЭТ"),]
#ds$delta_tremor_scale = ds$before_tremor_scale-ds$after_tremor_scale 
#ds$percent_improvement=ds$delta_tremor_scale*100/ds$before_tremor_scale
#ds$delta_opside = ds$before_opside-ds$after_opside
#ds$percent_improvement_opside=ds$delta_opside*100/ds$before_opside
#ds$delta_control = ds$before_control-ds$after_control
#ds$percent_improvement_control=ds$delta_control*100/ds$before_control
#ds$updrs3_percent_improvement=100*(ds$updrs_part_3_before-ds$updrs_part.3_after)/ds$updrs_part_3_before
#ds$updrs_part_3_after=ds$updrs_part.3_after
#library(gdata)
#ds$side_thalamus=trim(ds$side_thalamus)
#ds$PTT=as.factor(ds$PTT)
ds$zeroSumMindsetBool=(ds$zeroSumMindset-3.5)/abs(ds$zeroSumMindset-3.5)
ds$zeroSumMindsetBool[ds$zeroSumMindsetBool==1]=0
ds$zeroSumMindsetBool[ds$zeroSumMindsetBool==-1]=1
ds$zeroSumMindsetBool=as.factor(ds$zeroSumMindsetBool)
ds$id=as.factor(ds$id)
ds$sex=as.factor(ds$sex)
ds$degree=as.factor(ds$degree)
ds$talentUseFull=as.factor(ds$talentUseFull)
ds$talentUseOpportunity=as.factor(ds$talentUseOpportunity)
ds$wantValueMore=as.factor(ds$wantValueMore)
ds$zeroSumMindsetBool=as.factor(ds$zeroSumMindsetBool)
colnames(ds)[colnames(ds) == "zeroSumMindset"] <- "positiveSumMindset"

ds
}

convert_to_journal_format<-function(ds_column)
{
  #library("gdata")
  a= paste( quantile(ds_column, 0.5, na.rm=TRUE), " [" , (quantile(ds_column, 0.25, na.rm=TRUE)), "; ", (quantile(ds_column, 0.75, na.rm=TRUE)),"]", sep="")
  a
}


#class(x)
#mode(x)
#str(x)