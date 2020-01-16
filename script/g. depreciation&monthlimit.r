###################################################################################################################
###################################################################################################################
######################################### 9. Depreciation regression model ### ####################################
###################################################################################################################
###################################################################################################################


############ 9.A Depreciation Model: Prepare the data & input map 
## only run by category level schedules and then apply on all subcatGroup and make
## only use auction data

## only use age 8 - 15 (botyear+1 to dep_endyr)
# Number of groups
SchedRetBorw <-SchedR %>% filter(BorrowType=='AuctionBorrowRetail')
auc_depr_regr <- rbind(SchedRetBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Auc<-dim(auc_depr_regr)[1]

SaleDt_dep <- subset(Data_all,Data_all$SaleType=="Auction") %>% 
  filter(as.Date(EffectiveDate)<=publishDate & Flag =="inUse" & ModelYear <= botyear+1 & ModelYear >= dep_endyr) %>%
  group_by(Schedule) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 


outputdep<-matrix(0,nSched_Auc)

# loop for run regression

for (j in 1:nSched_Auc){

  groupData<-subset(SaleDt_dep,SaleDt_dep$Schedule==auc_depr_regr[j,1])
  
  if(nrow(groupData)>3){
    ################## regression Model #########################
    fit<-lm(log(SaleAB)~Age,data=groupData)
    
    outputdep[j]<-1-exp(fit$coefficients[2])
    
  } 
}

depreciation<-data.frame(auc_depr_regr[,1],outputdep)


colnames(depreciation)<-c('Schedule','outputdep')
#depreciation %>% filter(str_detect(Schedule,'Cranes'))
############  Cap the depreciation results, apply on both retail and auction
capDep <- depreciation %>%
  mutate(dep = ifelse(str_detect(Schedule,'Cranes USA'), pmin(depBound_upp_crane,pmax(depBound_bot,outputdep)),pmin(depBound_upp,pmax(depBound_bot,outputdep)))) %>%
  mutate(ModelYear = 'Dep',
         rate = ifelse(is.na(dep),depBound_na,dep)) %>%
  select(Schedule,ModelYear,rate)

############## combine the regular schedule and borrow schedule depr rate #########
Depr_all<-rbind(capDep,
                merge(capDep %>% rename(BorrowSchedule=Schedule), rbind(InB,InR) %>% select(Schedule,BorrowSchedule) %>% distinct(),by='BorrowSchedule') %>%
                  select(Schedule,ModelYear,rate))





############ 9.D Appreciation Model: Prepare the data & input map 
## only run by category level schedules and then apply on all subcatGroup and make
## only use retail data
## only use age -1 to 2 (currentyear+1 to currentyear-2)

# Number of groups
SchedAucBorw <-SchedR %>% filter(BorrowType =='RetailBorrowAuction')
ret_appr_regr <- rbind(SchedAucBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Ret<-dim(ret_appr_regr)[1]

SaleDt_app <- SaleDtRet %>% 
  filter(as.Date(EffectiveDate)<=publishDate & Flag =="inUse" & ModelYear <= topyear+1 & ModelYear >= topyear-2) %>%
  group_by(Schedule) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 

outputapp<-matrix(0,nSched_Ret)

for (j in 1:nSched_Ret){
  
  groupData<-subset(SaleDt_app,SaleDt_app$Schedule==ret_appr_regr[j,1])
  
  if(nrow(groupData)>3){
    ################## regression Model #########################
    
    fit<-lm(log(SaleAB)~Age,data=groupData)
    outputapp[j]<-1-exp(fit$coefficients[2])
    
  } 
}

## Manage the output format
appreciation<-data.frame(ret_appr_regr[,1],outputapp)
colnames(appreciation)<-c('Schedule','outputapp')


############  Cap the appreciation results, apply on both retail and auction
capApp <- appreciation %>%
  mutate(app = pmin(appBound_upp,pmax(appBound_bot,outputapp))) %>%
  mutate(ModelYear = 'App',
         rate0 = ifelse(is.na(app),appBound_na,app)) %>%
  select(Schedule,ModelYear,rate0)


################################# Appreciation side constrain - prevent rebase movement: ####################
### calcualte the topyear / second topyear fmv to get a ratio
ScheduleOut_apprInd = merge(ScheduleOut %>% filter(ModelYear == topyear),ScheduleOut %>% filter(ModelYear == topyear-1),
                        by='Schedule') %>%
  mutate(appr_idx = Adjfmv.x/Adjfmv.y -1) %>%
  select(Schedule,appr_idx)

### limit the appreciation rate between based on the ratio calcualted above
join_appr <- merge(capApp,ScheduleOut_apprInd,by='Schedule') %>%
  mutate(rate = pmax(pmin(appr_idx, rate0 + endYrRate),rate0 - endYrRate)) %>%
  select(Schedule,ModelYear,rate)

############## combine the regular schedule and borrow schedule apr rate ##############
Appr_all<-rbind(join_appr,
merge(join_appr %>% rename(BorrowSchedule=Schedule), rbind(InB,InA) %>% select(Schedule,BorrowSchedule) %>% distinct(),by='BorrowSchedule') %>%
  select(Schedule,ModelYear,rate))



############################# rowbine depreciationa and appreciation #############################
combDeprApr <- rbind(Depr_all,Appr_all)

### join for depreciation in Application tab
applydep<-merge(combDeprApr,comb_Out,by=c('Schedule')) %>% 
  select(ClassificationId, Schedule,ModelYear,rate) %>%
  arrange(ClassificationId, ModelYear)


###################################################################################################################
###################################################################################################################
#######################################  LIMIT THE MONTH OVER MONTH CHANGE #####################################
###################################################################################################################
###################################################################################################################


LM_deprapr <- gather(Last_depr %>% select(ClassificationId,Appreciation,Depreciation),ModelYear,LMvalue,Appreciation:Depreciation,factor_key = T) %>%
  mutate(ModelYear = ifelse(ModelYear=='Appreciation','App','Dep'))

MoM_deprapr<-merge(applydep,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_rate = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit)))) %>%
  arrange(ClassificationId,ModelYear)

MoM_deprapr %>% filter(str_detect(Schedule,'Cranes') & ModelYear =='Dep')

###################################################################################################################
###################################################################################################################
####################################### Schedule #####################################
###################################################################################################################
###################################################################################################################

### join for regular schedules
map_to_sched<-merge(ScheduleOut,comb_Out,by=c('Schedule')) %>%
  arrange(Schedule,ClassificationId, ModelYear)

### run a check, expect retun nothing
map_to_sched %>%
  group_by(ClassificationId) %>%
  summarise(n=n()) %>%
  filter(n>10)




############### Depreciation side constrain - prevent rebase movement:#####################
## 2% points 
Sched_joinDepr<-merge(map_to_sched,Depr_all %>% select(-ModelYear),by='Schedule') %>% arrange(ClassificationId,desc(ModelYear))

## modify the second last year if needed to prevent jump when rebase
depr_constr<- merge(Sched_joinDepr %>% filter(ModelYear == botyear +1) %>% select(ClassificationId,Adjfmv,Adjflv,ModelYear),
                    Sched_joinDepr %>% filter(ModelYear == botyear ) %>% select(ClassificationId,Adjfmv,Adjflv,ModelYear,rate),
                    by='ClassificationId') %>%
  mutate(Adjfmv = pmin(pmax(Adjfmv.y *(1 + rate) - endYrRate, Adjfmv.x),Adjfmv.y *(1 + rate) + endYrRate),
         Adjflv = pmin(pmax(Adjflv.y *(1 + rate) - endYrRate, Adjflv.x),Adjflv.y *(1 + rate) + endYrRate)) %>%
  select(ClassificationId,Adjfmv, Adjflv) 

## join back to schedule table and replace the second last year value
CapSchedule<-rbind(merge(depr_constr,map_to_sched %>% filter(ModelYear == botyear +1) %>% select(-Adjfmv, -Adjflv),by='ClassificationId') %>%
  select(Schedule, ModelYear, Adjfmv, Adjflv, ClassificationId, everything()),
  map_to_sched %>% filter(ModelYear != botyear +1)) %>%
  arrange(ClassificationId ,desc(ModelYear))


########################################## Calculate the Global values #####################################

### Schedules
GlobalSched<-CapSchedule %>% filter(CategoryId %in% GlobalList & Plot=='Y') %>%
  group_by(ModelYear) %>%
  summarise(Globalfmv = mean(Adjfmv),Globalflv = mean(Adjflv)) %>%
  mutate(ClassificationId = GlobalClassId)

### Depreciationa and Appreciation 
Global_Depr <- merge(CapSchedule %>% filter(CategoryId %in% GlobalList & Plot=='Y') %>% select(ClassificationId) %>% distinct(),
                     MoM_deprapr,by='ClassificationId') %>%
  group_by(ModelYear) %>%
  summarise(Globalfmv = mean(limit_rate),Globalflv = mean(limit_rate)) %>%
  mutate(ClassificationId = GlobalClassId)

### Global
GlobalValues<-rbind(GlobalSched,Global_Depr) %>%
  rename(limit_fmv=Globalfmv,limit_flv=Globalflv)


##################################### Limit the schedule by last month value #########################################
## manage last month schedule table which imported from BI.AppraisalBookClassificationValues
lastM_schedule<-LastMonth_import %>%
  filter(ModelYear>=botyear-1 & ModelYear <= topyear) %>%
  select(ClassificationId,ModelYear,CurrentFmv, CurrentFlv) %>%
  distinct()


### join to last month value and limit the movement
MoMSchedules <- merge(CapSchedule,lastM_schedule,by=c("ClassificationId","ModelYear"),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),Adjflv,ifelse(CategoryId == 2616, MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM_spec),MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM)))) %>%
  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM)),
  #       limit_flv = ifelse(is.na(CurrentFlv),Adjflv,MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM))) %>%
arrange(ClassificationId,desc(ModelYear))


#write.csv(CapSchedules,'CapSchedules.csv')



####################### 10.D Final export values with all schedules and depr and appr, with limitation by last month

FinalSchedules<-rbind(MoMSchedules %>% select(ClassificationId,ModelYear,limit_fmv, limit_flv),
                      MoM_deprapr %>% mutate(limit_fmv=limit_rate,limit_flv = limit_rate) %>% select(ClassificationId,ModelYear,limit_fmv, limit_flv),
                      GlobalValues %>% select(ClassificationId,ModelYear,limit_fmv, limit_flv)) %>%
  arrange(ClassificationId,desc(ModelYear))

#write.csv(FinalSchedules,'20181212FinalSchedules.csv')
#write.csv(applydep,"applydep.csv")

########## create a table to draw the depreciation line
selfJoin<- merge(FinalSchedules %>% filter(ModelYear == botyear), 
                 MoM_deprapr %>% filter(ModelYear == 'Dep') %>% select(ClassificationId,limit_rate) %>% rename(deprate = limit_rate),by='ClassificationId')

deprAge<-seq(0,7,1)

deprCurve<-merge(merge(selfJoin,deprAge),comb_Out %>% select(ClassificationId,Schedule,Plot),by='ClassificationId',all.x=T) %>%
  filter(Plot=='Y') %>%
  mutate(ModelYear  = as.numeric(ModelYear) - y,
         depfmv = limit_fmv*(1-deprate)^y,
         depflv = limit_flv*(1-deprate)^y) %>%
  select(ClassificationId, Schedule,ModelYear,deprate,depfmv ,depflv) %>%
  arrange(ClassificationId, ModelYear) 
  

####################### 10.E Final Checks before upload & Manual change #######################

FinalCheck <-MoMSchedules

####### Check across year 
for (i in 1:nrow(FinalCheck)){
  FinalCheck$retFlag[i] = ifelse(FinalCheck$ClassificationId[i]==FinalCheck$ClassificationId[i+1],
                                 ifelse(round(FinalCheck$limit_fmv[i]/FinalCheck$limit_fmv[i+1],digits=4)<1,'flag',''),'')
  FinalCheck$aucFlag[i]= ifelse(FinalCheck$ClassificationId[i]==FinalCheck$ClassificationId[i+1],
                                ifelse(round(FinalCheck$limit_flv[i]/FinalCheck$limit_flv[i+1],digits=4)<1,'flag',''),'')
}



####### number of flags on year 
FinalCheck %>%
  filter(retFlag=='flag' | aucFlag=='flag') %>%
  summarise(n=n())

ManualChangeYr<-FinalCheck %>%
  filter(retFlag=='flag' | aucFlag=='flag') %>%
  select(Schedule)
ManualChangeYr

#### if flag <>0, view the scheules and adjustments
FinalCheck %>% 
  filter(Schedule == ManualChangeYr[1,])

FinalCheck %>% 
  filter(Schedule == ManualChangeYr[2,])

FinalCheck %>% 
  filter(Schedule == ManualChangeYr[3,])


