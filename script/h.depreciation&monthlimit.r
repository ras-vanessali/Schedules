###################################################################################################################
###################################################################################################################
############################## Depreciation/ Appreciation regression model ########################################
############################## Depreciation/ Appreciation Rebase Control #########################################
############################## Depreciation/ Appreciation MoM Limit ###########################################
# Number of groups
SchedRetBorw <-SchedR %>% filter(BorrowType =='AuctionBorrowRetail')
auc_depr_regr <- rbind(SchedRetBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Auc<-dim(auc_depr_regr)[1]

## only use age between botyear+1 and dep_endyr
## only use auction data
## exclude some bad data
SaleDt_dep <- subset(Data_all,Data_all$SaleType=="Auction") %>% 
  filter(as.Date(EffectiveDate)<=publishDate & Flag =="inUse" & ModelYear <= botyear+1 & ModelYear >= dep_endyr) %>%
  group_by(Schedule) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 

## variable
outputdep<-matrix(0,nSched_Auc)

# run regression 
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

##  Cap the depreciation results, apply on both retail and auction
capDep <- depreciation %>%
  ## cranes get different cap
  mutate(dep = ifelse(str_detect(Schedule,'Cranes USA'), pmin(depBound_upp_crane,pmax(depBound_bot,outputdep)),pmin(depBound_upp,pmax(depBound_bot,outputdep)))) %>%
  mutate(ModelYear = 'Dep',
         rate = ifelse(is.na(dep),depBound_na,dep)) %>%
  select(Schedule,ModelYear,rate)

## apply borrow schedule with depreciation rate 
Depr_all<-rbind(capDep,
                merge(capDep %>% rename(BorrowSchedule=Schedule), rbind(InB,InR) %>% select(Schedule,BorrowSchedule) %>% distinct(),by='BorrowSchedule') %>%
                  select(Schedule,ModelYear,rate))



#### Appreciation Model
# Number of groups
SchedAucBorw <-SchedR %>% filter(BorrowType =='RetailBorrowAuction')
ret_appr_regr <- rbind(SchedAucBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Ret<-dim(ret_appr_regr)[1]


## only use retail data
## only use age <2
SaleDt_app <- SaleDtRet %>% 
  filter(as.Date(EffectiveDate)<=publishDate & Flag =="inUse" & Age <2) %>%
  group_by(Schedule) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 

## prepare the data for regression use. use age from youngest up to 1 at least, then check if enough x data points, if not, extend to use up to age 2
SaleDt_appr_modeluse<-Use_Latest_Data(SaleDt_app,'Age',threshold_appr,'',appr_ageuse_fix) 


## create variable
outputapp<-matrix(0,nSched_Ret)
n.app<-matrix(0,nSched_Ret)
for (j in 1:nSched_Ret){
  
  groupData<-subset(SaleDt_appr_modeluse,SaleDt_appr_modeluse$Schedule==ret_appr_regr[j,1])
  
  if(nrow(groupData)>3){
    ################## regression Model #########################
    fit<-lm(log(SaleAB)~Age,data=groupData)
    outputapp[j]<-1-exp(fit$coefficients[2])
    n.app[j]<-nrow(groupData)
    
  } 
}

## Manage the output format
appreciation<-data.frame(ret_appr_regr[,1],outputapp,n.app)
colnames(appreciation)<-c('Schedule','outputapp','NumComps.app')


######  Cap the appreciation results
capApp <- appreciation %>%
  mutate(app = pmin(appBound_upp,pmax(appBound_bot,outputapp))) %>%
  mutate(ModelYear = 'App',
         rate0 = ifelse(is.na(app),appBound_na,app)) %>%
  select(Schedule,ModelYear,rate0)


####### Rebase control on appreciation rate - only one newer year (topyear +1) impacted by the appr rate
####### so keep the schedules and twist the appr rate

ScheduleOut_apprInd = merge(ScheduleOut %>% filter(ModelYear == topyear),ScheduleOut %>% filter(ModelYear == topyear-1),
                        by='Schedule') %>%
  ##calculate the topyear / second topyear fmv to get a ratio
  mutate(appr_idx = Adjfmv.x/Adjfmv.y -1) %>%
  select(Schedule,appr_idx)

### cap the appreciation rate
join_appr <- merge(capApp,ScheduleOut_apprInd,by='Schedule') %>%
  mutate(rate = ifelse(rate0>appr_idx,pmax(pmin(rate0, appr_idx + rebase_limit), appr_idx - rebase_limit),rate0))%>%
 # mutate(rate = pmax(pmin(rate0, appr_idx + rebase_limit), appr_idx - rebase_limit)) %>%
  select(Schedule,ModelYear,rate)

############## combine the regular schedule and borrow schedule 
Appr_all<-rbind(join_appr,
merge(join_appr %>% rename(BorrowSchedule=Schedule), rbind(InB,InA) %>% select(Schedule,BorrowSchedule) %>% distinct(),by='BorrowSchedule') %>%
  select(Schedule,ModelYear,rate))


############# combine depreciation and appreciation 
combDeprApr <- rbind(Depr_all,Appr_all)

### join to the full table
applydep<-merge(combDeprApr,comb_Out,by=c('Schedule')) %>% 
  select(ClassificationId, Schedule,ModelYear,rate) %>%
  arrange(ClassificationId, ModelYear)


#######################################  LIMIT THE MONTH OVER MONTH CHANGE #####################################
## manage the depreciation and appreciation table
LM_deprapr <- gather(LastMonth_depr %>% select(ClassificationId,Appreciation,Depreciation),ModelYear,LMvalue,Appreciation:Depreciation,factor_key = T) %>%
  mutate(ModelYear = ifelse(ModelYear=='Appreciation','App','Dep'))

## MoM limit the rate
MoM_deprapr<-merge(applydep,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_rate = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit)))) %>% 
  mutate(limit_fmv=limit_rate,limit_flv = limit_rate) %>% 
  select(ClassificationId,ModelYear,limit_fmv, limit_flv) %>%
  arrange(ClassificationId,ModelYear)


### join output schedules to out tab
map_to_sched<-merge(ScheduleOut,comb_Out,by=c('Schedule')) %>%
  arrange(Schedule,ClassificationId, ModelYear)

### run a check, expect return blank
map_to_sched %>%
  group_by(ClassificationId) %>%
  summarise(n=n()) %>%
  filter(n>10)


#### Depreciation rebase control - so many years impacted by depreciation rate
#### So keep the depreciation rate and change the last second year to prevent the rebase jump
Sched_joinDepr<-merge(map_to_sched,Depr_all %>% select(-ModelYear),by='Schedule') %>% arrange(ClassificationId,desc(ModelYear))
## modify the second last year if needed to prevent jump when rebase
depr_constr<- merge(Sched_joinDepr %>% filter(ModelYear == botyear +1) %>% select(ClassificationId,Adjfmv,Adjflv,ModelYear),
                    Sched_joinDepr %>% filter(ModelYear == botyear ) %>% select(ClassificationId,Adjfmv,Adjflv,ModelYear,rate),
                    by='ClassificationId') %>%
  mutate(Adjfmv = pmin(pmax(Adjfmv.y *(1 + rate) / (1+rebase_limit), Adjfmv.x),Adjfmv.y *(1 + rate) * (1+rebase_limit)),
         Adjflv = pmin(pmax(Adjflv.y *(1 + rate) / (1+rebase_limit), Adjflv.x),Adjflv.y *(1 + rate) *(1+rebase_limit))) %>%  
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

### Depreciation and Appreciation 
Global_Depr <- merge(CapSchedule %>% filter(CategoryId %in% GlobalList & Plot=='Y') %>% select(ClassificationId) %>% distinct(),
                     MoM_deprapr,by='ClassificationId') %>%
  group_by(ModelYear) %>%
  summarise(Globalfmv = mean(limit_fmv),Globalflv = mean(limit_flv)) %>%
  mutate(ClassificationId = GlobalClassId)

## MoM depr and appr for global
MoM_deprapr_global<-merge(Global_Depr,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(LMvalue),Globalfmv,
                            ifelse(ModelYear=='App',  MoMlimit_depappr(LMvalue,Globalfmv,ApprMoMLimit),MoMlimit_depappr(LMvalue,Globalfmv,DeprMoMLimit))),
         limit_flv = ifelse(is.na(LMvalue),Globalflv,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,Globalflv,ApprMoMLimit),MoMlimit_depappr(LMvalue,Globalflv,DeprMoMLimit)))) %>%
  select(ClassificationId,ModelYear,limit_fmv, limit_flv) %>%
  arrange(ClassificationId,ModelYear)

## manage last month schedule table 
lastM_schedule<-LastMonth_Sched %>%
  filter(ModelYear>=botyear-1 & ModelYear <= topyear) %>%
  distinct()


### join to last month value and limit the movement
MoMSchedules <- merge(CapSchedule,lastM_schedule,by=c("ClassificationId","ModelYear"),all.x=T) %>%

  #mutate(limit_flv = ifelse(is.na(CurrentFlv),Adjflv,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFlv,Adjflv,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM))))%>%
  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFmv,Adjfmv,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM))))%>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),Adjflv,MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM))) %>%
  #mutate(limit_flv = ifelse(is.na(CurrentFlv),Adjflv,ifelse(CategoryId %in% c(2616),MoMlimitFunc(CurrentFlv,Adjflv,limDw_MoM_spec,limDw_MoM_spec),MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM))))%>%
 # mutate(rate=limit_flv/CurrentFlv)%>%
  arrange(ClassificationId,desc(ModelYear))

## limit by last month for global
MoMSched.global = merge(GlobalSched,lastM_schedule %>% filter(ClassificationId==1),by=c("ClassificationId","ModelYear"),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),Globalfmv,MoMlimitFunc(CurrentFmv,Globalfmv,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),Globalflv,MoMlimitFunc(CurrentFlv,Globalflv,limUp_MoM,limDw_MoM)))


########## create a table to draw the depreciation line (plot use)
selfJoin<- merge(MoMSchedules %>% filter(ModelYear == botyear), 
                 MoM_deprapr %>% filter(ModelYear == 'Dep') %>% select(ClassificationId,limit_fmv) %>% rename(deprate = limit_fmv),by='ClassificationId')%>%
  select(ClassificationId,ModelYear, limit_flv,limit_fmv,deprate)

## calculate the fmv flv in vintage years for plot use only
deprCurve<-merge(merge(selfJoin,deprAge),comb_Out %>% select(ClassificationId,Schedule,Plot),by='ClassificationId',all.x=T) %>%
  filter(Plot=='Y') %>%
  mutate(ModelYear  = as.numeric(ModelYear) - y,
         depfmv = limit_fmv*(1-deprate)^y,
         depflv = limit_flv*(1-deprate)^y) %>%
  select(ClassificationId, Schedule,ModelYear,deprate,depfmv ,depflv) %>%
  arrange(ClassificationId, ModelYear) 

