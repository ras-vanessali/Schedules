
### join the make map to the uploaded average make adjuster
makeAdj_calDt<-merge(joinmap_make,MakeAdjData%>% select(-rowNum),by=c('CategoryId','SubcategoryId'))

makeRowNum<-makeAdj_calDt%>%
  group_by(SaleType,Schedule,MakeId,MakeName) %>%
  arrange(desc(as.Date(SaleDate))) %>%
  mutate(rowNum = row_number()) %>%
  filter(!(as.Date(SaleDate)<=sixLastM & rowNum>thredtpts.sched))

### calculate the average sp/value (make adjusters) by schedule and make
MakeSFcalc_Sched<-makeRowNum %>%
  group_by(Schedule,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_sched = round(mean(spValue),digits=4),
            nMake_sched = max(rowNum),
            Dateback = min(as.Date(SaleDate)))
  
### calculate the average sp/value (make adjusters) by category and make
MakeSFcalc_Categ<-makeRowNum %>%
  group_by(CategoryId,CategoryName,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_cat = mean(spValue),
            nMake_cat = max(rowNum))

### calcualte the average sp/value (make adjusters) by report group and make
MakeSFcalc_rptGrp<-makeRowNum %>%
  group_by(ReportGroup,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_grp = mean(spValue),
            nMake_grp = max(rowNum))

### create a map between category and schedule 
Categ_sched <- rbind(In %>% select(CategoryId,Schedule),InR %>% select(CategoryId,Schedule),InA %>% select(CategoryId,Schedule)) %>% 
  distinct() %>%
  group_by(Schedule) %>%
  filter(row_number()==1)

CategLev_make<-merge(merge(MakeSFcalc_Sched,Categ_sched,by='Schedule',all.x=T),
                     MakeSFcalc_Categ,by=c('CategoryId','MakeId','MakeName','SaleType')) 
  
MakeSFcalc<-merge(merge(CategLev_make,ReportGrp,by="CategoryId", all.x=T),MakeSFcalc_rptGrp,by=c('ReportGroup','MakeId','MakeName','SaleType')) %>%
  mutate(rate_updt = ifelse(nMake_sched >=thredtpts, Makerate_sched,ifelse(nMake_cat>=thredtpts,Makerate_cat,Makerate_grp))) %>%
  # cap
  mutate(SFmakeCap = pmax(makeSFbot,pmin(rate_updt,makeSFupp))) %>%
  # limit SF by number of data points
  mutate(SFmakeFinal = (SFmakeCap-1)*pmin(nMake_grp,thredtpts)/thredtpts+1) %>%
  select(SaleType,Schedule,MakeId,MakeName,SFmakeFinal)


## transfer the data frame long to wide that different sale type in differnt columns
trans_MakeSFcalc <- spread(MakeSFcalc,key=SaleType,value=SFmakeFinal) %>%
  mutate(Retail = ifelse(is.na(Retail),1,Retail),
         Auction = ifelse(is.na(Auction),1,Auction))


### Apply minimum delta for channel 
MinDelta<-trans_MakeSFcalc %>%
  mutate(delta_auc = Auction -1, delta_ret = Retail -1) %>%
  mutate(move_auc = minimumDelta(delta_auc,delta_ret,delta_auc),
         move_ret = minimumDelta(delta_auc,delta_ret,delta_ret)) %>%
  mutate(chancheck_auc = Auction - move_auc, chancheck_ret = Retail - move_ret) %>%
  select(-delta_ret, -delta_auc,  -move_ret,-move_auc)


## outer join the make output to the calculated scale factor, assign 1 to those with no comps
joinMakeOut<-merge(MinDelta,make_output,by=c('Schedule','MakeId','MakeName'),all.y=TRUE) %>% 
  select(Schedule,ClassificationId, CategoryId,CategoryName,SubcategoryId,SubcategoryName, MakeId,MakeName,chancheck_ret,chancheck_auc) %>%
  mutate(Retail=round(chancheck_ret,digits=4),Auction=round(chancheck_auc,digits=4)) %>%
  select(-chancheck_ret,-chancheck_auc)

joinMakeOut[is.na(joinMakeOut)]<-1

## Apply make adjusters to each model year
joinMakeSF<-merge(ScheduleOut,joinMakeOut,by='Schedule') %>% arrange(Schedule,ClassificationId,desc(ModelYear))
#joinMakeSF<-joinMakeSF %>% filter(Schedule =='Compact Track Loaders Large USA' & MakeId==18)
## calculate deltas between the top year and phaseinAge
deltaMakeAdjcalc = merge(joinMakeSF %>% filter(ModelYear == topyear) %>% select(ClassificationId,Schedule,Adjflv,Adjfmv,Retail,Auction),
                         joinMakeSF %>% filter(ModelYear == topyear - phaseinAge) %>% select(ClassificationId,Adjflv,Adjfmv),by='ClassificationId') %>%
  mutate(parent.delta.flv = Adjflv.x - Adjflv.y,
         parent.delta.fmv = Adjfmv.x - Adjfmv.y) %>%
  mutate(topy.adj.flv = ((Auction - 1) * brwsched_move + 1) * Adjflv.x,
         topy.adj.fmv = ((Retail - 1) * brwsched_move + 1) * Adjfmv.x,
         child.delta.flv = topy.adj.flv  - Adjflv.y * Auction,
         child.delta.fmv = topy.adj.fmv  - Adjfmv.y * Retail) %>%
  select(ClassificationId,Schedule,topy.adj.flv,topy.adj.fmv,parent.delta.flv,parent.delta.fmv,child.delta.flv,child.delta.fmv)

## join the results back to the table with full years
joinMakeSF.upd <- merge(joinMakeSF,deltaMakeAdjcalc,by='ClassificationId') %>% arrange(ClassificationId,desc(ModelYear))

## data validation
dim(joinMakeOut)[1]==dim(joinMakeSF)[1]/10
dim(joinMakeOut)[1]==dim(deltaMakeAdjcalc)[1]
dim(joinMakeSF.upd)[1]==dim(joinMakeSF)[1]


## interpolate the years between top year and x year
for (i in 1:dim(joinMakeSF.upd)[1]){
  
  ### Auction
  ## deprecation rate by year
  joinMakeSF.upd$diffRate.auc[i] = ifelse(joinMakeSF.upd$ClassificationId[i] == joinMakeSF.upd$ClassificationId[i-1] &
                                        joinMakeSF.upd$ModelYear[i] == joinMakeSF.upd$ModelYear[i-1]-1,
                                         round((joinMakeSF.upd$Adjflv[i-1] - joinMakeSF.upd$Adjflv[i])/joinMakeSF.upd$parent.delta.flv[i],digits=4),0)
  
  ## adjusted schedule
  joinMakeSF.upd$adjustrate.auc[i] = joinMakeSF.upd$topy.adj.flv[i]
  joinMakeSF.upd$adjustrate.auc[i] = ifelse(joinMakeSF.upd$ModelYear[i] <= topyear -phaseinAge, joinMakeSF.upd$Adjflv[i]*joinMakeSF.upd$Auction[i],
                                           ifelse(joinMakeSF.upd$ModelYear[i] == topyear, joinMakeSF.upd$topy.adj.flv[i],
                                                 joinMakeSF.upd$adjustrate.auc[i-1]-joinMakeSF.upd$child.delta.flv[i]*joinMakeSF.upd$diffRate.auc[i]))
  ### Retail
  ## depreciation rate by year
  joinMakeSF.upd$diffRate.ret[i] = ifelse(joinMakeSF.upd$ClassificationId[i] == joinMakeSF.upd$ClassificationId[i-1] &
                                            joinMakeSF.upd$ModelYear[i] == joinMakeSF.upd$ModelYear[i-1]-1,
                                          round((joinMakeSF.upd$Adjfmv[i-1] - joinMakeSF.upd$Adjfmv[i])/joinMakeSF.upd$parent.delta.fmv[i],digits=4),0)
  
  ## adjusted schedule
  joinMakeSF.upd$adjustrate.ret[i] = joinMakeSF.upd$topy.adj.fmv[i]
  joinMakeSF.upd$adjustrate.ret[i] = ifelse(joinMakeSF.upd$ModelYear[i] <= topyear -phaseinAge, joinMakeSF.upd$Adjfmv[i]*joinMakeSF.upd$Retail[i],
                                            ifelse(joinMakeSF.upd$ModelYear[i] == topyear, joinMakeSF.upd$topy.adj.fmv[i],
                                                   joinMakeSF.upd$adjustrate.ret[i-1]-joinMakeSF.upd$child.delta.fmv[i]*joinMakeSF.upd$diffRate.ret[i]))
}

## manage the output table
AllTmake <-joinMakeSF.upd %>%
  select(ClassificationId,Schedule.x,CategoryId,SubcategoryId, SubcategoryName, MakeName,MakeId, ModelYear,adjustrate.auc,adjustrate.ret) %>%
  rename(flv_make=adjustrate.auc,fmv_make=adjustrate.ret,Schedule=Schedule.x) %>%
  arrange(ClassificationId,desc(ModelYear))

## combine the make schedules with their depr and appr rate
Make_joinDepr<-merge(AllTmake,combDeprApr %>% filter(ModelYear=='Dep') %>% select(-ModelYear),by='Schedule') %>% arrange(ClassificationId,desc(ModelYear))

## modify the second last year if needed to prevent jump when rebase
depr_constr.make<- merge(Make_joinDepr %>% filter(ModelYear == botyear +1) %>% select(ClassificationId,fmv_make,flv_make,ModelYear),
                         Make_joinDepr %>% filter(ModelYear == botyear ) %>% select(ClassificationId,fmv_make,flv_make,ModelYear,rate),
                    by='ClassificationId') %>%
  mutate(fmv_make = pmin(pmax(fmv_make.y *(1 + rate) / (1+rebase_limit), fmv_make.x),fmv_make.y *(1 + rate) * (1+rebase_limit)),
         flv_make = pmin(pmax(flv_make.y *(1 + rate) / (1+rebase_limit), flv_make.x),flv_make.y *(1 + rate) *(1+rebase_limit))) %>%  
  select(ClassificationId,fmv_make, flv_make) 

## join back to schedule table and replace the second last year value
CapMakeSchedule<-rbind(merge(depr_constr.make,Make_joinDepr %>% filter(ModelYear == botyear +1) %>% select(-fmv_make, -flv_make)
                             ,by='ClassificationId') %>%
                     select(Schedule, ModelYear, fmv_make, flv_make, ClassificationId, everything()),
                     Make_joinDepr %>% filter(ModelYear != botyear +1)) %>%
  arrange(ClassificationId ,desc(ModelYear))


#### MoM Limitation 
## Limit by last month 
lastM_schedule_make<-LastMonth_Sched %>%
  filter(ModelYear >= botyear-1 & ModelYear <= topyear) %>%
  distinct()

MoMlimit_csm<-merge(CapMakeSchedule,LastMonth_Sched %>% select(ClassificationId,ModelYear,CurrentFmv,CurrentFlv),by=c("ClassificationId","ModelYear"),all.x=T) %>%
  #mutate(limit_flv = ifelse(is.na(CurrentFlv),flv_make,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFlv,flv_make,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM))))%>%
  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFmv,fmv_make,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM))))%>%

  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,ifelse(CategoryId==2616,MoMlimitFunc(CurrentFmv,fmv_make,limDw_MoM_spec,limDw_MoM_spec),MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM))),
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),flv_make,MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM))) %>%
  arrange(ClassificationId,ModelYear)


#### join to get the depr and appr rate 
DeprecMake<-merge(MoMlimit_csm %>% select(ClassificationId,Schedule) %>% distinct(),combDeprApr,by='Schedule') 
#### MoM for depr and appr rate
MoM_deprapr_Make<-merge(DeprecMake,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App',  MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit))),
         limit_flv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit)))) %>%
  select(ClassificationId,ModelYear,limit_fmv,limit_flv) %>% 
  arrange(ClassificationId,ModelYear)



