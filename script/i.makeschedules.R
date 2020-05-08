###################################################################################################################
###################################################################################################################
######################################### 11.  MAKE ADJUSTMENTS - Tier 1,2 ########################################
###################################################################################################################
###################################################################################################################
### join the make map to the calcualted CSM sp/value
makeAdj_calDt<-merge(joinmap_make,MakeAdjData%>% select(-rowNum),by=c('CategoryId','SubcategoryId'))

makeRowNum<-makeAdj_calDt%>%
  group_by(SaleType,Schedule,MakeId,MakeName) %>%
  arrange(desc(as.Date(SaleDate))) %>%
  mutate(rowNum = row_number()) %>%
  filter(!(as.Date(SaleDate)<=sixLastM & rowNum>thredtpts.sched))

### calcualte the average sp/value (make adjusters) by schedule and make
MakeSFcalc_Sched<-makeRowNum %>%
  group_by(Schedule,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_sched = round(mean(spValue),digits=4),
            nMake_sched = max(rowNum),
            Dateback = min(as.Date(SaleDate)))
  
### calcualte the average sp/value (make adjusters) by category and make
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
  # cap by boundaries
  mutate(SFmakeCap = pmax(makeSFbot,pmin(rate_updt,makeSFupp))) %>%
  # limit SF by number of data points
  mutate(SFmakeFinal = (SFmakeCap-1)*pmin(nMake_grp,thredtpts)/thredtpts+1) %>%
  select(SaleType,Schedule,MakeId,MakeName,SFmakeFinal)



  
## transfer the data frame long to wide that sale type seperate in columns
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

#write.csv(MinDelta,'20190519 MakeAdjusters.csv')
## outer join the make output to the calculated scale factor, assign 1 to those with no comps
joinMakeOut<-merge(MinDelta,make_output,by=c('Schedule','MakeId','MakeName'),all.y=TRUE) %>% 
  select(Schedule,ClassificationId, CategoryId,CategoryName,SubcategoryId,SubcategoryName, MakeId,MakeName,chancheck_ret,chancheck_auc) %>%
  mutate(Retail=round(chancheck_ret,digits=4),Auction=round(chancheck_auc,digits=4)) %>%
  select(-chancheck_ret,-chancheck_auc)

joinMakeOut[is.na(joinMakeOut)]<-1

################## 11.C Apply make adjusters to each model year
joinMakeSF<-merge(ScheduleOut,joinMakeOut,by='Schedule') %>% arrange(Schedule,ClassificationId,desc(ModelYear))

## calculate deltas betweent the top year and 7th year
deltaMakeAdjcalc = merge(joinMakeSF %>% filter(ModelYear == topyear) %>% select(ClassificationId,Schedule,Adjflv,Adjfmv,Retail,Auction),
                         joinMakeSF %>% filter(ModelYear == topyear -6) %>% select(ClassificationId,Adjflv,Adjfmv),by='ClassificationId') %>%
  mutate(parent.delta.flv = Adjflv.x - Adjflv.y,
         parent.delta.fmv = Adjfmv.x - Adjfmv.y) %>%
  mutate(topy.adj.flv = ((Auction-1)*brwsched_move+1)*Adjflv.x,
         topy.adj.fmv = ((Retail-1)*brwsched_move+1)*Adjfmv.x,
         child.delta.flv = topy.adj.flv  - Adjflv.y *Auction,
         child.delta.fmv = topy.adj.fmv  - Adjfmv.y *Retail) %>%
  select(ClassificationId,Schedule,topy.adj.flv,topy.adj.fmv,parent.delta.flv,parent.delta.fmv,child.delta.flv,child.delta.fmv)

## join the results back to the table with full years
joinMakeSF.upd<-merge(joinMakeSF,deltaMakeAdjcalc,by='ClassificationId') %>% arrange(ClassificationId,desc(ModelYear))

## validation
dim(joinMakeOut)[1]==dim(joinMakeSF)[1]/10
dim(joinMakeOut)[1]==dim(deltaMakeAdjcalc)[1]
dim(joinMakeSF.upd)[1]==dim(joinMakeSF)[1]


## interpolate the years between top year and 7th year
for (i in 1:dim(joinMakeSF.upd)[1]){
  
  ### Auction
  ## deprecation rate by year
  joinMakeSF.upd$diffRate.auc[i] = ifelse(joinMakeSF.upd$ClassificationId[i] == joinMakeSF.upd$ClassificationId[i-1] &
                                        joinMakeSF.upd$ModelYear[i] == joinMakeSF.upd$ModelYear[i-1]-1,
                                         round((joinMakeSF.upd$Adjflv[i-1] - joinMakeSF.upd$Adjflv[i])/joinMakeSF.upd$parent.delta.flv[i],digits=4),0)
  
  ## adjusted schedule
  joinMakeSF.upd$adjustrate.auc[i] = joinMakeSF.upd$topy.adj.flv[i]
  joinMakeSF.upd$adjustrate.auc[i] = ifelse(joinMakeSF.upd$ModelYear[i] <= topyear -6, joinMakeSF.upd$Adjflv[i]*joinMakeSF.upd$Auction[i],
                                           ifelse(joinMakeSF.upd$ModelYear[i] == topyear, joinMakeSF.upd$topy.adj.flv[i],
                                                 joinMakeSF.upd$adjustrate.auc[i-1]-joinMakeSF.upd$child.delta.flv[i]*joinMakeSF.upd$diffRate.auc[i]))
  ### Retail
  ## deprecation rate by year
  joinMakeSF.upd$diffRate.ret[i] = ifelse(joinMakeSF.upd$ClassificationId[i] == joinMakeSF.upd$ClassificationId[i-1] &
                                            joinMakeSF.upd$ModelYear[i] == joinMakeSF.upd$ModelYear[i-1]-1,
                                          round((joinMakeSF.upd$Adjfmv[i-1] - joinMakeSF.upd$Adjfmv[i])/joinMakeSF.upd$parent.delta.fmv[i],digits=4),0)
  
  ## adjusted schedule
  joinMakeSF.upd$adjustrate.ret[i] = joinMakeSF.upd$topy.adj.fmv[i]
  joinMakeSF.upd$adjustrate.ret[i] = ifelse(joinMakeSF.upd$ModelYear[i] <= topyear -6, joinMakeSF.upd$Adjfmv[i]*joinMakeSF.upd$Retail[i],
                                            ifelse(joinMakeSF.upd$ModelYear[i] == topyear, joinMakeSF.upd$topy.adj.fmv[i],
                                                   joinMakeSF.upd$adjustrate.ret[i-1]-joinMakeSF.upd$child.delta.fmv[i]*joinMakeSF.upd$diffRate.ret[i]))
}

AllTmake <-joinMakeSF.upd %>%
  select(ClassificationId,Schedule.x,CategoryId,SubcategoryName, MakeName,MakeId, ModelYear,adjustrate.auc,adjustrate.ret) %>%
  rename(flv_make=adjustrate.auc,fmv_make=adjustrate.ret,Schedule=Schedule.x) %>%
  arrange(ClassificationId,desc(ModelYear))

###################################################################################################################
###################################################################################################################
########################################## 14.  MAKE ADJUSTMENTS - Checks #########################################
###################################################################################################################
###################################################################################################################
################################################################## MoM limit  ################################################################## 


#MoMlimit<-merge(AllTmake,LastMonth_Sched %>% filter(!is.na(MakeId)) %>% select(ClassificationId,ModelYear,CurrentFmv,CurrentFlv),by=c("ClassificationId","ModelYear"),all.x=T) %>%
#  mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM)),
#         limit_flv = ifelse(is.na(CurrentFlv),flv_make,MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM))) 




## Limit by last month 
lastM_schedule_make<-LastMonth_Sched %>%
  filter(ModelYear>=botyear-1 & ModelYear <= topyear) %>%
  select(ClassificationId,ModelYear,CurrentFmv, CurrentFlv) %>%
  distinct()


'              Temporary use for 04/30 effective date - rebasing                    '


RebaseTemp_1make<-merge(AllTmake %>% filter(ModelYear==botyear), lastM_schedule_make %>% filter(ModelYear==botyear-1),by=c("ClassificationId"),all.x=T) %>%
  mutate(CurrentFmv=ifelse(is.na(CurrentFmv),fmv_make/(1+x),CurrentFmv),
         CurrentFlv=ifelse(is.na(CurrentFlv),flv_make/(1+x),CurrentFlv)) %>%
  select(ClassificationId, Schedule, fmv_make,flv_make,CurrentFmv,CurrentFlv)

RebaseTemp_2make<-merge(RebaseTemp_1make,combDeprApr %>% filter(ModelYear=='Dep'),by=c("Schedule"),all.x=T) %>%
  replace(is.na(.),depBound_upp) %>%
  mutate(Adj2011Fmv = ifelse(CurrentFmv<fmv_make,pmax(pmin(fmv_make, CurrentFmv * (1 + rate)*.99*(1+x)),CurrentFmv * (1+rate)*(.99)*(1-x)),fmv_make),
         Adj2011Flv = ifelse(CurrentFlv<flv_make,pmax(pmin(flv_make, CurrentFlv * (1 + rate)*.99*(1+x)),CurrentFlv * (1+rate)*(.99)*(1-x)),flv_make)) %>%
  select(ClassificationId, Adj2011Fmv,Adj2011Flv) %>%
  mutate(ModelYear =botyear)


AllTmake_new <- merge(AllTmake,RebaseTemp_2make,by=c('ClassificationId','ModelYear'),all.x=T) %>%
  mutate(Adjfmv = ifelse(ModelYear == botyear,Adj2011Fmv,fmv_make),
         Adjflv = ifelse(ModelYear == botyear,Adj2011Flv,flv_make)) %>%
  select(-Adj2011Fmv,-Adj2011Flv)

'              Temporary use for 04/30 effective date - rebasing                    '


MoMlimit<-merge(AllTmake_new,LastMonth_Sched %>% filter(!is.na(MakeId)) %>% select(ClassificationId,ModelYear,CurrentFmv,CurrentFlv),by=c("ClassificationId","ModelYear"),all.x=T) %>%
 # mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM)),
 #        limit_flv = ifelse(is.na(CurrentFlv),Adjflv,ifelse(CategoryId ==2616,MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM_spec),MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM)))) %>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),Adjfmv,ifelse(ModelYear==botyear, MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM_spec),MoMlimitFunc(CurrentFmv,Adjfmv,limUp_MoM,limDw_MoM))),
         limit_flv = ifelse(is.na(CurrentFlv),Adjflv,ifelse(ModelYear==botyear, MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM_spec),MoMlimitFunc(CurrentFlv,Adjflv,limUp_MoM,limDw_MoM)))) %>%
 # mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,fmv_make),
 #        limit_flv = ifelse(is.na(CurrentFlv),flv_make,flv_make)) %>%
  #### One time change for market condition in March,2020
  #mutate(limit_flv = ifelse(CategoryId %in% c(2515, 360, 29, 362, 15, 6, 2509, 2505, 32, 2599, 164),limit_flv * .96, ifelse(CategoryId ==2616, limit_flv * .90,limit_flv * .93)))%>%
  arrange(ClassificationId,ModelYear)


#MoMlimit <- MoMlimit %>% filter(Schedule != 'Sweeper And Brooms Ride-On USA')


'              Temporary use for 04/30 effective date - rebasing                    '


######################################################### Retail vs Auction check ########################################################## 

Ck_Make<-MoMlimit %>%
  arrange(ClassificationId,ModelYear)%>% 
  mutate(rate = limit_fmv/limit_flv,
         YTD = topyear-ModelYear)%>%
  filter (rate < 1) %>%
  mutate(yearAge = ifelse(YTD<chanyr*2,"newer","elder")) %>%
  mutate(schedule = ifelse(yearAge=='elder',limit_flv*(1+capChannel/2),limit_fmv/(1+capChannel))) %>%
  select(ClassificationId,ModelYear,yearAge,schedule)


assignValMake<-merge(MoMlimit,Ck_Make,by=c("ClassificationId","ModelYear"),all.x =T)
assignValMake[is.na(assignValMake)] <- 0

for (i in 1:nrow(assignValMake)){
  assignValMake$chann[i] = if(assignValMake$yearAge[i] == 'elder'){
    assignValMake$schedule[i]
  }
  
  else{
    assignValMake$limit_fmv[i]
  } 
  
  
  assignValMake$chann2[i] = if(assignValMake$yearAge[i] == 'newer'){
    assignValMake$schedule[i]
  }
  else{
    assignValMake$limit_flv[i]
  } 
}



fixChannel_Make<-assignValMake %>% 
  select(ClassificationId, Schedule, CategoryId,MakeName,ModelYear,chann,chann2) %>%
  rename(limit_fmv=chann,limit_flv=chann2)

fixChannel_Make %>%
  arrange(ClassificationId,ModelYear)%>% 
  mutate(rate = limit_fmv/limit_flv) %>%
  filter (rate < 1) 




######################################################### Year over year check on Tier 1 & 2 ########################################################## 
fixYear_Make = fixChannel_Make %>% arrange(ClassificationId,desc(ModelYear))


for (i in 1:dim(fixYear_Make)[1]){
  
  fixYear_Make$limit_fmv_temp[i] = 
    if (i == 1) {fixYear_Make$limit_fmv[i]}
  else{
    if(fixYear_Make$ClassificationId[i] == fixYear_Make$ClassificationId[i-1] & fixYear_Make$ModelYear[i] == botyear+1 
       & fixYear_Make$limit_fmv[i-1] / fixYear_Make$limit_fmv[i] <1.02){
      fixYear_Make$limit_fmv[i-1] /1.02
    }
    else{
      fixYear_Make$limit_fmv[i]
    }
  }
  fixYear_Make$limit_flv_temp[i] = 
    
    if(i==1){fixYear_Make$limit_flv[i]}
  else{
    if(fixYear_Make$ClassificationId[i] == fixYear_Make$ClassificationId[i-1] & fixYear_Make$ModelYear[i] == botyear+1 
       & fixYear_Make$limit_flv[i-1] / fixYear_Make$limit_flv[i] <1.02){
      fixYear_Make$limit_flv[i-1] /1.02
    }
    else{
      fixYear_Make$limit_flv[i]
    }
  } 
}


fixYear_Make =fixYear_Make %>% select(-limit_fmv ,-limit_flv) %>% rename(limit_fmv=limit_fmv_temp ,  limit_flv=limit_flv_temp)


for (i in 1:nrow(fixYear_Make)){
  fixYear_Make$retFlag[i] = ifelse(fixYear_Make$ClassificationId[i]==fixYear_Make$ClassificationId[i+1],
                              ifelse(round(as.numeric(fixYear_Make$limit_fmv[i])/as.numeric(fixYear_Make$limit_fmv[i+1]),digits=4)<1,"flag",""),"")
  fixYear_Make$aucFlag[i]= ifelse(fixYear_Make$ClassificationId[i]==fixYear_Make$ClassificationId[i+1],
                             ifelse(round(fixYear_Make$limit_flv[i]/fixYear_Make$limit_flv[i+1],digits=4)<1,"flag",""),"")
}

### issued classifications on year
fixYear_Make %>% filter(retFlag=='flag' |aucFlag=='flag') 
MakeIssYear<-fixYear_Make %>% filter(retFlag=='flag' |aucFlag=='flag') %>% select(ClassificationId)  


############################################ Grab corresponding depreciation and appreciation ###############################################

#### Join the subcat level appreciation and depreciation with make map

DeprecMake<-merge(fixYear_Make %>% select(ClassificationId,Schedule) %>% distinct(),combDeprApr,by='Schedule') 


MoM_deprapr_Make<-merge(DeprecMake,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App',  MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit))),
         limit_flv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit)))) %>%
  arrange(ClassificationId,ModelYear)



MakeSchedOutput<-rbind(fixYear_Make %>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),MoM_deprapr_Make %>% select(ClassificationId,ModelYear,limit_fmv,limit_flv)) %>% 
  arrange(ClassificationId,desc(ModelYear))

Final_makeSched = MakeSchedOutput


#joinMakeOut<-MakeSchedOutput %>% filter(ClassificationId==101729)
#write.csv(Final_makeSched,"20190109Final_makeSched.csv")


