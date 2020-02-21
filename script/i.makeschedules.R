###################################################################################################################
###################################################################################################################
######################################### 11.  MAKE ADJUSTMENTS - Tier 1,2 ########################################
###################################################################################################################
###################################################################################################################
### join the make map to the calcualted CSM sp/value
makeAdj_calDt<-merge(joinmap_make,MakeAdjData,by=c('CategoryId','SubcategoryId'))

### calcualte the average sp/value (make adjusters) by schedule and make
MakeSFcalc_Sched<-makeAdj_calDt %>%
  group_by(Schedule,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_sched = sum(AvgspValue*Numcomps)/sum(Numcomps),
            nMake_sched = sum(Numcomps))
  
### calcualte the average sp/value (make adjusters) by category and make
MakeSFcalc_Categ<-makeAdj_calDt %>%
  group_by(CategoryId,CategoryName,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_cat = sum(AvgspValue*Numcomps)/sum(Numcomps),
            nMake_cat = sum(Numcomps)) 

### calcualte the average sp/value (make adjusters) by report group and make
MakeSFcalc_rptGrp<-makeAdj_calDt %>%
  group_by(ReportGroup,MakeId,MakeName,SaleType) %>%
  # base scale factor
  summarise(Makerate_grp = sum(AvgspValue*Numcomps)/sum(Numcomps),
            nMake_grp = sum(Numcomps)) 

### create a map between category and schedule 
Categ_sched <- rbind(In %>% select(CategoryId,Schedule),InR %>% select(CategoryId,Schedule),InA %>% select(CategoryId,Schedule)) %>% 
  distinct() %>%
  group_by(Schedule) %>%
  filter(row_number()==1)

CategLev_make<-merge(merge(MakeSFcalc_Sched,Categ_sched,by='Schedule',all.x=T),MakeSFcalc_Categ,by=c('CategoryId','MakeId','MakeName','SaleType')) 
  

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
  select(Schedule,ClassificationId, CategoryId,SubcategoryName, MakeName,MakeId,chancheck_ret,chancheck_auc) %>%
  rename(Retail=chancheck_ret,Auction=chancheck_auc)

joinMakeOut[is.na(joinMakeOut)]<-1
write.csv(joinMakeOut,paste(publishDate," MakeAdjuster.csv"))


################## 11.C Apply make adjusters to each model year

## list the index to weaken the scale factor for newer years
yrAdj = data.frame(ModelYear = topyear:botyear, indexYrMake = c(0.1,0.25,0.4,0.55,0.7,0.85,1,1,1,1))


## Apply and calculate the adjusted scale factors for each model year
MakeSF <- merge(joinMakeOut,yrAdj) %>% arrange(Schedule,ClassificationId,ModelYear) %>%
  mutate(lamda = pmin(1,((month(publishDate)-6)/12)*phaseinFactor + indexYrMake)) %>%
  mutate(Auction_Yr = ifelse(ModelYear <= topyear -phaseinAge, Auction, (Auction -1) * lamda + 1),
         Retail_Yr = ifelse(ModelYear <= topyear -phaseinAge, Retail, (Retail -1) * lamda + 1)) 



################## 12.A Join the make adjusters back to cat/subcat schedules and calculate base make level schedules

### calculate the makelevel schedules
joMakeAdjData <- merge(ScheduleOut,MakeSF,by=c('Schedule','ModelYear'),all.y=T) %>%
  mutate(fmv_make = Adjfmv*Retail_Yr,
         flv_make = Adjflv*Auction_Yr) %>%
  select(Schedule,ClassificationId, CategoryId,SubcategoryName, MakeId, MakeName,ModelYear,
         Retail,Auction,Retail_Yr,Auction_Yr,Adjfmv,Adjflv,fmv_make,flv_make)%>%
  arrange(Schedule,ClassificationId,MakeId,ModelYear)

#write.csv(joMakeAdjData,'20190313 MakeSchedules.csv')
AllTmake <- joMakeAdjData %>% select(Schedule,ClassificationId, CategoryId,SubcategoryName, MakeName,MakeId, ModelYear, fmv_make, flv_make)


###################################################################################################################
###################################################################################################################
########################################## 14.  MAKE ADJUSTMENTS - Checks #########################################
###################################################################################################################
###################################################################################################################
################################################################## MoM limit  ################################################################## 


#MoMlimit<-merge(AllTmake,LastMonth_Sched %>% filter(!is.na(MakeId)) %>% select(ClassificationId,ModelYear,CurrentFmv,CurrentFlv),by=c("ClassificationId","ModelYear"),all.x=T) %>%
#  mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM)),
#         limit_flv = ifelse(is.na(CurrentFlv),flv_make,MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM))) 



'              Temporary use for 04/30 effective date - rebasing                    '


## Limit by last month 
lastM_schedule_make<-LastMonth_Sched %>%
  filter(ModelYear>=botyear-1 & ModelYear <= topyear) %>%
  select(ClassificationId,ModelYear,CurrentFmv, CurrentFlv) %>%
  distinct()



RebaseTemp_1make<-merge(AllTmake %>% filter(ModelYear==botyear+1), lastM_schedule_make %>% filter(ModelYear==botyear),by=c("ClassificationId"),all.x=T) %>%
  mutate(CurrentFmv=ifelse(is.na(CurrentFmv),fmv_make/(1+x),CurrentFmv),
         CurrentFlv=ifelse(is.na(CurrentFlv),flv_make/(1+x),CurrentFlv)) %>%
  select(ClassificationId, Schedule, fmv_make,flv_make,CurrentFmv,CurrentFlv)

RebaseTemp_2make<-merge(RebaseTemp_1make,combDeprApr %>% filter(ModelYear=='Dep'),by=c("Schedule"),all.x=T) %>%
  replace(is.na(.),depBound_upp) %>%
  mutate(Adj2010Fmv = pmax(pmin(fmv_make, CurrentFmv * (1 + rate)*(1+x)),CurrentFmv * (1+rate)*(1-x)),
         Adj2010Flv = pmax(pmin(flv_make, CurrentFlv * (1 + rate)*(1+x)),CurrentFlv * (1+rate)*(1-x))) %>%
  select(ClassificationId, Adj2010Fmv,Adj2010Flv) %>%
  mutate(ModelYear =botyear+1)


AllTmake_new <- merge(AllTmake,RebaseTemp_2make,by=c('ClassificationId','ModelYear'),all.x=T) %>%
  mutate(Adjfmv = ifelse(ModelYear == botyear+1,Adj2010Fmv,fmv_make),
         Adjflv = ifelse(ModelYear == botyear+1,Adj2010Flv,flv_make)) %>%
  select(-Adj2010Fmv,-Adj2010Flv)


MoMlimit<-merge(AllTmake_new,LastMonth_Sched %>% filter(!is.na(MakeId)) %>% select(ClassificationId,ModelYear,CurrentFmv,CurrentFlv),by=c("ClassificationId","ModelYear"),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),fmv_make,MoMlimitFunc(CurrentFmv,fmv_make,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),flv_make,ifelse(CategoryId ==2616,MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM_spec),MoMlimitFunc(CurrentFlv,flv_make,limUp_MoM,limDw_MoM)))) 


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


