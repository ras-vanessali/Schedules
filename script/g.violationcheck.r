
###################################################################################################################
###################################################################################################################
####################################### 5. CHECK THE RULES - YEAR OVER YEAR #######################################
###################################################################################################################
###################################################################################################################


############################# 5.A YEARS - smooth the curve: older year with lower recovery than newer year

####################################### 5.A Retail ############################################


adjusted_Ret<-recencyRet.apply
head(adjusted_Ret,11)
## column 9: set the upper bound based on the year newer 
for (i in 1:(dim(adjusted_Ret)[1]-1)){
  
  adjusted_Ret$yoyUp[i] = 
    if (adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1]){
      as.numeric(adjusted_Ret$fmv_regrRect[i+1])/(1+adjusted_Ret$SlopeRet[i])
    }
  else{
    as.numeric(adjusted_Ret$fmv_regrRect[i])
  }
}



## column 10: set the lower bound based on the year older 
for (i in 2:dim(adjusted_Ret)[1]){
  
  adjusted_Ret$yoyLow[i] = 
    if (adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i-1]){
      as.numeric(adjusted_Ret$fmv_regrRect[i-1])*(1+adjusted_Ret$SlopeRet[i])
    }
  else{
    as.numeric(adjusted_Ret$fmv_regrRect[i])
  }
}


## column 11: update the adjusters with upper and lower bounds 
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoyUp[i] = ifelse(is.na(adjusted_Ret$yoyUp[i]),UpperB,ifelse(adjusted_Ret$ModelYear[i]==topyear,UpperB,adjusted_Ret$yoyUp[i]))
  adjusted_Ret$yoyLow[i] = ifelse(is.na(adjusted_Ret$yoyLow[i]),LowerB,ifelse(adjusted_Ret$ModelYear[i]==botyear,LowerB,adjusted_Ret$yoyLow[i]))
  adjusted_Ret$yoyCap[i] = min(max(adjusted_Ret$fmv_rec[i],adjusted_Ret$yoyLow[i]),adjusted_Ret$yoyUp[i])
  
}

## column 12: check the gap between adjacent years
## column 13: flag out the year which has close schedule with the newer year (row i & i+1)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoycheck[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                   adjusted_Ret$yoyCap[i+1]/adjusted_Ret$yoyCap[i],1+adjusted_Ret$SlopeRet[i])
  adjusted_Ret$yoyFlag[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                  ifelse(round(adjusted_Ret$yoyCap[i+1]/adjusted_Ret$yoyCap[i],digits=4)<1+adjusted_Ret$SlopeRet[i],"flag",""),"")
}

## column 14: if column 12 is flagged, check # of data points in i > i+1, if yes -->1, else -->0 (if tie, 0)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoydtpts[i]= ifelse(adjusted_Ret$yoyFlag[i]=='flag',
                                   ifelse(adjusted_Ret$n[i]>=adjusted_Ret$n[i+1],1,0),"")
}


## column 15: take the larger number of data points, keep it and change the schedule with less data 
for (i in 1:nrow(adjusted_Ret)){
  
  adjusted_Ret$yoyUpd[i]= ifelse(adjusted_Ret$yoydtpts[i]==0, adjusted_Ret$yoyCap[i+1]/(1+adjusted_Ret$SlopeRet[i]),
                                 ifelse(adjusted_Ret$yoydtpts[i-1]==1, adjusted_Ret$yoyCap[i-1]*(1+adjusted_Ret$SlopeRet[i]),adjusted_Ret$yoyCap[i])) 
  adjusted_Ret$yoyUpd[i]= ifelse(is.na(adjusted_Ret$yoyUpd[i]),adjusted_Ret$yoyCap[i],adjusted_Ret$yoyUpd[i])
}

## column 16 (same as column 12): flag out the year which has close schedule with the newer year (row i & i+1)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoyFinFlag[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                     ifelse(round(adjusted_Ret$yoyUpd[i+1]/adjusted_Ret$yoyUpd[i],digits=4)<1+adjusted_Ret$SlopeRet[i],"flag",""),"")
}







adjusted_Auc<-recencyAuc.apply

####################################### 5.B Auction ############################################

## column 9: set the upper bound based on the year newer 
for (i in 1:(dim(adjusted_Auc)[1]-1)){
  
  adjusted_Auc$yoyUp[i] = 
    if (adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1]){
      as.numeric(adjusted_Auc$flv_regrRect[i+1])/(1+adjusted_Auc$SlopeAuc[i])
    }
  else{
    as.numeric(adjusted_Auc$flv_regrRect[i])
  }
}

## column 10: set the lower bound based on the year older 
for (i in 2:dim(adjusted_Auc)[1]){
  
  adjusted_Auc$yoyLow[i] = 
    if (adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i-1]){
      as.numeric(adjusted_Auc$flv_regrRect[i-1])*(1+adjusted_Auc$SlopeAuc[i])
    }
  else{
    as.numeric(adjusted_Auc$flv_regrRect[i])
  }
}


## column 10: update the adjusters with upper and lower bounds 
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoyUp[i] = ifelse(is.na(adjusted_Auc$yoyUp[i]),UpperB,ifelse(adjusted_Auc$ModelYear[i]==topyear,UpperB,adjusted_Auc$yoyUp[i]))
  adjusted_Auc$yoyLow[i] = ifelse(is.na(adjusted_Auc$yoyLow[i]),LowerB,ifelse(adjusted_Auc$ModelYear[i]==botyear,LowerB,adjusted_Auc$yoyLow[i]))
  adjusted_Auc$yoyCap[i] = min(max(adjusted_Auc$flv_rec[i],adjusted_Auc$yoyLow[i]),adjusted_Auc$yoyUp[i])
  
}

## column 12: check the gap between adjacent years
## column 13: flag out the year which has close schedule with the newer year (row i & i+1)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoycheck[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                   adjusted_Auc$yoyCap[i+1]/adjusted_Auc$yoyCap[i],1+adjusted_Auc$SlopeAuc[i])
  
  adjusted_Auc$yoyFlag[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                  ifelse(round(adjusted_Auc$yoyCap[i+1]/adjusted_Auc$yoyCap[i],digits=4)<1+adjusted_Auc$SlopeAuc[i],"flag",""),"")
  adjusted_Auc$yoyFlag[i] = ifelse(is.na(adjusted_Auc$yoyFlag[i]),"",adjusted_Auc$yoyFlag[i])
}


## column 14: if column 13 is flagged, check # of data points in i > i+1, if yes -->1, else -->0 (if tie, 0)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoydtpts[i]= ifelse(adjusted_Auc$yoyFlag[i]=='flag',
                                   ifelse(adjusted_Auc$n[i]>=adjusted_Auc$n[i+1],1,0),"")
}


## column 15: take the larger number of data points, keep it and change the schedule with less data 
for (i in 1:nrow(adjusted_Auc)){
  
  adjusted_Auc$yoyUpd[i]= ifelse(adjusted_Auc$yoydtpts[i]==0, adjusted_Auc$yoyCap[i+1]/(1+adjusted_Auc$SlopeAuc[i]),
                                 ifelse(adjusted_Auc$yoydtpts[i-1]==1, adjusted_Auc$yoyCap[i-1]*(1+adjusted_Auc$SlopeAuc[i]),adjusted_Auc$yoyCap[i])) 
  adjusted_Auc$yoyUpd[i]= ifelse(is.na(adjusted_Auc$yoyUpd[i]),adjusted_Auc$yoyCap[i],adjusted_Auc$yoyUpd[i])
}


## column 16 (same as column 12): flag out the year which has close schedule with the newer year (row i & i+1)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoyFinFlag[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                     ifelse(round(adjusted_Auc$yoyUpd[i+1]/adjusted_Auc$yoyUpd[i],digits=4)<1+adjusted_Auc$SlopeAuc[i],"flag",""),"")
}



############################# 5.C YEARS - check the rule: older year with lower recovery than newer year
### check if there is 0 flag
# Retail
check_retail <- adjusted_Ret%>%
  filter(yoyFinFlag=='flag') %>%
  summarise(n=n())
check_retail

# if flag <> 0, who is the one violate the rule
schedgroup_retail <- adjusted_Ret%>%
  filter(yoyFinFlag=='flag') %>%
  group_by(Schedule) %>%
  summarise(n=n())
schedgroup_retail


# Auction
flagno_auction <- adjusted_Auc%>%
  filter(yoyFinFlag=='flag') %>%
  summarise(n=n())
flagno_auction

# if flag <> 0, who is the one violate the rule
schedgroup_auction <- adjusted_Auc%>%
  filter(yoyFinFlag=='flag') %>%
  group_by(Schedule) %>%
  summarise(n=n())
schedgroup_auction

############################ 5.D Format the tables and prepare for later use
RetSchedule<-adjusted_Ret %>%
  select(Schedule,ModelYear,yoyUpd,n) 
colnames(RetSchedule)<-c("Schedule","ModelYear","Adjfmv","UnitsRet")

AucSchedule<-adjusted_Auc %>%
  select(Schedule,ModelYear,yoyUpd,n) 
colnames(AucSchedule)<-c("Schedule","ModelYear","Adjflv","UnitsAuc")

print(if(nSched_Ret == dim(RetSchedule)[1]/10){paste('Yes, the N_retail is ', nSched_Ret)} else {'No'})
print(if(nSched_Auc == dim(AucSchedule)[1]/10){paste('Yes, the N_auction is ', nSched_Auc)} else {'No'})




"                           BORROW schedules                          "

######## Auction Borrow Schedules split - A. Ratio = retail / retail; B. Ratio = make adjuster ########
######## Retail Borrow Schedules split - A. Ratio = retail / retail; B. Ratio = make adjuster ########
# case A
BrwAucList <- SchedR %>% filter(BorrowType != 'AuctionBorrowRetail') %>% select(Schedule,BorrowSchedule, BorrowType) 
BrwRetList <- SchedR %>% filter(BorrowType != 'RetailBorrowAuction') %>% select(Schedule,BorrowSchedule, BorrowType) 


############################ Borrow Auction ############################ 
Num_dtpts_Auc<-merge(Data_all_plot,BrwAucList,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Auction' & YearFlag=='AdjusUseYr') %>%
  group_by(BorrowSchedule,Schedule) %>%
  summarise(Numcomps=n())


borwAuc_data<-merge(Data_all_plot,Num_dtpts_Auc,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Auction' & YearFlag=='AdjusUseYr') %>%
  group_by(Schedule) %>%
  arrange(desc(EffectiveDate)) %>%
  slice(1:ifelse(Numcomps>=threshold_brw,threshold_brw,99999)) 

borwAuc_Yr<- borwAuc_data %>%
  mutate(power = month(EffectiveDate)-month(publishDate),
         revised_SaleAB = SaleAB/(0.99^power)) %>%
  group_by(BorrowSchedule,Schedule,ModelYear) %>%
  summarise(recover= mean(revised_SaleAB),n=n())


borwAuc_Rate<-merge(borwAuc_Yr,OutRegression_Auc %>% rename(BorrowSchedule=Schedule),by=c('BorrowSchedule','ModelYear')) %>% 
  group_by(Schedule,BorrowSchedule) %>%
  summarise(diffperc= (sum(recover/flv *n))/sum(n),
            N=sum(n)) %>%
  mutate(offRate = ifelse(N>threshold_brw,1*diffperc,((threshold_brw-N)+N*diffperc)/threshold_brw))


## join to the full list, if there is no sale data will apply 1 as the rate
RetbAuc<-merge(BrwAucList %>% select(-BorrowType),borwAuc_Rate,all.x=T,by=c('Schedule','BorrowSchedule')) %>%
  mutate(offRate = ifelse(is.na(offRate),1,offRate)) %>%
  select(-diffperc, -N)


############################ Borrow Auction ############################ 
Num_dtpts_Ret<-merge(Data_all_plot,BrwRetList,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Retail' & YearFlag=='AdjusUseYr') %>%
  group_by(BorrowSchedule,Schedule) %>%
  summarise(Numcomps=n())


borwRet_data<-merge(Data_all_plot,Num_dtpts_Ret,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Retail' & YearFlag=='AdjusUseYr') %>%
  group_by(Schedule) %>%
  arrange(desc(EffectiveDate)) %>%
  slice(1:ifelse(Numcomps>=threshold_brw,threshold_brw,99999)) 

borwRet_Yr <- borwRet_data %>%
  mutate(power = month(EffectiveDate)-month(publishDate),
         revised_SaleAB = SaleAB/(0.99^power)) %>%
  group_by(BorrowSchedule,Schedule,ModelYear) %>%
  summarise(recover= mean(revised_SaleAB),n=n())


borwRet_Rate<-merge(borwRet_Yr,OutRegression_Ret %>% rename(BorrowSchedule=Schedule),by=c('BorrowSchedule','ModelYear')) %>% 
  group_by(Schedule,BorrowSchedule) %>%
  summarise(diffperc= (sum(recover/fmv *n))/sum(n),
            N=sum(n)) %>%
  mutate(offRate = ifelse(N>threshold_brw,1*diffperc,((threshold_brw-N)+N*diffperc)/threshold_brw))

## join to the full list, if there is no sale data will apply 1 as the rate
AucbRet<-merge(BrwRetList %>% select(-BorrowType),borwRet_Rate,all.x=T,by=c('Schedule','BorrowSchedule')) %>%
  mutate(offRate = ifelse(is.na(offRate),1,offRate)) %>%
  select(-diffperc, -N)


####### END temporary for March #########

#print(if(nSched_Auc == dim(inheritRet)[1]){paste('Yes, the N_auction is ', nSched_Auc)} else {'No'})
#print(if(nSched_Ret == dim(inheritAuc)[1]){paste('Yes, the N_auction is ', nSched_Ret)} else {'No'})

#write.csv(inheritRet,'AuctionBorrow Rates.csv')

####################################### Apply the borrow factors on all years ###################################
## list the index to weaken the scale factor for newer years
yrAdj = data.frame(ModelYear = topyear:botyear, indexYrMake = c(0.1,0.25,0.4,0.55,0.7,0.85,1,1,1,1))

# join the map to the auction values
inheritAucOut<-merge(merge(AucSchedule %>% rename(BorrowSchedule=Schedule),RetbAuc,by='BorrowSchedule'),yrAdj,by='ModelYear') %>% 
  mutate(lamda = pmin(1,((month(publishDate)-6)/12)*phaseinFactor + indexYrMake)) %>%
  mutate(offRate_Yr = ifelse(ModelYear <= topyear -phaseinAge, offRate, (offRate -1) * lamda + 1)) %>%
  mutate(Adjflv_make = as.numeric(Adjflv)*offRate_Yr) %>%
  select(Schedule,ModelYear,Adjflv,Adjflv_make,offRate_Yr,UnitsAuc) %>%
  arrange(Schedule,desc(ModelYear))



inheritRetOut<-merge(merge(RetSchedule %>% rename(BorrowSchedule=Schedule),AucbRet,by='BorrowSchedule'),yrAdj,by='ModelYear') %>% 
  mutate(lamda = pmin(1,((month(publishDate)-6)/12)*phaseinFactor + indexYrMake)) %>%
  mutate(offRate_Yr = ifelse(ModelYear <= topyear -phaseinAge, offRate, (offRate -1) * lamda + 1)) %>%
  mutate(Adjfmv_make = as.numeric(Adjfmv)*offRate_Yr) %>%
  select(Schedule,ModelYear,Adjfmv,Adjfmv_make,offRate_Yr,UnitsRet) %>%
  arrange(Schedule,desc(ModelYear))

#write.csv(inheritAucOut,'auctionborrow schedules.csv')

AucBorwOut <-inheritAucOut %>%
  select(Schedule,ModelYear,Adjflv_make,UnitsAuc) %>%
  rename(Adjflv=Adjflv_make) %>%
  arrange(Schedule,desc(ModelYear))


RetBorwOut <-inheritRetOut %>%
  select(Schedule,ModelYear,Adjfmv_make,UnitsRet) %>%
  rename(Adjfmv=Adjfmv_make) %>%
  arrange(Schedule,desc(ModelYear))

#### combine the regular shcedules with borrow schedules

AucSchedule_full<-rbind(AucSchedule,AucBorwOut)
RetSchedule_full<-rbind(RetSchedule,RetBorwOut)



#print(if(sum(nSched_Ret) == dim(RetSchedule_full)[1]/10){ paste('Yes, the N total is ',sum(nCat,nSched_Auc, nSched_Ret))} else{'No'})
#print(if(sum(nCat,nSched_Auc, nSched_Ret) == dim(AucSchedule_full)[1]/10){ paste('Yes, the N total is ',sum(nCat,nSched_Auc, nSched_Ret))} else{'No'})



JoinChannel<-merge(RetSchedule_full,AucSchedule_full,by=c("Schedule","ModelYear")) %>%
  arrange(Schedule,as.numeric(ModelYear))

#JoinChannel<-merge(RetSchedule,AucSchedule,by=c("Schedule","ModelYear")) %>%
#  arrange(Schedule,as.numeric(ModelYear))


###################################################################################################################
###################################################################################################################
######################################### 6. CAP - NEWEST YEARS IN RANGE ##########################################
###################################################################################################################
###################################################################################################################

################## 6.A prepare the new table with only the newest and 4th newest years with cap boundaries


## filter to newest year and 4th year
topyrSched<-JoinChannel %>% filter(ModelYear == topyear) %>% select(Schedule,Adjfmv,Adjflv) %>% rename(Adjfmvtop=Adjfmv,Adjflvtop=Adjflv)
fixyrSched<-JoinChannel %>% filter(ModelYear == topyear-fixyr_gap(Schedule)) %>% select(Schedule,Adjfmv,Adjflv) %>% rename(Adjfmvfix=Adjfmv,Adjflvfix=Adjflv)

BorrowBoundsin<-merge(Sched,SchedR %>% rename(Schedule = BorrowSchedule, ABSched=Schedule),by='Schedule') %>%
  mutate(RetailNewYrMin = ifelse(is.na(RetailNewYrMin.y),RetailNewYrMin.x, RetailNewYrMin.y),
         RetailNewYrMax = ifelse(is.na(RetailNewYrMax.y),RetailNewYrMax.x, RetailNewYrMax.y),
         AuctionNewYrMin = ifelse(is.na(AuctionNewYrMin.y),AuctionNewYrMin.x, AuctionNewYrMin.y),
         AuctionNewYrMax = ifelse(is.na(AuctionNewYrMax.y),AuctionNewYrMax.x, AuctionNewYrMax.y)) %>%
  select(ABSched,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax)


boundsIn <- rbind(Sched,BorrowBoundsin %>% rename(Schedule=ABSched))


## calculate the bounds
exactCaps <-boundsIn %>% 
  mutate(AuctionMin = NewYr_CapFunc(AuctionNewYrMin),
         AuctionMax = NewYr_CapFunc(AuctionNewYrMax) ,
         RetailMin = NewYr_CapFunc(RetailNewYrMin),
         RetailMax = NewYr_CapFunc(RetailNewYrMax)) %>%
  select(Schedule,AuctionMin,AuctionMax,RetailMin,RetailMax)


join_topfix <- merge(merge(topyrSched,fixyrSched,by=c("Schedule")),exactCaps,by=c("Schedule")) %>%
  ### check if the top year is capped or not, if yes, is it Max or Min
  mutate(isCapA = WithinRange(Adjflvtop,AuctionMax,AuctionMin),
         isCapR = WithinRange(Adjfmvtop,RetailMax,RetailMin)) %>%
  mutate(DiffA = ifelse(isCapA=='Max', AuctionMax-Adjflvfix,ifelse(isCapA=='Min',AuctionMin-Adjflvfix,0)),
         DiffR = ifelse(isCapR=='Max', RetailMax -Adjfmvfix,ifelse(isCapR=='Min',RetailMin -Adjfmvfix,0)),
         capsideA = ifelse(isCapA=='Max', AuctionMax,ifelse(isCapA=='Min',AuctionMin,0)),
         capsideR = ifelse(isCapR=='Max', RetailMax,ifelse(isCapR=='Min',RetailMin,0)),
         top_fixR = Adjfmvtop - Adjfmvfix,
         top_fixA = Adjflvtop - Adjflvfix) %>%
  select(Schedule, isCapA,isCapR, Adjflvtop, Adjfmvtop,DiffA, DiffR, capsideA, capsideR, top_fixR,top_fixA)


capSched <- merge(JoinChannel,join_topfix,by=c("Schedule")) %>%
  arrange(Schedule,ModelYear) %>%
  mutate(YTD = topyear-ModelYear) 

capSched_keep <- capSched %>% filter(YTD>fixyr_gap(Schedule)) %>%
  rename(Cap_fmv=Adjfmv,Cap_flv=Adjflv) %>%
  select(Schedule,ModelYear,Cap_fmv,UnitsRet,Cap_flv,UnitsAuc)

### retail: age 0 to 3 would interpolate the shape with its own adjusted fmv
### auction: age 0 to 3 of non-AbR schedules would interpolate the shape from retail. 
### auction AbR schedules interpolate the shape of exponential using the log regression slope.

capSched_cap <- capSched %>% filter(YTD<=fixyr_gap(Schedule)) %>%
  mutate(AbR_itp = ifelse(YTD == 1, .5, ifelse(YTD ==2, 5/6, ifelse(YTD ==3, 1, 0)))) %>%
  mutate(Cap_fmv= ifelse(isCapR=='', Adjfmv, capsideR - ((Adjfmvtop-Adjfmv)/top_fixR)*DiffR),
         Cap_flv= ifelse(isCapA=='', Adjflv, 
                         ifelse(Schedule %in% rbind(InA,InB)$Schedule, capsideA - (AbR_itp)*DiffA, capsideA - ((Adjfmvtop-Adjfmv)/top_fixR)*DiffA))) %>%
  select(Schedule,ModelYear,Cap_fmv,UnitsRet,Cap_flv,UnitsAuc)

bindCaptb<-rbind(capSched_cap,capSched_keep) %>%
  arrange(Schedule,ModelYear) %>%
  rename(Adjfmv=Cap_fmv,Adjflv=Cap_flv)


###################################################################################################################
###################################################################################################################
####################################### 7. CHECK THE RULES - RETAIL VS AUCTION ####################################
###################################################################################################################
###################################################################################################################


############################# 7.A CHANNELS - prevent cross over: auction side with lower recovery than retail side

####### Apply the logic of age younger or older than 5 to adjust fmv or flv 
if (CountryCode == 'USA'){
  checkChannel<-bindCaptb %>%
  mutate(rate = Adjfmv/Adjflv,
         YTD = ModelYear)%>%
  filter (rate < 1+capChannel) %>%
  mutate(yearAge = ifelse(YTD<=chanyr,"newer",ifelse(YTD>=chanyr*2,"elder","check"))) %>%
  mutate(ageGroup = ifelse(yearAge=='check',ifelse(UnitsRet<retBelieve,'elder',ifelse(UnitsRet>=UnitsAuc,'newer','elder')),yearAge)) %>%
  mutate(schedule = ifelse(ageGroup=='elder',Adjflv*(1+capChannel),Adjfmv/(1+capChannel))) %>%
  select(Schedule,ModelYear,ageGroup,schedule)
  
} else{
  checkChannel<-bindCaptb %>%
  mutate(rate = Adjfmv/Adjflv,
         YTD = ModelYear)%>%
  filter (rate < 1+capChannel) %>%
  mutate(yearAge = ifelse(str_detect(Schedule,'RbA'),"newer","elder")) %>%
  mutate(ageGroup = ifelse(yearAge=='check',ifelse(UnitsRet<retBelieve,'elder',ifelse(UnitsRet>=UnitsAuc,'newer','elder')),yearAge)) %>%
  mutate(schedule = ifelse(ageGroup=='elder',Adjflv*(1+capChannel),Adjfmv/(1+capChannel))) %>%
  select(Schedule,ModelYear,ageGroup,schedule)

}




assignVal<-merge(bindCaptb,checkChannel,by=c("Schedule","ModelYear"),all.x =T)
assignVal[is.na(assignVal)] <- 0

for (i in 1:nrow(assignVal)){
  assignVal$chann[i] = if(assignVal$ageGroup[i] == 'elder'){
    assignVal$schedule[i]
  }
  
  else{
    assignVal$Adjfmv[i]
  } 
  
  
  assignVal$chann2[i] = if(assignVal$ageGroup[i] == 'newer'){
    assignVal$schedule[i]
  }
  else{
    assignVal$Adjflv[i]
  } 
}



fixChannel<-assignVal %>% 
  select(Schedule, ModelYear,chann,chann2) %>%
  rename(Adjfmv=chann,Adjflv=chann2)

#write.csv(fixChannel,"fixChannel.csv")

############################# 7.B CHANNELS - check the rule: auction side with lower recovery than retail side
checkedOutput<-fixChannel %>%
  arrange(Schedule,as.numeric(ModelYear)) %>%
  mutate(ret_to_auc=Adjfmv/Adjflv)

check_channels <- checkedOutput %>%
  filter(ret_to_auc < 1+capChannel) %>%
  summarise(n=n())
check_channels



############################ 7.C Format the tables and prepare for the second round year checks
round2Check<-checkedOutput %>% 
  select(Schedule, ModelYear,Adjfmv,Adjflv) %>%
  arrange(Schedule,desc(ModelYear))


###################################################################################################################
###################################################################################################################
####################################### 8. 2ND ROUND CHECKS ####################################
###################################################################################################################
###################################################################################################################


########################### 8.A YEARS 2ND ROUND - check the rule: older year with lower recovery than newer year 

for (i in 1:nrow(round2Check)){
  round2Check$retFlag[i] = ifelse(round2Check$Schedule[i]==round2Check$Schedule[i+1],
                                  ifelse(round(round2Check$Adjfmv[i]/round2Check$Adjfmv[i+1],digits=4)<1,"flag",""),"")
  round2Check$aucFlag[i]= ifelse(round2Check$Schedule[i]==round2Check$Schedule[i+1],
                                 ifelse(round(round2Check$Adjflv[i]/round2Check$Adjflv[i+1],digits=4)<1,"flag",""),"")
}

##### number of flags on year 
round2results <- round2Check %>%
  filter(retFlag=='flag' |aucFlag=='flag') %>%
  summarise(n=n())
round2results

review_schedule = round2Check %>%
  filter(retFlag=='flag' |aucFlag=='flag') %>%
  select(Schedule)

review_schedule

#### if flag <>0, view the scheules and adjustments
round2Check %>% 
  filter(Schedule == review_schedule[1,])

round2Check %>% 
  filter(Schedule == review_schedule[2,])

round2Check %>% 
  filter(Schedule == review_schedule[3,])



########################### 8.B CHANNEL 2ND ROUND - should be no change from 1 st round check, but check again

##### Check across channel - prepare a shared table with schedules and flags
checkedOutput<-round2Check %>%
  mutate(rate = Adjfmv/Adjflv
         ,chanFlag = ifelse(Adjfmv/Adjflv < 1+capChannel, "flag","")) %>%
  select('Schedule', 'ModelYear','Adjfmv','Adjflv','retFlag','aucFlag','chanFlag','rate')

##### number of flags on channel (we set 10% gap between retail and auction)
checkedOutput %>%
  filter(chanFlag=="flag") %>%
  summarise(n=n())

##### check if there are cross over - THIS RESULT MUST BE 0
checkedOutput %>%
  filter(rate<1) %>%
  summarise(n=n())



########################### 5.Prepare the final table to join to application mappping
ScheduleOut <- checkedOutput %>%
  select('Schedule', 'ModelYear','Adjfmv','Adjflv')