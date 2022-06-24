
###################################################################################################################
###################################################################################################################
############################################# Check The Violations - YoY ##########################################
############## YoY check --> Borrow Sched --> Cap Newest Yr --> Channel check --> Second Round Check ##############
###################################################################################################################


##YEARS - smooth the curve: older year with lower recovery than newer year
### Retail 
adjusted_Ret<-recencyRet.apply
## step1 set the upper bound based on the year newer 
for (i in 1:(dim(adjusted_Ret)[1]-1)){
  
  adjusted_Ret$yoyUp[i] = 
    if (adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1]){
      as.numeric(adjusted_Ret$fmv_regrRect[i+1])/(1+adjusted_Ret$SlopeRet[i])
    }
  else{
    as.numeric(adjusted_Ret$fmv_regrRect[i])
  }
}



## step2 set the lower bound based on the year older 
for (i in 2:dim(adjusted_Ret)[1]){
  
  adjusted_Ret$yoyLow[i] = 
    if (adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i-1]){
      as.numeric(adjusted_Ret$fmv_regrRect[i-1])*(1+adjusted_Ret$SlopeRet[i])
    }
  else{
    as.numeric(adjusted_Ret$fmv_regrRect[i])
  }
}


## step3 update the adjusters with upper and lower bounds 
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoyUp[i] = ifelse(is.na(adjusted_Ret$yoyUp[i]),UpperB,ifelse(adjusted_Ret$ModelYear[i]==topyear,UpperB,adjusted_Ret$yoyUp[i]))
  adjusted_Ret$yoyLow[i] = ifelse(is.na(adjusted_Ret$yoyLow[i]),LowerB,ifelse(adjusted_Ret$ModelYear[i]==botyear,LowerB,adjusted_Ret$yoyLow[i]))
  adjusted_Ret$yoyCap[i] = min(max(adjusted_Ret$fmv_rec[i],adjusted_Ret$yoyLow[i]),adjusted_Ret$yoyUp[i])
}

## step4 check the gap between adjacent years
## flag out the year of violation (row i & i+1)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoycheck[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                   adjusted_Ret$yoyCap[i+1]/adjusted_Ret$yoyCap[i],1+adjusted_Ret$SlopeRet[i])
  adjusted_Ret$yoyFlag[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                  ifelse(round(adjusted_Ret$yoyCap[i+1]/adjusted_Ret$yoyCap[i],digits=4)<1+adjusted_Ret$SlopeRet[i],"flag",""),"")
}

## step5 if it is flagged in above step, check num of data points in i > i+1, if yes -->1, else -->0 (if tie, 0)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoydtpts[i]= ifelse(adjusted_Ret$yoyFlag[i]=='flag',
                                   ifelse(adjusted_Ret$n[i]>=adjusted_Ret$n[i+1],1,0),"")
}


## step6 keep the value with larger number of data points, change the one with less data 
for (i in 1:nrow(adjusted_Ret)){
  
  adjusted_Ret$yoyUpd[i]= ifelse(adjusted_Ret$yoydtpts[i]==0, adjusted_Ret$yoyCap[i+1]/(1+adjusted_Ret$SlopeRet[i]),
                                 ifelse(adjusted_Ret$yoydtpts[i-1]==1, adjusted_Ret$yoyCap[i-1]*(1+adjusted_Ret$SlopeRet[i]),adjusted_Ret$yoyCap[i])) 
  adjusted_Ret$yoyUpd[i]= ifelse(is.na(adjusted_Ret$yoyUpd[i]),adjusted_Ret$yoyCap[i],adjusted_Ret$yoyUpd[i])
}

## step7 (repeat step4): flag out the year of violation (row i & i+1)
for (i in 1:nrow(adjusted_Ret)){
  adjusted_Ret$yoyFinFlag[i]= ifelse(adjusted_Ret$Schedule[i]==adjusted_Ret$Schedule[i+1],
                                     ifelse(round(adjusted_Ret$yoyUpd[i+1]/adjusted_Ret$yoyUpd[i],digits=4)<1+adjusted_Ret$SlopeRet[i],"flag",""),"")
}


###### Auction
adjusted_Auc<-recencyAuc.apply
## step1: set the upper bound based on the year newer 
for (i in 1:(dim(adjusted_Auc)[1]-1)){
  
  adjusted_Auc$yoyUp[i] = 
    if (adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1]){
      as.numeric(adjusted_Auc$flv_regrRect[i+1])/(1+adjusted_Auc$SlopeAuc[i])
    }
  else{
    as.numeric(adjusted_Auc$flv_regrRect[i])
  }
}

## step2: set the lower bound based on the year older 
for (i in 2:dim(adjusted_Auc)[1]){
  
  adjusted_Auc$yoyLow[i] = 
    if (adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i-1]){
      as.numeric(adjusted_Auc$flv_regrRect[i-1])*(1+adjusted_Auc$SlopeAuc[i])
    }
  else{
    as.numeric(adjusted_Auc$flv_regrRect[i])
  }
}


## step3: update the adjusters with upper and lower bounds 
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoyUp[i] = ifelse(is.na(adjusted_Auc$yoyUp[i]),UpperB,ifelse(adjusted_Auc$ModelYear[i]==topyear,UpperB,adjusted_Auc$yoyUp[i]))
  adjusted_Auc$yoyLow[i] = ifelse(is.na(adjusted_Auc$yoyLow[i]),LowerB,ifelse(adjusted_Auc$ModelYear[i]==botyear,LowerB,adjusted_Auc$yoyLow[i]))
  adjusted_Auc$yoyCap[i] = min(max(adjusted_Auc$flv_rec[i],adjusted_Auc$yoyLow[i]),adjusted_Auc$yoyUp[i])
  
}

## step4: check the gap between adjacent years
## flag out the year of violation (row i & i+1)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoycheck[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                   adjusted_Auc$yoyCap[i+1]/adjusted_Auc$yoyCap[i],1+adjusted_Auc$SlopeAuc[i])
  
  adjusted_Auc$yoyFlag[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                  ifelse(round(adjusted_Auc$yoyCap[i+1]/adjusted_Auc$yoyCap[i],digits=4)<1+adjusted_Auc$SlopeAuc[i],"flag",""),"")
  adjusted_Auc$yoyFlag[i] = ifelse(is.na(adjusted_Auc$yoyFlag[i]),"",adjusted_Auc$yoyFlag[i])
}


## step5 if it is flagged in above step, check num of data points in i > i+1, if yes -->1, else -->0 (if tie, 0)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoydtpts[i]= ifelse(adjusted_Auc$yoyFlag[i]=='flag',
                                   ifelse(adjusted_Auc$n[i]>=adjusted_Auc$n[i+1],1,0),"")
}


## step6: keep the value with larger number of data points, change the one with less data 
for (i in 1:nrow(adjusted_Auc)){
  
  adjusted_Auc$yoyUpd[i]= ifelse(adjusted_Auc$yoydtpts[i]==0, adjusted_Auc$yoyCap[i+1]/(1+adjusted_Auc$SlopeAuc[i]),
                                 ifelse(adjusted_Auc$yoydtpts[i-1]==1, adjusted_Auc$yoyCap[i-1]*(1+adjusted_Auc$SlopeAuc[i]),adjusted_Auc$yoyCap[i])) 
  adjusted_Auc$yoyUpd[i]= ifelse(is.na(adjusted_Auc$yoyUpd[i]),adjusted_Auc$yoyCap[i],adjusted_Auc$yoyUpd[i])
}

## step7 (repeat step4): flag out the year of violation (row i & i+1)
for (i in 1:nrow(adjusted_Auc)){
  adjusted_Auc$yoyFinFlag[i]= ifelse(adjusted_Auc$Schedule[i]==adjusted_Auc$Schedule[i+1],
                                     ifelse(round(adjusted_Auc$yoyUpd[i+1]/adjusted_Auc$yoyUpd[i],digits=4)<1+adjusted_Auc$SlopeAuc[i],"flag",""),"")
}


##### Run the above test result - see flags

# Retail
check_retail <- adjusted_Ret%>%
  filter(yoyFinFlag=='flag') %>%
  summarise(n=n())
check_retail

# if flag > 0, who are the ones violate the rule
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

# if flag > 0, who are the ones violate the rule
schedgroup_auction <- adjusted_Auc%>%
  filter(yoyFinFlag=='flag') %>%
  group_by(Schedule) %>%
  summarise(n=n())
schedgroup_auction


#### Format the tables and prepare for later use
RetSchedule<-adjusted_Ret %>%
  select(Schedule,ModelYear,yoyUpd,n) 
colnames(RetSchedule)<-c("Schedule","ModelYear","Adjfmv","UnitsRet")

AucSchedule<-adjusted_Auc %>%
  select(Schedule,ModelYear,yoyUpd,n) 
colnames(AucSchedule)<-c("Schedule","ModelYear","Adjflv","UnitsAuc")

print(if(nSched_Ret == dim(RetSchedule)[1]/10){paste('Yes, the N_retail is ', nSched_Ret)} else {'No'})
print(if(nSched_Auc == dim(AucSchedule)[1]/10){paste('Yes, the N_auction is ', nSched_Auc)} else {'No'})



#############  Apply values to borrow schedules  ############

# split borrow schedules into AbR and RbA, borrow both shoud be in both lists
BrwAucList <- SchedR %>% filter(BorrowType != 'AuctionBorrowRetail') %>% select(Schedule,BorrowSchedule, BorrowType) 
BrwRetList <- SchedR %>% filter(BorrowType != 'RetailBorrowAuction') %>% select(Schedule,BorrowSchedule, BorrowType) 

############################ Retail Borrow Auction ############################ 
## count how many data in each borrow schedule
Num_dtpts_Auc<-merge(Data_all_plot,BrwAucList,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Auction' & YearFlag=='AdjusUseYr') %>%
  group_by(BorrowSchedule,Schedule) %>%
  summarise(Numcomps=n())

## Use most recent 'threshold_brw' data points
borwAuc_data<-merge(Data_all_plot,Num_dtpts_Auc,by=c('Schedule','BorrowSchedule')) %>%
  filter(SaleType=='Auction' & YearFlag=='AdjusUseYr') %>%
  group_by(Schedule) %>%
  arrange(desc(EffectiveDate)) %>%
  slice(1:ifelse(Numcomps>=threshold_brw,threshold_brw,99999)) 

## Calculate the revised SaleAB which compare to effective month
borwAuc_Yr<- borwAuc_data %>%
  mutate(power = month(EffectiveDate) - month(publishDate),
         revised_SaleAB = SaleAB/(0.99^power)) %>%
  group_by(BorrowSchedule,Schedule,ModelYear) %>%
  summarise(recover= mean(revised_SaleAB),n=n())

## Calculate the rate which indicate how much the curve move for borrow schedule, partial move by number of data
borwAuc_Rate<-merge(borwAuc_Yr,OutRegression_Auc %>% rename(BorrowSchedule=Schedule),by=c('BorrowSchedule','ModelYear')) %>% 
  group_by(Schedule,BorrowSchedule) %>%
  summarise(diffperc= (sum(recover/flv *n))/sum(n),
            N=sum(n)) %>%
  mutate(offRate = ifelse(N>threshold_brw,1*diffperc,((threshold_brw-N)+N*diffperc)/threshold_brw))


## join to the full list, if there is no sale data will apply 1 as the rate
RetbAuc<-merge(BrwAucList %>% select(-BorrowType),borwAuc_Rate,all.x=T,by=c('Schedule','BorrowSchedule')) %>%
  mutate(offRate = ifelse(is.na(offRate),1,offRate)) %>%
  select(-diffperc, -N)


############################Auction Borrow Auction ############################ 
## Repeat the steps as retail borrow auction
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


####################################### Apply the borrow rates on all years ###################################
######### RbA
inheritAucOut<-merge(AucSchedule %>% rename(BorrowSchedule=Schedule),RetbAuc,by='BorrowSchedule') %>%
  arrange(Schedule,desc(ModelYear)) 

## Interpolate the rate of move for each year. For example, we got x% downward move for one schedule.
## age 0: move down brwsched_move% * x%; age phaseinAge: move down 100% * x%
dw.RbA = merge(inheritAucOut %>% filter(ModelYear == topyear) %>% select(Schedule,Adjflv,offRate),
               inheritAucOut %>% filter(ModelYear == topyear - phaseinAge) %>% select(Schedule,Adjflv),by='Schedule') %>%
  mutate(parent.delta = Adjflv.x - Adjflv.y) %>%
  mutate(topy.adj = ((offRate-1)*brwsched_move + 1)*Adjflv.x,
         child.delta = topy.adj  - Adjflv.y *offRate) %>%
  select(Schedule,topy.adj,parent.delta,child.delta)

## join the results back to the full table 
inheritAucOut.upd<-merge(inheritAucOut,dw.RbA,by='Schedule') 

## interpolate the years in between keeping the shape of orginal schedule
for (i in 1:dim(inheritAucOut.upd)[1]){
  ## deprecation rate by year
  inheritAucOut.upd$diffRate[i] = ifelse(inheritAucOut.upd$Schedule[i] == inheritAucOut.upd$Schedule[i-1] &
                                           inheritAucOut.upd$ModelYear[i] == inheritAucOut.upd$ModelYear[i-1]-1,
                                         round((inheritAucOut.upd$Adjflv[i-1] - inheritAucOut.upd$Adjflv[i])/inheritAucOut.upd$parent.delta[i],digits=4),0)
  
  ## adjusted schedule
  inheritAucOut.upd$adjustrate[i] = inheritAucOut.upd$topy.adj[i]
  inheritAucOut.upd$adjustrate[i] = ifelse(inheritAucOut.upd$ModelYear[i] <= topyear -phaseinAge, inheritAucOut.upd$Adjflv[i]*inheritAucOut.upd$offRate[i],
                                           ifelse(inheritAucOut.upd$ModelYear[i] == topyear, inheritAucOut.upd$topy.adj[i],
                                                  inheritAucOut.upd$adjustrate[i-1]-inheritAucOut.upd$child.delta[i]*inheritAucOut.upd$diffRate[i]))
  
}

## Manage the output table
AucBorwOut <-inheritAucOut.upd %>%
  select(Schedule,ModelYear,adjustrate,UnitsAuc) %>%
  rename(Adjflv=adjustrate) %>%
  arrange(Schedule,desc(ModelYear))

######### AbR -- please refer to the above comments
inheritRetOut<-merge(RetSchedule %>% rename(BorrowSchedule=Schedule),AucbRet,by='BorrowSchedule') %>%
  arrange(Schedule,desc(ModelYear)) 

dw.AbR = merge(inheritRetOut %>% filter(ModelYear == topyear) %>% select(Schedule,Adjfmv,offRate),
               inheritRetOut %>% filter(ModelYear == topyear -phaseinAge) %>% select(Schedule,Adjfmv),by='Schedule') %>%
  mutate(parent.delta = Adjfmv.x - Adjfmv.y) %>%
  mutate(topy.adj = ((offRate-1)*brwsched_move+1)*Adjfmv.x,
         child.delta = topy.adj  - Adjfmv.y *offRate) %>%
  select(Schedule,topy.adj,parent.delta,child.delta)

inheritRetOut.upd<-merge(inheritRetOut,dw.AbR,by='Schedule') 

for (i in 1:dim(inheritRetOut.upd)[1]){
  ## deprecation rate by year
  inheritRetOut.upd$diffRate[i] = ifelse(inheritRetOut.upd$Schedule[i] == inheritRetOut.upd$Schedule[i-1] &
                                           inheritRetOut.upd$ModelYear[i] == inheritRetOut.upd$ModelYear[i-1]-1,
                                         round((inheritRetOut.upd$Adjfmv[i-1] - inheritRetOut.upd$Adjfmv[i])/inheritRetOut.upd$parent.delta[i],digits=4),0)
  
  ## adjusted schedule
  inheritRetOut.upd$adjustrate[i] = inheritRetOut.upd$topy.adj[i]
  inheritRetOut.upd$adjustrate[i] = ifelse(inheritRetOut.upd$ModelYear[i] <= topyear -phaseinAge, inheritRetOut.upd$Adjfmv[i]*inheritRetOut.upd$offRate[i],
                                           ifelse(inheritRetOut.upd$ModelYear[i] == topyear, inheritRetOut.upd$topy.adj[i],
                                                  inheritRetOut.upd$adjustrate[i-1]-inheritRetOut.upd$child.delta[i]*inheritRetOut.upd$diffRate[i]))
  
}

RetBorwOut <-inheritRetOut.upd %>%
  select(Schedule,ModelYear,adjustrate,UnitsRet) %>%
  rename(Adjfmv=adjustrate) %>%
  arrange(Schedule,desc(ModelYear))

#### combine the regular schedules with borrow schedules
AucSchedule_full<-rbind(AucSchedule,AucBorwOut)
RetSchedule_full<-rbind(RetSchedule,RetBorwOut)

## Join the two sale type into one table
JoinChannel<-merge(RetSchedule_full,AucSchedule_full,by=c("Schedule","ModelYear")) %>%
  arrange(Schedule,as.numeric(ModelYear))


###################################################################################################################
###################################################################################################################
############################### Cap the newest year in a reasonable range #########################################
###################################################################################################################
###################################################################################################################

## filter to newest year and xth year (fixed year)
topyrSched<-JoinChannel %>% filter(ModelYear == topyear) %>% select(Schedule,Adjfmv,Adjflv) %>% rename(Adjfmvtop=Adjfmv,Adjflvtop=Adjflv)
fixyrSched<-JoinChannel %>% filter(ModelYear == topyear-fixyr_gap(Schedule)) %>% select(Schedule,Adjfmv,Adjflv) %>% rename(Adjfmvfix=Adjfmv,Adjflvfix=Adjflv)

## join to get the input min and max values
BorrowBoundsin<-merge(Sched,SchedR %>% rename(Schedule = BorrowSchedule, ABSched=Schedule),by='Schedule') %>%
  mutate(RetailNewYrMin = ifelse(is.na(RetailNewYrMin.y),RetailNewYrMin.x, RetailNewYrMin.y),
         RetailNewYrMax = ifelse(is.na(RetailNewYrMax.y),RetailNewYrMax.x, RetailNewYrMax.y),
         AuctionNewYrMin = ifelse(is.na(AuctionNewYrMin.y),AuctionNewYrMin.x,AuctionNewYrMin.y),
         AuctionNewYrMax = ifelse(is.na(AuctionNewYrMax.y),AuctionNewYrMax.x, AuctionNewYrMax.y)) %>%
  select(ABSched,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax)

boundsIn <- rbind(Sched,BorrowBoundsin %>% rename(Schedule=ABSched))


## calculate the boundaries
exactCaps <-boundsIn %>% 
  mutate(AuctionMin = NewYr_CapFunc(AuctionNewYrMin),
         AuctionMax = NewYr_CapFunc(AuctionNewYrMax) ,
         RetailMin = NewYr_CapFunc(RetailNewYrMin),
         RetailMax = NewYr_CapFunc(RetailNewYrMax)) %>%
  select(Schedule,AuctionMin,AuctionMax,RetailMin,RetailMax)

### check if the top year is capped or not, if yes, is it Max or Min
### then interpolate the schedule values between newest year and the fixed year
# note: cranes have different fixed year than others
join_topfix <- merge(merge(topyrSched,fixyrSched,by=c("Schedule")),exactCaps,by=c("Schedule")) %>%
  mutate(isCapA = WithinRange(Adjflvtop,AuctionMax,AuctionMin),
         isCapR = WithinRange(Adjfmvtop,RetailMax,RetailMin)) %>%
  mutate(DiffA = ifelse(isCapA=='Max', AuctionMax-Adjflvfix,ifelse(isCapA=='Min',AuctionMin-Adjflvfix,0)),
         DiffR = ifelse(isCapR=='Max', RetailMax -Adjfmvfix,ifelse(isCapR=='Min',RetailMin -Adjfmvfix,0)),
         capsideA = ifelse(isCapA=='Max', AuctionMax,ifelse(isCapA=='Min',AuctionMin,0)),
         capsideR = ifelse(isCapR=='Max', RetailMax,ifelse(isCapR=='Min',RetailMin,0)),
         top_fixR = Adjfmvtop - Adjfmvfix,
         top_fixA = Adjflvtop - Adjflvfix) %>%
  select(Schedule, isCapA,isCapR, Adjflvtop, Adjfmvtop,DiffA, DiffR, capsideA, capsideR, top_fixR,top_fixA)

## calcualte age
capSched <- merge(JoinChannel,join_topfix,by=c("Schedule")) %>%
  arrange(Schedule,ModelYear) %>%
  mutate(age_yr = topyear-ModelYear) 

## subset unchanged (older age) years
capSched_keep <- capSched %>% filter(age_yr>fixyr_gap(Schedule)) %>%
  rename(Cap_fmv=Adjfmv,Cap_flv=Adjflv) %>%
  select(Schedule,ModelYear,Cap_fmv,UnitsRet,Cap_flv,UnitsAuc)

### retail: age 0 to x would interpolate the shape with its own adjusted fmv
### auction: age 0 to x of non-AbR schedules would interpolate the shape from retail. 
### auction AbR schedules interpolate the shape of exponential using the log regression slope.
capSched_interpl <- capSched %>% filter(age_yr<=fixyr_gap(Schedule)) %>%
  mutate(AbR_itp = ifelse(age_yr == 1, .5, ifelse(age_yr ==2, 5/6, ifelse(age_yr ==3, 1, 0)))) %>%
  mutate(Cap_fmv= ifelse(isCapR=='', Adjfmv, capsideR - ((Adjfmvtop-Adjfmv)/top_fixR)*DiffR),
         Cap_flv= ifelse(isCapA=='', Adjflv, 
                         ifelse(Schedule %in% rbind(InA,InB)$Schedule, capsideA - (AbR_itp)*DiffA, capsideA - ((Adjfmvtop-Adjfmv)/top_fixR)*DiffA))) %>%
  select(Schedule,ModelYear,Cap_fmv,UnitsRet,Cap_flv,UnitsAuc)

## combine the changed, and unchanged years  
bindCaptb<-rbind(capSched_interpl,capSched_keep) %>%
  arrange(Schedule,ModelYear) %>%
  rename(Adjfmv=Cap_fmv,Adjflv=Cap_flv)


###################################################################################################################
###################################################################################################################
#########################  Check retail recovery is always higher than auction ####################################
###################################################################################################################
###################################################################################################################

### Check if retail is always greater than auction
### If not, move auction down on newer year or move retail up on older year
## details: age <= chanyr: retail drive; age >= 2 *chanyr: auction dirve; else: channel with more data drive
if (CountryCode == 'USA'){
  checkChannel<-bindCaptb %>%
  mutate(rate = Adjfmv/Adjflv,
         YTD = ModelYear)%>%
  filter (rate < 1 + capChannel) %>%
  mutate(yearAge = ifelse(str_detect(Schedule,'RbA') | YTD <= chanyr,"retDrive",
                          ifelse(YTD >= chanyr*2 | str_detect(Schedule,'AbR'),"aucDrive","check"))) %>%
  ## if n_ret >= retBelieve and n_ret >= n_auc, retail drive otherwise auction drive
  mutate(ageGroup = ifelse(yearAge == 'check',ifelse(UnitsRet >=retBelieve & UnitsRet >= UnitsAuc, 'retDrive','aucDrive'),yearAge)) %>%
  mutate(schedule = ifelse(ageGroup == 'aucDrive',Adjflv*(1+capChannel),Adjfmv/(1+capChannel))) %>%
  select(Schedule,ModelYear,ageGroup,schedule)
  
} else{
  checkChannel<-bindCaptb %>%
  mutate(rate = Adjfmv/Adjflv,
         YTD = ModelYear)%>%
  filter (rate < 1 + capChannel) %>%
  ## for UK, retail has limited data. So only RbA retail govern. 
  mutate(ageGroup = ifelse(str_detect(Schedule,'RbA'),"retDrive","aucDrive")) %>%
  mutate(schedule = ifelse(ageGroup=='aucDrive',Adjflv*(1+capChannel),Adjfmv/(1+capChannel))) %>%
  select(Schedule,ModelYear,ageGroup,schedule)

}

## assign 0
assignVal<-merge(bindCaptb,checkChannel,by=c("Schedule","ModelYear"),all.x =T)
assignVal[is.na(assignVal)] <- 0

## pick the schedule for selected channel
for (i in 1:nrow(assignVal)){
  assignVal$chann[i] = 
    if(assignVal$ageGroup[i] == 'aucDrive'){
      assignVal$schedule[i]
  }
    else{
      assignVal$Adjfmv[i]
  } 
  
  
  assignVal$chann2[i] = 
    if(assignVal$ageGroup[i] == 'retDrive'){
      assignVal$schedule[i]
    }
    else{
      assignVal$Adjflv[i]
  } 
}

fixChannel<-assignVal %>% 
  select(Schedule, ModelYear,chann,chann2) %>%
  rename(Adjfmv=chann,Adjflv=chann2)

## check the rule: auction side with lower recovery than retail side
checkedOutput<-fixChannel %>%
  arrange(Schedule,as.numeric(ModelYear)) %>%
  mutate(ret_to_auc=Adjfmv/Adjflv)

## output schedule with violation
check_channels <- checkedOutput %>%
  filter(ret_to_auc < 1+capChannel) %>%
  summarise(n=n())
check_channels



### Format the tables and prepare for the second round year checks
round2Check<-checkedOutput %>% 
  select(Schedule, ModelYear,Adjfmv,Adjflv) %>%
  arrange(Schedule,desc(ModelYear))


###################################################################################################################
###################################################################################################################
################################################ 2ND ROUND CHECKS YoY /Channel ####################################
###################################################################################################################
###################################################################################################################


### check the rule: older year with lower recovery than newer year 
for (i in 1:nrow(round2Check)){
  round2Check$retFlag[i] = ifelse(round2Check$Schedule[i]==round2Check$Schedule[i+1],
                                  ifelse(round(round2Check$Adjfmv[i]/round2Check$Adjfmv[i+1],digits=4)<1,"flag",""),"")
  round2Check$aucFlag[i]= ifelse(round2Check$Schedule[i]==round2Check$Schedule[i+1],
                                 ifelse(round(round2Check$Adjflv[i]/round2Check$Adjflv[i+1],digits=4)<1,"flag",""),"")
}

###number of flags on year 
round2results <- round2Check %>%
  filter(retFlag=='flag' |aucFlag=='flag') %>%
  summarise(n=n())
round2results

### output schedule with violation
review_schedule = round2Check %>%
  filter(retFlag=='flag' |aucFlag=='flag') %>%
  select(Schedule)
review_schedule

#### if flag <>0, view the schedules and values
round2Check %>% 
  filter(Schedule == review_schedule[1,])

round2Check %>% 
  filter(Schedule == review_schedule[2,])

round2Check %>% 
  filter(Schedule == review_schedule[3,])



### CHANNEL 2ND ROUND - should be no change from 1 st round check, but check again
##### Check across channel - prepare a shared table with schedules and flags
checkedOutput<-round2Check %>%
  mutate(rate = Adjfmv/Adjflv
         ,chanFlag = ifelse(Adjfmv/Adjflv < 1+capChannel, "flag","")) %>%
  select('Schedule', 'ModelYear','Adjfmv','Adjflv','retFlag','aucFlag','chanFlag','rate')

##### number of flags on channel 
checkedOutput %>%
  filter(chanFlag=="flag") %>%
  summarise(n=n())

#### check if schedules are crossed by channel - not acceptible 
checkedOutput %>%
  filter(rate<1) %>%
  summarise(n=n())

### Prepare the final table to join to application in next script
ScheduleOut <- checkedOutput %>%
  select('Schedule', 'ModelYear','Adjfmv','Adjflv')

