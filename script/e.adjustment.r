###################################################################################################################
###################################################################################################################
################################# 4. Calculate the adjusterments then apply #######################################
###################################################################################################################
###################################################################################################################

############################# 4.B Format a table with schedule, model years
model_year <- data.frame(c(topyear:botyear))
colnames(model_year) <-'ModelYear'

# SchedTbRbA + SchedTbAbR + Sched =  SchedTbFull
SchedTbFull<- merge(SchedFullList,model_year) %>%
  arrange(Schedule,ModelYear)

SchedTbRbA<- merge(SchedR %>% filter(BorrowType=='RetailBorrowAuction'),model_year) %>%
  arrange(Schedule,ModelYear)

SchedTbAbR<- merge(SchedR %>% filter(BorrowType=='AuctionBorrowRetail'),model_year) %>%
  arrange(Schedule,ModelYear)



## join the slopes with the table created above - the slopes are used as minimum gap between years

SlopJoinAuc<-SchedTbFull %>% mutate(SlopeAuc = indexcap)
SlopJoinRet<-SchedTbFull %>% mutate(SlopeRet = indexcap)




############################ 4.C split sale data to auction and retail -- Validation Data

summarySM = Data_clean %>%
  group_by(EffectiveDate) %>%
  summarise(n=n())
SM<-data.frame(summarySM[,1])

### Data use for adjustment
SaleDtAuc_adjUse<-subset(Data_clean,Data_clean$SaleType=="Auction" & YearFlag=='AdjusUseYr') 

SaleDtRet_adjUse<-data.frame(subset(Data_clean,Data_clean$SaleType!="Auction" & YearFlag=='AdjusUseYr')) %>%
  mutate(SaleType ='Retail')

########################### 4.D Calcualte SP/value to adjust the curve 

###################################### Retail  ###################################### 
## Base adjusters
shiftRet.count<-merge(SaleDtRet_adjUse,OutRegression_Ret,by=c("Schedule","ModelYear")) %>%
  mutate(power = abs(month(EffectiveDate)-month(publishDate)),
         fmv.1 = fmv/(0.99^power)) %>%
  group_by(Schedule,ModelYear) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum = row_number()) 
   

shiftRet.calc1m <-shiftRet.count %>%
  filter(as.Date(EffectiveDate)==publishDate & rowNum ==threshold_adj) %>%
  select(Schedule,ModelYear)


shiftRet.calc <- rbind(data.frame(merge(shiftRet.count,shiftRet.calc1m,by=c("Schedule","ModelYear")) %>% filter(as.Date(EffectiveDate)==publishDate)),
                       data.frame(anti_join(shiftRet.count,shiftRet.calc1m,by=c("Schedule","ModelYear")) %>% filter(rowNum<=threshold_adj))) %>%
  mutate(fmvalue=CurrentABCost*as.numeric(fmv.1)
         ,SpFmv=SalePrice/fmvalue) %>%
  group_by(Schedule,ModelYear) %>%
  summarise(avgSPfmv=mean(SpFmv)
            ,n=n())

## join to the full list, some schedule years has no data will be assigned 0
factor_Retail<-merge(SlopJoinRet,shiftRet.calc,by=c("Schedule","ModelYear"),all.x=TRUE)
factor_Retail$n[is.na(factor_Retail$n)] <- 0
factor_Retail$avgSPfmv[is.na(factor_Retail$avgSPfmv)] <- 1

## cap the adjusters - based on how much data points there we believe to move
Ret.capshift<-factor_Retail %>%
  mutate(avgSPfmv_Cap = ifelse(n>threshold_adj,1*avgSPfmv,((threshold_adj-n)+n*avgSPfmv)/threshold_adj)) %>%
  select(Schedule,ModelYear,avgSPfmv_Cap,n,SlopeRet)

## apply the factor to the regression and only on ten years
shiftRet.apply<-merge(OutRegression_Ret,Ret.capshift,by=c('Schedule','ModelYear'),all.x=T) %>%
  mutate(fmv_shift = fmv * ifelse(is.na(avgSPfmv_Cap),1, avgSPfmv_Cap)
         ,fmv_shiftLim = ifelse(ModelYear==botyear, pmax(pmin(as.numeric(fmv)*(1+lastyr_1),fmv_shift),as.numeric(fmv)*(1-lastyr_1)),
                                ifelse(ModelYear==botyear+1,pmax(pmin(as.numeric(fmv)*(1+lastyr_2),fmv_shift),as.numeric(fmv)*(1-lastyr_2)),fmv_shift)))


########################### Calculate the recency scale factors #################
#### recency calculation: if N(recent 1 month) > m, use 1 month; else use most recent n til 3 months
## join the raw data to regression output
join.rec.ret<-merge(data.frame(Data_clean) %>% filter(SaleType !='Auction'), OutRegression_Ret,by=c('Schedule','ModelYear'))%>% 
  filter(as.Date(EffectiveDate)>=thirdLastM,ModelYear>=botyear) %>%
  mutate(power = abs(month(EffectiveDate)-month(publishDate)),
         fmv_shiftLim = fmv/(0.99^power)) 

## count how many rows(sales) in each schedules
RPSF_ret.count<- join.rec.ret %>%
  filter(Age>1 & YearFlag=='AdjusUseYr') %>%
  group_by(Schedule) %>% 
  arrange(desc(SaleDate)) %>%
  mutate(rowNum=row_number()) 

## get the list of schedules who have more than threshold_rec.calc data point in most recent 1 month
RPSF_ret.calc1m <- RPSF_ret.count %>%
  filter(as.Date(EffectiveDate)==publishDate & rowNum ==threshold_rec.calc) %>%
  select(Schedule)
  
## combine to get the dataset to calculate recency factor, then partial move, apply cap
RPSF_ret.cal <- rbind(data.frame(merge(RPSF_ret.count,RPSF_ret.calc1m,by='Schedule') %>% filter(as.Date(EffectiveDate)==publishDate))
                      ,data.frame(anti_join(RPSF_ret.count,RPSF_ret.calc1m,by='Schedule') %>% filter(rowNum<=threshold_rec.calc))) %>%
  group_by(Schedule) %>% 
  summarise(RPSF = mean(SalePrice/(CurrentABCost*as.numeric(fmv_shiftLim))),n=n()) %>%                                                               
  mutate(RPSF_Cap = ifelse(n>threshold_recency,1*RPSF,((threshold_recency-n)+n*RPSF)/threshold_recency)) %>%
  mutate(f.recency.ret = pmin(1+recency_cap,pmax(RPSF_Cap,1-recency_cap))) %>%
  select(Schedule,f.recency.ret)

## join to get the full list (no recency data get factor = 1), partial move factor by schedule, modelyear
RPSF_ret.full<-merge(SchedFullList %>% filter(!is.na(RetailNewYrMax)) %>% select(Schedule),RPSF_ret.cal,by='Schedule',all.x=TRUE) %>%
  mutate(f.recency.ret = ifelse(is.na(f.recency.ret),1,f.recency.ret))

recFact_ret<- merge(join.rec.ret %>% group_by(Schedule,ModelYear) %>% summarise(n=n()) %>%
                      ungroup() %>%
                      complete(Schedule,ModelYear,
                               fill = list(n = 0))
                    ,RPSF_ret.full,by='Schedule') %>%
  mutate(partialf.recency.ret = (1-pmin(1,n/threshold_adj))*(f.recency.ret-1)+1) %>% select(-n)


#write.csv(RPSF_ret,'0114recency_retail.csv')
## apply the recency factor to the shifted schedule
recencyRet.apply <- merge(shiftRet.apply,recFact_ret,by=c('Schedule','ModelYear'),all.x = T) %>%
  filter(ModelYear>=botyear) %>%
  mutate(recAdjust = ifelse(ModelYear == topyear, 1, ifelse(ModelYear == topyear - 1, 1/3*partialf.recency.ret+2/3, ifelse(ModelYear == topyear - 2, 2/3*partialf.recency.ret+1/3,partialf.recency.ret))),
         fmv_rec = fmv_shiftLim * recAdjust,
         fmv_regrRect = fmv * recAdjust) %>%
  arrange(Schedule,ModelYear)


###################################### Auction  ###################################### 
## Base adjusters
shiftAuc.count<-merge(SaleDtAuc_adjUse,OutRegression_Auc,by=c("Schedule","ModelYear")) %>%
  mutate(power = abs(month(EffectiveDate)-month(publishDate)),
         flv.1 = flv/(0.99^power)) %>%
  group_by(Schedule,ModelYear) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum = row_number()) 

shiftAuc.calc1m <-shiftAuc.count %>%
  filter(as.Date(EffectiveDate)==publishDate & rowNum ==threshold_adj) %>%
  select(Schedule,ModelYear)

shiftAuc.calc <- rbind(data.frame(merge(shiftAuc.count,shiftAuc.calc1m,by=c("Schedule","ModelYear")) %>% filter(as.Date(EffectiveDate)==publishDate)),
                       data.frame(anti_join(shiftAuc.count,shiftAuc.calc1m,by=c("Schedule","ModelYear")) %>% filter(rowNum<=threshold_adj))) %>%
  mutate(flvalue=CurrentABCost*as.numeric(flv.1)
         ,SpFlv=SalePrice/flvalue) %>%
  group_by(Schedule,ModelYear) %>%
  summarise(avgSPflv=mean(SpFlv)
            ,n=n())


## join to the full list, some schedule years has no data will be assigned 0
factor_Auction<-merge(SlopJoinAuc,shiftAuc.calc,by=c("Schedule","ModelYear"),all.x=TRUE)
dim(factor_Auction)[1]
factor_Auction$n[is.na(factor_Auction$n)] <- 0
factor_Auction$avgSPflv[is.na(factor_Auction$avgSPflv)] <- 1

## cap the adjusters - based on how much data points there we believe to move
Auc.capshift<-factor_Auction %>%
  mutate(avgSPflv_Cap = ifelse(n>threshold_adj,1*avgSPflv,((threshold_adj-n)+n*avgSPflv)/threshold_adj)) %>%
  select(Schedule,ModelYear,avgSPflv_Cap,n,SlopeAuc)

## apply the factor to the regression and only on ten years
shiftAuc.apply<-merge(OutRegression_Auc,Auc.capshift,by=c('Schedule','ModelYear'),all.x=T) %>%
  mutate(flv_shift = flv * ifelse(is.na(avgSPflv_Cap),1, avgSPflv_Cap)
         ,flv_shiftLim = ifelse(ModelYear==botyear, pmax(pmin(as.numeric(flv)*(1+lastyr_1),flv_shift),as.numeric(flv)*(1-lastyr_1)),
                                ifelse(ModelYear==botyear+1,pmax(pmin(as.numeric(flv)*(1+lastyr_2),flv_shift),as.numeric(flv)*(1-lastyr_2)),flv_shift)))



########################### Calculate the recency scale factors #################
#### recency calculation: if N(recent 1 month) > m, use 1 month; else use most recent n til 3 months
## join the raw data to regression output
join.rec.auc<-merge(data.frame(Data_clean) %>% filter(SaleType !='Retail'), OutRegression_Auc,by=c('Schedule','ModelYear'))%>% 
  filter(as.Date(EffectiveDate)>=thirdLastM,ModelYear>=botyear) %>%
  mutate(power = abs(month(EffectiveDate)-month(publishDate)),
         flv_shiftLim = flv/(0.99^power))

## count how many rows(sales) in each schedules
RPSF_auc.count<- join.rec.auc %>%
  filter(Age>1 & YearFlag=='AdjusUseYr') %>%
  group_by(Schedule) %>% 
  arrange(desc(SaleDate)) %>%
  mutate(rowNum=row_number()) 

## get the list of schedules who have more than threshold_rec.calc data point in most recent 1 month
RPSF_auc.calc1m <- RPSF_auc.count %>%
  filter(as.Date(EffectiveDate)==publishDate & rowNum ==threshold_rec.calc) %>%
  select(Schedule)

## combine to get the dataset to calculate recency factor, then partial move, apply cap
RPSF_auc.calc<- rbind(data.frame(merge(RPSF_auc.count,RPSF_auc.calc1m,by='Schedule') %>% filter(as.Date(EffectiveDate)==publishDate))
                      ,data.frame(anti_join(RPSF_auc.count,RPSF_auc.calc1m,by='Schedule') %>% filter(rowNum<=threshold_rec.calc))) %>%
  group_by(Schedule) %>% 
  summarise(RPSF = mean(SalePrice/(CurrentABCost*as.numeric(flv_shiftLim))),n=n()) %>%
  mutate(RPSF_Cap = ifelse(n>threshold_recency,1*RPSF,((threshold_recency-n)+n*RPSF)/threshold_recency)) %>%
  mutate(f.recency.auc = pmin(1+recency_cap,pmax(RPSF_Cap,1-recency_cap))) %>%
  select(Schedule,f.recency.auc)

## join to get the full list (no recency data get factor = 1), partial move factor by schedule, modelyear
RPSF_auc.full<-merge(SchedFullList %>% filter(!is.na(AuctionNewYrMax)) %>% select(Schedule),RPSF_auc.calc,by='Schedule',all.x=TRUE) %>%
  mutate(f.recency.auc = ifelse(is.na(f.recency.auc),1,f.recency.auc))

recFact_auc<- merge(join.rec.auc %>% group_by(Schedule,ModelYear) %>% summarise(n=n()) %>%
                      ungroup() %>%
                      complete(Schedule,ModelYear,
                               fill = list(n = 0))
                    ,RPSF_auc.full,by='Schedule') %>%
  mutate(partialf.recency.auc = (1-pmin(1,n/threshold_adj))*(f.recency.auc-1)+1) %>% select(-n)



#write.csv(RPSF_ret,'0114recency_retail.csv')
## apply the recency factor to the shifted schedule
recencyAuc.apply <- merge(shiftAuc.apply,recFact_auc,by=c('Schedule','ModelYear'),all.x = T) %>%
  filter(ModelYear>=botyear) %>%
  mutate(recAdjust = ifelse(ModelYear == topyear, 1, ifelse(ModelYear == topyear - 1, 1/3*partialf.recency.auc+2/3, ifelse(ModelYear == topyear - 2, 2/3*partialf.recency.auc+1/3,partialf.recency.auc))),
         flv_rec = flv_shiftLim * recAdjust,
         flv_regrRect = flv * recAdjust) %>%
  arrange(Schedule,ModelYear)


print(if(nSched_Ret == dim(recencyRet.apply)[1]/10){paste('Yes, the N_retail is ', nSched_Ret)} else {'No'})
print(if(nSched_Auc == dim(recencyAuc.apply)[1]/10){paste('Yes, the N_auction is ', nSched_Auc)} else {'No'})


