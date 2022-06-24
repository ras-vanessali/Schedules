###################################################################################################################
###################################################################################################################
##################################  Calculate the adjustments then apply ##########################################
####################################### Regression --> Shift --> recency ##########################################
###################################################################################################################

### Format a table with schedule, model years
model_year <- data.frame(c(topyear:botyear))
colnames(model_year) <-'ModelYear'

### split regular and borrow schedules
SchedTbFull<- merge(SchedFullList,model_year) %>%
  arrange(Schedule,ModelYear)

SchedTbRbA<- merge(SchedR %>% filter(BorrowType=='RetailBorrowAuction'),model_year) %>%
  arrange(Schedule,ModelYear)

SchedTbAbR<- merge(SchedR %>% filter(BorrowType=='AuctionBorrowRetail'),model_year) %>%
  arrange(Schedule,ModelYear)

## join the slopes with the table created above - the slopes are used as minimum gap between years
SlopJoinAuc<-SchedTbFull %>% mutate(SlopeAuc = indexcap)
SlopJoinRet<-SchedTbFull %>% mutate(SlopeRet = indexcap)


## split sale data to auction and retail -- Validation Data
summarySM = Data_clean %>%
  group_by(EffectiveDate) %>%
  summarise(n=n())
SM<-data.frame(summarySM[,1])

### Data use for adjustment
SaleDtAuc_adjUse<-subset(Data_clean,Data_clean$SaleType=="Auction" & YearFlag=='AdjusUseYr') 
SaleDtRet_adjUse<-data.frame(subset(Data_clean,Data_clean$SaleType!="Auction" & YearFlag=='AdjusUseYr')) %>%
  filter(SaleType =='Retail')


#### Calcualte SP/value to adjust the curve #####
## Retail
## power is month difference between publish date and sale date
shiftRet.count<-merge(SaleDtRet_adjUse,OutRegression_Ret,by=c("Schedule","ModelYear")) %>%
  mutate(power = abs(elapsed_months(EffectiveDate, publishDate)),
         fmv.1 = fmv/(0.99^power)) 

## shift adjuster = avg(sp/fmv) by schedule, modelyear
shiftRet.calc<-Use_Latest_Data(shiftRet.count,'SaleDate',threshold_adj,'shift',publishDate)%>%
  mutate(fmvalue=CurrentABCost*as.numeric(fmv.1)
         ,SpFmv=SalePrice/fmvalue) %>%
  group_by(Schedule,ModelYear) %>%
  summarise(avgSPfmv=mean(SpFmv)
            ,n=n())

## join to the full list, some schedule/year has no data will be assigned 0
factor_Retail<-merge(SlopJoinRet,shiftRet.calc,by=c("Schedule","ModelYear"),all.x=TRUE)
factor_Retail$n[is.na(factor_Retail$n)] <- 0
factor_Retail$avgSPfmv[is.na(factor_Retail$avgSPfmv)] <- 1

## cap the adjusters - movement based on the amount of data points
Ret.capshift<-factor_Retail %>%
  mutate(avgSPfmv_Cap = ifelse(str_detect(Schedule,'Crane'),cap_factor_calc(n, threshold_special,avgSPfmv),
                               cap_factor_calc(n,threshold_adj,avgSPfmv))) %>%
  select(Schedule,ModelYear,avgSPfmv_Cap,n,SlopeRet)

## apply the factor to the regression and only on ten years
shiftRet.apply<-merge(OutRegression_Ret,Ret.capshift,by=c('Schedule','ModelYear'),all.x=T) %>%
  mutate(fmv_shift = fmv * ifelse(is.na(avgSPfmv_Cap),1, avgSPfmv_Cap)
         ## limit the movement of the oldest two years 
         ,fmv_shiftLim = ifelse(ModelYear==botyear, pmax(pmin(as.numeric(fmv)*(1+lastyr_1),fmv_shift),as.numeric(fmv)*(1-lastyr_1)),
                                ifelse(ModelYear==botyear+1,pmax(pmin(as.numeric(fmv)*(1+lastyr_2),fmv_shift),as.numeric(fmv)*(1-lastyr_2)),fmv_shift)))


######Calculate the recency scale factors 
#### recency calculation: if N(recent 1 month) > n, use 1 month; else use most recent n til 3 months
## join the raw data to regression output
join.rec.ret<-merge(data.frame(Data_clean) %>% filter(SaleType !='Auction'), shiftRet.apply,by=c('Schedule','ModelYear'))%>% 
  filter(as.Date(EffectiveDate)>=thirdLastM & ModelYear>=botyear & ModelYear<=topyear & Age>1) %>%
  mutate(power = abs(elapsed_months(EffectiveDate, publishDate)),
         fmv.adjust = fmv_shiftLim/(0.99^power))


## alculate recency factor by schedule, apply cap
RPSF_ret.cal <-  Use_Latest_Data(join.rec.ret,'SaleDate',threshold_recency,'recency',publishDate) %>%
  group_by(Schedule) %>% 
  summarise(RPSF = mean(SalePrice/(CurrentABCost*as.numeric(fmv.adjust))),n=n()) %>%                                                               
  mutate(RPSF_Cap = ifelse(n > threshold_recency, 1*RPSF, ((threshold_recency-n) + n*RPSF)/threshold_recency)) %>%
  mutate(f.recency.ret = pmin(1+recency_cap,pmax(RPSF_Cap,1-recency_cap))) %>%
  select(Schedule,f.recency.ret)

## join to get the full list (no recency data get factor = 1)
RPSF_ret.full<-merge(merge(SchedFullList %>% filter(!is.na(RetailNewYrMax)) %>% select(Schedule),RPSF_ret.cal,by='Schedule',all.x=TRUE) %>%
                       mutate(f.recency.ret = ifelse(is.na(f.recency.ret),1,f.recency.ret)),model_year)

## finish applying the recency scale factor to each schedule - modelyear, then partial move. 
recFact_ret<- merge(join.rec.ret %>% group_by(Schedule,ModelYear) %>% summarise(n=n())
                    ,RPSF_ret.full,by=c('Schedule','ModelYear'),all.y=TRUE) %>%
  complete(Schedule,ModelYear,
           fill = list(n = 0)) %>%
  mutate(partialf.recency.ret = partial_move(n,threshold_adj,f.recency.ret,5)) %>% select(-n)


## apply the recency factor to the post-shifted schedule
recencyRet.apply <- merge(shiftRet.apply,recFact_ret,by=c('Schedule','ModelYear'),all.x = T) %>%
  filter(ModelYear>=botyear) %>%
  ## partial applying the recency factor to the first three years. On topyear - 3, apply full factor
  mutate(recAdjust = ifelse(ModelYear == topyear, 1, ifelse(ModelYear == topyear - 1, 1/3*partialf.recency.ret+2/3, ifelse(ModelYear == topyear - 2, 2/3*partialf.recency.ret+1/3,partialf.recency.ret))),
         fmv_rec = fmv_shiftLim * recAdjust,
         fmv_regrRect = fmv * recAdjust) %>%
  arrange(Schedule,ModelYear)


###################################### Auction - method explanation see retail above
## shift factor
shiftAuc.count<-merge(SaleDtAuc_adjUse,OutRegression_Auc,by=c("Schedule","ModelYear")) %>%
  mutate(power = abs(elapsed_months(EffectiveDate, publishDate)),
         flv.1 = flv/(0.99^power)) 

shiftAuc.calc <- Use_Latest_Data(shiftAuc.count,'SaleDate',threshold_adj,'shift',publishDate) %>%
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

## cap the adjusters 
Auc.capshift<-factor_Auction %>%
  mutate(avgSPflv_Cap = ifelse(str_detect(Schedule,'Crane'),cap_factor_calc(n, threshold_special,avgSPflv),
                               cap_factor_calc(n,threshold_adj,avgSPflv))) %>%
  select(Schedule,ModelYear,avgSPflv_Cap,n,SlopeAuc)

## apply the factor to the regression and only on ten years
shiftAuc.apply<-merge(OutRegression_Auc,Auc.capshift,by=c('Schedule','ModelYear'),all.x=T) %>%
  mutate(flv_shift = flv * ifelse(is.na(avgSPflv_Cap),1, avgSPflv_Cap)
         ,flv_shiftLim = ifelse(ModelYear==botyear, pmax(pmin(as.numeric(flv)*(1+lastyr_1),flv_shift),as.numeric(flv)*(1-lastyr_1)),
                                ifelse(ModelYear==botyear+1,pmax(pmin(as.numeric(flv)*(1+lastyr_2),flv_shift),as.numeric(flv)*(1-lastyr_2)),flv_shift)))


### Calculate the recency scale factors 
#### recency calculation: if N(recent 1 month) > n, use 1 month; else use most recent n til 3 months
## join the raw data to regression output
join.rec.auc<-merge(data.frame(Data_clean) %>% filter(SaleType !='Retail'), shiftAuc.apply,by=c('Schedule','ModelYear'))%>% 
  filter(as.Date(EffectiveDate)>=thirdLastM & ModelYear>=botyear & ModelYear<=topyear & Age>1) %>%
  mutate(power = abs(elapsed_months(EffectiveDate, publishDate)),
         flv.adjust = flv_shiftLim/(0.99^power))


## get the list of schedules who have more than threshold_rec.calc data point in most recent 1 month
## combine to get the dataset to calculate recency factor, apply cap
RPSF_auc.calc<-Use_Latest_Data(join.rec.auc,'SaleDate',threshold_rec.calc,'recency',publishDate) %>%
  group_by(Schedule) %>% 
  summarise(RPSF = mean(SalePrice/(CurrentABCost*as.numeric(flv.adjust))),n=n()) %>%
  mutate(RPSF_Cap = ifelse(n>threshold_recency,1*RPSF,((threshold_recency-n)+n*RPSF)/threshold_recency)) %>%
  mutate(f.recency.auc = pmin(1+recency_cap,pmax(RPSF_Cap,1-recency_cap))) %>%
  select(Schedule,f.recency.auc)


## join to get the full list (no recency data get factor = 1)
RPSF_auc.full<-merge(merge(SchedFullList %>% filter(!is.na(AuctionNewYrMax)) %>% select(Schedule),RPSF_auc.calc,by='Schedule',all.x=TRUE) %>%
                       mutate(f.recency.auc = ifelse(is.na(f.recency.auc),1,f.recency.auc)),model_year)
## apply recency factor to years with partial move 
recFact_auc<- merge(join.rec.auc %>% group_by(Schedule,ModelYear) %>% summarise(n=n())
                    ,RPSF_auc.full,by=c('Schedule','ModelYear'),all.y=TRUE) %>%
  complete(Schedule,ModelYear,
           fill = list(n = 0)) %>%
  mutate(partialf.recency.auc = partial_move(n,threshold_adj,f.recency.auc,5)) %>% select(-n)

## apply the recency factor to the shifted schedule
recencyAuc.apply <- merge(shiftAuc.apply,recFact_auc,by=c('Schedule','ModelYear'),all.x = T) %>%
  filter(ModelYear>=botyear) %>%
  mutate(recAdjust = ifelse(ModelYear == topyear, 1, ifelse(ModelYear == topyear - 1, 1/3*partialf.recency.auc+2/3, ifelse(ModelYear == topyear - 2, 2/3*partialf.recency.auc+1/3,partialf.recency.auc))),
         flv_rec = flv_shiftLim * recAdjust,
         flv_regrRect = flv * recAdjust) %>%
  arrange(Schedule,ModelYear)

## validate data
print(if(nSched_Ret == dim(recencyRet.apply)[1]/10){paste('Yes, the N_retail is ', nSched_Ret)} else {'No'})
print(if(nSched_Auc == dim(recencyAuc.apply)[1]/10){paste('Yes, the N_auction is ', nSched_Auc)} else {'No'})


