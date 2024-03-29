"
################# EDA of raw data ###################
## number of sales
EDAview.N<-uploadData %>%
  filter(Flag =='inUse') %>%
  group_by(SaleType,CategoryName,EffectiveDate) %>%
  summarise(n=n())

EDAview.N.trans<-spread(EDAview.N,EffectiveDate,n)
EDAview.N.trans[is.na(EDAview.N.trans)]=0

### read last month EDA file
read_lastmonth = read.xlsx(paste(LastM,CountryCode,'EDA.xlsx'), sheetIndex = 1,check.names = FALSE) 
merge_eda.lm<-edaview(read_lastmonth)
colnames(merge_eda.lm)<-c('SaleType','CategoryName','first.lm','last.lm','mid.lm')

eda.cm<-edaview(data.frame(EDAview.N.trans))
colnames(eda.cm)<-c('SaleType','CategoryName','first.cm','last.cm','mid.cm')

## join last month to this month
join.eda.counts<-merge(merge_eda.lm,eda.cm,all = TRUE) %>%
  mutate(change_inunchange = percent(mid.cm/mid.lm-1),
         change_inchangemonth = percent(last.cm/first.lm-1)) %>%
  select(SaleType,CategoryName,mid.cm,mid.lm,last.cm,first.lm,change_inunchange,change_inchangemonth) %>%
  rename(unchange_current = mid.cm, unchange_lastmonth = mid.lm,
          gain_current= last.cm,lost_lastmonth= first.lm)

## average sp/m1
EDAview.mean<-uploadData %>%
  filter(Flag =='inUse') %>%
  group_by(SaleType,CategoryName,EffectiveDate) %>%
  summarise(meanValue = round(mean(SPvalue),digits =4))

EDAview.mean.trans<-spread(EDAview.mean,EffectiveDate,meanValue)
EDAview.mean.trans[is.na(EDAview.mean.trans)]=0.0000

"
###################################################################################################################
###################################################################################################################
######################################## Make Data Merge & Clean ##################################################
###################################################################################################################
###################################################################################################################

####### ClassId list of make adjusted classifications= All - Out - OutR ########
Out_make <- anti_join(AllClass,comb_Out %>% select(ClassificationId),by='ClassificationId') %>%
  select(ClassificationId,CategoryName,CategoryId,SubcategoryName,SubcategoryId,MakeName,MakeId)

######## Join MList with Out + OutR tab (SubcatGroup) to get the CS - Schedule map ##########
### Subset of Out +OutR tab
## C-M level
CatMakemap<-merge(Mlist %>% filter(ApplyToAllCSMs =='Y' & SubcategoryId =='NULL') %>% select(CategoryId),comb_Out %>% filter(Level2=='SubcatGroup'), by='CategoryId') %>%
  filter(SubcategoryId !='NULL') %>%
  select(Schedule,CategoryId,SubcategoryId) 
## CS-M level
SubcatMakemap <-merge(Mlist %>% filter(ApplyToAllCSMs =='Y' & SubcategoryId != 'NULL') %>% select(SubcategoryId),comb_Out %>% filter(Level2=='SubcatGroup'), by='SubcategoryId') %>%
  select(Schedule,CategoryId,SubcategoryId) 
## join
joinmap_make <-merge(rbind(CatMakemap,SubcatMakemap),ReportGrp,by='CategoryId') 

######## Prepare the output mapping table for all makes schedules ########
make_output<-merge(joinmap_make,Out_make,by=c('CategoryId','SubcategoryId')) %>% filter(!is.na(MakeId))


###################################################################################################################
###################################################################################################################
################################# MANIPULATING DATA FORMATING FOR REGRESSION USE ##################################
###################################################################################################################
###################################################################################################################

###################################################################################################################
########################################### Part 1: Regular Schedule  #############################################
###################################################################################################################

### Combine C, CS and CSM levels of data into one & exclude bad data
Datainput<-split.joinlevel(In,uploadData,'') %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse')%>%
  mutate(CompId = factor(CompId)) %>%
  mutate(BorrowSchedule=NA,	BorrowType=NA) %>%
  group_by(Schedule,SaleType) %>%
  #summarise(mean = mean(SPvalue),st=sd(SPvalue))
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 

### same above for listing
if(CountryCode =='USA'){
  ### Listing - Join in Category level 
  CatListing <- revised_listing %>%
    ### Used for Feb to Nov 2019: bring in caterpillar data 
    mutate(M1AppraisalBookPublishDate=as.factor(publishDate)) %>%
    select(all_of(select.var))  %>%
    #mutate(SaleDate = as.Date(SaleDate),M1AppraisalBookPublishDate=as.Date(M1AppraisalBookPublishDate)) %>%
    mutate(CategoryId = as.integer(CategoryId),SubcategoryId = as.integer(SubcategoryId),MakeId = as.integer(MakeId),ModelId = as.integer(ModelId) ) %>%
    mutate(SaleDate = as.Date(SaleDate),M1AppraisalBookPublishDate = as.Date(M1AppraisalBookPublishDate))%>%
    mutate(BorrowSchedule=NA,	BorrowType=NA) %>%
    filter(Flag == 'inUse') %>%
    group_by(Schedule) %>%
    filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 
  
}


###################################################################################################################
############################################# Part 2: Auction Borrow  #############################################
###################################################################################################################

###### create a table auction data that not using for informing regression, this table only used for plot
aucBorrow_all<-split.joinlevel(InR,uploadData,'brw') %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse') %>%
  mutate(CompId = factor(CompId)) %>%
  group_by(Schedule,SaleType) %>%
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 

### Combine C, CS and CSM levels of data into one & exclude bad data
Datainput_Ret<-aucBorrow_all %>% filter(SaleType=='Retail')


###################################################################################################################
############################################## Part 3: Retail Borrow  #############################################
###################################################################################################################

###### create a table auction data that not using for informing regression, this table only used for plot
retBorrow_all<-split.joinlevel(InA,uploadData,'brw') %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse') %>%
  mutate(CompId = factor(CompId)) %>%
  group_by(Schedule,SaleType) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 

Datainput_Auc<-retBorrow_all %>% filter(SaleType=='Auction')
###################################################################################################################
############################################## Part 4: Both Side Borrow  #############################################
###################################################################################################################

### Combine C, CS and CSM levels of data into one & exclude bad data
Datainput_BothBrw<-split.joinlevel(InB,uploadData,'brw') %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse') %>%
  mutate(CompId = factor(CompId)) %>%
  group_by(Schedule,SaleType) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 


### combine regular and auction borrow - regression use data 
if (CountryCode == 'USA'){
  Data_comb <-rbind(Datainput,Datainput_Ret,Datainput_Auc,CatListing)
} else{
  Data_comb <-rbind(Datainput,Datainput_Ret,Datainput_Auc)
}

### Exclude sold in new comps 
Data_usedcomps.mark <- exc_new(merge(Data_comb,Usage_list,by=c('CategoryId','SubcategoryId'),all.x = T),time_min,meter_min)%>%
  mutate(SaleAB = ifelse(isNew == 'Y', SaleAB /(1 + new_discount), SaleAB))

Data_usedcomps<-rbind(Data_usedcomps.mark %>% filter(SaleType!='Retail'),
                     Use_Latest_Data(Data_usedcomps.mark %>% filter(SaleType=='Retail'),'SaleDate',threshold_newsales,'newsales',''))

  
###################################################################################################################
############ Running cooks distance using exponential regression for auction data to remove leverage points #########
###################################################################################################################
### subset auction data
SaleDtAuc_cd<-subset(Data_usedcomps,Data_usedcomps$SaleType=="Auction") %>% 
  filter(ModelYear >= ifelse(CategoryId %in% ListingIds,dep_endyr, ext_botYr) & ModelYear <=topyear) %>% 
  distinct()

## Regression model 
leveragelist<-list()
SchedRetBorw <-SchedR %>% filter(BorrowType=='AuctionBorrowRetail')
auc_regression <- rbind(SchedRetBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Auc<-dim(auc_regression)[1]

for (j in 1:nSched_Auc){
  
  groupData<-SaleDtAuc_cd %>% filter(Schedule==auc_regression[j,1])
  fit<-lm(SaleAB ~ Age,data = groupData)
  cooksd <- cooks.distance(fit)
  influential <- as.numeric(names(cooksd)[(cooksd > 40*mean(cooksd, na.rm=T) | cooksd >1)])
  
  leveragelist[[j]]<-groupData[influential,]
  
}

## save the list of ids that marked as leverage points
leverage.pts<-subset(do.call(rbind,leveragelist),Age<3)$CompId

## remove leverage points
##update data_all
Data_clean <- Data_usedcomps %>% filter(!(SaleType =='Auction' & CompId %in% leverage.pts))


############################## Use only recent data ############################
Data.count<-Data_clean %>%
  group_by(SaleType,Schedule,ModelYear) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum=row_number())

Data.count3m <- Data.count %>%
  filter(as.Date(EffectiveDate)>=thirdLastM & rowNum ==threshold_rec.calc) %>%
  select(SaleType,Schedule,ModelYear)

## Use at least 3 months data, up to threshold_rec.calc if there is not enough threshold_rec.calc data in recent 3 months
Data_all <- rbind(data.frame(merge(Data.count,Data.count3m,by=c('SaleType','Schedule','ModelYear')) %>% filter(as.Date(EffectiveDate)>=thirdLastM))
                  ,data.frame(anti_join(Data.count,Data.count3m,by=c('SaleType','Schedule','ModelYear')) %>% filter(rowNum<=threshold_rec.calc)))


### combine regular and borrow data for plots use only & exclude leverage points
if (CountryCode == 'USA'){
  Data_plot_combind <-rbind(Datainput,Datainput_BothBrw,aucBorrow_all,retBorrow_all,CatListing) %>% 
    filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
} else{
  Data_plot_combind <-rbind(Datainput,Datainput_BothBrw,aucBorrow_all,retBorrow_all) %>% 
    filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
}

### Exclude sold in new comps for plot
Data_all_plot.mark <- exc_new(merge(Data_plot_combind,Usage_list,by=c('CategoryId','SubcategoryId'),all.x = T),time_min,meter_min)%>%
  mutate(SaleAB = ifelse(isNew == 'Y', SaleAB /(1 + new_discount), SaleAB))

Data_all_plot<-rbind(Data_all_plot.mark %>% filter(SaleType!='Retail'),
            Use_Latest_Data(Data_all_plot.mark %>% filter(SaleType=='Retail'),'SaleDate',threshold_newsales,'newsales',''))

                                     
                                     
                         
