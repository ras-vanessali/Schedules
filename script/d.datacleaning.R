
### This process starts from effective date 2019-02-28 and will end in 2020-09-30. ###
EOMList<-seq(as.Date('2019-03-01'),length=20,by='1 month') -1
str <-sort(c(0:9,0:9))
CatDtUse <-data.frame(EOMList,str)
indexUse<- CatDtUse[CatDtUse$EOMList==publishDate,]$str


################# EDA of raw data ###################
## number of sales
EDAview.N<-uploadData %>%
  filter(Flag =='inUse') %>%
  group_by(SaleType,CategoryName,EffectiveDate) %>%
  summarise(n=n())

EDAview.N.trans<-spread(EDAview.N,EffectiveDate,n)
EDAview.N.trans[is.na(EDAview.N.trans)]=0

## average sp/m1
EDAview.mean<-uploadData %>%
  filter(Flag =='inUse') %>%
  group_by(SaleType,CategoryName,EffectiveDate) %>%
  summarise(meanValue = round(mean(SPvalue),digits =4))

EDAview.mean.trans<-spread(EDAview.mean,EffectiveDate,meanValue)
EDAview.mean.trans[is.na(EDAview.mean.trans)]=0.0000


###################################################################################################################
###################################################################################################################
######################################## Make Data Merge & Clean ##################################################
###################################################################################################################
###################################################################################################################
#if(CountryCode == 'USA'){
####### Prepare the candidate output list for Part 3 schedules ########
####### ClassId list = All - Out - OutR ########
Out_make <- anti_join(AllClass,comb_Out %>% select(ClassificationId),by='ClassificationId') %>%
  select(ClassificationId,CategoryName,CategoryId,SubcategoryName,SubcategoryId,MakeName,MakeId)

######## Join MList with Out + OutR tab (SubcatGroup) to get the CS - Schedule map ##########
### Subset of Out +OutR tab

CatMakemap<-merge(Mlist %>% filter(ApplyToAllCSMs =='Y' & SubcategoryId =='NULL') %>% select(CategoryId),comb_Out %>% filter(Level2=='SubcatGroup'), by='CategoryId') %>%
  filter(SubcategoryId !='NULL') %>%
  select(Schedule,CategoryId,SubcategoryId) 

SubcatMakemap <-merge(Mlist %>% filter(ApplyToAllCSMs =='Y' & SubcategoryId != 'NULL') %>% select(SubcategoryId),comb_Out %>% filter(Level2=='SubcatGroup'), by='SubcategoryId') %>%
  select(Schedule,CategoryId,SubcategoryId) 

joinmap_make <-merge(rbind(CatMakemap,SubcatMakemap),ReportGrp,by='CategoryId') 


######## Prepare the output mapping table for all makes schedules ########
make_output<-merge(joinmap_make,Out_make,by=c('CategoryId','SubcategoryId')) %>% filter(!is.na(MakeId))
#}

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
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse') %>%
  mutate(CompId = factor(CompId)) %>%
  group_by(Schedule,SaleType) %>%
  #summarise(mean = mean(SPvalue),st=sd(SPvalue))
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 

if(CountryCode =='USA'){
### Listing - Join in Category level 
CatListing <- revised_listing %>%
  ### Used for Feb to Nov 2019: bring in caterpillar data 
  mutate(M1AppraisalBookPublishDate=as.factor(publishDate)) %>%
  select(CompId,CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear, SaleDate,EffectiveDate, Country,SalePrice, M1Value
         ,SaleType,M1AppraisalBookPublishDate,SaleAB, SPvalue,CurrentABCost,Age,Flag,YearFlag,Schedule) %>%
  #mutate(SaleDate = as.Date(SaleDate),M1AppraisalBookPublishDate=as.Date(M1AppraisalBookPublishDate)) %>%
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

###################################################################################################################
############ Run cooks distance using exponential regression for auction data that remove leverage points #########
###################################################################################################################

SaleDtAuc_cd<-subset(Data_comb,Data_comb$SaleType=="Auction") %>% 
  filter(ModelYear >= ifelse(CategoryId %in% ListingIds,dep_endyr, ext_botYr) & ModelYear <=topyear) %>% 
  distinct()

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
#SaleDtAuc <-SaleDtAuc_cd %>% filter(!CompId %in% leverage.pts)
##update data_all
Data_clean <- Data_comb %>% filter(!(SaleType =='Auction' & CompId %in% leverage.pts))


## Use recent data
Data.count<-Data_clean %>%
  group_by(SaleType,Schedule,ModelYear) %>%
  arrange(desc(SaleDate)) %>%
  mutate(rowNum=row_number())

Data.count3m <- Data.count %>%
  filter(as.Date(EffectiveDate)>=thirdLastM & rowNum ==threshold_rec.calc) %>%
  select(SaleType,Schedule,ModelYear)

Data_all <- rbind(data.frame(merge(Data.count,Data.count3m,by=c('SaleType','Schedule','ModelYear')) %>% filter(as.Date(EffectiveDate)>=thirdLastM))
                      ,data.frame(anti_join(Data.count,Data.count3m,by=c('SaleType','Schedule','ModelYear')) %>% filter(rowNum<=25)))
  
  
### combine regular and borrow data for plots use only & exclude leverage points
if (CountryCode == 'USA'){
  Data_all_plot <-rbind(Datainput,Datainput_BothBrw,aucBorrow_all,retBorrow_all,CatListing) %>% 
    filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
} else{
  Data_all_plot <-rbind(Datainput,Datainput_BothBrw,aucBorrow_all,retBorrow_all) %>% 
    filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
}

