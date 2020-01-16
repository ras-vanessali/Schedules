
#######################################################################################################################
##################################################### Build Functions  ################################################
#######################################################################################################################

### build a function to do the minimum delta calculation 
minimumDelta <- function(deltaA,deltaB,move){
  delta = deltaA-deltaB
  result = ifelse(delta > Min_delta,(delta - Min_delta) * (move/delta),0)
  return(result)
}


### build a function to check if in range. 
WithinRange <- function(checkPoint, Max, Min){
  result = ifelse(checkPoint>Max, 'Max', ifelse(checkPoint<Min,'Min',''))
  return(result)
}

### build a function to do MoM limitation for schedules
MoMlimitFunc <- function(last_month,current_month,limitUp,limitDn){
  upline = last_month * (1+limitUp)
  btline = last_month / (1+limitDn)
  result = pmin(upline,pmax(btline,current_month))
  return(result)
}

### build a function to do MoM limitation for depreciation and appreciation
MoMlimit_depappr <- function(last_month,current_month,limit){
  upline = last_month + limit
  btline = last_month - limit
  result = pmin(upline,pmax(btline,current_month))
  return(result)
}


### build a function to calculate the newest year cap by effective date
NewYr_CapFunc<- function(inputcaps){
  age = year(publishDate) - topyear + (month(publishDate)-6)/12
  monthDiff = age *12
  cap = inputcaps *(1 - capMonthpct)^monthDiff
  return(cap)
}


### build a function to calcualte the extended years' schedule using depreciation and appreciation rate
depr_appr_sched <- function(type, endyrSched, rate, age_gap){
  if (type == 'Dep'){
    newSched = endyrSched * (1-rate)^age_gap
  }
  else if (type=='App'){
    newSched = endyrSched * (1+rate)^age_gap
  }
  else{newSched = ''}
  return(newSched)
}


### build a function for cranes special input factors
cranes_value <-function(Schedule,value,value_crane){
  if(str_detect(Schedule,'Cranes')){
    value_return = value_crane
  }
  else{value_return = value}
  return(value_return)
}




### This process starts from effective date 2019-02-28 and will end in 2020-09-30. ###
EOMList<-seq(as.Date('2019-03-01'),length=20,by='1 month') -1
str <-sort(c(0:9,0:9))
CatDtUse <-data.frame(EOMList,str)
indexUse<- CatDtUse[CatDtUse$EOMList==publishDate,]$str
###split join level
split.joinlevel<-function(input,dataload,brwtype){
  select.var<-c('CompId',	'CategoryId',	'CategoryName',	'SubcategoryId',	'SubcategoryName',	'MakeId',	'MakeName',	'ModelId',	'ModelName',	
                'ModelYear',	'SaleDate',	'EffectiveDate',	'SalePrice',	'M1Value',	'SaleType',	'M1AppraisalBookPublishDate',	'SaleAB',	
                'SPvalue',	'CurrentABCost',	'Age',	'Flag',	'YearFlag',	'Schedule') 
  select.var.brw<-c('BorrowSchedule',	'BorrowType')

  Catlevel<-input %>% filter(Level2 =='Category') %>% select(-SubcategoryId,-MakeId)
  Subcatlevel<-input %>% filter(Level2 == "SubcatGroup") %>% select(-MakeId)
  Makelevel<-input %>% filter(Level2 =='Make')
  
  if(brwtype=='brw'){
    CatData <- merge(dataload,Catlevel, by=c("CategoryId","Country")) %>% 
      mutate(str = str_sub(CompId,-1)) %>%
      filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
      select(c(select.var,select.var.brw)) 
  
    SubcatData <- merge(dataload,Subcatlevel, by=c('CategoryId',"SubcategoryId","Country")) %>% 
      mutate(str = str_sub(CompId,-1)) %>%
      filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
      select(c(select.var,select.var.brw)) 
  
    MakeData<-merge(dataload,Makelevel, by=c('CategoryId',"SubcategoryId","MakeId","Country")) %>% select(c(select.var,select.var.brw)) 
  }
  else{
    CatData <- merge(dataload,Catlevel, by=c("CategoryId","Country")) %>% 
      mutate(str = str_sub(CompId,-1)) %>%
      filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
      select(select.var) 
    
    SubcatData <- merge(dataload,Subcatlevel, by=c('CategoryId',"SubcategoryId","Country")) %>% 
      mutate(str = str_sub(CompId,-1)) %>%
      filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
      select(select.var) 
    
    MakeData<-merge(dataload,Makelevel, by=c('CategoryId',"SubcategoryId","MakeId","Country")) %>% select(select.var) 
  }
  
return(rbind(CatData,SubcatData,MakeData))}



###################################################################################################################
###################################################################################################################
############################################# Data Merge & Clean ##################################################
###################################################################################################################
###################################################################################################################

####### Prepare the candidate output list for Part 3 schedules ########
####### ClassId list = All - Out - OutR ########
Out_make <- anti_join(AllClass,comb_Out %>% select(ClassificationId),by='ClassificationId') %>%
  select(ClassificationId,CategoryName,CategoryId,SubcategoryName,SubcategoryId,MakeName,MakeId)
dim(comb_Out)

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
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 
  
### Listing - Join in Category level 
CatListing <- revised_listing %>%
  ### Used for Feb to Nov 2019: bring in caterpillar data 
  mutate(M1AppraisalBookPublishDate=as.factor(publishDate)) %>%
  select(CompId,CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear, SaleDate,EffectiveDate, Country,SalePrice, M1Value
         ,SaleType,M1AppraisalBookPublishDate,SaleAB, SPvalue,CurrentABCost,Age,Flag,YearFlag,Schedule) %>%
  filter(Flag == 'inUse') %>%
  group_by(Schedule) %>%
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 


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
Datainput_Ret<-aucBorrow_all %>%
  filter(SaleType=='Retail')


###################################################################################################################
############################################## Part 3: Retail Borrow  #############################################

###### create a table auction data that not using for informing regression, this table only used for plot
retBorrow_all<-split.joinlevel(InA,uploadData,'brw') %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse') %>%
  mutate(CompId = factor(CompId)) %>%
  group_by(Schedule,SaleType) %>%
  filter(SPvalue <= ave(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= ave(SPvalue) - stdInd*sd(SPvalue)) 

Datainput_Auc<-retBorrow_all %>%
  filter(SaleType=='Auction')
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
Data_all <-rbind(Datainput,Datainput_Ret,Datainput_Auc,CatListing)

###################################################################################################################
############ Run cooks distance using exponential regression for auction data that remove leverage points #########
###################################################################################################################

SaleDtAuc_cd<-subset(Data_all,Data_all$SaleType=="Auction") %>% 
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
SaleDtAuc <-SaleDtAuc_cd %>% filter(!CompId %in% leverage.pts)
##update data_all
Data_all <- Data_all %>% filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
l<-Data_all_plot %>% filter(SaleType=='Listing')

### combine regular and borrow data for plots use only & exclude leverage points
Data_all_plot <-rbind(Datainput,Datainput_BothBrw,aucBorrow_all,retBorrow_all,CatListing) %>% 
  filter(!(SaleType =='Auction' & CompId %in% leverage.pts))
