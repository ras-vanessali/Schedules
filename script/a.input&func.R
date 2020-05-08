
#################### The upload file consists of 4 parts #####################
# Part 1: regular schedules - Out
# Part 2: auction borrow schedules - OutR
# Part 3: make level schedules (scaled with make adjusters)
# Part 4: tier 3 make schedules

scal=3
## Model Year in use
topyear = 2020 ## top model year
comingyr = topyear+1 ## top model year pulling from db, used for appreciation
botyear = topyear-9 ## bottom model year
ext_botYr = topyear-11 ## bottom model year of putting in regression
dep_endyr = topyear-15 ## bottom model year pulling from db, used for depreciation

## at what age joint the logistic and exponential 
age_joint = 7

## set the minimum gap between adjacent years generally, some specific cases may not use this value
indexcap = 0.0300

## set the limit movement from last month
limUp_MoM = 0.0400
limDw_MoM = 0.0600
limDw_MoM_spec = 0.1

## thresholds of #datapoints - use to move schedules from regression
#t1<-3 t2<-6 t3<-10 t4<-15 t5<-21 t6<-28
threshold_adj = 25
threshold_brw = 30
threshold_recency = 40
threshold_rec.calc = 25


recency_cap = 0.25
## set the minimum gap between two channels
if (CountryCode == 'USA'){
  capChannel = 0.10
} else{
  capChannel = 0.05
  }

## use for channel confict - distinguish older and newer age
chanyr = 3
## use for channel check who govern in middel years. retail data >5, Auction governs
retBelieve = 5 

### borrow schedule first year move
brwsched_move = .1

## newest year in auction have to be in the range
capMonthpct = 0.01

## set the bounds of could-be lowest and highest schedule
UpperB = 3
LowerB = 0


## logistic growth regression slope cap
slopecap_low = -1.5
slopecap_up_auc = -.05
slopecap_up_ret = -.02
## caps of appreciation and depreciation
app_yrgap = 3.00
appBound_upp = 0.15
appBound_bot = 0.05
appBound_na = 0.10

depBound_upp = 0.06
depBound_upp_crane = 0.10
depBound_bot = 0.020
depBound_na = 0.040

DeprMoMLimit = 0.002
ApprMoMLimit = 0.01
## set the index of standard deviation side of mean 
stdInd =2


## Make adjusters caps
makeSFupp = 1.3
makeSFbot = 0.7

## Make adjustment channel gap 
Min_delta =0.1

## threshold of data points for make adjusters
thredtpts.sched = 50
thredtpts = 15

## use for phase in factors on each model year
phaseinAge = 6
phaseinFactor =0.15

## Last two years in schedules - limit the shift
lastyr_1 = 0.05
lastyr_2 = 0.10


## use for the second end year - prevend rebasing jump
x = 0.05
## use for the first end year violate second end year
endYrRate = 0.02

## Global category list
GlobalList<-c(313,	6,	2509,	15,	29,	315,	360,	451,	316,	362)
#Articulating Booms,	Backhoe Loaders,	Compact Track Loaders,	Dozers,	Excavators,	Scissor Lifts,	Skid Steer Loaders,	Telehandlers,	Telescopic Booms,	Wheel Loaders
ListingIds<-c(2605,2603,2608,2604,2606,2577,19,2607) 
#'Carry-Deck Cranes','Rough-Terrain Cranes','All-Terrain Cranes','Truck-Mounted Cranes','Crawler Cranes'
GlobalClassId=1


setwd(file_path)  
################################################# Read tabs in file ##########################################################
### load the inputfeed file
In<-data.frame(read.xlsx(excelfile,sheetName='In')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule)
InR<-data.frame(read.xlsx(excelfile,sheetName='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='RetailBorrowAuction')
InA<-data.frame(read.xlsx(excelfile,sheetName='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='AuctionBorrowRetail')
InB<-data.frame(read.xlsx(excelfile,sheetName='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='BorrowBoth')
In.brwcrane<-data.frame(read.xlsx(excelfile,sheetName='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(str_detect(Schedule,'Crane'))

### load the application file
Out<-data.frame(read.xlsx(excelfile,sheetName='Out')) %>% filter(Country==CountryCode)
OutR<-data.frame(read.xlsx(excelfile,sheetName='OutR')) %>% filter(Country==CountryCode)

### Application tab
comb_Out<-rbind(Out %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId, Level2,Plot),OutR %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId,Level2,Plot))

### load the Sched file
Sched<-data.frame(read.xlsx(excelfile,sheetName='Sched',startRow=6)) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax) 
SchedR<-data.frame(read.xlsx(excelfile,sheetName='SchedR',startRow=6)) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax,BorrowSchedule,BorrowType) 
SchedFullList<-rbind(Sched,SchedR %>% select(-BorrowSchedule,-BorrowType))

### load make adjustments related tabs
Mlist<-data.frame(read.xlsx(excelfile,sheetName='MList')) %>% filter(Country==CountryCode) %>% select(ClassificationId,ApplyToAllCSMs,Country,CategoryId,SubcategoryId,CategoryName,SubcategoryName)
# Category who needs to run make level schedule
MakeAdj_CatL <- Mlist %>% distinct(CategoryId)
CategoryId <- cbind(MakeAdj_CatL, MakeAdj_CatL)

Categ_fullLs <- rbind(In['CategoryId'],InR['CategoryId'],InA['CategoryId']) %>% distinct()



#######################################################################################################################
##################################################### Build Functions  ################################################
#######################################################################################################################
### build function for which year use for drive
fixyr_gap <- function(Schedule){
  fixyr = ifelse(str_detect(Schedule,'Cranes'), 5, 3)
  return(fixyr)
}

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

### build function to comput how many months different between two dates. 
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

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
      select(all_of(select.var)) 
    
    SubcatData <- merge(dataload,Subcatlevel, by=c('CategoryId',"SubcategoryId","Country")) %>% 
      mutate(str = str_sub(CompId,-1)) %>%
      filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
      select(all_of(select.var)) 
    
    MakeData<-merge(dataload,Makelevel, by=c('CategoryId',"SubcategoryId","MakeId","Country")) %>% select(all_of(select.var)) 
  }
  
  return(rbind(CatData,SubcatData,MakeData))}

