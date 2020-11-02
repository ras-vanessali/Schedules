

###################################################################################################################
############################################## Static Schedule ####################################################
##################################### Package Installation & File Loading #########################################
############################################## Executive file #####################################################
###################################################################################################################

library(odbc)
library(lattice)
library(latticeExtra)
library(xlsx)
library(lubridate)
library(tidyverse)

# connect to db
con <- dbConnect(
  odbc::odbc(),
  Driver='freetds',
  Server='rasdata.rasgcp.net',
  Database='ras_sas',
  uid='RASDOMAIN\\vanessa.li',
  pwd=rstudioapi::askForPassword("Database password"),
  Port=1433
)

# country market
#CountryCode = 'GBR'
CountryCode = 'USA'


## file path where management file is
input_path = "~/Project/ManagementFiles"
## file path where plots and files export to
file_path = "~/Project/Schedulescopy/doc"
## file path where script are
scripts_path = "~/Project/Schedulescopy/script"

## management file
excelfile = '20201022 SchedulesManagement.xlsx'

## create folders to save the plots
setwd(file_path)  
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## run inputs and functions
setwd(scripts_path) 
runa<-parse('a.input&func.r')
eval(runa)

## load data
starttime_dtload<-Sys.time()
## run sql queries
if (CountryCode == 'USA'){
  setwd(scripts_path) 
  runb<-parse('b.sqlQueries_US.r')
  eval(runb)
  publishDate <- Sys.Date() - days(day(Sys.Date()))
  uploadData <- dbGetQuery(con,US_dataload)
  uploadListing <- dbGetQuery(con,Listing_dataload)
  ########## Parameterized Sql #############
  #MakeAdjData <- sqlExecute(con,MakeAdjust_dataload,CategoryId, fetch=T)
  Query <- odbc::dbSendQuery(con, MakeAdjust_dataload)
  
  MakeAdjreturn = list()
  for (i in 1:nrow(CategoryId)){
    odbc::dbBind(Query, CategoryId[i,])
    dat<-odbc::dbFetch(Query)
    MakeAdjreturn[[i]] <- dat
  }
  
  MakeAdjData = do.call(rbind, MakeAdjreturn)
  ############################################
  LastMonth_Sched <- dbGetQuery(con,LM_USA_load)
  LastMonth_depr <- dbGetQuery(con,Last_depr_USAload)
  AllClass <- dbGetQuery(con,AllClass_query)
  ReportGrp <-dbGetQuery(con,ReportGrp_query)
  Usage_list<-dbGetQuery(con,UsageList_query)
  
} else{
  setwd(scripts_path) 
  runb<-parse('b.sqlQueries_UK.r')
  eval(runb)
  publishDate <- Sys.Date() - days(day(Sys.Date())) - days(day(Sys.Date() - days(day(Sys.Date()))))
  uploadData <- dbGetQuery(con,UK_dataload)
  #listingData <- dbGetQuery(con,Listing_dataload)
  MakeAdjData <- dbGetQuery(con,MakeAdjust_dataloadUK,CategoryId, fetch=T)
  LastMonth_Sched <- dbGetQuery(con,LM_UK_load)
  LastMonth_depr <- dbGetQuery(con,Last_depr_UKload)
  AllClass <- dbGetQuery(con,AllClass_query)
  ReportGrp <-dbGetQuery(con,ReportGrp_query)
}
end_time_dtload <- Sys.time()
end_time_dtload - starttime_dtload

#### effective dates
thirdLastM<-as.Date(publishDate%m-% months(1)- days(day(publishDate)))
sixLastM<-as.Date(publishDate%m-% months(5)- days(day(publishDate)))



setwd(scripts_path) 
## Read the R script
runc<-parse('c.listings.r')
rund<-parse('d.datacleaning.r')
rune<-parse('e.buildmodel.r')
runf<-parse('f.adjustment.r')
rung<-parse('g.violationcheck.r')
runh<-parse('h. depreciation&monthlimit.r')
runi<-parse('i.makeschedules.r')
runj<-parse('j.upload&plots.r')



## Execute
## USA
start_time_r <- Sys.time()
if (CountryCode == 'USA'){
  eval(runc)
  print('Listing is done')
  eval(rund)
  print('Data cleaning is done')
  eval(rune)
  print('Stats model is done')
  eval(runf)
  print('Adjustments is done')
  eval(rung)
  print('Violation check is done')
  eval(runh)
  print('Depreciation and MoM are done')
  eval(runi)
  print('Make Adjusters is done')
  setwd(file_path)  
  eval(runj)
  print('Plots generation is done')
  setwd(file_path)  
  write.xlsx2(as.data.frame(joinMakeOut),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjusters',row.names = F)
  write.xlsx2(as.data.frame(MakeSFcalc_Sched),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjCalc',append=T,row.names = F)
  write.xlsx2(as.data.frame(list_reductfact),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'ListingReduction',append=T,row.names = F)
  write.xlsx(regressionCoef,file = paste(publishDate, 'Coefficients.xlsx'), sheetName='Sheet1',row.names=F)
  write.xlsx2(as.data.frame(EDAview.N.trans),file = paste(publishDate,CountryCode," EDA.xlsx"),sheetName = 'NumUnits',row.names =F)
  write.xlsx2(as.data.frame(EDAview.mean.trans),file = paste(publishDate,CountryCode," EDA.xlsx"),sheetName = 'SPValMean',row.names =F,append=T)
} else{
# UK  
  eval(rund)
  print('Data cleaning is done')
  eval(rune)
  print('Stats model is done')
  eval(runf)
  print('Adjustments is done')
  eval(rung)
  print('Violation check is done')
  eval(runh)
  print('Depreciation and MoM are done')
  eval(runi)
  print('Make Adjusters is done')
  setwd(file_path)  
  eval(runj)
  write.xlsx2(as.data.frame(joinMakeOut),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjusters',row.names = F)
  write.xlsx2(as.data.frame(MakeSFcalc_Sched),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjCalc',append=T,row.names = F)
  write.xlsx(regressionCoef,file = paste(publishDate, 'Coefficients.xlsx'), sheetName='Sheet1',row.names=F)
}

end_time_r <- Sys.time()
end_time_r - start_time_r

