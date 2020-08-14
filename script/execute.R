

###################################################################################################################
############################################## Static Schedule ####################################################
##################################### Package Installation & File Loading #########################################
############################################## Executive file #####################################################
###################################################################################################################

library(RODBC)
library(RODBCext)
library(lattice)
library(latticeExtra)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(xlsx)
#library(readxl)
library(stringr)

# country
CountryCode = 'GBR'
#CountryCode = 'USA'
# db enviroment & connect
DBserver = 'production' 
#DBserver = 'rasquery'
#DBserver ='gfdev04.rasgcp.net'


## read input excel file and create a plot for storing the plots
file_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules/doc"
setwd(file_path)  
excelfile = '20200710 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## set directory of r scripts
scripts_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules/script"
setwd(scripts_path) 
runa<-parse('a.input&func.r')
eval(runa)

channel<-odbcConnect(DBserver)

starttime_dtload<-Sys.time()
## run sql queries
if (CountryCode == 'USA'){
  setwd(scripts_path) 
  runb<-parse('b.sqlQueries_US.r')
  eval(runb)
  publishDate <- Sys.Date() - days(day(Sys.Date()))
  uploadData <- sqlQuery(channel,US_dataload)
  uploadListing <- sqlQuery(channel,Listing_dataload)
  MakeAdjData <- sqlExecute(channel,MakeAdjust_dataload,CategoryId, fetch=T)
  LastMonth_Sched <- sqlQuery(channel,LM_USA_load)
  LastMonth_depr <- sqlQuery(channel,Last_depr_USAload)
  AllClass <- sqlQuery(channel,AllClass)
  ReportGrp <-sqlQuery(channel,ReportGrp)
  
} else{
  setwd(scripts_path) 
  runb<-parse('b.sqlQueries_UK.r')
  eval(runb)
  publishDate <- Sys.Date() - days(day(Sys.Date())) - days(day(Sys.Date() - days(day(Sys.Date()))))
  uploadData <- sqlQuery(channel,UK_dataload)
  #listingData <- sqlQuery(channel,Listing_dataload)
  MakeAdjData <- sqlExecute(channel,MakeAdjust_dataloadUK,CategoryId, fetch=T)
  LastMonth_Sched <- sqlQuery(channel,LM_UK_load)
  LastMonth_depr <- sqlQuery(channel,Last_depr_UKload)
  AllClass <- sqlQuery(channel,AllClass)
  ReportGrp <-sqlQuery(channel,ReportGrp)
}
end_time_dtload <- Sys.time()
end_time_dtload - starttime_dtload

####
#publishDate = as.Date('2019-08-31')
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

