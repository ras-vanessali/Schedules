

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
library(readxl)
library(stringr)

# country
#CountryCode = 'GBR'
CountryCode = 'USA'
# db enviroment & connect
DBserver = 'production' 
#DBserver = 'rasquery'


## read input excel file and create a plot for storing the plots
file_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules-USNA/doc"
setwd(file_path)  
excelfile = '20200211 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## set directory of r scripts
scripts_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules-USNA/script"
setwd(scripts_path) 
runa<-parse('a.input&func.r')
eval(runa)
setwd(scripts_path) 
runb<-parse('b.sqlQueries.r')
eval(runb)

channel<-odbcConnect(DBserver)
## run sql queries
if (CountryCode == 'USA'){
  publishDate <- Sys.Date() - days(day(Sys.Date()))
  uploadData <- sqlQuery(channel,US_dataload)
  uploadListing <- sqlQuery(channel,Listing_dataload)
  MakeAdjData <- sqlExecute(channel,MakeAdjust_dataload,CategoryId, fetch=T)
  LastMonth_Sched <- sqlQuery(channel,LM_USA_load)
  LastMonth_depr <- sqlQuery(channel,Last_depr_USAload)
  AllClass <- sqlQuery(channel,AllClass)
  ReportGrp <-sqlQuery(channel,ReportGrp)
  
} else{
  publishDate <- Sys.Date() - days(day(Sys.Date())) - days(day(Sys.Date() - days(day(Sys.Date()))))
  uploadData <- sqlQuery(channel,UK_dataload)
  #listingData <- sqlQuery(channel,Listing_dataload)
  #MakeAdjData <- sqlQuery(channel,MakeAdjust_dataload,,CategoryId, fetch=T)
  LastMonth_Sched <- sqlQuery(channel,LM_UK_load)
  LastMonth_depr <- sqlQuery(channel,Last_depr_UKload)
  AllClass <- sqlQuery(channel,AllClass)
}

#publishDate = as.Date('2019-08-31')
thirdLastM<-publishDate - days(day(publishDate)) - days(day(publishDate - days(day(publishDate))))

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
if (CountryCode == 'USA'){
  eval(runc)
  eval(rund)
  eval(rune)
  eval(runf)
  eval(rung)
  eval(runh)
  eval(runi)
  setwd(file_path)  
  eval(runj)
} else{
  
  eval(rund)
  eval(rune)
  eval(runf)
  eval(rung)
  eval(runh)
  
  setwd(file_path)  
  eval(runj)
}
