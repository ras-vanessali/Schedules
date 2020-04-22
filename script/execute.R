

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
#DBserver = 'production' 
DBserver = 'rasquery'


## read input excel file and create a plot for storing the plots
file_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules/doc"
setwd(file_path)  
excelfile = '20200331 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## set directory of r scripts
scripts_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules/script"
setwd(scripts_path) 
runa<-parse('a.input&func.r')
eval(runa)

channel<-odbcConnect(DBserver)
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


####
catchupSched<-c(328,	90456,	90457,	90459,	68584,	71815,	40,	66831,	83867,	66824,	66825,	37117,	46,	
              90458,	90455,	101043,	50844,	47,	90454,	52,	66826,	72115,		84552,	66830,	3,	34,	17,	12)
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
  eval(runi)
  setwd(file_path)  
  eval(runj)
}

