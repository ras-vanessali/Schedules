

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



## DB Server
DBserver = 'production' 
#DBserver = 'rasquery'

## file path & read file name
file_path = "H:/Projects/72_ Regression_Schedule/201912"
setwd(file_path)  
excelfile = '20200109 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## publish date
publishDate<-Sys.Date() - days(day(Sys.Date()))
#publishDate = as.Date('2019-08-31')
## Read the R script
runa_USA<-parse('a. DB_DataImport_US.r')
runa_UK<-parse('a. DB_DataImport_UK.r')
run_listing<-parse('listings.r')
runb_DataClean<-parse('b. Inputs&DataCleaning.r')
runc<-parse('c. LogisticModel_v5.r')
rund<-parse('d. Adjustments&RuleCheck_v4.r')
rune<-parse('e. Depreciation&MoMLimit_v2.r')
runf<-parse('f. MakeSchedules.r')
rung<-parse('g. UploadFile&Plots.r')

## Execute
## USA
eval(runa_USA)
eval(run_listing)
eval(runb_DataClean)
eval(runc)
eval(rund)
eval(rune)
eval(runf)
eval(rung)


## UK
eval(runa_UK)
eval(runb_DataClean)
eval(runc)
eval(rund)
eval(rune)
eval(runf)
eval(rung)
