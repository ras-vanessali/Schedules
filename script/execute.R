

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

## publish date
publishDate<-Sys.Date() - days(day(Sys.Date()))
#publishDate = as.Date('2019-08-31')
thirdLastM<-publishDate - days(day(publishDate)) - days(day(publishDate - days(day(publishDate))))

## DB Server
#DBserver = 'production' 
DBserver = 'rasquery'

scripts_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules-USNA/script"
setwd(scripts_path)  



## Read the R script
runa<-parse('a.dataload.r')
runb<-parse('b.listings.r')
runc<-parse('c.datacleaning.r')
rund<-parse('d.buildmodel.r')
rune<-parse('e.adjustment.r')
runf<-parse('f.violationcheck.r')
rung<-parse('g. depreciation&monthlimit.r')
runh<-parse('h.makeschedules.r')
runi<-parse('i.upload&plots.r')



## file path & read file name
file_path = "C:/Users/vanessa.li/Documents/GitHub/Schedules-USNA/doc"
setwd(file_path)  
excelfile = '20200109 SchedulesManagement.xlsx'
plotFolder = paste("Plots",Sys.Date())
dir.create(plotFolder)


## Execute
## USA
eval(runa)
eval(runb)
eval(runc)
eval(rund)
eval(rune)
eval(runf)
eval(rung)
eval(runh)
eval(runi)
