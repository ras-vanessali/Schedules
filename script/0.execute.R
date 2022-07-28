

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
library(googleAuthR)
library(googleCloudStorageR)


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
## choose the environment, dev vs prod
env = "prod"


###### output market code
if(CountryCode == "USA"){
  market = 'USNA'
} else{
  market = 'GBUK'
}

## file path where management file is
input_path = "~/Project/ManagementFiles"
## file path where plots and files export to
file_path = "~/Project/Schedules/doc"
## file path where script are
scripts_path = "~/Project/Schedules/script"

## management file
excelfile = '20220713 SchedulesManagement.xlsx'

## create folders to save the plots
setwd(file_path)  
plotFolder = paste("Plots",CountryCode,Sys.Date())
dir.create(plotFolder)


## run inputs and functions
setwd(scripts_path) 
runa<-parse('a.input&func.r')
eval(runa)

## load data
starttime_dtload<-Sys.time()
## run sql queries
if (CountryCode == 'USA'){
       setwd(scripts_path) ``
       runb<-parse('b.sqlQueries_US.r')
       eval(runb)
       publishDate <- Sys.Date() - days(day(Sys.Date()))
       uploadData <- dbGetQuery(con,US_dataload)
       print('uploadData is loaded')
       uploadListing <- dbGetQuery(con,Listing_dataload)
       print('listingData is loaded')
       ########## Make Adjusters #############
       #MakeAdjData <- sqlExecute(con,MakeAdjust_dataload,CategoryId, fetch=T)
         Query <- odbc::dbSendQuery(con, MakeAdjust_dataload)
         
           MakeAdjreturn = list()
           for (i in 1:nrow(CategoryId)){
               odbc::dbBind(Query, CategoryId[i,])
               dat<-odbc::dbFetch(Query)
               MakeAdjreturn[[i]] <- dat
             }
           
             MakeAdjData = do.call(rbind, MakeAdjreturn)
             print('makeAdjustData is loaded')
             ############################################
             ########## Model Adjusters #############
             model_Query <- odbc::dbSendQuery(con, ModelAdjust_dataload)
             ModelAdjreturn = list()
             for (i in 1:nrow(SubcategoryId)){
                 odbc::dbBind(model_Query, SubcategoryId[i,])
                 dat1<-odbc::dbFetch(model_Query)
                 ModelAdjreturn[[i]] <- dat1 }
             ModelAdjData = do.call(rbind, ModelAdjreturn)
             print('ModelAdjustData is loaded')
             ########## Model classifications #############
             modelclass_Query <- odbc::dbSendQuery(con, ModelClass_query)
             ModelClassreturn = list()
             for (i in 1:nrow(SubcategoryId)){
                 odbc::dbBind(modelclass_Query, SubcategoryId[i,1])
                 dat2<-odbc::dbFetch(modelclass_Query)
                 ModelClassreturn[[i]] <-dat2
               }
             All_ModelClass = do.call(rbind, ModelClassreturn)
             ############################################
           
               Model_customSched <- dbGetQuery(con,Model_custom)
               LastMonth_Sched <- dbGetQuery(con,LM_USA_load)
               LastMonth_depr <- dbGetQuery(con,Last_depr_USAload)
               print('lastMonth is loaded')
               AllClass <- dbGetQuery(con,AllClass_query)
               ReportGrp <-dbGetQuery(con,ReportGrp_query)
               Usage_list<-dbGetQuery(con,UsageList_query)
               print('SQL Query Reading Is Finished')
             } else{
                 setwd(scripts_path) 
                 runb<-parse('b.sqlQueries_UK.r')
                 eval(runb)
                 publishDate <- Sys.Date() - days(day(Sys.Date()))
                 uploadData <- dbGetQuery(con,UK_dataload)
                 print('uploadData is loaded')
                 ########## Parameterized Sql #############
                 #MakeAdjData <- sqlExecute(con,MakeAdjust_dataload,CategoryId, fetch=T)
                   Query <- odbc::dbSendQuery(con, MakeAdjust_dataloadUK)
                   
                     MakeAdjreturn = list()
                     for (i in 1:nrow(CategoryId)){
                         odbc::dbBind(Query, CategoryId[i,])
                         dat<-odbc::dbFetch(Query)
                         MakeAdjreturn[[i]] <- dat
}
                     
                       MakeAdjData = do.call(rbind, MakeAdjreturn)
                       print('MakeAdjData is loaded')
                       ############################################
                       LastMonth_Sched <- dbGetQuery(con,LM_UK_load)
                       LastMonth_depr <- dbGetQuery(con,Last_depr_UKload)
                       print('lastMonth is loaded')
                       AllClass <- dbGetQuery(con,AllClass_query)
                       ReportGrp <-dbGetQuery(con,ReportGrp_query)
                       Usage_list<-dbGetQuery(con,UsageList_query)
                       print('SQL Query Reading Is Finished')
}
end_time_dtload <- Sys.time()
end_time_dtload - starttime_dtload

#### effective dates
LastM<-Sys.Date()%m-% months(1) - days(day(Sys.Date()))
thirdLastM<-Sys.Date()%m-% months(2) - days(day(Sys.Date()))
sixLastM<-Sys.Date()%m-% months(6) - days(day(Sys.Date()))



setwd(scripts_path) 
## Read the R script
runc<-parse('c.listings.r')
rund<-parse('d.datacleaning.r')
rune<-parse('e.buildmodel.r')
runf<-parse('f.adjustment.r')
rung<-parse('g.violationcheck.r')
runh<-parse('h.depreciation&monthlimit.r')
runi<-parse('i.makeschedules.r')
runj<-parse('j.modelschedules.R')
runk<-parse('k.violationcheck_2nd.r')
runl<-parse('l.drawplots.r')
runm<-parse('m.upload.r')
runn<-parse('n.backtest.r')


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
  eval(runj)
  print('Model Adjusters is done')
  eval(runk)
  print('2nd violation check is done')
  setwd(file_path)  
  eval(runl)
  print('Plots generation is done')
  setwd(file_path)  
  #write.csv(excel_out,paste('ClassificationModifySchedule',CountryCode,format(Sys.time(),format='%Y%m%d%H%M'),'VL.csv',sep=''),row.names = F)
  write.xlsx2(as.data.frame(joinMakeOut),file = paste(publishDate,CountryCode,"Share.xlsx"), sheetName = 'MakeAdjusters',row.names = F)
  write.xlsx2(as.data.frame(MakeSFcalc_Sched),file = paste(publishDate,CountryCode,"Share.xlsx"), sheetName = 'MakeAdjCalc',append=T,row.names = F)
  write.xlsx2(as.data.frame(list_reductfact),file = paste(publishDate,CountryCode,"Share.xlsx"), sheetName = 'ListingReduction',append=T,row.names = F)
  write.xlsx(regressionCoef,file = paste(publishDate, 'Coefficients.xlsx'), sheetName='Sheet1',row.names=F)
  write.xlsx2(as.data.frame(EDAview.N.trans),file = paste(publishDate,CountryCode,"EDA.xlsx"),sheetName = 'NumUnits',row.names =F)
  write.xlsx2(as.data.frame(join.eda.counts),file = paste(publishDate,CountryCode,"EDA.xlsx"),sheetName = 'CountsChangePerc',row.names =F,append=T)
  write.xlsx2(as.data.frame(EDAview.mean.trans),file = paste(publishDate,CountryCode,"EDA.xlsx"),sheetName = 'SPValMean',row.names =F,append=T)
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
  #eval(runj)
  #print('Model Adjusters is done')
  eval(runk)
  print('2nd violation check is done')
  setwd(file_path)  
  eval(runl)
  print('Plots generation is done')
  #write.csv(excel_out,paste('ClassificationModifySchedule',format(Sys.time(),format='%Y%m%d%H%M'),'VL.csv',sep=''),row.names = F)
  write.xlsx2(as.data.frame(joinMakeOut),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjusters',row.names = F)
  write.xlsx2(as.data.frame(MakeSFcalc_Sched),file = paste(publishDate,CountryCode," Share.xlsx"), sheetName = 'MakeAdjCalc',append=T,row.names = F)
  write.xlsx(regressionCoef,file = paste(publishDate, 'Coefficients.xlsx'), sheetName='Sheet1',row.names=F)
}

end_time_r <- Sys.time()
end_time_r - start_time_r


## prepare the final import files (good file and file w/ failures)
yearcheck_list = fix_Yearcheck %>% filter(retFlag=='flag' |aucFlag=='flag') %>% select (ClassificationId)
chancheck_list = fix_channelcheck %>%mutate(rate = limit_fmv/limit_flv) %>% filter (rate < 1) %>% select (ClassificationId)
apprcheck_list = full_depr_results %>%filter(!limit_fmv>0)%>% select (ClassificationId)

failure_rows = rbind(yearcheck_list,chancheck_list,apprcheck_list)

file_B <- merge(excel_out,failure_rows,how='inner',by.x = "ClassificationID",by.y = "ClassificationId")
file_A = anti_join(excel_out,failure_rows,by = c("ClassificationID" = "ClassificationId"))        
  
if(dim(file_A %>%filter(ClassificationID==1))[1]==22){
  write.csv(file_A,paste('ClassificationModifySchedule',CountryCode,format(Sys.time(),format='%Y%m%d%H%M'),'VL_A.csv',sep=''),row.names = F)
  write.csv(file_B,paste('ClassificationModifySchedule',CountryCode,format(Sys.time(),format='%Y%m%d%H%M'),'VL_B.csv',sep=''),row.names = F)
}else{
  print("Missing Global")
}
