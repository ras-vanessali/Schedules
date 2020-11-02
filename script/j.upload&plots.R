###################################################################################################################
###################################################################################################################
########################################## 15. PREPARE THE UPLOAD FILE #############################################
###################################################################################################################
###################################################################################################################
if (CountryCode =='USA'){
######################################################## Cat/Subcat Schedules #########################################################
#### 13.A concatenate the columns
FinalSchedules$concat_fmv<-paste(FinalSchedules$ClassificationId,"|","FMV","|",FinalSchedules$ModelYear,"|",FinalSchedules$limit_fmv,sep="")
FinalSchedules$concat_flv<-paste(FinalSchedules$ClassificationId,"|","FLV","|",FinalSchedules$ModelYear,"|",FinalSchedules$limit_flv,sep="")

concatfmv<-FinalSchedules %>%
  select(concat_fmv)
colnames(concatfmv)<-"ClassificationID|Type|Year|Schedule Value"

concatflv<-FinalSchedules %>%
  select(concat_flv)
colnames(concatflv)<-"ClassificationID|Type|Year|Schedule Value"

a<-FinalSchedules %>%
  group_by(ClassificationId) %>%
  summarise(n=n()) %>%
  filter(n %in% c(10))

DimRegular<-dim(FinalSchedules %>% group_by(ClassificationId) %>% summarise(n=n()))



######################################################## Make Schedules #########################################################
#### 13.B concatenate the columns
Final_makeSched$concat_fmv<-paste(Final_makeSched$ClassificationId,"|","FMV","|",Final_makeSched$ModelYear,"|",Final_makeSched$limit_fmv,sep="")
Final_makeSched$concat_flv<-paste(Final_makeSched$ClassificationId,"|","FLV","|",Final_makeSched$ModelYear,"|",Final_makeSched$limit_flv,sep="")


concatfmv_Make<-Final_makeSched %>%
  select(concat_fmv)
colnames(concatfmv_Make)<-"ClassificationID|Type|Year|Schedule Value"

concatflv_Make<-Final_makeSched %>%
  select(concat_flv)
colnames(concatflv_Make)<-"ClassificationID|Type|Year|Schedule Value"

DimMake<-dim(Final_makeSched %>% group_by(ClassificationId) %>% summarise(n=n()))

##################################################### Finalize upload file ##########################################################
uploadTb<-rbind(concatfmv,concatfmv_Make,concatflv,concatflv_Make) 

uploadTb2 <- as.data.frame(sapply(uploadTb,gsub,pattern = "V|App",replacement = "VApp",fixed=T))
uploadTb3 <- as.data.frame(sapply(uploadTb2,gsub,pattern = "V|Dep",replacement = "VDep",fixed=T))


#### 13.D Write the upload file in the folder
write.table(uploadTb3,paste(Sys.Date(),CountryCode,'ScheduleImport_VL.txt'),row.names=FALSE,quote=FALSE)
write.csv(uploadTb3,paste(Sys.Date(),CountryCode,'ScheduleImport_VL.csv'),row.names=FALSE,quote=FALSE)
} else{
  

schedule_form<-rbind(rbind(Final_makeSched %>% select(ClassificationId,ModelYear,limit_fmv),MoMSchedules %>% select(ClassificationId,ModelYear,limit_fmv)) %>%
                       mutate(ScheduleType = 'FMV') %>% rename(CostPercent = limit_fmv,ClassificationID = ClassificationId),
                     rbind(Final_makeSched %>% select(ClassificationId,ModelYear,limit_flv),MoMSchedules %>% select(ClassificationId,ModelYear,limit_flv)) %>%
                       mutate(ScheduleType = 'FLV') %>% rename(CostPercent = limit_flv,ClassificationID = ClassificationId)) %>%
  filter(ModelYear>=botyear & ModelYear<=topyear) %>%
  mutate(MarketCode = 'GBUK', AppreciationPercentage='', DepreciationPercentage='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)

depapp.make<-Final_makeSched %>% select(-limit_flv) %>% filter(ModelYear %in% c('App','Dep')) %>% rename(rate = limit_fmv)

dep<-spread(rbind(applydep %>% select(ClassificationId,ModelYear,rate),depapp.make),ModelYear,rate) %>%
  rename(AppreciationPercentage = App, DepreciationPercentage = Dep, ClassificationID = ClassificationId) %>%
  mutate(MarketCode = 'GBUK', ScheduleType ='FMV', ModelYear ='', CostPercent='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)

app<-spread(rbind(applydep %>% select(ClassificationId,ModelYear,rate),depapp.make),ModelYear,rate) %>%
  rename(AppreciationPercentage = App, DepreciationPercentage = Dep, ClassificationID = ClassificationId) %>%
  mutate(MarketCode = 'GBUK', ScheduleType ='FLV', ModelYear ='', CostPercent='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)


### global

global_out<- rbind(MoMSched.global %>% select(ClassificationId,ModelYear,limit_fmv ) %>%
                     mutate(ScheduleType = 'FMV') %>% rename(CostPercent = limit_fmv ,ClassificationID = ClassificationId),
                   MoMSched.global %>% select(ClassificationId,ModelYear,limit_flv ) %>%
                     mutate(ScheduleType = 'FLV') %>% rename(CostPercent = limit_flv ,ClassificationID = ClassificationId)) %>%
  filter(ModelYear>=botyear & ModelYear<=topyear) %>%
  mutate(MarketCode = 'GBUK', AppreciationPercentage='', DepreciationPercentage='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)



dep_global<-spread(MoMSched.global %>% select(ClassificationId,ModelYear,limit_fmv ),ModelYear,limit_fmv ) %>%
  rename(AppreciationPercentage = App, DepreciationPercentage = Dep, ClassificationID = ClassificationId) %>%
  mutate(MarketCode = 'GBUK', ScheduleType ='FMV', ModelYear ='', CostPercent='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)


app_global<-spread(MoMSched.global %>% select(ClassificationId,ModelYear,limit_flv ),ModelYear,limit_flv) %>%
  rename(AppreciationPercentage = App, DepreciationPercentage = Dep, ClassificationID = ClassificationId) %>%
  mutate(MarketCode = 'GBUK', ScheduleType ='FLV', ModelYear ='', CostPercent='') %>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)

excel_out <- rbind(global_out,dep_global,app_global,schedule_form,dep,app) %>% arrange(MarketCode, ClassificationID,ScheduleType,	ModelYear)


write.csv(excel_out,paste('ClassificationModifySchedule',format(Sys.time(),format='%Y%m%d%H%M'),'VL.csv',sep=''),row.names = F)

  
}


###################################################################################################################
###################################################################################################################
################################################# 14. PLOTS #######################################################
###################################################################################################################
###################################################################################################################

# split sale data to auction and retail -- Test Data
#SaleData_all=subset(Datainput,as.Date(Datainput$EffectiveDate)>publishDate)
#SaleDtAuc_all<-subset(SaleData_all,SaleData_all$SaleType=="Auction" & YearInuse=='Outputyr')
#SaleDtRet_all<-subset(SaleData_all,SaleData_all$SaleType=="Retail" & YearInuse=='Outputyr')
####### Test Data #######
#Auc_testUse_plot<-SaleDtAuc_all %>%
#  select(Schedule,Age,ModelYear,SaleAB)

#Ret_testUse_plot<-SaleDtRet_all %>%
#  select(Schedule,Age,ModelYear,SaleAB)

#################################### 14.A Cat/Subcat Schedules #############################################

###### Validation Data #######
Auc_ValidDt_plot<-Data_all_plot %>%
  filter(SaleType =='Auction' & as.Date(EffectiveDate)>=thirdLastM & ModelYear>=dep_endyr) %>%
  select(SaleType,Schedule,Age,ModelYear,SaleAB)

Ret_ValidDt_plot<-Data_all_plot %>%
  filter(SaleType !='Auction' & as.Date(EffectiveDate)>=thirdLastM & ModelYear>=dep_endyr) %>%
  select(SaleType,Schedule,Age,ModelYear,SaleAB)


###### regression line this month #####
Regression_ret<-OutRegression_Ret %>%
  select(Schedule,ModelYear,fmv) %>%
  mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00) 

Regression_auc<-OutRegression_Auc %>%
  select(Schedule,ModelYear,flv) %>%
  mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00) 

###### Schedule this month #####
New_Sched<-MoMSchedules %>%
  filter(Plot=='Y') %>%
  select(Schedule,ClassificationId,ModelYear,Adjfmv,Adjflv,limit_fmv,limit_flv) %>%
  mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00)


###### Schedule last month #####
Last_Sched <- MoMSchedules %>%
  filter(Plot=='Y') %>%
  select(Schedule,ClassificationId,ModelYear, CurrentFmv,CurrentFlv) %>%
  mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00-1/12.00)

##### depreciation lines
Deprline<-deprCurve %>% mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00)


#SchedFullList_plot = SchedFullList %>% filter(Schedule %in% c('Telehandlers Small JLG USA'))
SchedFullList_plot = SchedFullList %>% filter(!str_detect(Schedule,'SubcatGrp'))
#SchedFullList_plot = SchedFullList %>% filter(Schedule=='Containers Tanks Boxes RbA Trailers Grp USA')
N_plot=dim(SchedFullList_plot)[1]
for (j in 1:N_plot){

  ###### Dots - Regression Data
  Auc_RegDt <- Data_all_plot %>% filter(SaleType =='Auction' & Schedule==SchedFullList_plot[j,1])
  Ret_RegDt <- Data_all_plot %>% filter(SaleType !='Auction' & Schedule==SchedFullList_plot[j,1])
  
  if (CountryCode=='USA'){
    listing_RegDt<-subset(Data_all_plot,Data_all_plot$SaleType=='Listing' & Data_all_plot$Schedule==SchedFullList_plot[j,1])
  }
  
  ###### Dots - Adjsuted Data
  Auc_ValidDt<-subset(Auc_ValidDt_plot,Auc_ValidDt_plot$Schedule==SchedFullList_plot[j,1])
  Ret_ValidDt<-subset(Ret_ValidDt_plot,Ret_ValidDt_plot$SaleType=='Retail' & Ret_ValidDt_plot$Schedule==SchedFullList_plot[j,1])
  
  
  ###### Lines - Final -this month
  NewMonth_Sched<-subset(New_Sched,New_Sched$Schedule==SchedFullList_plot[j,1])
  ###### Lines - Final -last month
  PrevMonth_Sched<-subset(Last_Sched,Last_Sched$Schedule==SchedFullList_plot[j,1])
 
  ###### Lines - Regression Data
  AucRegression<-subset(Regression_auc,Regression_auc$Schedule==SchedFullList_plot[j,1])
  RetRegression<-subset(Regression_ret,Regression_ret$Schedule==SchedFullList_plot[j,1])
  
  
  ###### Lines - depreciation
  DrawDepreciation<- Deprline %>% filter(Schedule==SchedFullList_plot[j,1])
  
  xaxis = c(-.5,15)
  yaxis = c(0,2)

  yaxis_name='SP /AB Cost'
  ##Auction Plots
  # lines
  draw_Sched_Auc<-xyplot(limit_flv ~ Age, NewMonth_Sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                         ,type=c('p','l','g'),col='dodgerblue3',lwd=3,pch=4,cex=1.5, lty=1
                         ,ylab = yaxis_name, main=list(label=paste(SchedFullList_plot[j,1],' - ' ,publishDate),font=2,cex=2)
                         ,panel = function(x,y,...){
                          panel.abline(v = 0:14, h = seq(0,2,.5), col="lightgrey")
                           panel.xyplot(x, y, ...)
                          })
  draw_lastM_Auc<-xyplot(CurrentFlv ~ Age, PrevMonth_Sched,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='dodgerblue3',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)

  draw_regLine_Auc<-xyplot(flv ~ Age, AucRegression,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                           ,type=c("l","g"),col='dodgerblue3',lwd=1.5,lty=1,cex=1,
                           ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,1],' - ',publishDate),font=2,cex=2))
  
  draw_depr_Auc<-xyplot(depflv ~ Age, DrawDepreciation,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,type=c('p','l','g'),col='dodgerblue3',lwd=3,pch=4,cex=1.5, lty=1)
  
  
  #dots
  
  draw_RegDt_Auc<-xyplot(SaleAB ~ Age, Auc_RegDt,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="deepskyblue1")
  draw_ValidDt_Auc<-xyplot(SaleAB ~ Age , Auc_ValidDt, pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="dodgerblue3")
  
  
  
  ##Retail Plots
  #lines
  
  draw_Sched_Ret<-xyplot(limit_fmv ~ Age, NewMonth_Sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                         ,type=c('p','l','g'),col='firebrick2',lwd=3,pch=4,cex=1.5,lty=1,
                         ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,1],' - ' ,publishDate),font=2,cex=2))
  draw_lastM_Ret<-xyplot(CurrentFmv ~ Age, PrevMonth_Sched,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='firebrick2',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)

  draw_regLine_Ret<-xyplot(fmv ~ Age, RetRegression,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                           ,type=c("l","g"),col='firebrick2',lwd=1.5,lty=1,cex=1,
                           ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,1],' - ',publishDate),font=2,cex=2))
  
  draw_depr_Ret<-xyplot(depfmv ~ Age, DrawDepreciation,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,type=c('p','l','g'),col='firebrick2',lwd=3,pch=4,cex=1.5, lty=1)
  
  #dots
  draw_RegDt_Ret<-xyplot(SaleAB ~ Age, Ret_RegDt,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="lightpink")
  draw_ValidDt_Ret<-xyplot(SaleAB ~ Age , Ret_ValidDt, pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col='firebrick2')
  
  if (CountryCode=='USA'){
    draw_RegDt_List<-xyplot(SaleAB ~ Age , listing_RegDt, pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col='darkgreen')
   # listdt<-xyplot(SaleAB ~ Age, list, pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col='lightgreen',main=list(label=paste(SchedFullList_plot[j,1],' - ',publishDate),font=2,cex=2))
  }
  ### Draw retail and auction in one chart
  if (CountryCode=='USA'){
    draw<- draw_regLine_Ret + as.layer(draw_Sched_Ret)  +as.layer(draw_lastM_Ret) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) + as.layer(draw_depr_Ret) + as.layer(draw_depr_Auc) + as.layer(draw_ValidDt_Ret)+ as.layer(draw_regLine_Auc) + as.layer(draw_Sched_Auc) + as.layer(draw_lastM_Auc) + as.layer(draw_ValidDt_Auc) + as.layer(draw_RegDt_List) 
  } else{
    draw<- draw_regLine_Ret + as.layer(draw_Sched_Ret)  +as.layer(draw_lastM_Ret) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) + as.layer(draw_depr_Ret) + as.layer(draw_depr_Auc) + as.layer(draw_ValidDt_Ret)+ as.layer(draw_regLine_Auc) + as.layer(draw_Sched_Auc) + as.layer(draw_lastM_Auc) + as.layer(draw_ValidDt_Auc) 
  }
    #draw<- draw_regLine_Ret +as.layer(draw_ValidDt_Ret)+ as.layer(draw_regLine_Auc) +as.layer(draw_ValidDt_Auc) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) 
  #draw<- listdt+ as.layer(draw_RegDt_Ret) +as.layer(draw_RegDt_List) 
  
  #### save
  mypath<-file.path(file_path,plotFolder,paste(SchedFullList_plot[j,1] ,".png"))
  png(file=mypath,width=1600,height=1200)
  print(draw)
  dev.off()
  
  
}

