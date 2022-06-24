###################################################################################################################
###################################################################################################################
########################################## 15. PREPARE THE UPLOAD FILE #############################################
###################################################################################################################
###################################################################################################################

#### format the output file for regular and make schedules
schedule_form<-rbind(full_sched_results %>% mutate(ScheduleType = 'FMV') %>% rename(CostPercent = limit_fmv)%>% select(-limit_flv),
                     full_sched_results %>% mutate(ScheduleType = 'FLV') %>% rename(CostPercent = limit_flv)%>% select(-limit_fmv)) %>%
  filter(ModelYear>=botyear & ModelYear<=topyear) %>%
  mutate(MarketCode = market, AppreciationPercentage='', DepreciationPercentage='') %>%
  rename(ClassificationID = ClassificationId)%>%
  select(MarketCode, ClassificationID,	ScheduleType,	ModelYear, CostPercent, AppreciationPercentage, DepreciationPercentage)

#### format the output file for depreciation and appreciation rate
dep<-spread(full_depr_results,ModelYear,limit_fmv) 
dep_trans<-format_depr(dep,'FMV')

app<-spread(full_depr_results,ModelYear,limit_fmv) 
app_trans<-format_depr(app,'FLV')  

excel_out <- rbind(schedule_form,dep_trans,app_trans) %>% arrange(MarketCode, ClassificationID,ScheduleType,	ModelYear)


###################################################################################################################
###################################################################################################################
################################################# 14. PLOTS #######################################################
###################################################################################################################
###################################################################################################################

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
#SchedFullList_plot = SchedFullList %>% filter(str_detect(Schedule,'Crane'))
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

  yaxis_name='SalePrice  / ReplacementCost'
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

