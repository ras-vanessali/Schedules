################# CSMM - model adjusters calculation #####################
################# CSMM level mapping - model adjuster = avg(sp/value) by Schedule, Make, Model #####################
# join the model level avg of saleprice to value to the list of subcategories
## Calculate the average sp/value and count units 
modellevel_summary<- ModelAdjData %>%
  group_by(SubcategoryId,SubcategoryName,MakeId,MakeName,ModelId,ModelName,SaleType) %>%
  summarise(n=n(), avgadj = mean(spValue), DateBack = min(as.Date(SaleDate)))

## Existing custom schedules in CSMM
existing<-merge(Model_customSched,ModelList%>% select(SubcategoryId), by='SubcategoryId')
## create a full list of schedules/makes/models that need model level schedules
## number of sales by sched/make/model >= 50 or we did its model schedules before
modellevel_list <- rbind(data.frame(modellevel_summary) %>% filter(n>=50) %>% select(SubcategoryId,MakeId,ModelId),
                         existing %>% select(SubcategoryId,MakeId,ModelId)) %>% distinct()     

## calculate the adjusters for the above list
## if one saletype satisfy >50 criteria above, do it on both side
join_model<- merge(modellevel_summary,modellevel_list,by=c('SubcategoryId','MakeId','ModelId')) %>%
  mutate(SFmodelCap = pmax(makeSFbot,pmin(avgadj,makeSFupp))) %>%
  # limit SF by number of data points
  mutate(SFmodelFinal = (SFmodelCap-1)*pmin(n,thredtpts)/thredtpts+1)%>%
  select(-avgadj,-SFmodelCap,-DateBack)

## transfer the data frame long to wide that different sale type in different columns
trans_ModelSFcalc <- spread(join_model %>% select(-n),key=SaleType,value=SFmodelFinal) %>%
  mutate(Retail = ifelse(is.na(Retail),1,Retail),
         Auction = ifelse(is.na(Auction),1,Auction))

trans_Model_counts <- spread(join_model %>% select(SubcategoryId,ModelId,SaleType,n),key=SaleType,value=n) %>%
  mutate(counts_ret = ifelse(is.na(Retail),0,Retail),
         counts_auc = ifelse(is.na(Auction),0,Auction)) %>%
  select(-Auction,-Retail)

join_factor_counts<-merge(trans_ModelSFcalc,trans_Model_counts,by=c('SubcategoryId','ModelId'))%>%
  #mutate(upd_ret = ifelse(counts_ret <thredtpts, sum_product(Retail,Auction,counts_ret,pmin(counts_auc,thredtpts)),Retail),
  #       upd_auc = ifelse(counts_auc <thredtpts, sum_product(Retail,Auction,pmin(counts_ret,thredtpts),counts_auc),Auction))%>%
  mutate(Auction_upd = ifelse(counts_auc < 5 | counts_auc/counts_ret < 0.05, 0.1*(Retail-1)+1,Auction),
         Retail_upd = ifelse(counts_ret < 5 | counts_ret/counts_auc < 0.05, 0.1*(Auction-1)+1, Retail))%>%
  select(-Auction, -Retail)%>%
  rename(Auction = Auction_upd, Retail = Retail_upd) %>%
  arrange(SubcategoryId,ModelId)



################################ Prepare a table of CSM schedules ##############################
######### From two parts: 1) CSM self schedules; 2)Schedule with make adjusters ################ 
selfsched_makeonly<-CapSchedule %>% filter(Level2 == 'Make')%>% 
  rename(fmv_make=Adjfmv, flv_make=Adjflv)%>%
  select(ClassificationId,ModelYear,fmv_make,flv_make)

regulSched_getmake<- merge(selfsched_makeonly,AllClass %>%select(ClassificationId ,SubcategoryId,MakeId), 
                           by ='ClassificationId') %>%
  arrange(ClassificationId,ModelYear)

### combine self schedules in CSM and schedules with make adjsuters 
MakeSched_CSMM<-rbind(CapMakeSchedule %>% select(SubcategoryId, MakeId,ModelYear,fmv_make,flv_make),
                      regulSched_getmake %>% select(SubcategoryId,MakeId,ModelYear,fmv_make,flv_make))



################################ Apply model adjusters to CSM schedules ##############################
joinModelSF<-merge(MakeSched_CSMM,join_factor_counts,by=c('SubcategoryId','MakeId')) %>% select(-SubcategoryName,-MakeName,-ModelName) %>%
  arrange(SubcategoryId,ModelId,ModelYear)
class_modelsched<-merge(joinModelSF,All_ModelClass,by=c('SubcategoryId', 'MakeId', 'ModelId'),all.x=T)%>%
  select(ClassificationId, CategoryId, SubcategoryId,MakeId, ModelId,
         CategoryName,SubcategoryName,MakeName,ModelName,ModelYear, everything())

ModelSF_phasein = merge(class_modelsched %>% filter(ModelYear == topyear) %>% select(ClassificationId,fmv_make,flv_make,Retail,Auction),
                        class_modelsched %>% filter(ModelYear == topyear - phaseinAge) %>% select(ClassificationId,fmv_make,flv_make),by='ClassificationId') %>%
  mutate(parent.delta.flv = flv_make.x - flv_make.y,
         parent.delta.fmv = fmv_make.x - fmv_make.y) %>%
  mutate(topy.adj.flv = ((Auction - 1) * brwsched_move + 1) * flv_make.x,
         topy.adj.fmv = ((Retail - 1) * brwsched_move + 1) * fmv_make.x,
         child.delta.flv = topy.adj.flv  - flv_make.y * Auction,
         child.delta.fmv = topy.adj.fmv  - fmv_make.y * Retail) %>%
  select(ClassificationId,topy.adj.flv,topy.adj.fmv,parent.delta.flv,parent.delta.fmv,child.delta.flv,child.delta.fmv)

  #mutate(fmv_model = fmv_make * Retail,
  #       flv_model = flv_make * Auction) %>%
  #select(SubcategoryId, MakeId, ModelId, ModelYear, fmv_model, flv_model) %>%
  #mutate( Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00)%>%
  #arrange(SubcategoryId, MakeId, ModelId, ModelYear)
## join the results back to the table with full years
joinModelSF.upd <- merge(class_modelsched,ModelSF_phasein,by='ClassificationId') %>% arrange(ClassificationId,desc(ModelYear))

## data validation
dim(joinMakeOut)[1]==dim(joinMakeSF)[1]/10
dim(joinMakeOut)[1]==dim(deltaMakeAdjcalc)[1]
dim(joinModelSF.upd)[1]==dim(joinModelSF)[1]

## interpolate the years between top year and x year
for (i in 1:dim(joinModelSF.upd)[1]){
  
  ### Auction
  ## deprecation rate by year
  joinModelSF.upd$diffRate.auc[i] = ifelse(joinModelSF.upd$ClassificationId[i] == joinModelSF.upd$ClassificationId[i-1] &
                                             joinModelSF.upd$ModelYear[i] == joinModelSF.upd$ModelYear[i-1]-1,
                                          round((joinModelSF.upd$flv_make[i-1] - joinModelSF.upd$flv_make[i])/joinModelSF.upd$parent.delta.flv[i],digits=4),0)
  
  ## adjusted schedule
  joinModelSF.upd$adjustrate.auc[i] = joinModelSF.upd$topy.adj.flv[i]
  joinModelSF.upd$adjustrate.auc[i] = ifelse(joinModelSF.upd$ModelYear[i] <= topyear -phaseinAge, joinModelSF.upd$flv_make[i]*joinModelSF.upd$Auction[i],
                                            ifelse(joinModelSF.upd$ModelYear[i] == topyear, joinModelSF.upd$topy.adj.flv[i],
                                                   joinModelSF.upd$adjustrate.auc[i-1]-joinModelSF.upd$child.delta.flv[i]*joinModelSF.upd$diffRate.auc[i]))
  ### Retail
  ## depreciation rate by year
  joinModelSF.upd$diffRate.ret[i] = ifelse(joinModelSF.upd$ClassificationId[i] == joinModelSF.upd$ClassificationId[i-1] &
                                            joinModelSF.upd$ModelYear[i] == joinModelSF.upd$ModelYear[i-1]-1,
                                          round((joinModelSF.upd$fmv_make[i-1] - joinModelSF.upd$fmv_make[i])/joinModelSF.upd$parent.delta.fmv[i],digits=4),0)
  
  ## adjusted schedule
  joinModelSF.upd$adjustrate.ret[i] = joinModelSF.upd$topy.adj.fmv[i]
  joinModelSF.upd$adjustrate.ret[i] = ifelse(joinModelSF.upd$ModelYear[i] <= topyear -phaseinAge, joinModelSF.upd$fmv_make[i]*joinModelSF.upd$Retail[i],
                                            ifelse(joinModelSF.upd$ModelYear[i] == topyear, joinModelSF.upd$topy.adj.fmv[i],
                                                   joinModelSF.upd$adjustrate.ret[i-1]-joinModelSF.upd$child.delta.fmv[i]*joinModelSF.upd$diffRate.ret[i]))
}

## manage the output table
AlloutModel <-joinModelSF.upd %>%
  select(ClassificationId,CategoryId,SubcategoryId, MakeId,MakeId, ModelId,ModelYear,adjustrate.auc,adjustrate.ret) %>%
  rename(finalflv_model=adjustrate.auc,finalfmv_model=adjustrate.ret) %>%
  arrange(ClassificationId,desc(ModelYear))



############ get the depreciation and appreciation rate and limit for the rebase #############
csm_depr_appr<-merge(DeprecMake,AllClass,by='ClassificationId') %>% select(CategoryId, SubcategoryId,MakeId,ModelYear,rate)
reg_depr_appr<-merge(combDeprApr,comb_Out,by='Schedule') %>% 
              filter(Level2=='Make') %>% 
              select(CategoryId, SubcategoryId,MakeId,ModelYear,rate)
makelevel_depr<-rbind(csm_depr_appr,reg_depr_appr)%>%
  filter(ModelYear =='Dep') %>% select(-ModelYear)

model_joindepr<-merge(AlloutModel,makelevel_depr,by=c('CategoryId','SubcategoryId', 'MakeId'))%>% 
  arrange(ClassificationId,ModelYear)

## modify the second last year if needed to prevent jump when rebase
depr_constr.model<- merge(model_joindepr %>% filter(ModelYear == botyear +1) %>% select(ClassificationId,finalflv_model,finalfmv_model,ModelYear),
                         model_joindepr %>% filter(ModelYear == botyear ) %>% select(ClassificationId,finalflv_model,finalfmv_model,ModelYear,rate),
                         by='ClassificationId') %>%
  mutate(finalfmv_model = pmin(pmax(finalfmv_model.y *(1 + rate) / (1+rebase_limit), finalfmv_model.x),finalfmv_model.y *(1 + rate) * (1+rebase_limit)),
         finalflv_model = pmin(pmax(finalflv_model.y *(1 + rate) / (1+rebase_limit), finalflv_model.x),finalflv_model.y *(1 + rate) *(1+rebase_limit))) %>%  
  select(ClassificationId,finalfmv_model, finalflv_model) 

## join back to schedule table and replace the second last year value
CapModelSchedule<-rbind(merge(depr_constr.model,model_joindepr %>% filter(ModelYear == botyear +1) %>% select(-finalfmv_model, -finalflv_model)
                             ,by='ClassificationId') %>%
                         select(CategoryId,SubcategoryId, MakeId, ClassificationId, ModelId, ModelYear, finalflv_model, finalfmv_model, rate),
                        model_joindepr %>% filter(ModelYear != botyear +1)) %>%
  arrange(ClassificationId ,desc(ModelYear))
################################ Last months schedules ##############################
LastMonth_modelSched<-lastM_schedule %>%
  filter(ModelYear>=botyear-1 & ModelYear <= topyear) %>%
  distinct()
MoMlimit_csmm<-merge(CapModelSchedule,LastMonth_modelSched,by=c('ClassificationId','ModelYear'),all.x=T)%>%
  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),finalfmv_model,ifelse(CategoryId==2616,MoMlimitFunc(CurrentFmv,finalfmv_model,limDw_MoM_spec,limDw_MoM_spec),MoMlimitFunc(CurrentFmv,finalfmv_model,limUp_MoM,limDw_MoM))),
  #mutate(limit_flv = ifelse(is.na(CurrentFlv),finalflv_model,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFlv,finalflv_model,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFlv,finalflv_model,limUp_MoM,limDw_MoM))))%>%
  #mutate(limit_fmv = ifelse(is.na(CurrentFmv),finalfmv_model,ifelse(CategoryId %in% c(2754,2753,2756,1952,2750,2236,2205,2752,2751,2755),MoMlimitFunc(CurrentFmv,finalfmv_model,limDw_MoM_spec,limDw_MoM),MoMlimitFunc(CurrentFmv,finalfmv_model,limUp_MoM,limDw_MoM))))%>%
  mutate(limit_fmv = ifelse(is.na(CurrentFmv),finalfmv_model,MoMlimitFunc(CurrentFmv,finalfmv_model,limUp_MoM,limDw_MoM)),
         limit_flv = ifelse(is.na(CurrentFlv),finalflv_model,MoMlimitFunc(CurrentFlv,finalflv_model,limUp_MoM,limDw_MoM))) %>%
  arrange(ClassificationId,ModelYear)

## Last month depr appr rate
model_depr_appr<-merge(AlloutModel%>%select(ClassificationId,CategoryId,SubcategoryId,MakeId,ModelId) %>% distinct(),
                       rbind(csm_depr_appr,reg_depr_appr),by=c('CategoryId','SubcategoryId', 'MakeId'))

MoM_deprapr_Model<-merge(model_depr_appr,LM_deprapr,by=c("ClassificationId",'ModelYear'),all.x=T) %>%
  mutate(limit_fmv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App',  MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit))),
         limit_flv = ifelse(is.na(LMvalue),rate,
                            ifelse(ModelYear=='App', MoMlimit_depappr(LMvalue,rate,ApprMoMLimit),MoMlimit_depappr(LMvalue,rate,DeprMoMLimit)))) %>%
  select(ClassificationId,ModelYear,limit_fmv,limit_flv) %>% 
  arrange(ClassificationId,ModelYear)

#   
# 
# plotusemodel<-merge(full_sched_results,MoMlimit_csmm %>%select(ClassificationId,SubcategoryId, ModelId,ModelYear,CurrentFmv,CurrentFlv),by=c('ClassificationId','ModelYear'))
# join_modelMoM<-merge(plotusemodel,All_ModelClass%>% select(ClassificationId,SubcategoryName,MakeName,ModelName),by='ClassificationId') %>%
#   mutate(csmm = paste(SubcategoryName,'|',MakeName,'|',ModelName),
#          Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00)%>%
#   arrange(ModelName,ModelYear)
# 
# joinModelSF<-joinModelSF%>%mutate(Age= year(publishDate)-ModelYear + (month(publishDate)-6)/12.00)
# 
# 
# SchedFullList_plot = join_modelMoM %>% select(ModelId,csmm) %>%distinct()
# #SchedFullList_plot = SchedFullList %>% filter(Schedule=='Containers Tanks Boxes RbA Trailers Grp USA')
# N_plot=dim(SchedFullList_plot)[1]
# for (j in 1:N_plot){
#   ###### Dots - Regression Data
#   Auc_RegDt <- Data_all_plot %>% filter(SaleType =='Auction' & ModelId==SchedFullList_plot[j,1])
#   Ret_RegDt <- Data_all_plot %>% filter(SaleType !='Auction' & ModelId==SchedFullList_plot[j,1])
#   
#   
#   ###### Lines - Final -this month
#   NewMonth_Sched<-subset(join_modelMoM,join_modelMoM$ModelId==SchedFullList_plot[j,1])
#   ###### Lines - Final -last month
#   PrevMonth_Sched<-subset(join_modelMoM,join_modelMoM$ModelId==SchedFullList_plot[j,1])
#   ## lines - make level sched
#   make_sched<-subset(joinModelSF,joinModelSF$ModelId==SchedFullList_plot[j,1])
#   
#   xaxis = c(-.5,10)
#   yaxis = c(0,2)
#   
#   yaxis_name='SP /AB Cost'
#   ##Auction Plots
#   # lines
#   draw_Sched_Auc<-xyplot(limit_flv ~ Age, NewMonth_Sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
#                          ,type=c('p','l','g'),col='dodgerblue3',lwd=3,pch=4,cex=1.5, lty=1
#                          ,ylab = yaxis_name, main=list(label=paste(SchedFullList_plot[j,2],' - ' ,publishDate),font=2,cex=2)
#                          ,panel = function(x,y,...){
#                            panel.abline(v = 0:14, h = seq(0,2,.5), col="lightgrey")
#                            panel.xyplot(x, y, ...)
#                          })
#   draw_lastM_Auc<-xyplot(CurrentFlv ~ Age, PrevMonth_Sched,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='dodgerblue3',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)
#   
#   draw_regLine_Auc<-xyplot(flv_make ~ Age, make_sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
#                            ,type=c("l","g"),col='dodgerblue3',lwd=1.5,lty=1,cex=1,
#                            ylab = yaxis_name,)
#   
#   #dots
#   
#   draw_RegDt_Auc<-xyplot(SaleAB ~ Age, Auc_RegDt,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="deepskyblue1")
#   
#   
#   ##Retail Plots
#   #lines
#   
#   draw_Sched_Ret<-xyplot(limit_fmv ~ Age, NewMonth_Sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
#                          ,type=c('p','l','g'),col='firebrick2',lwd=3,pch=4,cex=1.5,lty=1,
#                          ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,2],' - ' ,publishDate),font=2,cex=2))
#   draw_lastM_Ret<-xyplot(CurrentFmv ~ Age, PrevMonth_Sched,ylim=yaxis,xlim=xaxis,type=c('p','l'),lty=3,col='firebrick2',ylab = yaxis_name,lwd=3,pch=4,cex=1.5)
#   
#   draw_regLine_Ret<-xyplot(fmv_make ~ Age, make_sched,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
#                            ,type=c("l","g"),col='firebrick2',lwd=1.5,lty=1,cex=1,
#                            ylab = yaxis_name)
#   
#   #dots
#   draw_RegDt_Ret<-xyplot(SaleAB ~ Age, Ret_RegDt,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="lightpink")
#   
#   ### Draw retail and auction in one chart
#   if (CountryCode=='USA'){
#     draw<- draw_Sched_Ret  +as.layer(draw_lastM_Ret) + as.layer(draw_regLine_Ret) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) + as.layer(draw_Sched_Auc) + as.layer(draw_regLine_Auc)+ as.layer(draw_lastM_Auc)
#   } else{
#     draw<- draw_regLine_Ret + as.layer(draw_Sched_Ret)  +as.layer(draw_lastM_Ret) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) + as.layer(draw_depr_Ret) + as.layer(draw_depr_Auc) + as.layer(draw_ValidDt_Ret)+ as.layer(draw_regLine_Auc) + as.layer(draw_Sched_Auc) + as.layer(draw_lastM_Auc) + as.layer(draw_ValidDt_Auc) 
#   }
#   #draw<- draw_regLine_Ret +as.layer(draw_ValidDt_Ret)+ as.layer(draw_regLine_Auc) +as.layer(draw_ValidDt_Auc) + as.layer(draw_RegDt_Auc)+ as.layer(draw_RegDt_Ret) 
#   #draw<- listdt+ as.layer(draw_RegDt_Ret) +as.layer(draw_RegDt_List) 
#   
#   #### save
#   mypath<-file.path(file_path,plotFolder,paste(SchedFullList_plot[j,2] ,".png"))
#   png(file=mypath,width=1600,height=1200)
#   print(draw)
#   dev.off()
#   
#   
# }
# 
# 
# ########## backup tests
# trucktractor = test%>% filter(SubcategoryId==1964) %>%
#   filter(Flag == 'inUse' & YearFlag == 'AdjusUseYr') %>%
#   select(CompId,ModelYear, EffectiveDate, SaleType, SalePrice, CurrentABCost,SubcategoryId,MakeId,ModelId)
# 
# 
# data_to_model<-merge(trucktractor,join_modelMoM,by = c('SubcategoryId','ModelId','ModelYear'),all.x = T)
# data_to_make<-merge(data_to_model,MakeSched_CSMM,by = c('SubcategoryId','MakeId','ModelYear'),all.x = T) %>%
#   filter(!is.na(limit_flv))
# 
# join_sched_sales <- data_to_make%>%
#   mutate(new_sp_value = ifelse(SaleType == 'Auction', SalePrice/(limit_flv*CurrentABCost), SalePrice/(limit_fmv*CurrentABCost)),
#          old_sp_value = ifelse(SaleType == 'Auction', SalePrice/(flv_make*CurrentABCost), SalePrice/(fmv_make*CurrentABCost))) %>%
#   filter(new_sp_value>.4 &new_sp_value<1.6)
# 
# 
# write.csv(join_sched_sales,"0326tractors_backtest_6m.csv")