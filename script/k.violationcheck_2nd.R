if(CountryCode == "USA"){
  comb_make_model<-rbind(MoMSched.global%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),
                         MoMSchedules%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),
                         MoMlimit_csm%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),
                         MoMlimit_csmm%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv))
} else{
  comb_make_model<-rbind(MoMSched.global%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),
                         MoMSchedules%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv),
                         MoMlimit_csm%>% select(ClassificationId,ModelYear,limit_fmv,limit_flv))
}




################## Channel Retail vs Auction check
channelviolation<-comb_make_model %>%
  arrange(ClassificationId,ModelYear)%>% 
  mutate(rate = limit_fmv/limit_flv)%>%
  filter (rate < 1) %>%
  mutate(schedule = ifelse(ModelYear<=topyear-chanyr*2,limit_flv*(1+capChannel/4),limit_fmv/(1+capChannel/4))) %>%
  select(ClassificationId,ModelYear,schedule)


#### fix the values
fix_channelcheck<-merge(comb_make_model,channelviolation,by=c("ClassificationId","ModelYear"),all.x =T)%>%
  mutate(limit_fmv = ifelse(!is.na(schedule) & ModelYear<=topyear-chanyr*2,schedule,limit_fmv),
         limit_flv = ifelse(!is.na(schedule) & ModelYear>topyear-chanyr*2,schedule,limit_flv)) %>%
  arrange(ClassificationId,desc(ModelYear))

########### return the violated ones
fix_channelcheck %>%
  mutate(rate = limit_fmv/limit_flv) %>%
  filter (rate < 1) 


### check the violation between years by rule of .02
fix_Yearcheck = fix_channelcheck
for (i in 1:dim(fix_Yearcheck)[1]){
  
  fix_Yearcheck$limit_fmv_temp[i] = 
    if (i == 1) {fix_Yearcheck$limit_fmv[i]}
  else{
    if(fix_Yearcheck$ClassificationId[i] == fix_Yearcheck$ClassificationId[i-1] & fix_Yearcheck$ModelYear[i] == fix_Yearcheck$ModelYear[i-1]-1 
       & fix_Yearcheck$limit_fmv[i-1] / fix_Yearcheck$limit_fmv[i] <1.02){
      fix_Yearcheck$limit_fmv[i-1] /1.02
    }
    else{
      fix_Yearcheck$limit_fmv[i]
    }
  }
  fix_Yearcheck$limit_flv_temp[i] = 
    
    if(i == 1){fix_Yearcheck$limit_flv[i]}
  else{
    if(fix_Yearcheck$ClassificationId[i] == fix_Yearcheck$ClassificationId[i-1] & fix_Yearcheck$ModelYear[i] == fix_Yearcheck$ModelYear[i-1]-1 
       & fix_Yearcheck$limit_flv[i-1] / fix_Yearcheck$limit_flv[i] <1.02){
      fix_Yearcheck$limit_flv[i-1] /1.02
    }
    else{
      fix_Yearcheck$limit_flv[i]
    }
  } 
}


fix_Yearcheck = fix_Yearcheck %>% select(-limit_fmv ,-limit_flv) %>% rename(limit_fmv=limit_fmv_temp, limit_flv=limit_flv_temp)


for (i in 1:nrow(fix_Yearcheck)){
  fix_Yearcheck$retFlag[i] = ifelse(fix_Yearcheck$ClassificationId[i]==fix_Yearcheck$ClassificationId[i+1],
                                   ifelse(round(as.numeric(fix_Yearcheck$limit_fmv[i])/as.numeric(fix_Yearcheck$limit_fmv[i+1]),digits=4)<1,"flag",""),"")
  fix_Yearcheck$aucFlag[i]= ifelse(fix_Yearcheck$ClassificationId[i]==fix_Yearcheck$ClassificationId[i+1],
                                  ifelse(round(fix_Yearcheck$limit_flv[i]/fix_Yearcheck$limit_flv[i+1],digits=4)<1,"flag",""),"")
}

### flagged classifications on year
fix_Yearcheck %>% filter(retFlag=='flag' |aucFlag=='flag') 


## manage the table to the export format
full_sched_results<-fix_Yearcheck%>% select(ClassificationId,ModelYear,limit_fmv, limit_flv)

####### final depr and appr combination
if(CountryCode == "USA"){
  full_depr_results<-rbind(MoM_deprapr_global,MoM_deprapr,MoM_deprapr_Make,MoM_deprapr_Model) %>%
    select(-limit_flv)%>%
    arrange(ClassificationId,desc(ModelYear))
} else{
  full_depr_results<-rbind(MoM_deprapr_global,MoM_deprapr,MoM_deprapr_Make) %>%
    select(-limit_flv)%>%
    arrange(ClassificationId,desc(ModelYear))
}

