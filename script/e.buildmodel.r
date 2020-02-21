###################################################################################################################
###################################################################################################################
#################################  Regression Model - iteration by schedule  ######################################
###################################################################################################################
###################################################################################################################
### Data use for regression that split to auc and ret
SaleDtAuc<-subset(Data_all,Data_all$SaleType=="Auction") %>% 
  filter(ModelYear >= ifelse(CategoryId %in% ListingIds,dep_endyr, ext_botYr) & ModelYear <=topyear)
SaleDtRet<-data.frame(subset(Data_all,Data_all$SaleType!="Auction") %>% 
                        filter(ModelYear >= ifelse(CategoryId %in% ListingIds,dep_endyr, ext_botYr) & ModelYear <=topyear)) %>%
  mutate(SaleType ='Retail')

###################################################################################################################

###################################################################################################################
######################################## Regression Model on Retail only ##########################################
###################################################################################################################
# model year List
modelYr<- data.frame(ext_botYr:topyear)
colnames(modelYr) <-'ModelYear'
# number of modelyears in regression
nMY<-dim(modelYr)[1]


###### Age used in regression model
# nice rounding model years
topAge<-year(publishDate)-topyear + (month(publishDate)-6)/12

# month treated model years
CurrentAge<-seq(topAge,by=1,length.out = 12)

################# Prepare the list for regression looping #########################################################

# Number of groups
SchedAucBorw <-SchedR %>% filter(BorrowType=='RetailBorrowAuction')
retail_regression <- rbind(SchedAucBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Ret<-dim(retail_regression)[1]
######################### Declare the variables #########################################################

RetailOutput_R<-matrix(0,nSched_Ret,length(CurrentAge))
AgeName<-paste("Age",CurrentAge,sep="_")
alpha2<-rep(NA,nSched_Ret)
checklist_ret<-rep(NA, nSched_Ret)
coef_ret = matrix(0,nSched_Ret,3)
#################### 3.A loop to run regression by schedule and calcualte recoveries by month treated age

for (j in 1:nSched_Ret){
  
  groupData<-SaleDtRet %>% filter(Schedule==retail_regression[j,1])
  
  if(nrow(groupData)>0){
    
    ################## regression Model #########################
    check <- tryCatch(nls(SaleAB ~ L/(1+exp(K*(-Age+scal)))+S,start=list(L=1.2,K=-0.5,S=.3),control = nls.control(maxiter = 1000,minFactor=2^-24),data = groupData),error = function(e) e)
    checklist_ret[j]=any(class(check) == 'error')
    
    if(any(class(check) == 'error') == F){
      
      fit<-nls(SaleAB ~ L/(1+exp(K*(-Age+scal)))+S,start=list(L=1.2,K=-0.5,S=.3),control = nls.control(maxiter = 1000,minFactor=2^-24),data = groupData)
      alpha <- coef(fit) 
      coef_ret [j,]<- alpha
      
      ################## schedules #########################
      for (k in 1:length(CurrentAge)){
        RetailOutput_R[j,k]<-alpha[1]/(1+exp(min(max(slopecap_low,alpha[2]),slopecap_up)*(-CurrentAge[k]+scal)))+alpha[3]
        
      }
    }
    
    else {
      
      fit<-nls(SaleAB ~ L/(1+exp(K*(-Age+scal))),start=list(L=1.2,K=-0.5),control = nls.control(maxiter = 1000,minFactor=2^-24),data=groupData)
      alpha <- coef(fit) 
      coef_ret [j,]<- c(alpha,0)
      
      for (k in 1:length(CurrentAge)){
        
        RetailOutput_R[j,k] <- alpha[1]/(1+exp(min(max(slopecap_low,alpha[2]),slopecap_up)*(-CurrentAge[k]+scal)))
      }
      
    }
    alpha2[j]=alpha[2]
    rownames(RetailOutput_R)<-retail_regression[,1]
    colnames(RetailOutput_R)<-AgeName 
    rownames(coef_ret)<-retail_regression[,1]
  }
}

alpha2_ls = data.frame(retail_regression,alpha2)
export_ret_regression = data.frame(retail_regression,checklist_ret)

################################################ 3.B Manage the output format ##############################################

# trim the table - build first column
outputRetail_AucB<-rownames_to_column(as.data.frame(RetailOutput_R))
coef_ret_out<-rownames_to_column(as.data.frame(coef_ret))
#name the row and columns
colnames(outputRetail_AucB)<-c("Schedule",topyear:ext_botYr)
colnames(coef_ret_out)<-c("Schedule",'coef1','coef2','coef3')

# transfer table from wide to long
transOut_AucB<-gather(outputRetail_AucB,ModelYear,fmv,as.character(topyear):as.character(ext_botYr),factor_key = TRUE) %>%
  arrange(Schedule,ModelYear) %>%
  filter(!is.na(fmv))

transOut_AucB$ModelYear <- as.numeric(as.character(transOut_AucB$ModelYear))


print(if(nSched_Ret == dim(transOut_AucB)[1]/12){paste('Yes, the N is ', nSched_Ret)} else {'No'})

#write.csv(transOut_AucB,"transOut_AucB.csv")
###################################################################################################################
######################################## Regression Model on Auction only ##########################################
###################################################################################################################

################# Prepare the list for regression looping #########################################################

# Number of groups
SchedRetBorw <-SchedR %>% filter(BorrowType=='AuctionBorrowRetail')
auc_regression <- rbind(SchedRetBorw %>% select(Schedule),Sched %>% select(Schedule)) %>% distinct()
nSched_Auc<-dim(auc_regression)[1]

checklist_auc<-rep(NA, nSched_Auc)
alpha2.auc<-rep(NA,nSched_Auc)
coef_auc = matrix(0,nSched_Auc,3)
######################### Declare the variables #########################################################

AuctionOutput_A<-matrix(0,nSched_Auc,length(CurrentAge))
#################### 3.A loop to run regression by schedule and calcualte recoveries by month treated age

for (j in 1:nSched_Auc){
  
  groupData<-SaleDtAuc %>% filter(Schedule==auc_regression[j,1])
  
  if(nrow(groupData)>0){
    
    ################## regression Model #########################
    check <- tryCatch(nls(SaleAB ~ L/(1+exp(K*(-Age+scal)))+S,start=list(L=0.6,K=-0.5,S=0.3),control = nls.control(maxiter = 1000,minFactor=2^-24),data = groupData),error = function(e) e)
    checklist_auc [j] = any(class(check) == 'error')
    
    if(any(class(check) == 'error') == F){
      
      #fit<-nls(SaleAB ~ L/(1+exp(K*(-Age+scal)))+S,start=list(L=1,K=-0.1,S=1),data = groupData)
      fit<-nls(SaleAB ~ L/(1+exp(K*(-Age+scal)))+S,start=list(L=0.6,K=-0.5,S=0.3),control = nls.control(maxiter = 1000,minFactor=2^-24),data = groupData)
      alpha <- coef(fit) 
      coef_auc [j,]<- alpha
      ################## schedules #########################
      for (k in 1:length(CurrentAge)){
        
        AuctionOutput_A[j,k]<-alpha[1]/(1+exp(min(max(slopecap_low,alpha[2]),slopecap_up)*(-CurrentAge[k]+scal)))+alpha[3]
        
        #rownames(RetailOutput_R)<-retail_regression[,1]
        #colnames(RetailOutput_R)<-AgeName
      }
    }
    else {
      
      fit<-nls(SaleAB ~ L/(1+exp(K*(-Age+scal))),start=list(L=0.6,K=-0.2),control = nls.control(maxiter = 1000,minFactor=2^-24),data=groupData)
      alpha <- coef(fit) 
      coef_auc [j,]<- c(alpha,0)
      for (k in 1:length(CurrentAge)){
        
        AuctionOutput_A[j,k]<-alpha[1]/(1+exp(min(max(slopecap_low,alpha[2]),slopecap_up)*(-CurrentAge[k]+scal)))
        
      }
      
    }
      alpha2.auc[j]=alpha[2]
      rownames(AuctionOutput_A)<-auc_regression[,1]
      colnames(AuctionOutput_A)<-AgeName 
      rownames(coef_auc)<-auc_regression[,1]
  }
}
alpha2_ls.auc = data.frame(auc_regression,alpha2.auc)
export_auction_regression = data.frame(auc_regression,checklist_auc)

################################################ 3.B Manage the output format ##############################################

# trim the table - build first column
outputAuction_RetB<-rownames_to_column(as.data.frame(AuctionOutput_A))
coef_auc_out<-rownames_to_column(as.data.frame(coef_auc))
#name the row and columns
colnames(outputAuction_RetB)<-c("Schedule",topyear:ext_botYr)
colnames(coef_auc_out)<-c("Schedule",'coef1','coef2','coef3')

# transfer table from wide to long
transOut_RetB<-gather(outputAuction_RetB,ModelYear,flv,as.character(topyear):as.character(ext_botYr),factor_key = TRUE) %>%
  arrange(Schedule,ModelYear) %>%
  filter(!is.na(flv))
transOut_RetB$ModelYear <- as.numeric(as.character(transOut_RetB$ModelYear))

print(if(nSched_Auc == dim(transOut_RetB)[1]/12){paste('Yes, the N is ', nSched_Auc)} else {'No'})







###################################################################################################################
###################################################################################################################
################################### Exponential Function Slope ####################################################
###################################################################################################################
###################################################################################################################

####################################### Auction ############################################
ExpSlopeAuc=matrix(0,nSched_Auc)

for (j in 1:nSched_Auc){
  groupData<-SaleDtAuc %>% filter(Schedule==auc_regression[j,1])
  
  if(nrow(groupData)>0){
    
    fit<-lm(log(SaleAB)~Age,data=groupData)
    ExpSlopeAuc[j]<-exp(min(fit$coefficients[2],slopecap_up))
  }
}

rownames(ExpSlopeAuc)<-c(as.vector(auc_regression[,1]))
SlopetransAuc<-rownames_to_column(as.data.frame(ExpSlopeAuc))
colnames(SlopetransAuc)<-c('Schedule','SlopeAuc') 


################################## Joint the logistic (age 0-7) and exponential (age 8,9) ##############################
## indicate age from the joint year
depr_curve <- 1:4
## calculate the schedules by using the exponential slope
expon_sched_auc<- merge(merge(transOut_RetB %>% filter(ModelYear == topyear - age_joint), SlopetransAuc,by='Schedule'),depr_curve) %>%
  mutate(Year = topyear - y - age_joint) %>%
  mutate(depr_regres = flv * SlopeAuc^y) %>%
  select(Schedule,Year,depr_regres) %>%
  rename(ModelYear = Year, flv = depr_regres)

### wrap up the regression output 
# auction
OutRegression_Auc<-rbind(transOut_RetB %>% filter(ModelYear >= topyear - age_joint),expon_sched_auc) %>%
  arrange(Schedule,ModelYear)








####################################### Retail ############################################

ExpSlopeRet=matrix(0,nSched_Ret)

for (j in 1:nSched_Ret){
  
  groupData<-SaleDtRet %>% filter(Schedule==retail_regression[j,1])
  
  if(nrow(groupData)>0){
    
    fit<-lm(log(SaleAB)~Age,data=groupData)
    ExpSlopeRet[j]<-exp(min(fit$coefficients[2],slopecap_up))
  }
}

rownames(ExpSlopeRet)<-c(as.vector(retail_regression[,1]))
SlopetransRet<-rownames_to_column(as.data.frame(ExpSlopeRet))
colnames(SlopetransRet)<-c('Schedule','SlopeRet') 


################################## Joint the logistic (age 0-7) and exponential (age 8,9) ##############################
## calculate the schedules by using the exponential slope
expon_sched_ret<- merge(merge(transOut_AucB %>% filter(ModelYear == topyear - age_joint), SlopetransRet,by='Schedule'),depr_curve) %>%
  mutate(Year = topyear - y - age_joint) %>%
  mutate(depr_regres = fmv * SlopeRet^y) %>%
  select(Schedule,Year,depr_regres) %>%
  rename(ModelYear = Year, fmv = depr_regres)

### wrap up the regression output 
# auction
OutRegression_Ret<-rbind(transOut_AucB %>% filter(ModelYear >= topyear - age_joint),expon_sched_ret) %>%
  arrange(Schedule,ModelYear)




#print(if(sum(nCat,nSched_Ret) == dim(OutRegression_Ret)[1]/12){paste('Yes, the N_retail is ', sum(nCat,nSched_Ret))} else {'No'})
#print(if(sum(nCat,nSched_Auc) == dim(OutRegression_Auc)[1]/12){paste('Yes, the N_auction is ', sum(nCat,nSched_Auc))} else {'No'})


"
# map to auction regular schedules to borrow the values
inheritmap<-SchedR %>% select(Schedule,AuctionBorrow)

# join the map to the auction values
inheritAuc<-merge(AucSched_pre %>% rename(AuctionBorrow=Schedule),inheritmap,by=c('AuctionBorrow')) %>% 
  select(Schedule,M1AppraisalBookPublishDate,ModelYear,flv) %>%
  arrange(Schedule,M1AppraisalBookPublishDate,ModelYear)


AucSched_all<-rbind(AucSched_pre,inheritAuc) %>% arrange(Schedule,ModelYear)

"
