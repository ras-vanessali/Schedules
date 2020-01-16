CatlevelListing<-rbind(In %>% filter(str_detect(Schedule,'Crane')),In.brwcrane %>% select(-BorrowSchedule,-BorrowType)) %>% select (Schedule,Country,CategoryId) %>% distinct()


### Join in Category level 
retailCrane <- merge(uploadData,CatlevelListing, by=c("CategoryId","Country")) %>%
  filter(CategoryId %in% ListingIds & SaleType == 'Retail' & Age >=2 & Age<=20) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear ,SaleType,SaleAB, Age,Schedule,Flag,SaleDate,EffectiveDate)


### Listing - Join in Category level 
listingCrane <- merge(uploadListing,CatlevelListing, by=c("CategoryId","Country")) %>%
  filter(CategoryId %in% ListingIds & Age >=2 & Age<=20) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear,SaleType,SaleAB, Age,Schedule,Flag,DateScrapedMostRecent,EffectiveDate) %>%
  rename(SaleDate =DateScrapedMostRecent)


Datainput_pre<-rbind(retailCrane,listingCrane) %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse')

ListingSched<-Datainput_pre %>% filter(SaleType=='Listing') %>% distinct(Schedule) 
Nlisting<-dim(ListingSched)[1]


coef3<-rep(0,Nlisting)


# coef1<-rep(0,Nlisting)
# coef2<-rep(0,Nlisting)
# 
# tb1 = data.frame(seq(-1,20,.1),rep('Listing'))
# colnames(tb1)<-c('x','SaleType')
# 
# tb2 = data.frame(seq(-1,20,.1),rep('Retail'))
# colnames(tb2)<-c('x','SaleType')
# 
# ageR<-rbind(tb1,tb2)



for (i in 1:Nlisting) {
  reduc_Sche_data<- Datainput_pre %>% filter(Schedule ==ListingSched[i,1])
  fit = lm(log(SaleAB) ~ Age + SaleType ,data = reduc_Sche_data)
  coef3[i]<-fit$coefficients[3]

  # coef1[i]<-fit$coefficients[1]
  # coef2[i]<-fit$coefficients[2]
  # coef3[i]<-fit$coefficients[3]
  # 
  # curve<- ageR %>% 
  #   mutate(y = ifelse(SaleType=='Retail', exp(coef1[i]+coef2[i]*x), exp(coef1[i]+coef2[i]*x +coef3[i])))
  # 
  # 
  # plt<-ggplot(reduc_Sche_data,aes(x=Age,y=SaleAB, color = factor(SaleType))) +
  #   geom_point()+
  #   geom_line(data = curve, aes(x,y,color=factor(SaleType)),linetype=1) + 
  #   ggtitle(paste(ListingSched[i,1])) +
  #   scale_color_manual(values = c( 'red2','springgreen3'))
  # 
  # mypath<-file.path(file_path,plotFolder,paste(ListingSched[i,1],'.png'))
  # png(file=mypath,width=1000,height=600)
  # print(plt)
  # dev.off()


  
}



list_reductfact<-data.frame(ListingSched,coef3) %>%
  mutate(reduction = pmax(pmin(coef3+1,1.2),1)) 

write.csv(list_reductfact,paste(publishDate,"ListingReduction.csv"),row.names = F)

revised_listing<-merge(merge(uploadListing,CatlevelListing, by=c("CategoryId","Country")),list_reductfact,by='Schedule',all.x=T) %>%
  mutate(SalePrice = ListPrice/reduction,
         SaleAB = SalePrice/CurrentABCost,
         SPvalue = SalePrice/M1Value) %>%
  rename(SaleDate =DateScrapedMostRecent)


     