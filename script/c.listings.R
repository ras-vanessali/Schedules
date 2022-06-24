# In table from input file for cranes
CatlevelListing<-rbind(In %>% filter(str_detect(Schedule,'Crane')),In.brwcrane %>% select(-BorrowSchedule,-BorrowType)) %>% select (Schedule,Country,CategoryId) %>% distinct()

### retail - Join in Category level 
retailCrane <- merge(uploadData,CatlevelListing, by=c("CategoryId","Country")) %>%
  filter(CategoryId %in% ListingIds & SaleType == 'Retail' & Age >=3 & Age<=20) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear ,SaleType,SaleAB, 
         Age,Schedule,Flag,SaleDate,AcquisitionDate,EffectiveDate,MilesHours,MilesHoursCode)


### Listing - Join in Category level 
listingCrane <- merge(uploadListing,CatlevelListing, by=c("CategoryId","Country")) %>%
  filter(CategoryId %in% ListingIds & Age >=3 & Age<=20) %>%
  filter(SaleAB>0)%>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear,SaleType,SaleAB, 
         Age,Schedule,Flag,DateScrapedMostRecent,AcquisitionDate,EffectiveDate,MilesHours,MilesHoursCode) %>%
  rename(SaleDate =DateScrapedMostRecent)

## combine retail and listing 
Datainput_pre<-rbind(retailCrane,listingCrane) %>%
  filter(as.Date(EffectiveDate)<=publishDate & Flag =='inUse')

## listing of crane schedules
ListingSched<-rbind(In %>% select(Schedule),InR %>% select(Schedule)) %>% filter(str_detect(Schedule,'Crane')) %>% distinct(Schedule)
Nlisting<-dim(ListingSched)[1]

## create variables
coef3<-rep(0,Nlisting)
coef2<-rep(0,Nlisting)
coef1<-rep(0,Nlisting)
unitcounts<-rep(0,Nlisting)
ageR<-data.frame('x' = seq(0,20,1),SaleType = rep(c('Retail','Listing'),each=21))
for (i in 1:Nlisting) {
  
  reduc_Sche_data<- Datainput_pre %>% filter(Schedule ==ListingSched[i,1])
  unitcounts[i]<-dim(reduc_Sche_data)[1]
  fitData<-within(reduc_Sche_data,SaleType<-relevel(factor(SaleType,ordered = F),ref="Retail"))
  fit = lm(log(SaleAB) ~ Age + SaleType ,data = fitData)
  coef3[i]<-fit$coefficients[3]
  
## plot retail vs listing
   coef1[i]<-fit$coefficients[1]
   coef2[i]<-fit$coefficients[2]
   #coef3[i]<-fit$coefficients[3]
  # 
   curve<- ageR %>% 
     mutate(y = ifelse(SaleType=='Retail', exp(coef1[i]+coef2[i]*x), exp(coef1[i]+coef2[i]*x +coef3[i])))
  # 
  # 
   plt<-ggplot(reduc_Sche_data,aes(x=Age,y=SaleAB, color = factor(SaleType))) +
     geom_point()+
     geom_line(data = curve, aes(x,y,color=factor(SaleType)),linetype=1) + 
     ggtitle(paste(ListingSched[i,1])) +
     scale_color_manual(values = c( 'red2','springgreen3'))
  # 
   mypath<-file.path(file_path,plotFolder,paste(ListingSched[i,1],'listing.png'))
   png(file=mypath,width=1000,height=600)
   print(plt)
   dev.off()


  
}

# calculate and cap the reduction factor
standard<-data.frame(ListingSched,coef3,unitcounts)%>%filter(Schedule=='Cranes Group ForBorrowOnly USA')%>%select(coef3)
list_reductfact<-data.frame(ListingSched,coef3,unitcounts) %>%
  mutate(reduction = ifelse(Schedule=='Cranes Group ForBorrowOnly USA',1+coef3,
         partial_listing_discount(100,unitcounts,coef3,as.numeric(standard))))%>%
  mutate(cap_discount = pmax(pmin(reduction,cap_listing+1),cap_listing-1))

## apply reduction factor to listing data to convert to retail use
revised_listing<-merge(merge(uploadListing,CatlevelListing, by=c("CategoryId","Country")),list_reductfact,by='Schedule',all.x=T) %>%
  filter(Age>=3)%>%
  mutate(SalePrice = ListPrice/reduction,
         SaleAB = SalePrice/CurrentABCost,
         SPvalue = SalePrice/M1Value) %>%
  rename(SaleDate =DateScrapedMostRecent)


     