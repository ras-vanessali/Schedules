CountryCode = 'USA'

#################### The upload file consists of 4 parts #####################
# Part 1: regular schedules - Out
# Part 2: auction borrow schedules - OutR
# Part 3: make level schedules (scaled with make adjusters)
# Part 4: tier 3 make schedules

scal=3
## Model Year in use
topyear = 2019 ## top model year
comingyr = topyear+1 ## top model year pulling from db, used for appreciation
botyear = topyear-9 ## bottom model year
ext_botYr = topyear-11 ## bottom model year of putting in regression
dep_endyr = topyear-15 ## bottom model year pulling from db, used for depreciation

## at what age joint the logistic and exponential 
age_joint = 7

## set the minimum gap between adjacent years generally, some specific cases may not use this value
indexcap = 0.0300

## set the limit movement from last month
limUp_MoM = 0.0400
limDw_MoM = 0.0600
limDw_MoM_spec = 0.1500

## thresholds of #datapoints - use to move schedules from regression
#t1<-3 t2<-6 t3<-10 t4<-15 t5<-21 t6<-28
threshold_adj = 25
threshold_brw = 30
threshold_recency = 40
threshold_rec.calc = 25


recency_cap = 0.25
## set the minimum gap between two channels
capChannel = 0.10
## use for channel confict - distinguish older and newer age
chanyr = 3
## use for channel check who govern in middel years. retail data >5, Auction governs
retBelieve = 5 


## newest year in auction have to be in the range
capMonthpct = 0.01

fixyr_gap <- function(Schedule){
  fixyr = ifelse(str_detect(Schedule,'Cranes'), 5, 3)
  return(fixyr)
}

## set the bounds of could-be lowest and highest schedule
UpperB = 3
LowerB = 0


## logistic growth regression slope minimum
slopecap = -1.5

## caps of appreciation and depreciation
app_yrgap = 3.00
appBound_upp = 0.15
appBound_bot = 0.05
appBound_na = 0.10

depBound_upp = 0.06
depBound_upp_crane = 0.10
depBound_bot = 0.020
depBound_na = 0.040

DeprMoMLimit = 0.002
ApprMoMLimit = 0.01
## set the index of standard deviation side of mean 
stdInd =2


## Make adjusters caps
makeSFupp = 1.3
makeSFbot = 0.7

## Make adjustment channel gap 
Min_delta =0.1

## threshold of data points for make adjusters
thredtpts = 15

## use for phase in factors on each model year
phaseinAge = 6
phaseinFactor =0.15

## Last two years in schedules - limit the shift
lastyr_1 = 0.05
lastyr_2 = 0.10


## use for the second end year - prevend rebasing jump
x = 0.05
## use for the first end year violate second end year
endYrRate = 0.02

## Global category list
GlobalList<-c(313,	6,	2509,	15,	29,	315,	360,	451,	316,	362)
#Articulating Booms,	Backhoe Loaders,	Compact Track Loaders,	Dozers,	Excavators,	Scissor Lifts,	Skid Steer Loaders,	Telehandlers,	Telescopic Booms,	Wheel Loaders
ListingIds<-c(2605,2603,2608,2604,2606,2577,19,2607) 
#'Carry-Deck Cranes','Rough-Terrain Cranes','All-Terrain Cranes','Truck-Mounted Cranes','Crawler Cranes'
GlobalClassId=1



################################################# Read tabs in file ##########################################################
### load the inputfeed file
In<-data.frame(read_excel(excelfile,sheet='In')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule)
InR<-data.frame(read_excel(excelfile,sheet='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='RetailBorrowAuction')
InA<-data.frame(read_excel(excelfile,sheet='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='AuctionBorrowRetail')
InB<-data.frame(read_excel(excelfile,sheet='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='BorrowBoth')
In.brwcrane<-data.frame(read_excel(excelfile,sheet='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(str_detect(Schedule,'Crane'))

### load the application file
Out<-data.frame(read_excel(excelfile,sheet='Out')) %>% filter(Country==CountryCode)
OutR<-data.frame(read_excel(excelfile,sheet='OutR')) %>% filter(Country==CountryCode)

### Application tab
comb_Out<-rbind(Out %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId, Level2,Plot),OutR %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId,Level2,Plot))

### load the Sched file
Sched<-data.frame(read_excel(excelfile,sheet='Sched')) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax) 
SchedR<-data.frame(read_excel(excelfile,sheet='SchedR')) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax,BorrowSchedule,BorrowType) 
SchedFullList<-rbind(Sched,SchedR %>% select(-BorrowSchedule,-BorrowType))

### load make adjustments related tabs
Mlist<-data.frame(read_excel(excelfile,sheet='MList')) %>% filter(Country==CountryCode)
#Make3<-data.frame(read_excel(excelfile,sheet='Make3')) %>% filter(Country==CountryCode) %>% select(MakeId, Adjuster)

Categ_fullLs <- rbind(In['CategoryId'],InR['CategoryId'],InA['CategoryId']) %>% distinct()

###################################################################################################################
###################################################################################################################
##################################### INPUT DATA FROM SQL DB /INTAKE FLAT FILE ####################################
###################################################################################################################
###################################################################################################################

################################## Input sales data for category and subcat level schedule #####################################
channel<-odbcConnect(DBserver)
uploadData<-sqlQuery(channel,"
                               
                                         
             SET NOCOUNT ON                    

			      Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
			      Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

            Declare @topyear INT = 2019
            Declare @compingyr INT = @topyear+1
					  Declare @botyear INT =  @compingyr-10
					  Declare @ext_botYr INT = @compingyr-12
            Declare @dep_endyr INT = @compingyr-15
            Declare @year_20 INT = @topyear-20
            
            
             Declare @intercept_RetLow decimal(10,1) = 0.5    
            Declare @intercept_AucLow decimal(10,1) = 0.3  
			Declare @intercept_RetHi decimal(10,1) = 1.5    
            Declare @intercept_AucHi decimal(10,1) = 1.2
            
			Declare @denom_low decimal(10,1) =4.0
            Declare @denom_hi decimal(10,1) =25.0
			  
                    SELECT 
					         CASE WHEN SaleType='Retail' THEN CustomerAssetId
                    WHEN SaleType='Auction' THEN InternetComparableId END AS CompId
                    ,[EquipmentTypeId]
                    ,[CategoryId]
                    ,[CategoryName]
                    ,[SubcategoryId]
                    ,[SubcategoryName]
                    ,[MakeId]
                    ,[MakeName]
                    ,[ModelId]
                    ,[ModelName]
                    ,[ModelYear]
                    ,SaleDate
                    ,EOMONTH(SaleDate) as EffectiveDate
                    ,[SalePriceSF] as [SalePrice]
                    ,SaleType
                    ,MilesHours
                    ,MilesHoursCode
                    ,[M1AppraisalBookPublishDate]
                    
                    ,CASE WHEN SaleType='Retail' THEN M1PrecedingFmv
                    WHEN SaleType='Auction' THEN M1PrecedingFlv END AS M1Value
                    ,CurrentABCost
                    ,[SalePriceSF]/CurrentABCost AS [SaleAB]
                    ,CASE WHEN SaleType='Retail' THEN SalePriceSF/M1PrecedingFmv
                    WHEN SaleType='Auction' THEN SalePriceSF/M1PrecedingFlv END AS SPvalue
                    
                    ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                   ,'USA' as Country
                     ,CASE                    
                    WHEN SaleType='Retail' AND ([SalePriceSF]/CurrentABCost < @intercept_RetLow - (@intercept_RetLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCost > @intercept_RetHi - (@intercept_RetHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi) THEN 'EcxRegr'

                    WHEN SaleType='Auction' AND ([SalePriceSF]/CurrentABCost < @intercept_AucLow - (@intercept_AucLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCost > @intercept_AucHi - (@intercept_AucHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi)THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
				
                    ,CASE WHEN Modelyear < @botyear or Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                    
				
                    FROM [ras_sas].[BI].[Comparables]
                    
                    WHERE 
                    ( ([source]='internet' AND NOT ((AuctioneerClassification LIKE '%unused%' OR [Description] LIKE '%unused%') AND (SaleYear - ModelYear > 1))
										   AND NOT (auctioneer = 'Alex Lyon & Son' and age between 0 and 24) 
										   AND NOT ([Description] like '%reman%' or [Description] like '%refurb%' or [Description] like '%recon%')
                    or (SaleType='retail' AND IsUsedForComparables='Y')))     
                    AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
                    AND MakeId NOT in (58137,78,14086) --Miscellaneous,Not Attributed,Various
					          AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')  
                    AND SaleDate >@dateStart AND saledate<=@dateEnd 
                    --and categoryid in (2606,2612,30,2515,2616)
                    --AND SaleDate >='2018-09-01' AND saledate<='2019-08-31'
                    AND ModelYear <= @compingyr 
                    and ModelYear>= CASE WHEN categoryid in (2605,2603,2608,2604,2606)  THEN @year_20 ELSE @dep_endyr END
                    AND [SalePriceSF]>100
                    AND CurrentABCost is not NULL 
                    AND M1PrecedingABCost is not NULL
                    AND Option15 is NULL 

              ")

###################################### Input listing data for Cranes #########################################
channel<-odbcConnect(DBserver)
uploadListing<-sqlQuery(channel,"
Declare @PublishDate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
Declare @EOpriorM DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

Declare @topyear INT = 2019
Declare @compingyr INT = @topyear+1
Declare @botyear INT =  @compingyr-10
Declare @ext_botYr INT = @compingyr-12
Declare @year_20 INT = @topyear-20
            
 Declare @index_RetL decimal(10,1) = 0.5    
 Declare @index_RetH decimal(10,1) = 1.5    
 Declare @ind_low decimal(10,1) =4.0
 Declare @ind_hi decimal(10,1) =25.0     

 SELECT [ListingId] CompId
     -- ,LU.[ClassificationID]
	 ,LU.SourceId
	    ,LU.CategoryId
      ,LU.[CategoryName]
	    ,LU.SubcategoryId
      ,LU.[SubcategoryName]
	    ,LU.MakeId
      ,LU.[MakeName]
	    ,LU.ModelId
      ,LU.[ModelName]
      ,EOMONTH([DateScrapedMostRecent]) as EffectiveDate
      ,[DateScrapedMostRecent]
	  ,[DateChangedMostRecent]
      ,Price ListPrice
      ,[Year] ModelYear
      ,MeterHours
      ,ETV.ABCost CurrentABCost
	  ,ETV.[FmvSchedulePercentage]
	  ,ETV.ABCost*ETV.[FmvSchedulePercentage]/100 as M1Value
	  ,'Listing' AS SaleType
      ,cast(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00 as decimal(10,4))  as Age
      ,CountryCode as Country
      ,CASE WHEN Year < @botyear or Year >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
     ,CASE                    
        WHEN (Price/ETV.ABCost < @index_RetL - (@index_RetL*(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00))/@ind_low
					       OR Price/ETV.ABCost > @index_RetH - (@index_RetH*(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00))/@ind_hi) THEN 'EcxRegr'
          ELSE 'inUse' END AS 'Flag'         
	  ,Price/ETV.ABCost as SaleAB


  FROM [Listings].[BI].[ListingsUnique] LU
 -- inner join [ras_sas].[BI].[AppraisalBookEquipmentTypesMKT] ET
 -- on LU.SubcategoryId=ET.SubcategoryId and LU.ModelId = ET.ModelId and ET.AppraisalBookPublishDate = EOMONTH([DateChangedMostRecent])
  left join [ras_sas].[BI].[AppraisalBookEquipmentTypeValues] ETV
  on LU.SubcategoryId=ETV.SubcategoryId and LU.ModelId = ETV.ModelId and LU.[Year] = ETV.ModelYear
     AND ETV.[AppraisalBookPublishDate]=@EOpriorM

  Where LU.CountryCode = 'USA' 
  and [DateScrapedMostRecent] in (SELECT Max([ScrapeDate])
								  FROM [Listings].[dbo].[vw_ListingsScrapes]
								  Where [SourceId] ='cranenetwork.com' and ScrapeDate between @EOpriorM and @PublishDate)

  --and ([DateChangedMostRecent]>='2019-09-01')
  AND [Year] >= @year_20 AND [Year] <= @compingyr
  and LU.categoryid in (2605,2603,2608,2604,2606,2577,19,2607) 
  AND Lu.Price is not null AND ETV.ABCost is not null 
  AND LU.MakeId NOT in (58137,78,14086,7363) --Miscellaneous,Not Attributed,Various,N/A")



################################## 1.A Calculate average make adjusters for each CSM in MList #####################################
MakeAdj_CatL <- Mlist %>% distinct(CategoryId)
CategoryId <- cbind(MakeAdj_CatL, MakeAdj_CatL)

In_makeAdj<-sqlExecute(channel,"

/*************************************** Define Variables ***************************************/           
SET NOCOUNT ON                    
Declare @topyear INT = 2019
Declare @ext_botYr INT = @topyear-11

Declare @Age_0 INT = 2
Declare @Age_f INT = 12

Declare @badData_low decimal(10,1) = .4
Declare @badData_hi decimal(10,1) = 1.6

Declare @recentThreshold Int = 15

Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-3,GETDATE()))-1, -1) as date)
Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

 Declare @intercept_RetLow decimal(10,1) = 0.5    
            Declare @intercept_AucLow decimal(10,1) = 0.3  
			Declare @intercept_RetHi decimal(10,1) = 1.5    
            Declare @intercept_AucHi decimal(10,1) = 1.2
            
			Declare @denom_low decimal(10,1) =4.0
            Declare @denom_hi decimal(10,1) =25.0


/*************************************** Comps - Bi.comparables ***************************************/     
Drop Table If exists #Data 
                    SELECT 
                    [InternetComparableId]
                    ,CustomerAssetId
                    ,[EquipmentTypeId]
                    ,[CategoryId]
                    ,[CategoryName]
                    ,[SubcategoryId]
                    ,[SubcategoryName]
                    ,[MakeId]
                    ,[MakeName]
                    ,[ModelId]
                    ,[ModelName]
                    ,[ModelYear]
					,SaleDate
                    ,EOMONTH(SaleDate) as EffectiveDate
                    ,[SalePriceSF] as [SalePrice]
                    ,SaleType
                    ,MilesHours
                    ,MilesHoursCode
                    ,[M1AppraisalBookPublishDate]
                    ,CurrentABCost
                    ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                    ,M1PrecedingABCost
                     ,CASE                    
                    WHEN SaleType='Retail' AND ([SalePriceSF]/CurrentABCost < @intercept_RetLow - (@intercept_RetLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCost > @intercept_RetHi - (@intercept_RetHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi) THEN 'EcxRegr'

                    WHEN SaleType='Auction' AND ([SalePriceSF]/CurrentABCost < @intercept_AucLow - (@intercept_AucLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCost > @intercept_AucHi - (@intercept_AucHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi)THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
				
                    INTO #Data

                    FROM [ras_sas].[BI].[Comparables]
                    
                    WHERE 
                   ( ([source]='internet' AND NOT ((AuctioneerClassification LIKE '%unused%' OR [Description] LIKE '%unused%') AND (SaleYear - ModelYear > 1))
										   AND NOT (auctioneer = 'Alex Lyon & Son' and age between 0 and 24)
										   AND NOT ([Description] like '%reman%' or [Description] like '%refurb%' or [Description] like '%recon%')
                    or (SaleType='retail' AND IsUsedForComparables='Y')))     
                    AND CategoryId =?
					AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')                            
                    AND EOMONTH(SaleDate)>=@dateStart AND EOMONTH(SaleDate)<@dateEnd
                    AND ModelYear <= @topyear and ModelYear>=@ext_botYr
                    AND [SalePriceSF]>100
                    AND CurrentABCost is not NULL 
                    AND M1PrecedingABCost is not NULL
                    AND MakeId NOT in (58137,78,14086) --Miscellaneous,Not Attributed,Various


/*************************************** M1 Values - Bi.ABCV ***************************************/     
Drop Table If exists #M1Value 
	  SELECT  [ClassificationId]
          ,[CategoryId]
         ,[CategoryName]
         ,[SubcategoryId]
         ,[SubcategoryName]
         ,[AppraisalBookIssueID]
         ,[AppraisalBookPublishDate]
         ,[ModelYear]
         ,[FmvSchedulePercentage]
         ,[FlvSchedulePercentage]
      INTO #M1Value 
      FROM [ras_sas].[BI].[AppraisalBookClassificationValues]
      Where CategoryId =?
            AND SubcategoryName is NOT NULL
	          AND MakeId is null                                                       
            AND AppraisalBookPublishDate>=@dateStart AND AppraisalBookPublishDate<@dateEnd
            AND ModelYear <= @topyear and ModelYear>=@ext_botYr



/*************************************** Join & Aggregation ***************************************/     
SELECT CSM.CategoryId,CSM.CategoryName, CSM.SubcategoryId,CSM.SubcategoryName,CSM.MakeId, CSM.MakeName, CSM.SaleType, avg(spValue) as AvgspValue, count(*) as Numcomps
FROM( 

	SELECT #Data.CategoryId,#Data.CategoryName, #Data.SubcategoryId,#Data.SubcategoryName, #Data.MakeId, #Data.MakeName,#Data.SaleType,SaleDate
	,row_number() over (partition by #Data.CategoryId, #Data.SubcategoryId, #Data.MakeId, #Data.SaleType order by #Data.SaleDate desc) as rowNum
	,CASE WHEN SaleType='Auction' THEN SalePrice/(FlvSchedulePercentage*M1PrecedingABCost/100)
	ELSE SalePrice/(FmvSchedulePercentage*M1PrecedingABCost/100) END AS spValue

	FROM #Data 
	Inner Join #M1Value
	ON #Data.SubcategoryId = #M1Value.SubcategoryId AND #Data.EffectiveDate = #M1Value.AppraisalBookPublishDate
	AND #Data.ModelYear = #M1Value.ModelYear
	WHERE #Data.Flag = 'inUse' AND #Data.Age between @Age_0 and @Age_f 
) as CSM
WHERE spValue < @badData_hi AND spValue > @badData_low and rowNum<=@recentThreshold
Group By CSM.CategoryId,CSM.CategoryName, CSM.SubcategoryId,CSM.SubcategoryName, CSM.MakeId, CSM.MakeName, CSM.SaleType
ORder By CSM.CategoryId,CSM.CategoryName, CSM.SubcategoryId,CSM.SubcategoryName, CSM.MakeId, CSM.MakeName, CSM.SaleType

" ,CategoryId, fetch=T)


################################## 1.C Input last month category and subcat level schedule #####################################
channel<-odbcConnect("production")
LastMonth_import<-sqlQuery(channel,"
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[AppraisalBookPublishDate]
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as CurrentFmv
      ,[FlvSchedulePercentage]/100 as CurrentFlv
  FROM [ras_sas].[BI].[AppraisalBookClassificationValues]
  WHERE [AppraisalBookPublishDate] = @EffectiveDate
  AND NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%')
  AND ModelId is null 
  AND ModelYear Between 2007 And 2020")

################################## 1.D Input last month apprciation and depreciation values #####################################

Last_depr<-sqlQuery(channel,"
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

SELECT [ClassificationId]
      ,[AppraisalBookIssueID]
      ,[AppraisalBookPublishDate]
      ,[FLVAppreciationPercentage]/100 AS Appreciation
      ,[FLVDepreciationPercentage]/100 AS Depreciation
  FROM [ras_sas].[BI].[AppraisalBookSchedules]
  Where [AppraisalBookPublishDate] = @EffectiveDate
  AND ModelId is null AND [FLVAppreciationPercentage] IS NOT NULL ")



################################ Classification - csm #########################################

AllClass<-sqlQuery(channel,"
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
  FROM [ras_sas].[BI].[Classifications]
  Where  Modelid is null
    And NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%')
--	AND SubcategoryName NOT LIKE 'DO NOT USE%' 
	AND (MakeId NOT IN (78,7363,58137,14086) AND MakeName Not LIke 'DNU%') -- Misc, N/A, Not Attributed, Various
  Order By 
      [CategoryName]
      ,[SubcategoryName]
      ,[MakeName]
")

################################ Grouping #########################################
ReportGrp<-sqlQuery(channel,
"SET NOCOUNT ON
DROP TABLE IF EXISTS #reptGrp
SELECT Distinct
      [CategoryId]
     
      ,CASE WHEN [ReportGroup] LIKE '% Earth Moving' THEN 'Earth Moving' 
	  WHEN ReportGroup in ('Aerial','Telehandlers') THEN 'Aerial' ELSE [ReportGroup] END AS [ReportGroup]
  INTO #reptGrp
  FROM [ras_sas].[BI].[Subcategories]
  Where CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  

DELETE #reptGrp
WHERE [ReportGroup] ='Global' AND CategoryId in (15,29,362)

SELECT * FROM #reptGrp Order By ReportGroup")
