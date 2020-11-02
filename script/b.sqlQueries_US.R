
###################################################################################################################
###################################################################################################################
################################################# SQL Queries #####################################################
###################################################################################################################
###################################################################################################################

## US retail & auction data load
US_dataload<-"SET NOCOUNT ON                    

			      Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
			      Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

            Declare @topyear INT = 2020
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
                    ,CustomerId
                    ,Equipno
                   ,CASE WHEN AcquisitionDate IS NULL THEN datefromparts(ModelYear, 7, 1)
					ELSE AcquisitionDate END AS ModAcqDate
					,AcquisitionDate
                    ,EOMONTH(SaleDate) as EffectiveDate
                    ,[SalePriceSF] as [SalePrice]
                    ,SaleType
                    ,MilesHours
                    ,MilesHoursCode
                    ,[M1AppraisalBookPublishDate]
                    
                    ,CASE WHEN SaleType='Retail' THEN M1PrecedingFmv
                    WHEN SaleType='Auction' THEN M1PrecedingFlv END AS M1Value
                    ,CurrentABCostUSNA as CurrentABCost
                    ,[SalePriceSF]/CurrentABCostUSNA AS [SaleAB]
                    ,CASE WHEN SaleType='Retail' THEN SalePriceSF/M1PrecedingFmv
                    WHEN SaleType='Auction' THEN SalePriceSF/M1PrecedingFlv END AS SPvalue
                    
                    ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                   ,'USA' as Country
                     ,CASE                    
                    WHEN SaleType='Retail' AND ([SalePriceSF]/CurrentABCostUSNA < @intercept_RetLow - (@intercept_RetLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCostUSNA > @intercept_RetHi - (@intercept_RetHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi) THEN 'EcxRegr'

                    WHEN SaleType='Auction' AND ([SalePriceSF]/CurrentABCostUSNA < @intercept_AucLow - (@intercept_AucLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCostUSNA > @intercept_AucHi - (@intercept_AucHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi)THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
				
                    ,CASE WHEN Modelyear < @botyear or Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                 
                    FROM [ras_sas].[BI].[Comparables]
                    
                    WHERE 
                    ( ([source]='internet' 
                       AND NOT ((AuctioneerClassification LIKE '%unused%' OR [Description] LIKE '%unused%') AND (SaleYear - ModelYear > 1))
					             AND NOT (auctioneer = 'Alex Lyon & Son' and age between 0 and 24) 
					             AND NOT ([Description] like '%reman%' or [Description] like '%refurb%' or [Description] like '%recon%'))

					          OR ([source]='internet' AND (AuctioneerClassification IS NULL OR [Description] IS  NULL) 
					          AND NOT (auctioneer = 'Alex Lyon & Son' and age between 0 and 24) )
					   
                        OR (SaleType='retail' AND IsUsedForComparablesUSNA='Y' 
                        --AND [MilesHours]>=100
                        ))
                    AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
                    AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed
					          AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')  --generators
					         
                    AND SaleDate >@dateStart AND saledate<=@dateEnd 
                    --and categoryid in (453)
                    --AND SaleDate >='2018-09-01' AND saledate<='2019-08-31'
                    AND ModelYear <= @compingyr 
                    and ModelYear>= CASE WHEN categoryid in (2605,2603,2608,2604,2606)  THEN @year_20 ELSE @dep_endyr END
                    AND [SalePriceSF]>100
                    AND CurrentABCostUSNA is not NULL 
                    AND M1PrecedingFmv IS NOT NULL
                    AND M1PrecedingFlv IS NOT NULL
                    AND M1PrecedingABCostUSNA is not NULL
                    AND Option15 is NULL "

###################################### Input listing data for Cranes #########################################


Listing_dataload<-" SET NOCOUNT ON
Declare @PublishDate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
Declare @EOpriorM DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

Declare @topyear INT = 2020
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
      ,datefromparts(ModelYear, 7, 1) as AcquisitionDate
      ,datefromparts(ModelYear, 7, 1) as ModAcqDate
      ,[DateScrapedMostRecent]
	  ,[DateChangedMostRecent]
      ,Price ListPrice
      ,[Year] ModelYear
       ,iif(MeterMiles is null, MeterHours,MeterMiles) as MilesHours
      ,iif(MeterMiles is null and MeterHours is NULL , Null ,iif(MeterMiles is Null, 'H','M')) as MilesHoursCode
      ,ETV.ABCostUSNA CurrentABCost
	  ,ETV.[FmvSchedulePercentage]
	  ,ETV.ABCostUSNA*ETV.[FmvSchedulePercentage]/100 as M1Value
	  ,'Listing' AS SaleType
      ,cast(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00 as decimal(10,4))  as Age
      ,CountryCode as Country
      ,CASE WHEN Year < @botyear or Year >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
     ,CASE                    
        WHEN (Price/ETV.ABCostUSNA < @index_RetL - (@index_RetL*(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00))/@ind_low
					       OR Price/ETV.ABCostUSNA > @index_RetH - (@index_RetH*(YEAR(@PublishDate)-Year + (MONTH(@PublishDate)-6)/12.00))/@ind_hi) THEN 'EcxRegr'
          ELSE 'inUse' END AS 'Flag'         
	  ,Price/ETV.ABCostUSNA as SaleAB


  FROM [Listings].[BI].[ListingsUnique] LU
  left join [ras_sas].[BI].[AppraisalBookEquipmentTypeValuesUSNA] ETV
  on LU.SubcategoryId=ETV.SubcategoryId and LU.ModelId = ETV.ModelId and LU.[Year] = ETV.ModelYear
     AND ETV.[AppraisalBookPublishDate]=@EOpriorM

  Where LU.CountryCode = 'USA' 
  and LU.[DateScrapedMostRecent] between @EOpriorM and @PublishDate -- listing sraped in the effective months. about 4 to 5 scrapes

  and DATEDIFF(Day,LU.[DateChangedMostRecent],LU.[DateScrapedMostRecent]) <=90
  AND [Year] >= @year_20 AND [Year] <= @compingyr
  and LU.categoryid in (2605,2603,2608,2604,2606,2577,19,2607) 
  AND Lu.Price is not null AND ETV.ABCostUSNA is not null 
  AND LU.MakeId NOT in (58137,78,7363) --Miscellaneous,Not Attributed,Various,N/A"



################################## 1.A Calculate average make adjusters for each CSM in MList #####################################
MakeAdj_CatL <- Mlist %>% distinct(CategoryId)
CategoryId <- cbind(MakeAdj_CatL, MakeAdj_CatL)

MakeAdjust_dataload<-"

/*************************************** Define Variables ***************************************/           
SET NOCOUNT ON                    
Declare @topyear INT = 2020
Declare @ext_botYr INT = @topyear-11

Declare @Age_0 INT = 2
Declare @Age_f INT = 12

Declare @badData_low decimal(10,1) = .4
Declare @badData_hi decimal(10,1) = 1.6

Declare @recentThreshold Int = 50

Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-3,GETDATE()))-1, -1) as date)
Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
Declare @month6 Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-7, -1) AS date) 

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
                    ,CurrentABCostUSNA as CurrentABCost
                    ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                    ,M1PrecedingABCostUSNA as M1PrecedingABCost
                     ,CASE                    
                    WHEN SaleType='Retail' AND ([SalePriceSF]/CurrentABCostUSNA < @intercept_RetLow - (@intercept_RetLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCostUSNA > @intercept_RetHi - (@intercept_RetHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi) THEN 'EcxRegr'

                    WHEN SaleType='Auction' AND ([SalePriceSF]/CurrentABCostUSNA < @intercept_AucLow - (@intercept_AucLow*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_low
					                           OR [SalePriceSF]/CurrentABCostUSNA > @intercept_AucHi - (@intercept_AucHi*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/@denom_hi)THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
				
                    INTO #Data

                    FROM [ras_sas].[BI].[Comparables]
                    
                    WHERE 
                   ( ([source]='internet' AND NOT ((AuctioneerClassification LIKE '%unused%' OR [Description] LIKE '%unused%') AND (SaleYear - ModelYear > 1))
										   AND NOT (auctioneer = 'Alex Lyon & Son' and age between 0 and 24)
										   AND NOT ([Description] like '%reman%' or [Description] like '%refurb%' or [Description] like '%recon%')
                    or (SaleType='retail' AND IsUsedForComparablesUSNA='Y')))     
                    AND CategoryId =?
					          AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')         
					         
                    AND EOMONTH(SaleDate)>=@dateStart AND EOMONTH(SaleDate)<@dateEnd
                    AND ModelYear <= @topyear and ModelYear>=@ext_botYr
                    AND [SalePriceSF]>100
                    AND CurrentABCostUSNA is not NULL 
                    AND M1PrecedingABCostUSNA is not NULL
                    AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed


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
      FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
      Where CategoryId =?
            AND SubcategoryName is NOT NULL
	          AND MakeId is null                                                       
            AND AppraisalBookPublishDate>=@dateStart AND AppraisalBookPublishDate<@dateEnd
            AND ModelYear <= @topyear and ModelYear>=@ext_botYr



/*************************************** Join & filter ***************************************/     
select * 
FROM(
select *,row_number() over (partition by CategoryId, SubcategoryId, MakeId, SaleType order by SaleDate desc) as rowNum
from(
	SELECT #Data.CategoryId,#Data.CategoryName, #Data.SubcategoryId,#Data.SubcategoryName, #Data.MakeId, #Data.MakeName,#Data.SaleType,SaleDate	
	,CASE WHEN SaleType='Auction' THEN SalePrice/(FlvSchedulePercentage*M1PrecedingABCost/100)
	ELSE SalePrice/(FmvSchedulePercentage*M1PrecedingABCost/100) END AS spValue

	FROM #Data 
	Inner Join #M1Value
	ON #Data.SubcategoryId = #M1Value.SubcategoryId AND #Data.EffectiveDate = #M1Value.AppraisalBookPublishDate
	AND #Data.ModelYear = #M1Value.ModelYear
	WHERE #Data.Flag = 'inUse' AND #Data.Age between @Age_0 and @Age_f ) as CSM
WHERE spValue < @badData_hi AND spValue > @badData_low) as rowN
where not(SaleDate<=@month6 and rowNum >@recentThreshold)

" 


################################## 1.C Input last month category and subcat level schedule #####################################

LM_USA_load<-"
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
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
  WHERE [AppraisalBookPublishDate] = @EffectiveDate
  AND (NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%') OR [ClassificationId]=1)
  AND ModelId is null 
  AND ModelYear Between 2008 And 2021"

################################## 1.D Input last month apprciation and depreciation values #####################################

Last_depr_USAload<-"Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

SELECT [ClassificationId]
      ,[AppraisalBookIssueID]
      ,[AppraisalBookPublishDate]
      ,[FLVAppreciationPercentage]/100 AS Appreciation
      ,[FLVDepreciationPercentage]/100 AS Depreciation
  FROM [ras_sas].[BI].[AppraisalBookSchedulesUSNA]
  Where [AppraisalBookPublishDate] = @EffectiveDate
  AND ModelId is null AND [FLVAppreciationPercentage] IS NOT NULL "



################################ Classification - csm #########################################

AllClass_query<-"
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
	AND (MakeId NOT IN (78,7363,58137) AND MakeName Not LIke 'DNU%') -- Misc, N/A, Not Attributed, Various
  Order By 
      [CategoryName]
      ,[SubcategoryName]
      ,[MakeName]"

################################ Grouping #########################################
ReportGrp_query<-
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

SELECT * FROM #reptGrp Order By ReportGroup"

UsageList_query<-"SELECT  [CategoryId]
      ,[SubcategoryId]   
      ,[MeterCode]
  FROM [ras_sas].[BI].[UsageAdjCoefficientsInProgress]"