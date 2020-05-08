
################################## 1.A UK data load #####################################
UK_dataload<-"
                               
             SET NOCOUNT ON                    

			      Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-2,GETDATE()))-2, -1) as date)
			      Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)

            Declare @topyear INT = 2020
            Declare @compingyr INT = @topyear+1
					  Declare @botyear INT =  @compingyr-10
					  Declare @ext_botYr INT = @compingyr-12
            Declare @dep_endyr INT = @compingyr-15
            
            
            Declare @index_RetL decimal(10,1) = 0.5    
            Declare @index_AucL decimal(10,1) = 0.3  
			      Declare @index_RetH decimal(10,1) = 1.5    
            Declare @index_AucH decimal(10,1) = 1.2


            Drop Table if exists #Retail
                 SELECT 
                    
                    BIC.CustomerAssetId as CompId
                    ,BIC.[EquipmentTypeId]
                    ,BIC.[CategoryId]
                    ,BIC.[CategoryName]
                    ,BIC.[SubcategoryId]
                    ,BIC.[SubcategoryName]
                    ,BIC.[MakeId]
                    ,BIC.[MakeName]
                    ,BIC.[ModelId]
                    ,BIC.[ModelName]
                    ,BIC.[ModelYear]
                    ,BIC.SaleDate
                    ,EOMONTH(BIC.SaleDate) as EffectiveDate
					,BIC.OptionValue03 as SalePrice
					,'Retail' as SaleType
                    ,BIC.MilesHours
                    ,BIC.MilesHoursCode
                    ,BIC.[M1AppraisalBookPublishDate]
                    ,BIC.M1PrecedingFmv as M1Value
					,ET.[CurrentABCost]
					,BIC.OptionValue03/ET.[CurrentABCost]  AS SaleAB
                    ,BIC.OptionValue03/BIC.M1PrecedingFmv as SPvalue
                    ,cast(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00 as decimal(10,4))  as Age
					,'GBR' as Country
                    ,CASE                    
                    WHEN BIC.OptionValue03/ET.[CurrentABCost] < @index_RetL - (@index_RetL*(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00))/4.0
					  OR BIC.OptionValue03/ET.[CurrentABCost] > @index_RetH - (@index_RetH*(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00))/25.0 THEN 'EcxRegr'
                    ELSE 'inUse' END AS 'Flag'

                    ,CASE WHEN BIC.Modelyear < @botyear or BIC.Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                
				INTO #Retail      
                FROM [ras_sas].[BI].[Comparables] BIC
				Inner Join [ras_sas].[BI].[EquipmentTypesInProgressMKT] ET

				On BIC.[EquipmentTypeId] = ET.[EquipmentTypeId] 
                    
                WHERE 
                  ET.[MarketCode]='GBUK' and
                 (BIC.SaleType='retail' AND BIC.CustomerId in (SELECT [CustomerID]
														FROM [ras_sas].[BI].[Customers]
														where [IsUsedForABCostGBUK]='Y'))

                AND BIC.CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
                AND BIC.MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
					        AND NOT (BIC.[SubcategoryId] in (2806,2808,2001,2636) and BIC.makeid=31 and BIC.ModelName not like 'XQ%')  
					       
                AND BIC.SaleDate >@dateStart AND BIC.saledate<=@dateEnd
                --AND BIC.SaleDate >='2017-11-01' AND BIC.saledate<='2019-10-31'
                AND BIC.ModelYear <= @compingyr and BIC.ModelYear>=@dep_endyr
                AND BIC.OptionValue03>100
                AND ET.[CurrentABCost] is not NULL
                AND BIC.M1PrecedingFmv  is not NULL
                AND BIC.Option15 is NULL 


				Drop Table if exists #Auction
				 SELECT                    
				  AucS.InternetComparableId as CompId
				  ,AucS.[EquipmentTypeId]
                    ,AucS.[CategoryId]
                    ,AucS.[CategoryName]
                    ,AucS.[SubcategoryId]
                    ,AucS.[SubcategoryName]
                    ,AucS.[MakeId]
                    ,AucS.[MakeName]
                    ,AucS.[ModelId]
                    ,AucS.[ModelName]
                    ,AucS.[ModelYear]
                    ,AucS.SaleDate
                    ,EOMONTH(AucS.SaleDate) as EffectiveDate
					,AucS.SalePrice as SalePrice
					,'Auction' as SaleType
                    ,AucS.MilesHours
                    ,AucS.MilesHoursCode
                    ,AucS.[M1AppraisalBookPublishDate]
                    ,AucS.M1PrecedingFlv as M1Value
					,ET.[CurrentABCost] as CurrentABCost
					,AucS.SalePrice/ET.[CurrentABCost]  AS SaleAB
					,AucS.SalePrice/AucS.M1PrecedingFlv as SPvalue
                    ,cast(YEAR(AucS.SaleDate)-AucS.ModelYear + (MONTH(AucS.SaleDate)-6)/12.00 as decimal(10,4))  as Age
					,CountryCode as Country
                    ,CASE                    
                    WHEN AucS.SalePrice/ET.[CurrentABCost] < @index_AucL - (@index_AucL*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/4.0
					  OR AucS.SalePrice/ET.[CurrentABCost] > @index_AucH - (@index_AucH*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/25.0 THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
                    
				   ,CASE WHEN AucS.Modelyear < @botyear or AucS.Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                INTO #Auction    
                FROM [ras_sas].[BI].[AuctionSales] AucS
				Inner Join [ras_sas].[BI].[EquipmentTypesInProgressMKT] ET

				On AucS.[EquipmentTypeId] = ET.[EquipmentTypeId] 
                    
                WHERE  ET.[MarketCode]='GBUK' and AucS.CountryCode ='GBR' AND AucS.CurrencyCode='GBP' 
				AND AucS.CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
                AND AucS.MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
					        AND NOT (AucS.[SubcategoryId] in (2806,2808,2001,2636) and AucS.makeid=31 and AucS.ModelName not like 'XQ%')  
                AND AucS.SaleDate >@dateStart AND AucS.saledate<=@dateEnd
                --AND AucS.SaleDate >='2017-11-01' AND AucS.saledate<='2019-10-31'
                AND AucS.ModelYear <= @compingyr and AucS.ModelYear>=@dep_endyr
                AND AucS.SalePrice>100
                AND ET.[CurrentABCost] is not NULL
                AND AucS.M1PrecedingFlv is not NULL

 SELECT * FROM #Retail 
 Union ALL
 SELECT * FROM #Auction"

################################## 1.B UK make adjusters #####################################
MakeAdj_CatL <- Mlist %>% distinct(CategoryId)
CategoryId <- cbind(MakeAdj_CatL, MakeAdj_CatL,MakeAdj_CatL)

MakeAdjust_dataloadUK<-
"                    
             SET NOCOUNT ON                    

			      Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-3,GETDATE()))-2, -1) as date)
			      Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
				  Declare @month6 Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-8, -1) AS date) 

            Declare @topyear INT = 2020
            Declare @compingyr INT = @topyear+1
					  Declare @botyear INT =  @compingyr-10
					  Declare @ext_botYr INT = @compingyr-12
            Declare @dep_endyr INT = @compingyr-15
            
            
            Declare @index_RetL decimal(10,1) = 0.5    
            Declare @index_AucL decimal(10,1) = 0.3  
			      Declare @index_RetH decimal(10,1) = 1.5    
            Declare @index_AucH decimal(10,1) = 1.2

			Declare @badData_low decimal(10,1) = .4
Declare @badData_hi decimal(10,1) = 1.6

Declare @recentThreshold Int = 50
Declare @Age_0 INT = 2
Declare @Age_f INT = 12

            Drop Table if exists #Retail
                 SELECT 
                    
                    BIC.CustomerAssetId as CompId
                    ,BIC.[EquipmentTypeId]
                    ,BIC.[CategoryId]
                    ,BIC.[CategoryName]
                    ,BIC.[SubcategoryId]
                    ,BIC.[SubcategoryName]
                    ,BIC.[MakeId]
                    ,BIC.[MakeName]
                    ,BIC.[ModelId]
                    ,BIC.[ModelName]
                    ,BIC.[ModelYear]
                    ,BIC.SaleDate
                    ,EOMONTH(BIC.SaleDate) as EffectiveDate
					,BIC.OptionValue03 as SalePrice
					,'Retail' as SaleType
                    ,BIC.MilesHours
                    ,BIC.MilesHoursCode
                    ,BIC.[M1AppraisalBookPublishDate]
                    ,BIC.M1PrecedingFmv as M1Value
					,ET.[CurrentABCost]
					,BIC.OptionValue03/ET.[CurrentABCost]  AS SaleAB
                    ,BIC.OptionValue03/BIC.M1PrecedingFmv as SPvalue
                    ,cast(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00 as decimal(10,4))  as Age
					,'GBR' as Country
                    ,CASE                    
                    WHEN BIC.OptionValue03/ET.[CurrentABCost] < @index_RetL - (@index_RetL*(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00))/4.0
					  OR BIC.OptionValue03/ET.[CurrentABCost] > @index_RetH - (@index_RetH*(YEAR(BIC.SaleDate)-BIC.ModelYear + (MONTH(BIC.SaleDate)-6)/12.00))/25.0 THEN 'EcxRegr'
                    ELSE 'inUse' END AS 'Flag'

                    ,CASE WHEN BIC.Modelyear < @botyear or BIC.Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                
				INTO #Retail      
                FROM [ras_sas].[BI].[Comparables] BIC
				Inner Join [ras_sas].[BI].[EquipmentTypesInProgressMKT] ET

				On BIC.[EquipmentTypeId] = ET.[EquipmentTypeId] 
                    
                WHERE 
                  ET.[MarketCode]='GBUK' and
                 (BIC.SaleType='retail' AND BIC.CustomerId in (SELECT [CustomerID]
														FROM [ras_sas].[BI].[Customers]
														where [IsUsedForABCostGBUK]='Y'))

                AND BIC.CategoryId =?
                AND BIC.MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
					        AND NOT (BIC.[SubcategoryId] in (2806,2808,2001,2636) and BIC.makeid=31 and BIC.ModelName not like 'XQ%')  
					       
                AND BIC.SaleDate >@dateStart AND BIC.saledate<=@dateEnd
                --AND BIC.SaleDate >='2017-11-01' AND BIC.saledate<='2019-10-31'
                AND BIC.ModelYear <= @compingyr and BIC.ModelYear>=@dep_endyr
                AND BIC.OptionValue03>100
                AND ET.[CurrentABCost] is not NULL
                AND BIC.M1PrecedingFmv  is not NULL
                AND BIC.Option15 is NULL 


				Drop Table if exists #Auction
				 SELECT                    
				  AucS.InternetComparableId as CompId
				  ,AucS.[EquipmentTypeId]
                    ,AucS.[CategoryId]
                    ,AucS.[CategoryName]
                    ,AucS.[SubcategoryId]
                    ,AucS.[SubcategoryName]
                    ,AucS.[MakeId]
                    ,AucS.[MakeName]
                    ,AucS.[ModelId]
                    ,AucS.[ModelName]
                    ,AucS.[ModelYear]
                    ,AucS.SaleDate
                    ,EOMONTH(AucS.SaleDate) as EffectiveDate
					,AucS.SalePrice as SalePrice
					,'Auction' as SaleType
                    ,AucS.MilesHours
                    ,AucS.MilesHoursCode
                    ,AucS.[M1AppraisalBookPublishDate]
                    ,AucS.M1PrecedingFlv as M1Value
					,ET.[CurrentABCost] as CurrentABCost
					,AucS.SalePrice/ET.[CurrentABCost]  AS SaleAB
					,AucS.SalePrice/AucS.M1PrecedingFlv as SPvalue
                    ,cast(YEAR(AucS.SaleDate)-AucS.ModelYear + (MONTH(AucS.SaleDate)-6)/12.00 as decimal(10,4))  as Age
					,CountryCode as Country
                    ,CASE                    
                    WHEN AucS.SalePrice/ET.[CurrentABCost] < @index_AucL - (@index_AucL*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/4.0
					  OR AucS.SalePrice/ET.[CurrentABCost] > @index_AucH - (@index_AucH*(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00))/25.0 THEN 'EcxRegr'

                    ELSE 'inUse' END AS 'Flag'
                    
				   ,CASE WHEN AucS.Modelyear < @botyear or AucS.Modelyear >@topyear THEN 'ExtYrs' ELSE 'AdjusUseYr' END AS 'YearFlag'
                INTO #Auction    
                FROM [ras_sas].[BI].[AuctionSales] AucS
				Inner Join [ras_sas].[BI].[EquipmentTypesInProgressMKT] ET

				On AucS.[EquipmentTypeId] = ET.[EquipmentTypeId] 
                    
                WHERE  ET.[MarketCode]='GBUK' and AucS.CountryCode ='GBR' AND AucS.CurrencyCode='GBP' 
				AND AucS.CategoryId =?
                AND AucS.MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
					        AND NOT (AucS.[SubcategoryId] in (2806,2808,2001,2636) and AucS.makeid=31 and AucS.ModelName not like 'XQ%')  
                AND AucS.SaleDate >@dateStart AND AucS.saledate<=@dateEnd
                --AND AucS.SaleDate >='2017-11-01' AND AucS.saledate<='2019-10-31'
                AND AucS.ModelYear <= @compingyr and AucS.ModelYear>=@dep_endyr
                AND AucS.SalePrice>100
                AND ET.[CurrentABCost] is not NULL
                AND AucS.M1PrecedingFlv is not NULL


							 
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
      FROM [ras_sas].[BI].[AppraisalBookClassificationValuesGBUK]
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
	SELECT tbData.CategoryId,tbData.CategoryName, tbData.SubcategoryId,tbData.SubcategoryName, tbData.MakeId, tbData.MakeName,tbData.SaleType,SaleDate	
	,CASE WHEN SaleType='Auction' THEN SalePrice/(FlvSchedulePercentage*tbData.CurrentABCost/100)
	ELSE SalePrice/(FmvSchedulePercentage*tbData.CurrentABCost/100) END AS spValue

	FROM 
	( SELECT * FROM #Retail 
		Union ALL
		SELECT * FROM #Auction) as tbData
	Inner Join #M1Value
	ON tbData.SubcategoryId = #M1Value.SubcategoryId AND tbData.EffectiveDate = #M1Value.AppraisalBookPublishDate
	AND tbData.ModelYear = #M1Value.ModelYear
	WHERE tbData.Flag = 'inUse' AND tbData.Age between @Age_0 and @Age_f ) as CSM
WHERE spValue < @badData_hi AND spValue > @badData_low) as rowN
where not(SaleDate<=@month6 and rowNum >@recentThreshold)
"


################################## 1.C Input last month category and subcat level schedule #####################################

LM_UK_load<-"
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-2, -1) AS date)

SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
       ,[MakeId]
      , MakeName
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as CurrentFmv
      ,[FlvSchedulePercentage]/100 as CurrentFlv
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesGBUK]
  WHERE 
 [MarketCode]='GBUK' and [AppraisalBookPublishDate]=@EffectiveDate
   AND (NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%') OR [ClassificationId]=1)
  AND ModelId is null 
  AND ModelYear Between 2007 And 2020"
################################## 1.D Input last month apprciation and depreciation values #####################################

Last_depr_UKload<-"
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-2, -1) AS date)

SELECT [ClassificationId]
      ,[FLVAppreciationPercentage]/100 AS Appreciation
      ,[FLVDepreciationPercentage]/100 AS Depreciation
  FROM [ras_sas].[BI].[AppraisalBookSchedulesGBUK]
  Where  [MarketCode]='GBUK' and [AppraisalBookPublishDate]=@EffectiveDate
   and ModelId is null AND [FLVAppreciationPercentage] IS NOT NULL "


################################ Classification - csm #########################################

AllClass<-"
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
  FROM [ras_sas].[BI].[Classifications]
  Where  Modelid is null
  Order By 
      [CategoryName]
      ,[SubcategoryName]
      ,[MakeName]"



################################ Grouping #########################################
ReportGrp<-
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
