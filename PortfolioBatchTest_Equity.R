rm(list=ls())

#Load packages
library(grid)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(ggplot2)
library(png)
library(tidyr)
library(zoo)
#Version - Control

# --- DATE ---|--- Editor ---| --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 04 - 24 |KH|3 | Adjust code to updated bridge files
# 2017-09-22  |     KH      | 15 | Changed to new IEA Targets for automotive (i.e. ETP 2017)
# 2017-09-25  |     KH      | 16 | Changed to new financial data (added the swiss BBG look-ups) including adjusting for the new PORT lookup (e.g. changing to MarketVal for SharePrice, etc.)

#-------------
# Get user-name & set folder locations
#-------------
UserName <- sub("/.*","",sub(".*Users/","",getwd()))
DataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/")
CodeFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/01_Code/02_AlignmentTest/")
FinancialDataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/02_FinancialData/")
PortfolioDataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/")
OutputFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/03_Results/01_BatchResults/")

#-------------
# Set parameter file
#-------------


UserName <- sub("/.*","",sub(".*Users/","",getwd()))
source(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/01_Code/02_AlignmentTest/GlobalPortCheckFunctions.R"))

ParameterFile <- ReadParameterFile(PortfolioDataFolder)
SetParameters(ParameterFile)              # Sets BatchName, Scenario, BenchmarkRegion etc. 


# ParameterFileName <- choose.files(default = paste0(PortfolioDataFolder,"99_ParameterFiles/*_ParameterFile.csv"),caption = "Select a parameter file", multi = FALSE)
# ParameterFileInfo <- file.info(ParameterFileName, extra_cols = TRUE)
# BatchName <- sub(".*/","",gsub("\\","/",sub("_ParameterFile.csv","",ParameterFileName), fixed = TRUE))
# ParameterFile <- read.csv(ParameterFileName, stringsAsFactors = FALSE, strip.white = TRUE)
# BatchName <- ParameterFile$BatchName

#-------------
# All Input parameters & make the code interactive (adjust to ParameterFile)
#------------
# Please select the input parameters here
# BBGPORTOutputName <- "FinancialDataUnique"
# Startyear <- ParameterFile$Startyear #Date when the analysis starts - time horizon is always 5 years starting from here: e.g. if Startyear is 2016 the analysis will go to 2021
# FinancialDataDate <- "2017-04-19"
BBGDataDate <- "30/12/2016"

# select sectors and technologies to be assessed and regions for the output
TechnologyList <- c("Electric","Hybrid","ICE","GasCap","CoalCap","OilCap","RenewablesCap","HydroCap","NuclearCap", "Coal","Oil","Gas")
SectorList <- c("Power","Fossil Fuels","Automotive")
OutputCountryDomicileRegionList <- c("MSCIWorld","Global")
OutputBenchmarkRegionList <- c("Global","GlobalAggregate","OECDAggregate","NonOECDAggregate","EU","US")

#------------
# Lists (e.g. tech-list, sector-list, etc.)
#------------
PowerBenchmarkRegionOECD <- c("OECDEurope", "US", "OECDAsiaOceaniaWoJP", "OECDAmericasWoUS","Japan")
PowerBenchmarkRegionNonOECD <- c("AfricaWoZA", "SouthAfrica", "Russia", "EEurope_EurasiaWoRU", "Brazil","LatinAmericaWoBR", "China", "India", "NonOECDAsiaRest", "MiddleEast") 
PowerBenchmarkRegionGlobal <- c(PowerBenchmarkRegionNonOECD,PowerBenchmarkRegionOECD)
FossilFuelBenchmarkRegions <- c("OECDAmericas" , "LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia","MiddleEast", "OECDAsiaOceania", "OECDEurope")
FossilFuelBenchmarkRegionsOECD <- c("OECDAmericas" , "OECDAsiaOceania", "OECDEurope")
FossilFuelBenchmarkRegionsNonOECD <- c("LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia","MiddleEast")
MutualExclusiveCompanyDomicileRegions <- c("MSCIEmergingMarkets", "MSCIWorld", "OutsideACWI")

#Define sector claissification based of ICB subsector, to be used within functions
UtilitiesICB <- c("Alternative Electricity","Conventional Electricity","Multiutilities")
OilGasICB <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production")
FuturesecsICB <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation")
AutoICB <- c("Automobiles","Commercial Vehicles & Trucks")

AllLists <- list(TechList = TechnologyList, SectorList = SectorList, 
                 OutputCountryDomicileRegionList = OutputCountryDomicileRegionList, OutputBenchmarkRegionList = OutputBenchmarkRegionList, 
                 PowerBenchmarkRegionOECD = PowerBenchmarkRegionOECD, PowerBenchmarkRegionNonOECD = PowerBenchmarkRegionNonOECD, PowerBenchmarkRegionGlobal = PowerBenchmarkRegionGlobal,
                 FossilFuelBenchmarkRegions = FossilFuelBenchmarkRegions, FossilFuelBenchmarkRegionsOECD = FossilFuelBenchmarkRegionsOECD, FossilFuelBenchmarkRegionsNonOECD = FossilFuelBenchmarkRegionsNonOECD,
                 MutualExclusiveCompanyDomicileRegions = MutualExclusiveCompanyDomicileRegions)


#-------------
# Set workdrive
#-------------
setwd(DataFolder)

# ------
# |------------------------------|
# |------Data Loading------|
# |------------------------------|
# ------

# a) Read in Asset Level Data / MasterData
# b) Read in Asset level Data Bridge
# c) Read in financial data (Data retrieved from BBG PORT function)
# d) Read in portoflio holdings (Portfolio holdings data given by Regulator or Instituion)
# e) Read in regions data
# f) Read in Scenario data
# g) Read in Portfolio data

# ------
# a) Read in asset level data
# ------
MasterData <- read.csv(paste0(DataFolder,"MasterData",ParameterFile$DateofFinancialData,".csv"),stringsAsFactors=FALSE,strip.white=TRUE)

# ADD NEW COAL DATA
# MasterData <- read.csv(paste0(FolderLocation,"Data/MasterData31.12.2016.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
##Add coal edits
# NewCoalData <- read.csv(paste0(DataFolder,"CoalEquityMaster_201706_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# #Clear old Coal data
# MasterData <- subset(MasterData, MasterData$Technology != "Coal")
# #Bind new Coal data
# MasterData <- rbind(MasterData, NewCoalData)


#Trim Masterdata to startyear
MasterData<-subset(MasterData, MasterData$Year >= Startyear)

# ------
# b) Read in Asset level Data Bridge
# ------
ALDBridge <- read.csv(paste0(DataFolder,"ALDEquityBridge_2017-04-25.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
EquityBridge <- read.csv(paste0(DataFolder,"EquityBridge_2017-06-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
# DebtToALDBridge <- read.csv(paste0(DataFolder,"DebtToALDBridge_2017-05-03.csv"),stringsAsFactors = FALSE, strip.white = TRUE)

# ------
# c) Read in stored company level financial data (Data retrieved from API and PORT function) 
# ------
CompanylvlBBGEquityData <- read.csv(paste0(DataFolder,"CompanylvlBBGData",ParameterFile$DateofFinancialData,".csv"),stringsAsFactors = FALSE, strip.white = TRUE)
CompanylvlBBGEquityData$FFperc <- as.numeric(CompanylvlBBGEquityData$FFperc)/100
CompanylvlBBGEquityData$FFperc[is.na(CompanylvlBBGEquityData$FFperc)] <- 1
# CompanylvlBBGEquityData$TotalEqyOut <- as.numeric(CompanylvlBBGEquityData$TotalEqyOut)
# CompanylvlBBGEquityData$TotalFloat <- CompanylvlBBGEquityData$TotalEqyOut * CompanylvlBBGEquityData$FFperc
# UnlistedDebt <- read.csv(paste0(FolderLocation,"Data/UnlistedDebt_2017-05-03.csv"), stringsAsFactors = FALSE, strip.white = TRUE) #This is debt at a corporate debt ticker level

# ------
# d) Read in financial data (Data retrieved from BBG PORT function)
# ------
BBG_Data <- read.csv(paste0(FinancialDataFolder,ParameterFile$DateofFinancialData,"/",ParameterFile$SourceFinancialData,"/FinancialData_20170925.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
BBG_Data <- rename(BBG_Data, c( "Mkt.Val..P." = "SharePrice"))
BBGPORTOutput <- BBG_Data
CompNames <- unique(subset(BBGPORTOutput, select = c("Ticker","Issuer")))
CompNames <- rename(CompNames, c("Issuer" = "Name"))

# ------
# e) Read in portfolio holdings (Portfolio holdings data given by Regulator or Instituion)
# ------
PortfolioAllPorts <- read.csv(paste0(PortfolioDataFolder,ParameterFile$ProjektName,"/",BatchName,"/",BatchName,"Port_EQY.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
# PortfolioAllPorts <- read.csv(paste0(PortfolioDataFolder,ParameterFile$ProjektName,"/",BatchName,"Port_EQY.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
# PortfolioAllPorts <- subset(PortfolioAllPorts, InvestorName == "GEPABU")

# ------
# f) Read in regions data
# ------
# Get market size information ($USD) for Listed markets
MarketSizeData <- read.csv(paste0(DataFolder,"ListedMarketSizeAUM.csv"), strip.white = TRUE, stringsAsFactors = FALSE)
MarketSizeData$MarketSize <- MarketSizeData[,eval(paste0("MarketSize",ParameterFile$DateofFinancialData))]
MarketSizeData <- subset(MarketSizeData, MarketRegion != "", select = c("MarketRegion","MarketSize"))

# Create a List of all existing Benchmark-Region and all assessed CompanyLocation-Regions
BenchRegionLists <- read.csv(paste0(DataFolder,"BenchRegions.csv"))
BenchRegionLists[is.na(BenchRegionLists)] <- ""
BenchRegionLists <- rename(BenchRegionLists, c("BenchRegions" = "BenchmarkRegions", "BenchRegions_ISO_colnames" = "BenchmarkRegions_ISO_colnames"))
BenchmarkRegionList <- data.frame(BenchmarkRegion = BenchRegionLists$BenchmarkRegions[!is.na(BenchRegionLists$BenchmarkRegions) & BenchRegionLists$BenchmarkRegions != ""], BenchmarkRegionColname = BenchRegionLists$BenchmarkRegions_ISO_colnames[!is.na(BenchRegionLists$BenchmarkRegions_ISO_colnames) & BenchRegionLists$BenchmarkRegions_ISO_colnames != ""])
CompanyDomicileRegion <- read.csv(paste0(DataFolder,"IndexRegions.csv"))
CompanyDomicileRegion <- rename(CompanyDomicileRegion, c("IndexUniverse" = "CompanyDomicileRegion", "IndexUniverseColname" = "CompanyDomicileRegionColname")) 
#if(any(is.na(CompanyDomicileRegion))) {CompanyDomicileRegion[is.na(CompanyDomicileRegion)] <- ""}
CompanyDomicileRegionList <- data.frame(CompanyDomicileRegion = CompanyDomicileRegion$CompanyDomicileRegion[!is.na(CompanyDomicileRegion$CompanyDomicileRegion) & CompanyDomicileRegion$CompanyDomicileRegion != ""], CompanyDomicileRegionColname = CompanyDomicileRegion$CompanyDomicileRegionColname[!is.na(CompanyDomicileRegion$CompanyDomicileRegionColname) & CompanyDomicileRegion$CompanyDomicileRegionColname != ""])

# Read countryname-conversion file to abbreviation
CountryISOList <- read.csv(paste0(DataFolder,"CountryISOCodes.csv"), stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c(""))
CountryISOList <- subset(CountryISOList, CountryISOList$COUNTRY != "#N/A")
CountryISOList$GDPlantLocation<-as.character(CountryISOList$GDPlantLocation)

# merge with ALD
if (ParameterFile$CalculateMarketData == TRUE){

  #First only take one FUND ticker per company in BBGdata
  BBGEquityDataSub <- CompanylvlBBGEquityData[!duplicated(CompanylvlBBGEquityData[,c("EQY_FUND_TICKER")]),]
  EconomyData <- merge(MasterData, BBGEquityDataSub, by = c("EQY_FUND_TICKER"), all.x=TRUE)
  EconomyData <- EconomyData[,!names(EconomyData) %in% c("ShareProduction")]
  
  #Allocate the portion of production that is not floating to nonlisted.
  ListedParents<- subset(EconomyData, !is.na(EconomyData$FFperc) & EconomyData$Listed == "Y")
  ListedParents$CompLvlProduction <- ListedParents$CompLvlProduction*(1 - ListedParents$FFperc)
  ListedParents$EQY_FUND_TICKER <- "NonFloatingProduction"
  ListedParents$FFperc <- NA
  
  EconomyData$FFperc <- EconomyData$FFperc
  EconomyData <- rbind(EconomyData, ListedParents)
  EconomyData$FFperc[is.na(EconomyData$FFperc)]<- 1
  EconomyData$CompLvlProduction <- EconomyData$CompLvlProduction* EconomyData$FFperc
  EconomyData <- EconomyData[,!names(EconomyData) %in% c("FFperc")]

  g <- with(EconomyData, paste(EQY_FUND_TICKER, CNTRY_OF_DOMICILE,	PlantLocation,	Sector,	Year,	Technology, StockRegion, Listed))
  x <- with(EconomyData, c(tapply(CompLvlProduction, g, sum) ))
  EconomyData <- EconomyData[match(names(x), g), c("EQY_FUND_TICKER", "CNTRY_OF_DOMICILE",	"PlantLocation", "Sector",	"Year",	"Technology", "StockRegion", "Listed")]
  EconomyData$Production <- x
  
  rm(g,x)
  
  # just slightly slower altermative
  # EconomyData <- aggregate(EconomyData["CompLvlProduction"], by=EconomyData[,c("EQY_FUND_TICKER", "CNTRY_OF_DOMICILE",	"PlantLocation", "Sector",	"Year",	"Technology", "StockRegion", "Listed")], FUN=sum)
  
  EconomyData <- subset(EconomyData, EconomyData$Production != 0)
  SaveEconomyData <- merge(EconomyData, CompNames, by.x = "EQY_FUND_TICKER", by.y = "Ticker", all.x = TRUE, all.y = FALSE)
  write.csv(SaveEconomyData, paste0(DataFolder,"EconomyData_",ParameterFile$DateofFinancialData,"_",Sys.Date(),".csv"), row.names = FALSE)


  CountryISOListUniqueISO <- CountryISOList[!duplicated(CountryISOList$COUNTRY_ISO),]
  EconomyData<-merge(EconomyData, CountryISOListUniqueISO, by.x = "CNTRY_OF_DOMICILE", by.y = "COUNTRY_ISO", all.x = TRUE)
  EconomyData$GDPlantLocation<-as.character(EconomyData$GDPlantLocation)
  EconomyData$StockRegion[EconomyData$Sector != "Power"] <- EconomyData$GDPlantLocation[EconomyData$Sector != "Power"]
  drops <- c("GDPlantLocation")
  EconomyData <- EconomyData[,!names(EconomyData) %in% drops]
  EconomyDataSmall <- subset(EconomyData, Technology %in% AllLists$TechList)
  EconomyDataSmall <- merge(EconomyDataSmall, CountryISOList, by.x = "PlantLocation", by.y = "GDPlantLocation", all.x = TRUE)
  EconomyDataSmall <- rename(EconomyDataSmall, c("COUNTRY_ISO" = "PlantLocation_ISO"))
  EconomyDataSmall$PlantLocation_ISO[EconomyDataSmall$Technology == "Coal"] <- "Global"
  
  for (j in 1:length(BenchmarkRegionList$BenchmarkRegion)){
    #print(j)
    EcoDataSub <- subset(EconomyDataSmall, PlantLocation_ISO %in% BenchRegionLists[,names(BenchRegionLists) == BenchmarkRegionList$BenchmarkRegionColname[j]])
    EcoDataSub$BenchmarkRegion <- BenchmarkRegionList$BenchmarkRegion[j]
    if(j==1){
      EcoDataAllBenchmarkRegions <- EcoDataSub
    }else{
      EcoDataAllBenchmarkRegions <- rbind(EcoDataAllBenchmarkRegions, EcoDataSub)
    }
  }
  EconomyDataAllBenchmarkRegions <- ddply(EcoDataAllBenchmarkRegions,.(BenchmarkRegion, Sector, Technology, Year),summarize,Production = sum(Production,na.rm=TRUE))
  write.csv(EconomyDataAllBenchmarkRegions, paste0(DataFolder,"/EconomyDataAllBenchmarkRegions",ParameterFile$DateofFinancialData,"_",Sys.Date(),".csv"), row.names = FALSE)


  # Calculate the total regional production owned by listed companies for calculating the benchmark-starting point (Market exposure) for global portfolios
  EconomyDatasub <- subset(EconomyDataSmall, ! EQY_FUND_TICKER %in% c("NonListedProduction", "govt","PrivateProduction"))
  EconomyDatasub <- subset(EconomyDataSmall, EconomyDataSmall$Listed == "Y")
  EconomyDatasub <- merge(EconomyDatasub, CountryISOList, by.x = "StockRegion", by.y = "GDPlantLocation", all.x = TRUE, all.y = FALSE)
  EconomyDatasub$COUNTRY_ISO [which(EconomyDatasub$CNTRY_OF_DOMICILE != "")] <- EconomyDatasub$CNTRY_OF_DOMICILE[which(EconomyDatasub$CNTRY_OF_DOMICILE != "")]
  EconomyDatasub$StockRegion <- EconomyDatasub$COUNTRY_ISO
  
  EconomyDataAllBenchmarkRegionsCompLvl <- merge(EconomyDatasub, CompNames, by.x = "EQY_FUND_TICKER", by.y = "Ticker", all.x = TRUE, all.y = FALSE)

  system.time({
  g <- with(EconomyDataAllBenchmarkRegionsCompLvl, paste(EQY_FUND_TICKER, Name, Sector,Technology, Year, PlantLocation_ISO, StockRegion))
  x <- with(EconomyDataAllBenchmarkRegionsCompLvl, c(tapply(Production, g, sum) ))
  Marketmix <- EconomyDataAllBenchmarkRegionsCompLvl[match(names(x), g), c("EQY_FUND_TICKER", "Name", "Sector","Technology", "Year", "PlantLocation_ISO", "StockRegion")]
  Marketmix$Production <- x
  })
  
  rm(g,x)

  
  
  Marketmix <- Marketmix %>% complete(PlantLocation_ISO, StockRegion)
  
  for(k in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
    MarketmixSub <- subset(Marketmix, StockRegion %in% CompanyDomicileRegion[,names(CompanyDomicileRegion) == CompanyDomicileRegionList$CompanyDomicileRegionColname[k]])
    for (j in 1:length(BenchmarkRegionList$BenchmarkRegion)){
      # print(j)
      MarketMixSubBM <- subset(MarketmixSub, PlantLocation_ISO %in% BenchRegionLists[,names(BenchRegionLists) == BenchmarkRegionList$BenchmarkRegionColname[j]])
      MarketMixSubBM$BenchmarkRegion <- BenchmarkRegionList$BenchmarkRegion[j]
      if(j==1){
        MarketData <- MarketMixSubBM
      }else{
        MarketData <- rbind(MarketData, MarketMixSubBM)
      }
    }
    MarketData$CompanyDomicileRegion <- CompanyDomicileRegionList$CompanyDomicileRegion[k]
    if(k==1){
      MarketDataAll <- MarketData
    }else{
      MarketDataAll <- rbind(MarketDataAll, MarketData)
    }
    rm(MarketData)
  }


  #A faster version of ddply 22s vs 246s
  system.time({
    g <- with(MarketDataAll, paste(BenchmarkRegion, Sector, Name, EQY_FUND_TICKER, Technology, Year, CompanyDomicileRegion))
    x <- with(MarketDataAll, c(tapply(Production, g, sum) ))
    MarketDataCompLvl <- MarketDataAll[match(names(x), g), c("BenchmarkRegion", "Sector", "Name", "EQY_FUND_TICKER", "Technology", "Year", "CompanyDomicileRegion")]
    MarketDataCompLvl$Production <- x
  })
  
  write.csv(MarketDataCompLvl, paste0(DataFolder,"CompanyLevelData_",Sys.Date(),".csv"), row.names = FALSE)
  
  #Remove duplicate production for multi_fund _tickers for the same Asset level data entity. E.g BHP AU, BLT LN etc.
  # ALDBridgesub<-subset(ALDBridge, select= c("GDCompanyID","EQY_FUND_TICKER"))
  # MarketData <- merge(MarketDataCompLvl,ALDBridgesub, by = "EQY_FUND_TICKER",all.x = TRUE)
  # MarketData <- subset(MarketData , select = -c(Name, EQY_FUND_TICKER))
  # MarketData2 <- unique(MarketData)
  MarketData <- subset(MarketDataCompLvl, !MarketDataCompLvl$EQY_FUND_TICKER %in% c("BLT LN", "NonFloatingProduction", "NonListedProduction", "govt"))
  MarketData <- ddply(MarketData,.(BenchmarkRegion, Sector, Technology, Year, CompanyDomicileRegion),summarize,Production = sum(Production,na.rm=TRUE))
  
  MarketData <- datacompletion(MarketData)
  
  MarketSectorref <- ddply(subset(MarketData, Year == Startyear),.(BenchmarkRegion, Sector, CompanyDomicileRegion),summarize, RefProdMarketSector = sum(Production,na.rm=TRUE))
  MarketTechref <- subset(MarketData, Year == Startyear, select = -c(Year))
  MarketTechref <- rename(MarketTechref, c("Production" = "RefProdMarketTech"))
  
  MarketData <- merge(MarketData,MarketSectorref, by = c("BenchmarkRegion", "Sector", "CompanyDomicileRegion"), all.x=TRUE, all.y=FALSE)
  MarketData <- merge(MarketData,MarketTechref, by = c("BenchmarkRegion", "Sector", "CompanyDomicileRegion", "Technology" ), all.x=TRUE, all.y=FALSE)
  
  saveMarketData <- MarketData
  
  write.csv(MarketData, paste0(DataFolder,"MarketData.csv"), row.names = FALSE, na = "")
  # write.csv(Marketmix,paste0("Data/MarketMixWithCompanyLvlData_",Sys.Date(),".csv"),row.names = FALSE, na = "")
}else{
  MarketData <- read.csv(paste0(DataFolder,"MarketData.csv"), stringsAsFactors = FALSE, strip.white = TRUE)
}

#Calculate technology share in the the starting year (This will be scaled to portfolio size later on to calculate the starting point of the 2Â°C benchmark)
MarketRef <- subset(MarketData, Year == Startyear, select = c("BenchmarkRegion","Sector","Technology", "CompanyDomicileRegion", "RefProdMarketTech", "RefProdMarketSector" )) #CompanyDomicileRegion == EvalRegion,
MarketRef$MarketTechShare[MarketRef$Sector == "Fossil Fuels"] <- 1
MarketRef$MarketTechShare[MarketRef$Sector != "Fossil Fuels"] <- MarketRef$RefProdMarketTech[MarketRef$Sector != "Fossil Fuels"] / MarketRef$RefProdMarketSector[MarketRef$Sector != "Fossil Fuels"]
MarketRef <- subset(MarketRef)
#MarketRef <- rename(MarketRef, c("Production" = "RefMarketProduction"))



# ------
# g) Read in scenario data
# ------
#Calculate fair share ratios

if (ParameterFile$CalculateIEATArgets == TRUE){
  
  # IEATargets <- read.csv(paste0(DataFolder,"IEATargets_linear",ParameterFile$IEAScenarioYear,".csv"),stringsAsFactors=FALSE)
  
  IEATargetsNew <- read.csv(paste0(DataFolder,"IEATargets_WEO2016+ETP2017.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
  IEATargets <- subset(IEATargetsNew, (Source == "WEO2016" & Sector %in% c("Power","Fossil Fuels")) | Source == "ETP2017" & Sector == "Automotive", select = -c(Source))
  IEATargets$Scenario[IEATargets$Scenario == "2DS" & IEATargets$Sector == "Automotive"] <- "450S"
  
  #shift to long form and clean database to reduce to relevant units and sectors
  IEATargets <- melt(IEATargets, id.vars = c("Technology","Region","Scenario","Sector","Units"),variable.name = "Year", value.name = "AnnualvalIEAtech")
  IEATargets <- subset(IEATargets, Sector %in% c("Power","Automotive","Fossil Fuels") & Units %in% c("GW","tce","cm","b/a","#") & !Technology %in% c("Diesel","LPGCNG", "Petrol"))
  
  #trim names to give numeric values
  IEATargets$Year <- str_sub(IEATargets$Year,-4,-1)
  IEATargets$Region[IEATargets$Region == "World"] <- "Global"
  IEATargets$Region[IEATargets$Region == "Europe"] <- "EU"
  IEATargets$Region[IEATargets$Region == "Americas"] <- "US"
  
  # Convert GW values in MW values in the Power sector and clean database (just keep Regions used)
  # RegionList = c("EU","Global","OECD","Non-OECD","US")
  # IEATargets <- subset(IEATargets, Region %in% RegionList)
  IEATargets$AnnualvalIEAtech [IEATargets$Sector == "Power" & IEATargets$Units == "GW"] <- as.numeric(IEATargets$AnnualvalIEAtech [IEATargets$Sector == "Power" & IEATargets$Units == "GW"]) * 1000
  IEATargets$Units [IEATargets$Sector == "Power" & IEATargets$Units == "GW"] <- "MW"
  
  # Create "new" regions to allow assessment and most regional level (e.g. OECDAmerica without US, OECD Asia without Japan, etc.)
  IEATargets$AnnualvalIEAtech <- as.numeric(IEATargets$AnnualvalIEAtech)
  saveIEATars <- IEATargets
  IEATargetswide <- dcast(subset(IEATargets, Sector == "Power"), Technology + Scenario + Sector + Units + Year ~ Region, value.var = "AnnualvalIEAtech")
  IEATargetswide$OECDAmericasWoUS <- IEATargetswide$OECDAmericas - IEATargetswide$US # OECD-Americas with out US consist of: Chile, Canada and Mexico
  IEATargetswide$OECDAsiaOceaniaWoJP <- IEATargetswide$OECDAsiaOceania - IEATargetswide$Japan # OECD-Asia with out Japan consist of: 
  IEATargetswide$LatinAmericaWoBR <- IEATargetswide$LatinAmerica - IEATargetswide$Brazil # Latin America with out Brazil consist of: 
  IEATargetswide$AfricaWoZA <- IEATargetswide$Africa - IEATargetswide$SouthAfrica # Afrika with out South Africa consist of:
  IEATargetswide$EEurope_EurasiaWoRU <- IEATargetswide$EEurope_Eurasia - IEATargetswide$Russia # Eastern Europe & Eurasia with out Russia consists of: 
  IEATargetswide$NonOECDAsiaRest <- IEATargetswide$`NonOECDAsia` - (IEATargetswide$China + IEATargetswide$India) # Non OECD Asia Rest consists of Non-OECD asia without China and India: 
  # IEATargetswide$NonOECDRest <- IEATargetswide$`Non-OECD` - (IEATargetswide$Africa + IEATargetswide$`Non-OECDAsia` + IEATargetswide$LatinAmerica + IEATargetswide$MiddleEast + IEATargetswide$EEurope_Eurasia) # this should be 0 but isn?t..
  IEATargetsPower <- melt(IEATargetswide,id.vars = c("Technology","Year","Scenario","Sector", "Units"),variable.name = "Region", value.name = "AnnualvalIEAtech")
  IEATargets <- rbind(subset(IEATargets, Sector != "Power"), IEATargetsPower)
  
  # Calculate Renewable Energy Capacity Targets by summing over all RE-Technologies
  RETargets <- subset(IEATargets, IEATargets$Technology %in% c("BioenergyCap", "CSPCap", "GeothermalCap", "MarineCap", "SolarPVCap", "WindCap"))
  IEATargets <- subset(IEATargets, !IEATargets$Technology %in% c("BioenergyCap", "CSPCap", "GeothermalCap", "MarineCap", "SolarPVCap", "WindCap"))
  RETargets<- ddply(RETargets,.(Scenario, Region, Sector, Year, Units), summarize, AnnualvalIEAtech=sum(as.numeric(AnnualvalIEAtech), na.rm=TRUE))
  RETargets$Technology <- "RenewablesCap"
  
  # Merging RE-Targets with other targets and cleaning database to include only the relevant years (for the assessment)
  IEATargets<-rbind(IEATargets,RETargets)
  IEATargets <- subset(IEATargets, Year >= Startyear)
  
  # Get the reference value in the starting year of the analysis to calculate percentage additions based on this 
  refvalIEApre <- subset(IEATargets,Year == Startyear)
  refvalIEApre <- rename(refvalIEApre, c("AnnualvalIEAtech" = "refvalIEAtech"))
  
  # Set evaluation method (declining technologies use the technology approach, while increasing technologies are assessed with the sector-approach)
  EndRefValIEA <- subset(IEATargets,Year == Startyear+5)
  EndRefValIEA <- subset(EndRefValIEA, select = c("Technology","Region","Scenario","Sector","Units","AnnualvalIEAtech"))
  EndRefValIEA<- rename(EndRefValIEA , c("AnnualvalIEAtech" = "EndRefValIEA"))
  MethodChoose <- merge(subset(refvalIEApre,select = c("Technology","Region","Scenario","Sector","Units","refvalIEAtech")),EndRefValIEA, by = c("Technology","Region","Scenario","Sector","Units"))
  MethodChoose$Direction <- "increasing"
  MethodChoose$Direction[MethodChoose$refvalIEAtech >= MethodChoose$EndRefValIEA] <- "declining" 
  MethodChoose <- subset(MethodChoose,select = c("Technology","Region","Scenario","Sector","Units","Direction"))
  
  #subset to derive sector level ref values
  refvalIEA <- subset(refvalIEApre,select = c("Region","Scenario","Sector","refvalIEAtech"))
  #sum at sector level for tech FS
  refvalIEA$refvalIEAtech<-as.numeric(refvalIEA$refvalIEAtech)
  refvalIEAsecs <- ddply(refvalIEA,.(Region,Scenario,Sector),summarize,refvalIEAsec = sum(refvalIEAtech))
  refvalIEA <- merge(refvalIEApre,refvalIEAsecs,by=c("Region","Scenario","Sector"))
  refvalIEA <- refvalIEA[,!(names(refvalIEA) %in% "Year")]
  
  # Merge the reference value of the starting year to all years and calculate the percentual fair share changes 
  IEATargets <- merge(IEATargets,refvalIEA,by=c("Region","Scenario","Sector","Technology","Units"),all.x=TRUE,all.y=FALSE)
  IEATargets <- merge(IEATargets,MethodChoose, by=c("Region","Scenario","Sector","Technology", "Units"), all.x = TRUE, all.y = FALSE)
  IEATargets$AnnualvalIEAtech<-as.numeric(IEATargets$AnnualvalIEAtech)
  IEATargets$refvalIEAsec<-as.numeric(IEATargets$refvalIEAsec)
  IEATargets$refvalIEAtech<-as.numeric(IEATargets$refvalIEAtech)
  IEATargets$refvalIEAsec [IEATargets$Sector == "Fossil Fuels"] <- IEATargets$refvalIEAtech [IEATargets$Sector == "Fossil Fuels"]
  IEATargets$mktFSRatio <- (IEATargets$AnnualvalIEAtech - IEATargets$refvalIEAtech)/IEATargets$refvalIEAsec
  IEATargets$techFSRatio <- (IEATargets$AnnualvalIEAtech - IEATargets$refvalIEAtech)/IEATargets$refvalIEAtech
  
  
  IEATargets <- IEATargets[order(IEATargets$Scenario,IEATargets$Region,IEATargets$Sector, IEATargets$Technology,IEATargets$Year),] 
  IEATargets$Region[IEATargets$Sector == "Automotive"] <- "Global"
  IEATargets <- IEATargets[!duplicated(IEATargets),]
  
  IEATargets$FairSharePerc<-IEATargets$mktFSRatio
  IEATargets$FairSharePerc[IEATargets$Direction == "declining"] <- IEATargets$techFSRatio[IEATargets$Direction == "declining"]
  
  IEATargets <- rename(IEATargets, c("Region" ="BenchmarkRegion"))
  IEATargetssub <- subset(IEATargets, Year <= (Startyear + 10), select = c("Sector","Technology","Year","BenchmarkRegion","FairSharePerc","Scenario","Direction")) # select scenario 450 if problems with the results otherwise!
  
  if(file.exists(paste0(DataFolder,"IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions.csv"))){
    # file.copy(paste0(DataFolder,"IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions.csv"), paste0(DataFolder,"Archive/IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions_UsedUntil",Sys.Date(),".csv"))
    file.rename(paste0(DataFolder,"IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions.csv"), paste0(DataFolder,"00_Archive/IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions_UsedUntil",Sys.Date(),".csv"))
  }
  write.csv(IEATargets, paste0(DataFolder,"IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions.csv"), row.names = FALSE, na = "")
}else{
  IEATargets <-  read.csv(paste0(DataFolder,"IEATargets",ParameterFile$IEAScenarioYear,"_AllRegions.csv"), stringsAsFactors = FALSE, strip.white = TRUE)
  IEATargetssub <- subset(IEATargets, Year <= (Startyear + 10), select = c("Sector","Technology","Year","BenchmarkRegion","FairSharePerc","Scenario","Direction")) # select scenario 450 if problems with the results otherwise!
}

# ------
# h) Read in portfolio data
# ------

#Get List of portfolios either from folder or from 1 file (fund-check format) depending on Bulk-Methode
#Clean Holdingds data
MissingISIN <- subset(PortfolioAllPorts, ISIN == "")
PortfolioAllPorts <- subset(PortfolioAllPorts, ISIN != "")
saveAllPorts <- PortfolioAllPorts
PortfolioAllPorts <- saveAllPorts

if("Cnty.of.Dom" %in% colnames(BBGPORTOutput) & !"CNTRY_OF_DOMICILE" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Cnty.of.Dom" = "CNTRY_OF_DOMICILE"))
}else if("Country.ISO.Code" %in% colnames(BBGPORTOutput) & !"CNTRY_OF_DOMICILE" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Country.ISO.Code" = "CNTRY_OF_DOMICILE"))
}

#Merge Funds with BBG Data, ADR data (for foreign companies issued in the US through ADRs)
# BBGPORTOutput <- subset(BBG_Data, !ISIN %in% c("#N/A N/A","") | !is.na(BBG_Data$ISIN)) # use this when using BBG-Data-Bind_v2
BBGPORTOutput <- subset(BBGPORTOutput, !(ISIN %in% c("#N/A N/A","") | is.na(BBGPORTOutput$ISIN)), c("Ticker" , "Subgroup", "Group" , "ICB.Subsector.Name", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE"))
PortfolioAllPorts <- merge(PortfolioAllPorts, BBGPORTOutput, by = c("ISIN"),all.x = TRUE)

# Filter for funds
PortfolioAllPorts <- subset(PortfolioAllPorts, !Group %in% grep("Fund",unique(PortfolioAllPorts$Group), value = TRUE) & SharePrice != "" & Group != "Alternative Investment")

###Change this to the new Bridge file
EquityBridgeSub <- unique(EquityBridge)
PortfolioAllPorts <- merge(PortfolioAllPorts,EquityBridgeSub, by.x = "Ticker", by.y = "TICKER_AND_EXCH_CODE",all.x = TRUE)

MissingBBGInfo <- unique(subset(PortfolioAllPorts, is.na(SharePrice) | SharePrice == 0, select = "ISIN"))
if(nrow(MissingBBGInfo) > 0) {
  MissingBBGInfo$QTY <- 1
  MissingBBGInfo$Date <- BBGDataDate}


# PortfolioAllPorts <- rename(PortfolioAllPorts, c("NumberofShares"="Number.of.shares")) 

if(!"Position" %in% colnames(PortfolioAllPorts)){
  if("ValueUSD" %in% colnames(PortfolioAllPorts)){
    PortfolioAllPorts <- subset(PortfolioAllPorts, !is.na(SharePrice) & SharePrice != 0)
    PortfolioAllPorts$Position <- PortfolioAllPorts$ValueUSD / PortfolioAllPorts$SharePrice
  }else  if("Number.of.shares" %in% colnames(PortfolioAllPorts)){
    PortfolioAllPorts$Position <- PortfolioAllPorts$Number.of.shares
  }else{
    print("Check portfolio input: No position input or value given")
  }
}

#Get rid of NA?s and negative or NAN values in Number of shares
PortfolioAllPorts$Position <- as.numeric(PortfolioAllPorts$Position)
PortfolioAllPorts$Position[PortfolioAllPorts$Position <= 0] <- 0
PortfolioAllPorts <- subset(PortfolioAllPorts, !is.na(Position))
PortfolioAllPorts$PortfolioName <- str_replace_all(PortfolioAllPorts$PortfolioName, "[[:punct:]]", "")
PortfolioAllPorts$InvestorName <- str_replace_all(PortfolioAllPorts$InvestorName, "[[:punct:]]", "")
PortfolioAllPorts$PortfolioName <- str_replace_all(PortfolioAllPorts$PortfolioName, "", "")
PortfolioAllPorts$InvestorName <- str_replace_all(PortfolioAllPorts$InvestorName, "", "")
PortfolioAllPorts$PortfolioName <- str_replace_all(PortfolioAllPorts$PortfolioName, "", "")
PortfolioAllPorts$InvestorName <- str_replace_all(PortfolioAllPorts$InvestorName, "", "")
PortfolioAllPorts$PortfolioName <- trim(PortfolioAllPorts$PortfolioName)
PortfolioAllPorts$InvestorName <- trim(PortfolioAllPorts$InvestorName)
PortfolioAllPorts$InvestorName <- gsub(" ", "", PortfolioAllPorts$InvestorName, fixed = TRUE)
PortfolioAllPorts$PortfolioName <- gsub(" ", "", PortfolioAllPorts$PortfolioName, fixed = TRUE)

PortfolioNameCount <- data.frame(table(PortfolioAllPorts$PortfolioName))
BrandList <- data.frame(InvestorName = unique(PortfolioAllPorts$InvestorName))
BrandList$Type <- "Investor"
BrandList$PortfolioName <- NA
UniquePortList <- data.frame (unique(subset(PortfolioAllPorts, select = c("PortfolioName","InvestorName"))))
UniquePortList$Type <- "Portfolio"
# if (max(PortfolioNameCount$Freq) > 1){
#PortList <- data.frame(PortfolioName = unique(PortfolioAllPorts$PortfolioName))
#PortList$InvestorName <- NA
#PortList$Type <- "FundType"
#ListAllPorts <- rbind(PortList,BrandList,UniquePortList)
# }else{
ListAllPorts <- rbind(BrandList, UniquePortList, data.frame("InvestorName" = "ListedMarket", "Type" = "Portfolio", "PortfolioName" = "ListedMarket"))
# }

# ------
# |------------------------------|
# |----- Portfolio Analysis -----|
# |------------------------------|
# ------
for (i in  1:length(ListAllPorts$PortfolioName)) {
  tryCatch({
    print(i)
  
    if (ListAllPorts$Type[i] == "Portfolio"){
      Portfolio = subset(PortfolioAllPorts, PortfolioName == ListAllPorts$PortfolioName[i] & InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- paste0(ListAllPorts$PortfolioName[i],"_",ListAllPorts$InvestorName[i])
      # } else if(ListAllPorts$Type[i] == "FundType"){
      #Portfolio = subset(PortfolioAllPorts, PortfolioName == ListAllPorts$PortfolioName[i])
      #PortfolioName <- ListAllPorts$PortfolioName[i]
    }else{
      Portfolio = subset(PortfolioAllPorts, InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- ListAllPorts$InvestorName[i]
    }
    
    
    Portfolio <- subset(Portfolio, select = c("EQY_FUND_TICKER" , "Position", "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE"))
    
    #Sum over same ISIN in one Brand
    # use ddply or tapply/faster ddply version for this and save duplicates in sanity check file
    ISINCount <- as.data.frame(table(Portfolio$ISIN))
    
    if (dim(Portfolio)[1]>0){
    Portfolio <- aggregate(Portfolio["Position"], by = Portfolio[, c("EQY_FUND_TICKER" , "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE")], FUN=sum)
    }
    # if (length(ISINCount)>1){
    # system.time({
    #   listdoubles <- as.list(subset(ISINCount, Freq > 1 & Var1 != "", select = "Var1"))
    #   SanityCheckISINdups <- subset(Portfolio, ISIN %in% listdoubles$Var1) 
    #   g <- with(Portfolio, paste(EQY_FUND_TICKER, Position, Subgroup, ICB.Subsector.Name, Ticker, ISIN, SharePrice, CNTRY_OF_DOMICILE))
    #   x <- with(Portfolio, c(tapply(Position, g, sum) ))
    #   Portfolio <- Portfolio[match(names(x), g), c("EQY_FUND_TICKER" , "Subgroup" , "ICB.Subsector.Name", "Ticker", "ISIN", "SharePrice", "CNTRY_OF_DOMICILE")]
    #   Portfolio$Position <- x
    # })}

    #Calculate assets under management and total number of shares if there is no given toal number of shares
    #Clean price list
    Portfolio$SharePrice[Portfolio$SharePrice == "#N/A N/A"] <- 0
    
    ##if there is no price information or if the asset is outside fo the region
    Portfolio$SharePrice <- as.numeric(Portfolio$SharePrice)
    Portfolio$Position <- as.numeric(Portfolio$Position)
    
    if (sum(Portfolio$Position[Portfolio$SharePrice != 0 & !is.na(Portfolio$SharePrice)], na.rm = TRUE) != 0) {
      # Subset the portfolio by securities that cannot be assessed, i.e. with missing price or country information
      PortMissingInfo <- subset(Portfolio, SharePrice == "#N/A N/A" | CNTRY_OF_DOMICILE == "#N/A Invalid Security")
      Portfolio <-  subset(Portfolio, SharePrice != "#N/A N/A" & CNTRY_OF_DOMICILE != "#N/A Invalid Security")
      Portfolio$AUM <- Portfolio$Position * Portfolio$SharePrice
      # ClimateWorks-line to include securities without all: Add line to include lines without Position information
      # Portfolio$AUM[is.na(Portfolio$Position) & !is.na(Portfolio$ValueUSD)] <- Portfolio$ValueUSD[is.na(Portfolio$Position) & !is.na(Portfolio$ValueUSD)] 
      PortAUM <- sum(Portfolio$AUM, na.rm = TRUE)
      Portfolio$PortWeight <- Portfolio$AUM / PortAUM
      
      # Introduce regional split up of Portfolio AUM!!!
      AUMmixInput <-
        ddply(Portfolio,.(CNTRY_OF_DOMICILE), summarize, AUM = sum(AUM, na.rm = TRUE))
      for (f in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)) {
        AUMsub <-
          data.frame(AUM = sum(AUMmixInput$AUM[AUMmixInput$CNTRY_OF_DOMICILE %in% CompanyDomicileRegion[, names(CompanyDomicileRegion) == CompanyDomicileRegion$CompanyDomicileRegionColname[f]]]))
        AUMsub$Region <-
          CompanyDomicileRegionList$CompanyDomicileRegion[f]
        if (exists("AUMmixOutput") == FALSE) {
          AUMmixOutput <- AUMsub
        } else{
          AUMmixOutput <- rbind(AUMmixOutput, AUMsub)
        }
      }
      AUMmix <- AUMmixOutput
      rm(AUMsub, AUMmixOutput)
      
      # Add sector to the Portfolio
      SectorProduction <- unique(subset(MasterData, select = c("EQY_FUND_TICKER","Sector")))
      Portfolio <- merge(Portfolio, SectorProduction, by = "EQY_FUND_TICKER", all.x = TRUE, all.y = FALSE)
      
      #Meta-Analysis for piechart & Moodys Risk map
      # Sector exposure merging
      Portfolio$piesector <- "Not Assessed" #label non-benchmarked sectors
      Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% OilGasICB] <- "Fossil Fuels"
      Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% AutoICB] <- "Automotive"
      Portfolio$piesector[which(Portfolio$ICB.Subsector.Name %in% UtilitiesICB & Portfolio$Sector %in% "Power")] <- "Utility Power"
      Portfolio$piesector[which(!Portfolio$ICB.Subsector.Name %in% UtilitiesICB & Portfolio$Sector %in% "Power")] <- "NonUtility Power"
      # Portfolio$piesector[Portfolio$piesector == "Not Assessed" & Portfolio$Sector %in% "Fossil Fuels"] <- "NonUtility Fossil Fuels"
      
      # set weighting of companies that are duplicates (ff or auto and non-utility) to 0 for the non-utility part
      temp <- subset(as.data.frame(table(Portfolio$ISIN)),Freq > 1)
      temp2 <- subset(Portfolio, ISIN %in% temp$Var1 & piesector %in% c("NonUtility Power","Not Assessed"))
      temp2 <- subset(as.data.frame(table(temp2$ISIN)),Freq > 1)
      if(dim(temp)[1]>0){
        Portfolio$PortWeight[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
        Portfolio$AUM[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
        Portfolio$Position[Portfolio$piesector != Portfolio$Sector & Portfolio$ISIN %in% temp$Var1 & !(Portfolio$piesector == "Utility Power" & Portfolio$Sector == "Power") & !(Portfolio$piesector == "NonUtility Power" & Portfolio$ISIN %in% temp2$Var1)] <- 0
        }
      
      Portfolio$piesector[Portfolio$ICB.Subsector.Name %in% FuturesecsICB] <- Portfolio$ICB.Subsector.Name[Portfolio$ICB.Subsector.Name %in% FuturesecsICB]
      
      PortfolioSub <- ddply(Portfolio,.(EQY_FUND_TICKER,CNTRY_OF_DOMICILE),summarize, Position = sum(Position))
      
      # Merge with Asset level Data
      ReducedList <- merge(MasterData, PortfolioSub, by.x = c("EQY_FUND_TICKER","CNTRY_OF_DOMICILE"), by.y = c("EQY_FUND_TICKER","CNTRY_OF_DOMICILE"), all.x=FALSE, all.y=FALSE)
      
      # Calculate portfolio production
      if(dim(ReducedList)[1]>0){
        ReducedList$ShareProduction[is.na(ReducedList$ShareProduction)] <- 0
        ReducedList$Position<-as.numeric(ReducedList$Position)
        # Adjust number of shares if it's an American Depository Share.
        ReducedList$Position[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"]<-ReducedList$Position[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"]/as.numeric(ReducedList$ADR_ADR_PER_SH[!ReducedList$ADR_ADR_PER_SH == "#N/A Field Not Applicable"])
        # Calculate portfolio production
        ReducedList$Production = as.numeric(ReducedList$ShareProduction) * ReducedList$Position #if it's a portfolio, production is from the total number of owned shares, if and index or market it from the total free floating shares
        # Minimise data frame size by restricti results to only a 10 year forcast
        ReducedList <- subset (ReducedList, Year <= (Startyear + 10))
        ReducedList <- merge(ReducedList, CountryISOList, by.x = "PlantLocation", by.y = "GDPlantLocation", all.x = TRUE)
        ReducedList <- rename(ReducedList, c("COUNTRY_ISO" = "PlantLocation_ISO"))
        # If there is no plant location for the fossil fuel production the production is considered to have come the country of doimicle of the owner
        ReducedList$PlantLocation_ISO[is.na(ReducedList$PlantLocation_ISO) & ReducedList$Sector == "Fossil Fuels"] <-ReducedList$CNTRY_OF_DOMICILE[is.na(ReducedList$PlantLocation_ISO) & ReducedList$Sector == "Fossil Fuels"]
        
        # Portfoliomix <- ddply(ReducedList,.(Sector,Technology, Year, PlantLocation_ISO, CNTRY_OF_DOMICILE),summarize,Production = sum(Production,na.rm=TRUE))
        #aggregate instead of ddply!!!
        Portfoliomix <- aggregate(ReducedList["Production"], by=ReducedList[,c("Sector","Technology", "Year", "PlantLocation_ISO", "CNTRY_OF_DOMICILE")], FUN=sum)
        
        
        for(k in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
          # print(k)
          PortfoliomixSub <- subset(Portfoliomix, CNTRY_OF_DOMICILE %in% CompanyDomicileRegion[,names(CompanyDomicileRegion) == CompanyDomicileRegionList$CompanyDomicileRegionColname[k]])
          if(dim(PortfoliomixSub)[1] > 0) {
            for (j in 1:length(BenchmarkRegionList$BenchmarkRegion)){
              # print(j)
              PortfolioMixBM <- subset(PortfoliomixSub, PlantLocation_ISO %in% BenchRegionLists[,names(BenchRegionLists) == BenchmarkRegionList$BenchmarkRegionColname[j]])
              
              if(dim(PortfolioMixBM)[1] > 0) {
                PortfolioMixBM$BenchmarkRegion <- BenchmarkRegionList$BenchmarkRegion[j]
                if(exists("PortfolioData") == FALSE){
                  PortfolioData <- PortfolioMixBM
                  rm(PortfolioMixBM)
                }else{
                  PortfolioData <- rbind(PortfolioData, PortfolioMixBM)
                  rm(PortfolioMixBM)
                }
              }
            }
            PortfolioData$CompanyDomicileRegion <- CompanyDomicileRegionList$CompanyDomicileRegion[k]
            if(exists("PortfolioDataAll") == FALSE){
              PortfolioDataAll <- PortfolioData
              rm(PortfolioData)
            }else{
              PortfolioDataAll <- rbind(PortfolioDataAll, PortfolioData)
              rm(PortfolioData)
            }
          }
        }
        
        # Portfoliomix <- ddply(PortfolioDataAll,.(Sector,Technology, Year, BenchmarkRegion, CompanyDomicileRegion),summarize,Production = sum(Production,na.rm=TRUE))
        Portfoliomix <- aggregate(PortfolioDataAll["Production"], by=PortfolioDataAll[,c("Sector","Technology", "Year", "BenchmarkRegion", "CompanyDomicileRegion")], FUN=sum)
        rm(PortfolioDataAll)
        Portmix <- Portfoliomix
        Portmix <- datacompletion(Portmix)
        
      }
    }
    
    ## if you are assessing the market then use the MarketData as the porfolio, and create the market AUMmix as the total AUM of the market
    if(ListAllPorts$InvestorName[i] == "ListedMarket"){
      Portmix <- subset(MarketData, select = -c(RefProdMarketTech, RefProdMarketSector))
      AUMmix <- rename(MarketSizeData, c("MarketRegion" = "Region", "MarketSize" = "AUM"))
      }
    
    if(sum(Portfolio$Position[Portfolio$SharePrice != 0 & !is.na(Portfolio$SharePrice)] & dim(ReducedList)[1]>0 , na.rm = TRUE) != 0 | ListAllPorts$InvestorName[i] == "ListedMarket"){
      
      # Calculate the reference values (for the technology as well as for the sector in the start year/initial year) and merge it with the portfolio production mix data
      Sectorref <- ddply(subset(Portmix, Year == Startyear & Sector %in% c("Automotive","Power")),.(BenchmarkRegion, CompanyDomicileRegion,Sector),summarize,RefSectorProd = sum(Production,na.rm=TRUE))
      techlist2 <- unique(subset(Portmix, Year == Startyear & Sector %in% c("Automotive","Power"), select = c("Sector","Technology")))
      Sectorref <- merge(Sectorref,techlist2, by = "Sector", all.x = TRUE, all.y = TRUE)
      Portmix <- merge(Portmix,Sectorref, by = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology"), all.x=TRUE, all.y=FALSE)
      RefTechProd <- subset(Portmix, Year == Startyear, select = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology","Production"))
      RefTechProd <- rename(RefTechProd, c("Production" = "RefTechProd"))
      Portmix <- merge(Portmix,RefTechProd, by = c("BenchmarkRegion", "CompanyDomicileRegion", "Sector", "Technology"), all.x=TRUE, all.y=FALSE)
      Portmix$RefSectorProd[Portmix$Sector == "Fossil Fuels"] <- Portmix$RefTechProd[Portmix$Sector == "Fossil Fuels"]
      
      Combin <- merge(Portmix,MarketRef, by = c("BenchmarkRegion", "CompanyDomicileRegion","Sector", "Technology"), all = TRUE)
      
      # Calculate scaled reference production
      Combin$RefMarketScaledProd <- Combin$RefSectorProd * Combin$MarketTechShare
      
      for (l in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
        Combin$PortAUM[Combin$CompanyDomicileRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]] <- AUMmix$AUM[AUMmix$Region == CompanyDomicileRegionList$CompanyDomicileRegion[l]]
      }
      # Combin$AUMRefMarketScaledProd <- PortAUM * Combin$MarketAUMTechShare
      
      #Merge with IEA targets
      Combin <- merge(Combin,IEATargetssub, by = c("BenchmarkRegion","Sector","Technology","Year"), all.x = TRUE)
      Combin <- subset(Combin, !is.na(Direction)) 
      
      ### Calculate benchmark production values
      ## Relative to the market and including current assets and future plans 
      Combin$TargetProductionAlignment <- Combin$RefMarketScaledProd + Combin$RefSectorProd * Combin$FairSharePerc
      Combin$TargetProductionAlignment[Combin$Direction == "declining"] <- Combin$RefMarketScaledProd[Combin$Direction == "declining"] * (1+Combin$FairSharePerc[Combin$Direction == "declining"])
      
      ## Future plans only
      Combin$ScenarioTrajectoryProd <- Combin$RefTechProd + Combin$RefSectorProd * Combin$FairSharePerc
      Combin$ScenarioTrajectoryProd[which(Combin$Direction == "declining")] <- Combin$RefTechProd[which(Combin$Direction == "declining")] + Combin$RefTechProd[which(Combin$Direction == "declining")] * Combin$FairSharePerc[which(Combin$Direction == "declining")]
      
      ## AUM approach, Get market AUM for each CompanyDomicileRegion (which is each investment universe)
      for (l in 1:length(CompanyDomicileRegionList$CompanyDomicileRegion)){
        Combin$MarketAUM[Combin$CompanyDomicileRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]] <- MarketSizeData$MarketSize[MarketSizeData$MarketRegion == CompanyDomicileRegionList$CompanyDomicileRegion[l]]
      }
      
      Combin$TargetProductionAUMIntensity <- (Combin$RefProdMarketTech + (Combin$RefProdMarketSector * Combin$FairSharePerc)) * (Combin$PortAUM / Combin$MarketAUM)
      Combin$TargetProductionAUMIntensity[Combin$Direction == "declining"] <- Combin$RefProdMarketTech[Combin$Direction == "declining"] * (1 + Combin$FairSharePerc[Combin$Direction == "declining"]) * (Combin$PortAUM[Combin$Direction == "declining"] / Combin$MarketAUM[Combin$Direction == "declining"])
      Combin$TargetProductionAUMIntensity[Combin$Sector == "Fossil Fuels"] <- Combin$RefProdMarketTech[Combin$Sector == "Fossil Fuels"] * (1 + Combin$FairSharePerc[Combin$Sector == "Fossil Fuels"]) * (Combin$PortAUM[Combin$Sector == "Fossil Fuels"] / Combin$MarketAUM[Combin$Sector == "Fossil Fuels"])
      
      
      #Combin$ScenarioTrajectoryProd[which(Combin$Direction == "declining")] <- Combin$RefTechProd[which(Combin$Direction == "declining")] + Combin$RefTechProd[which(Combin$Direction == "declining")] * Combin$FairSharePerc[which(Combin$Direction == "declining")]
      
      ## Aggregate benchmark production for Global Aggregate
      #Subset Combin the mutual exclusive benchmarking regions for each sector
      GlobalAggregate <- subset(Combin, CompanyDomicileRegion %in% c("Global", AllLists$MutualExclusiveCompanyDomicileRegions) & 
                                  (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | 
                                  (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                                  (Sector == "Fossil Fuels" & Technology != "Coal" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | 
                                  (Sector == "Fossil Fuels" & Technology == "Coal" & BenchmarkRegion %in% "Global"))
      GlobalAggregate$BenchmarkRegion2 = "GlobalAggregate"
      
      OECDAggregate <- subset(Combin, CompanyDomicileRegion %in% c("Global", AllLists$MutualExclusiveCompanyDomicileRegions) & 
                                (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionOECD) | 
                                (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                                (Sector == "Fossil Fuels" & Technology != "Coal" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegionsOECD) | 
                                (Sector == "Fossil Fuels" & Technology == "Coal" & BenchmarkRegion %in% "Global"))
      OECDAggregate$BenchmarkRegion2 = "OECDAggregate"
      
      NonOECDAggregate <-subset(Combin, CompanyDomicileRegion %in% c("Global",AllLists$MutualExclusiveCompanyDomicileRegions) & 
                                  (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionNonOECD) | 
                                  (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                                  (Sector == "Fossil Fuels" & Technology != "Coal" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegionsNonOECD) | 
                                  (Sector == "Fossil Fuels" & Technology == "Coal" & BenchmarkRegion %in% "Global"))
      NonOECDAggregate$BenchmarkRegion2 = "NonOECDAggregate"
      
      AggregatedResults <- rbind(GlobalAggregate,OECDAggregate,NonOECDAggregate)
      
      #Sum production
      GlobalAggregate <- ddply(AggregatedResults,.(Sector, Technology, Scenario, Year, CompanyDomicileRegion, PortAUM, BenchmarkRegion2), summarize, Production = sum(Production, na.rm = TRUE), TargetProductionAlignment= sum(TargetProductionAlignment, na.rm = TRUE),TargetProductionAUMIntensity = sum(TargetProductionAUMIntensity, na.rm = TRUE), ScenarioTrajectoryProd = sum(ScenarioTrajectoryProd, na.rm = TRUE))
      GlobalAggregate <- rename(GlobalAggregate, c("BenchmarkRegion2" = "BenchmarkRegion"))
      GlobalAggregateSave <- GlobalAggregate
      
      # #Check if it makes sense!
      # GlobalAggregateMarket <- subset(GlobalAggregate, CompanyDomicileRegion == "Global", select = -c(TargetProductionAUMIntensity))
      # GlobalAggregateAUM <- ddply(subset(GlobalAggregate, CompanyDomicileRegion %in% MutualExclusiveCompanyDomicileRegions),.(Sector, Technology, Scenario, Year,BenchmarkRegion), summarize, TargetProductionAUMIntensity = sum(TargetProductionAUMIntensity, na.rm = TRUE))
      # GlobalAggregateAUMregional <- subset(GlobalAggregate, CompanyDomicileRegion != "Global")
      # GlobalAggregate <- merge(GlobalAggregateMarket, GlobalAggregateAUM, by = c("Sector", "Technology", "Scenario", "Year", "BenchmarkRegion"))
      # GlobalAggregate <- rbind(GlobalAggregate,GlobalAggregateAUMregional)
      
      MissingCols <- (setdiff(names(Combin), names(GlobalAggregate)))# Find names of missing columns
      tempdf <- data.frame(matrix(ncol = length(MissingCols), nrow = nrow(GlobalAggregate)))
      names(tempdf) <- MissingCols
      GlobalAggregate <- cbind(GlobalAggregate,tempdf)
      
      #Add with Combin
      CombinSave<-Combin
      Combin <- rbind(Combin, GlobalAggregate)
      
      # Calculate Exposure percentages
      Combin$MarketExposure <- (Combin$Production - Combin$TargetProductionAlignment) / Combin$TargetProductionAlignment
      Combin$TrajectoryExposure <- (Combin$Production - Combin$ScenarioTrajectoryProd) / Combin$ScenarioTrajectoryProd
      Combin$AUMExposure <- (Combin$Production - Combin$TargetProductionAUMIntensity) / Combin$TargetProductionAUMIntensity
      
      #implement AUM in sector values
      Combin$PortName <- PortfolioName
      Combin$Type <- ListAllPorts$Type[i]
      Combin$InvestorName <- ListAllPorts$InvestorName[i]
      
      if(ListAllPorts$InvestorName[i] != "ListedMarket"){ 
        ReducedList$Type <- ListAllPorts$Type[i]
        ReducedList$PortName <- PortfolioName
        Portfolio$Type <- ListAllPorts$Type[i]
        Portfolio$PortName <- PortfolioName
      }
      
      #Portfolio$EvalRegion <- EvalRegion
      PortfolioList <- data.frame(PortfolioName = PortfolioName, Type = ListAllPorts$Type[i])
      
      # Combin <- subset(Combin, BenchmarkRegion %in% c("GlobalAggregate", "OECDAggregate", "NonOECDAggregate")  & CompanyDomicileRegion %in% ParameterFile$CompanyDomicileRegion, select = c("InvestorName","PortName", "Type", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion",	"PortAUM",	"MarketAUM",	"Production",		"FairSharePerc",	"Direction",	"TargetProductionAlignment",	"TargetProductionAUMIntensity",	"ScenarioTrajectoryProd",	"MarketExposure",	"AUMExposure",	"TrajectoryExposure"))
      BenchmarkRegionstoPrint <- c("GlobalAggregate", "OECDAggregate", "NonOECDAggregate")
      # BenchmarkRegionstoPrint <- c("GlobalAggregate", "OECDAggregate", "NonOECDAggregate", "Africa","EU","China","India","Japan","US","Brazil","MiddleEast","LatinAmerica","Russia","OECDAsiaOceaniaWoJP")
      Combin <- subset(Combin, BenchmarkRegion %in% BenchmarkRegionstoPrint & Scenario == ParameterFile$Scenario & CompanyDomicileRegion %in% ParameterFile$CompanyDomicileRegion, select = c("InvestorName","PortName", "Type", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion",	"PortAUM",	"MarketAUM",	"Production",		"FairSharePerc",	"Direction",	"TargetProductionAlignment",	"TargetProductionAUMIntensity",	"ScenarioTrajectoryProd",	"MarketExposure",	"AUMExposure",	"TrajectoryExposure"))
      # 
      if (i > 1){
        CombinAll <- rbind(CombinAll,Combin)
        ReducedListAll <- rbind(ReducedListAll,subset(ReducedList,Year %in% c(Startyear,Startyear+5)))
        PortfolioAll <- rbind(PortfolioAll,Portfolio)
        PortfolioListAll <- rbind(PortfolioListAll,PortfolioList)
      }else{
        CombinAll <- Combin
        ReducedListAll <- subset(ReducedList,Year %in% c(Startyear,Startyear+5))
        PortfolioAll <- Portfolio
        PortfolioListAll <- PortfolioList
      }
      rm(AUMmix)
    }
    
  })
}

# Add the Company Names
PortfolioAll <- merge(PortfolioAll, CompNames, by.x = "EQY_FUND_TICKER", by.y = "Ticker", all.x = TRUE, all.y = FALSE)
ReducedListAll <- merge(ReducedListAll, CompNames, by.x = "EQY_FUND_TICKER", by.y = "Ticker", all.x = TRUE, all.y = FALSE)


# Create a folder for portolio results and go to that folder, 
#Definitely need to check for these
BatchFolder <- paste0(OutputFolder, BatchName,"/")
if(!dir.exists(file.path(BatchFolder))){dir.create(file.path(BatchFolder), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
BatchFolder <- paste0(OutputFolder, BatchName,"/",ParameterFile$AssessmentDate,"/")
if(!dir.exists(file.path(BatchFolder))){dir.create(file.path(BatchFolder), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  

# If there is already existing data, then copy all png,pdf and csv files into the next RunDirectory
flist <- list.files(BatchFolder,pattern=c("\\.csv$"), full.names = TRUE)
dlist <- list.dirs(BatchFolder, recursive = FALSE, full.names = FALSE)

if(length(flist)>0){
  nextrun <- 1
  if (length(dlist)>0){nextrun <- as.numeric(gsub("Run","",dlist[length(dlist)]))+1}
  RunDirectory <- paste0(BatchFolder,"Run",nextrun,"/")
  dir.create(file.path(RunDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")
  if(length(flist)>0){
    for (file in flist){file.copy(file, RunDirectory)}}
}

setwd(BatchFolder)


# Order the variables in CombinAll
CombinAll<- subset(CombinAll, select = c("InvestorName", "PortName", "Type", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion",	"PortAUM",	"MarketAUM",	"Production",	"FairSharePerc",	"Direction",	"TargetProductionAlignment",	"TargetProductionAUMIntensity",	"ScenarioTrajectoryProd",	"MarketExposure",	"AUMExposure",	"TrajectoryExposure"))

#only for Swiss!!!
if (length(grep("Swiss",BatchName))[1] == 1){
  SwissLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/02_Swiss/") #Output-folder for the results
  ParticipantList <- read.csv(paste0(SwissLocation,"ParticipantsOverview.csv"),strip.white = TRUE, stringsAsFactors = FALSE)
  CombinAll <- merge(CombinAll,ParticipantList, by = "InvestorName", all.x = TRUE)
}




# resultslist <- addcompanyeqy(BatchName, BatchFolder, CombinAll, ReducedListDebtAll,PortfolioAll,PortfolioListAll,"GEPABU")
# CombinAll <- resultslist[[1]]
# ReducedListAll <- resultslist[[2]]
# PortfolioAll <- resultslist[[3]]



write.csv(CombinAll,paste(BatchName,"_EquityAnalysisResults.csv",sep = ""),row.names = FALSE, na = "")
write.csv(ReducedListAll,paste(BatchName,"_CompanysProduction_Snapshot.csv",sep = ""),row.names = FALSE, na = "")
write.csv(PortfolioAll,paste(BatchName,"_PortfolioData_Snapshot",Startyear,".csv",sep = ""),row.names = FALSE, na = "")
write.csv(MarketRef,paste(BatchName,"_MarketPortfolio_Snapshot",Startyear,".csv",sep = ""),row.names = FALSE, na = "")
write.csv(PortfolioListAll,paste(BatchName,"_ListPortfolioHoldings",Startyear,".csv",sep = ""),row.names = FALSE, na = "")

# Create subsets
# keeps <- c("PortName", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion", "Production", "TargetProductionAlignment",	"TargetProductionAUMIntensity",	"MarketExposure",	"AUMExposure")
# EquityAnalysisSubset <- subset(CombinAll, select = keeps)
# write.csv(EquityAnalysisSubset,paste(BatchName,"_EquityAnalysisResults-subset.csv",sep = ""),row.names = FALSE, na = "")
Sub450Scenario <- subset(CombinAll, Scenario == "450S")
write.csv(Sub450Scenario,paste(BatchName,"_EquityAnalysisResults-450S-only.csv",sep = ""),row.names = FALSE, na = "")

keepBenchmarkRegion <- c("China","EU","Global","Japan","NonOECD","OECD","US","GlobalAggregate")
# keepBenchmarkRegion <- c("GlobalAggregate", "OECDAggregate", "NonOECDAggregate", "Africa","EU","China","India","Japan","US","Brazil","MiddleEast","LatinAmerica","Russia","OECDAsiaOceaniaWoJP")

keepCompanyDomicileRegion <- c("Global", "MSCIWorld", "Stoxx600", "SP500","OutsideACWI","MSCIEmergingMarkets","MSCIACWI")


keeps2 <- c("PortName", "Year",	"Sector",	"Technology",	"Scenario",	"CompanyDomicileRegion",	"BenchmarkRegion", "Production", "TargetProductionAlignment",	"TargetProductionAUMIntensity",	"ScenarioTrajectoryProd","MarketExposure",	"AUMExposure", "TrajectoryExposure")  
Sub450SubBMR <- subset(CombinAll, Scenario == "450S" & BenchmarkRegion %in% keepBenchmarkRegion & CompanyDomicileRegion %in% keepCompanyDomicileRegion, select = keeps2)
# write.csv(Sub450SubBMR,paste(BatchName,"_EquityAnalysisResults_450S-lessRegions.csv",sep = ""),row.names = FALSE, na = "")
PowerSubset <- subset(Sub450SubBMR, Sector == "Power")
# write.csv(PowerSubset,paste(BatchName,"_EquityAnalysisResults_450S-lessRegions_Power.csv",sep = ""),row.names = FALSE, na = "")
FossilFuelSubset <- subset(Sub450SubBMR, Sector == "Fossil Fuels")
# write.csv(FossilFuelSubset,paste(BatchName,"_EquityAnalysisResults_450S-lessRegions_FossilFuel.csv",sep = ""),row.names = FALSE, na = "")
AutomotiveSubset <- subset(Sub450SubBMR, Sector == "Automotive")
# write.csv(AutomotiveSubset,paste(BatchName,"_EquityAnalysisResults_450S-lessRegions_Automotive.csv",sep = ""),row.names = FALSE, na = "")

RegionalSubset <- subset(CombinAll, Scenario == ParameterFile$Scenario & BenchmarkRegion %in% ParameterFile$BenchmarkRegion & CompanyDomicileRegion %in% ParameterFile$CompanyDomicileRegion)
write.csv(RegionalSubset,paste0(BatchName,"_EquityAnalysisResults_",ParameterFile$Scenario,"_",ParameterFile$BenchmarkRegion,"_",ParameterFile$CompanyDomicileRegion,".csv"),row.names = FALSE, na = "")

# Write parameter file and add all necessary information
ParameterFile$Date <- Sys.Date() #get todays Date
write.csv(ParameterFile,paste0(BatchName,"_ParameterFile.csv"),row.names = FALSE, na = "")

# add Missing Isins to PORT-Input 
#
# CurrentQuater <- gsub(" ", "", as.yearqtr(Sys.Date(), format = "%Y%q"))
# LastQuater <- gsub(" ", "", as.yearqtr(Sys.Date(), format = "%Y%q")-1/4)
# 
# if(!dir.exists(file.path(paste0(FinancialDataFolder, CurrentQuater,"/")))){
#   dir.create(file.path(paste0(FinancialDataFolder, CurrentQuater,"/")), showWarnings = TRUE, recursive = FALSE, mode = "0777")
#   flist <- list.files(BatchFolder,pattern=c("\\.csv$"), full.names = TRUE)
#   if(length(flist)>0){
#     RunDirectory <- paste0(BatchFolder,"Run",nextrun,"/")
#     dir.create(file.path(RunDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")
#     if(length(flist)>0){
#       for (file in flist){file.copy(file, RunDirectory)}}
#   }
#   
# }  
# 
# paste0(FinancialDataFolder, CurrentQuater,"/")
write.csv(MissingBBGInfo, paste0(BatchName,"_",dim(MissingBBGInfo)[1],"ISINs_wo_BBG_Info_from_Port.csv"), row.names = FALSE, na = "")



# EQData_existing <- read.csv("C:/Users/Work/Dropbox (2Â° Investing)/PortCheck/03_Results/01_BatchResults/Swiss/2016Q4/Swiss_EquityAnalysisResults.csv", strip.white = TRUE, stringsAsFactors = TRUE)
# Test1 <- subset(CombinAll,!CombinAll$PortName %in% EQData_existing$PortName)
# Test1 <- rbind(Test1,EQData_existing)
# write.csv(Test1, "EQDataAll.csv",row.names = FALSE)