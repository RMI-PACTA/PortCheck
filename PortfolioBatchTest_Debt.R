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

#Version - Control

# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 04 - 24 |        KH        |           3          | Adjust code to updated bridge files
# 2017 - 10 - 04 |        CM        |           13         | Read ValueUSD rather than position
# 2017 - 10 - 04 |        KH        |           13         | Read ValueUSD only if position is not given
# 2017 - 10 - 04 |        KH        |           14         | Implemented updated IEATargets (Automotive sector based on ETP2017)

#-------------
# All Input parameters & make the code interactive
#------------
# Please select the input parameters here
BBGDataYear <- "2016" #The date of which the financial data is from - this is used to select the FFperc of BBGDataYear
BBGDataDate <- "30/12/2016"
MarketDataDate <- "06_07_2016"
AssessmentDate <- "2016Q4"
Date <- Sys.Date() #get todays date for appending to file names to track historical calcualtion results
BatchName <- "FebPortChecks2" #PortfolioData file name
# FinancialDataDate <- "2017-04-19" # just in case the financial data got created by BBG-Data-Bind_v2 - Should create a function to look this up automotically and takes the most recent file (e.g. by using list function, then cut first 10 digits and sort by "date")
BBGPORTOutput <- "BondPORTOutputMixedSource"
BBGPORTOutput <- "FinancialData_20180131"
# BatchName2 <- "Swiss_BatchAllPort_Bonds"

Startyear <- 2017 #Date when the analysis starts - time horizon is always 5 years starting from here: e.g. if Startyear is 2016 the analysis will go to 2021
UserName <- sub("/.*","",sub(".*Users/","",getwd()))
# FolderLocation <- paste0("W:/General Insurance Team/Analytics Team/Analytics 2016/322903/R - GranularAssetData/2D-II/") #Input folder (the substructure needs to be consistent to be able to read in all files)
# OutputFolder <- paste0("W:/General Insurance Team/Analytics Team/Analytics 2016/322903/R - GranularAssetData/2D-II/PortfolioResults/") #Output-folder for the results
FolderLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/2DPORTFOLIO/PortfolioCheck/Data/Finance Reg Data/") #Input folder (the substructure needs to be consistent to be able to read in all files)
OutputFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/2DPORTFOLIO/PortfolioCheck/Data/Finance Reg Data/") #Output-folder for the results
#CalculateMarketData <- 1 # set to 1 to calculate new MarketData, only needed when the underlying asset level data got updated. CAUTION: BETTER ARCHIVE THE OLD MARKETDATA BEFORE DOING THIS!
#Preperation: Create a folder in the follow directory. One Named 2degAlignmentTest, within it create PortfolioResults, Data, PortfolioInput, BBG-Data

#-------------
# All functions used
#-------------
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

TechnologyList <- c("Electric","Hybrid","ICE","GasCap","CoalCap","OilCap","RenewablesCap","HydroCap","NuclearCap", "Coal","Oil","Gas")
SectorList <- c("Automotive","Coal","Power","Oil&Gas")
datacompletion <- function (Data){
  Data <- subset(Data, Technology %in% AllLists$TechList)
  Data <- Data %>% complete(Technology = AllLists$TechList, Year = full_seq(c(Startyear,Startyear+10),1), fill = list(CompanyLvlProd = 0, Wt = 0, WtProduction = 0,PortWtTechShare = 0,Intensification = 0, CarstensMetric = 0))
  Data$Sector <- "Power"
  Data$Sector[Data$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
  Data$Sector[Data$Technology %in% c("Coal")] <- "Coal" 
  Data$Sector[Data$Technology %in%c("Electric","Hybrid","ICE")] <-"Automotive"
  return(Data)
}

Regiondatacompletion <- function (Data){
  Data <- Data %>% complete(BenchmarkRegion = subset(BenchmarkRegionList, !BenchmarkRegion %in% c("NonOECDRest"), select = "BenchmarkRegion"), 
                            Sector = SectorList, 
                            fill = list(RegionalWtPortfolioWt = 0))
  Data <- subset(Data, (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | (Sector %in% c("Oil&Gas") & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | (Sector %in% c("Automotive","Coal") & BenchmarkRegion == "Global"))
  Data$RegionalWtPortfolioWt[Data$Sector %in% c("Coal","Automotive")] <- 1
  return(Data)
}


addcompanydebt <- function(BatchName, BatchFolder, CombinAll, ReducedListDebtAll,PortfolioAll,InvestorToRemove){
  
  CombinAllLong <- read.csv(paste(BatchName,"_DebtAnalysisResults.csv",sep = ""),stringsAsFactors = FALSE)
  ReducedListDebtAllLong <- read.csv(paste(BatchName,"_DebtProductionCompanies_Snapshot",Startyear+5,".csv",sep = ""),stringsAsFactors = FALSE)
  PortfolioAllLong <- read.csv(paste(BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv",sep = ""),stringsAsFactors = FALSE)
  
  
  CombinAllLong <- subset(CombinAllLong, InvestorName != InvestorToRemove)
  ReducedListDebtAllLong <- subset(ReducedListDebtAllLong, PortName != InvestorToRemove)
  PortfolioAllLong <- subset(PortfolioAllLong, InvestorName != InvestorToRemove)                                
  # CombinAllLong<- subset(CombinAllLong, !CombinAllLong$PortName %in% exclusionlist)
  # CombinAllLong <- CombinAllLong[!(CombinAllLong$InvestorName %in% exclusionlist),]
  # 
  # 
  # ReducedListDebtAllLong <- subset(ReducedListDebtAllLong, !ReducedListDebtAllLong$PortName %in% exclusionlist)
  # PortfolioAllLong <- subset(PortfolioAllLong, !PortfolioAllLong$InvestorName %in% exclusionlist)
  
  
  CombinAll <- rbind(CombinAllLong, CombinAll)
  ReducedListDebtAll <- rbind(ReducedListDebtAllLong, ReducedListDebtAll)
  PortfolioAll <- rbind(PortfolioAllLong, PortfolioAll)
  
  results <- list(CombinAll,ReducedListDebtAll, PortfolioAll)
  
  return(results)
}




#-------------
# Set workdrive
#-------------
setwd(FolderLocation)

# ------------------------------
# |------   Data Loading   ------|
# ------------------------------

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

## temp fix for fossil fuels to get numbers right
# OGCDebtData <- read.csv(paste0(FolderLocation,"Data/OGDebtMaster_2016.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
OGCDebtData <- read.csv(paste0(FolderLocation,"Data/OGDebtMaster_201704_UpdatedCoal.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
OGCDebtData$Sector <- "Fossil Fuels"
OGCDebtData <- subset(OGCDebtData, select = -c(RollUpType,GDCompanyType))
OGCDebtData <- rename(OGCDebtData, c("Region" ="PlantLocation"))

AutoDebtData <- read.csv(paste0(FolderLocation,"Data/AutoDebtMaster_2016.csv"),stringsAsFactors=FALSE,strip.white=TRUE) # this is just the equity roll up, matched to corporate debt ticker
DebtToEquityBridge <- read.csv(paste0(FolderLocation,"Data/EquityToDebtBridge_2017-09-05.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
AutoDebtData <- merge(AutoDebtData, DebtToEquityBridge, by.x = "EQY_FUND_TICKER", by.y = "EquityTicker", all.x = TRUE)
AutoDebtData <- subset(AutoDebtData, !is.na(AutoDebtData$DebtTicker), select = -c(EQY_FUND_TICKER))

PowerDebtData <- read.csv(paste0(FolderLocation,"Data/2017-08-28powermasterDebt.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
#PowerDebtData[,setdiff(colnames(OGC),colnames(PowerDebtData))] <- NA
PowerDebtData$Technology[PowerDebtData$Technology == "Coal"] <- "CoalCap"
PowerDebtData$Technology[PowerDebtData$Technology == "Oil"] <- "OilCap"
PowerDebtData$Technology[PowerDebtData$Technology == "Gas"] <- "GasCap"
PowerDebtData$Technology[PowerDebtData$Technology == "Nuclear"] <- "NuclearCap"
PowerDebtData$Technology[PowerDebtData$Technology == "Hydro"] <- "HydroCap"
PowerDebtData$Technology[PowerDebtData$Technology == "Renewables"] <- "RenewablesCap"
PowerDebtData$Sector <- "Power"

MasterData <- rbind(PowerDebtData, OGCDebtData)
MasterData <- rbind(MasterData, AutoDebtData)

MasterData<- rename(MasterData, c("Production" = "CompanyLvlProd"))


#Trim Masterdata to startyear
MasterData<-subset(MasterData, MasterData$Year >= Startyear)

# ------
# b) Read in Asset level Data Bridges
# ------
ALDBridge <- read.csv(paste0(FolderLocation,"Data/ALDEquityBridge_2017-04-25.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
#EquityBridge <- read.csv(paste0(FolderLocation,"Data/EquityBridge_2017-04-23.csv"),stringsAsFactors = FALSE, strip.white = TRUE)


# c) Read in stored company level financial data (Data retrieved from API and PORT function) 

#UnlistedDebt <- read.csv(paste0(FolderLocation,"Data/UnlistedDebt_2017-05-03.csv"), stringsAsFactors = FALSE, strip.white = TRUE) #This is debt at a corporate debt ticker level
#BenchmarkIndex <- read.csv(paste0(FolderLocation,"Data/", BenchmarkIndex),stringsAsFactors = FALSE, strip.white = TRUE)

# ------
# c) Read in financial data (Data retrieved from BBG PORT function)
# ------
BBGPORTOutput <- read.csv(paste0(FolderLocation, BBGPORTOutput,".csv"),stringsAsFactors=FALSE)
#drop duplicate ISINs due to position-based variables
BBGPORTOutput <- BBGPORTOutput[!duplicated(BBGPORTOutput$ISIN),]

# ------
# d) Read in portoflio holdings (Portfolio holdings data given by Regulator or Instituion)
PortfolioAllPorts <- read.csv(paste0(FolderLocation,"PortfolioData/",BatchName,"Port_Bonds.csv"),stringsAsFactors=FALSE)

# PortfolioAllPorts <- subset(PortfolioAllPorts, PortfolioAllPorts$InvestorName == "GEPABU")
# ------
# PortfolioAllPorts2 <- read.csv(paste0(FolderLocation,"PortfolioData/",BatchName2,".csv"),stringsAsFactors=FALSE)
# PortfolioAllPorts <- subset(PortfolioAllPorts, !PortfolioAllPorts$PortfolioName %in% PortfolioAllPorts2$PortfolioName)
# colnames(PortfolioAllPorts) <- colnames(PortfolioAllPorts2)
# PortfolioAllPorts <- rbind(PortfolioAllPorts, PortfolioAllPorts2)
# ------

# ------
# e) #Define sector claissification based of BIC subgroup. Need to consoludiate SERCH BIC level 2 calssfication with whatever we were using for moodies.
PowerBIC <- c("Electric-Generation", "Electric-Integrated", "Independ Power Producer","Energy-Alternate Sources", "Utilities","Power.Generation")
PowerBICdf <- data.frame("BIClvl2" = PowerBIC, "Sector" = "Power")
OilGasBIC <-c("Oil Comp-Explor&Prodtn", "Oil Comp-Integrated","Oil&Gas Drilling" ,"Exploration...Production","Integrated.Oils") #Metal-Diversified is to get Rio Tinto...
OilGasBICdf <- data.frame("BIClvl2" = OilGasBIC, "Sector" = "Oil&Gas")
CoalBIC <-c("Coal","Metal-Diversified",  "Coal.Operations", "Metals...Mining", "Diversified Minerals") #Metal-Diversified is to get Rio Tinto...
CoalBICdf <- data.frame("BIClvl2" = CoalBIC, "Sector" = "Coal")
AutoBIC <- c("Auto-Cars/Light Trucks", "Automobiles.Manufacturing")
AutoBICdf <- data.frame("BIClvl2" = AutoBIC, "Sector" = "Automotive")
FuturesecsBIC <- c("Bldg Prod-Cement/Aggreg","Steel-Producers", "Metal-Iron","Metal-Aluminum","Airlines","Transport-Air Freight", "Transport-Marine")
FuturesecsBICdf <- data.frame("BIClvl2" = FuturesecsBIC, "Sector" = "Futuresecs")
BICSectors <- rbind(PowerBICdf,OilGasBICdf,AutoBICdf,FuturesecsBICdf,CoalBICdf)
# ------

# f) Read in regions data
# Import Regionsfile
RegionLists <- read.csv(paste0(FolderLocation,"Data/RegionLists.csv"), na.strings = c(""))

# Create a List of all existing Benchmark-Region and all assessed CompanyLocation-Regions
BenchRegionLists <- read.csv(paste0(FolderLocation,"Data/BenchRegions.csv"))
BenchRegionLists[is.na(BenchRegionLists)] <- ""
BenchRegionLists <- rename(BenchRegionLists, c("BenchRegions" = "BenchmarkRegions", "BenchRegions_ISO_colnames" = "BenchmarkRegions_ISO_colnames"))
BenchmarkRegionList <- data.frame(BenchmarkRegion = BenchRegionLists$BenchmarkRegions[!is.na(BenchRegionLists$BenchmarkRegions) & BenchRegionLists$BenchmarkRegions != ""], BenchmarkRegionColname = BenchRegionLists$BenchmarkRegions_ISO_colnames[!is.na(BenchRegionLists$BenchmarkRegions_ISO_colnames) & BenchRegionLists$BenchmarkRegions_ISO_colnames != ""])
CompanyDomicileRegion <- read.csv(paste0(FolderLocation,"Data/IndexRegions.csv"))
CompanyDomicileRegion <- rename(CompanyDomicileRegion, c("IndexUniverse" = "CompanyDomicileRegion", "IndexUniverseColname" = "CompanyDomicileRegionColname")) 
#CompanyDomicileRegion[is.na(CompanyDomicileRegion)] <- ""
CompanyDomicileRegionList <- data.frame(CompanyDomicileRegion = CompanyDomicileRegion$CompanyDomicileRegion[!is.na(CompanyDomicileRegion$CompanyDomicileRegion) & CompanyDomicileRegion$CompanyDomicileRegion != ""], CompanyDomicileRegionColname = CompanyDomicileRegion$CompanyDomicileRegionColname[!is.na(CompanyDomicileRegion$CompanyDomicileRegionColname) & CompanyDomicileRegion$CompanyDomicileRegionColname != ""])

# Read countryname-conversion file to abbreviation
CountryISOList <- read.csv(paste0(FolderLocation,"Data/CountryISOCodes.csv"), stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c(""))
CountryISOList <- subset(CountryISOList, CountryISOList$COUNTRY != "#N/A")
CountryISOList$GDPlantLocation<-as.character(CountryISOList$GDPlantLocation)


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

AllLists <- list(TechList = TechnologyList, PowerBenchmarkRegionOECD = PowerBenchmarkRegionOECD, PowerBenchmarkRegionNonOECD = PowerBenchmarkRegionNonOECD, PowerBenchmarkRegionGlobal = PowerBenchmarkRegionGlobal,
                 FossilFuelBenchmarkRegions = FossilFuelBenchmarkRegions, FossilFuelBenchmarkRegionsOECD = FossilFuelBenchmarkRegionsOECD, FossilFuelBenchmarkRegionsNonOECD = FossilFuelBenchmarkRegionsNonOECD,
                 MutualExclusiveCompanyDomicileRegions = MutualExclusiveCompanyDomicileRegions)

# ------
# g) Read in scenario data
# ------
# Calculate fair share ratios
# calculate the targets. Set this to be switched off if reload is set to 0
# IEATargets <- read.csv(paste0(FolderLocation,"Data/IEATargets_linear_2016_fix2.csv"),stringsAsFactors=FALSE)

IEATargetsNew <- read.csv(paste0(FolderLocation,"Data/IEATargets_WEO2016+ETP2017.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
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
# IEATargetswide$NonOECDRest <- IEATargetswide$`Non-OECD` - (IEATargetswide$Africa + IEATargetswide$`Non-OECDAsia` + IEATargetswide$LatinAmerica + IEATargetswide$MiddleEast + IEATargetswide$EEurope_Eurasia) # this should be 0 but isn´t..
IEATargetsPower <- melt(IEATargetswide,id.vars = c("Technology","Year","Scenario","Sector", "Units"),variable.name = "Region", value.name = "AnnualvalIEAtech")
IEATargets <- rbind(subset(IEATargets, Sector != "Power"), IEATargetsPower)

# Calculate Renewable Energy Capacity Targets by summing over all RE-Technologies
RETargets <- subset(IEATargets, IEATargets$Technology %in% c("BioenergyCap", "CSPCap", "GeothermalCap", "MarineCap", "SolarPVCap", "WindCap"))
IEATargets <- subset(IEATargets, !IEATargets$Technology %in% c("BioenergyCap", "CSPCap", "GeothermalCap", "MarineCap", "SolarPVCap", "WindCap"))
RETargets  <- ddply(RETargets,.(Scenario, Region, Sector, Year, Units), summarize, AnnualvalIEAtech=sum(as.numeric(AnnualvalIEAtech), na.rm=TRUE))
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
EndRefValIEA  <- rename(EndRefValIEA , c("AnnualvalIEAtech" = "EndRefValIEA"))
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
IEATargets$refvalIEAsec [IEATargets$Sector == "Fossil Fuels"] <- IEATargets$refvalIEAtech[IEATargets$Sector == "Fossil Fuels"]
IEATargets$mktFSRatio <- (IEATargets$AnnualvalIEAtech - IEATargets$refvalIEAtech)/IEATargets$refvalIEAsec
IEATargets$techFSRatio <- (IEATargets$AnnualvalIEAtech - IEATargets$refvalIEAtech)/IEATargets$refvalIEAtech

IEATargets <- IEATargets[order(IEATargets$Scenario,IEATargets$Region,IEATargets$Sector, IEATargets$Technology,IEATargets$Year),] 
IEATargets$Region[IEATargets$Sector == "Automotive"] <- "Global"
IEATargets <- IEATargets[!duplicated(IEATargets),]

IEATargets$FairSharePerc<-IEATargets$mktFSRatio
IEATargets$FairSharePerc[IEATargets$Direction == "declining"] <- IEATargets$techFSRatio[IEATargets$Direction == "declining"]

IEATargets <- rename(IEATargets, c("Region" ="BenchmarkRegion"))
IEATargetssub <- subset(IEATargets, Year <= (Startyear + 10), select = c("Sector","Technology","Year","BenchmarkRegion","FairSharePerc","Scenario","Direction")) # select scenario 450 if problems with the results otherwise!

#write.csv(IEATargets, paste0(Sys.Date(),"_IEATargets_AllRegions_", Date, ".csv"), row.names = FALSE, na = "")

# Create market data or read it in
# For debt the market data is different as the market is different. It's no longer the listed universe, but the debt universe.
#if (CalculateMarketData == 1){

#add debt data
DebtData <- read.csv(paste0(FolderLocation,"Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# Rename Total row as to not confuse it with TOTAL SA...
DebtData$Co..Corp.TKR[DebtData$Co..Corp.TKR == "Total"] <- "TotalDebt"

GovBanksSupraNat <- subset(DebtData, (DebtData$Government.Development.Banks !=0 | DebtData$Supranationals != 0) & DebtData$Co..Corp.TKR != "TotalDebt") 
#sum(GovBanksSupraNat$Total)
#sum(DebtData$Total)-subset(DebtData,DebtData$Co..Corp.TKR =="TotalDebt")$Total
DebtData <- subset(DebtData, !DebtData$Co..Corp.TKR %in% GovBanksSupraNat$Co..Corp.TKR)
#sum(DebtData$Total)-subset(DebtData,DebtData$Co..Corp.TKR =="TotalDebt")$Total
TotalDebt <- sum(DebtData$Total)-subset(DebtData,DebtData$Co..Corp.TKR =="TotalDebt")$Total

# Process debt according to 2DII sector classification
# Subset
DebtData <- DebtData[,colnames(DebtData) %in% BICSectors$BIClvl2 | colnames(DebtData) %in% c("Co..Corp.TKR", "Total")] 

# Sum Debt that is not in the 2DII sectors and save it as Other.
DebtData$Other <- apply(DebtData[,c(3:length(names(DebtData)))], 1, sum, na.rm=TRUE)
DebtData$Other <- DebtData$Total - DebtData$Other
# Merge with ALD bridge
DebtData <- rename(DebtData, c("Total" = "TotalCorpDebt"))
DebtData[is.na(DebtData)] <- 0

# Create portfolio for bond universe to be rbound with PortfolioAllPorts. have to have a bond for every secotr classifcation... so need to melt.
UniversePort <- subset(DebtData, select = -c(TotalCorpDebt), DebtData$Co..Corp.TKR != "TotalDebt")
UniversePort <- melt(UniversePort, id.vars = c("Co..Corp.TKR"), variable.name = "Subgroup", value.name = "Position")
UniversePort <- subset(UniversePort, UniversePort$Position != 0)
UniversePort$SectorWeight <- UniversePort$Position/sum(UniversePort$Position, na.rm = TRUE)
MasterDatasub <- MasterData
MasterDatasub$Sector[MasterDatasub$Sector == "Fossil Fuels" & MasterDatasub$Technology %in% c("Oil","Gas")] <- "Oil&Gas"
MasterDatasub$Sector[MasterDatasub$Sector == "Fossil Fuels" & MasterDatasub$Technology == "Coal"] <- "Coal"
Mastersub <- subset(MasterDatasub, CompanyLvlProd != 0 & Year == Startyear, select = c("DebtTicker", "Sector", "CompanyLvlProd"))

library(dplyr)
Mastersub <- Mastersub %>%
  distinct(DebtTicker, Sector, .keep_all = TRUE)
detach("package:dplyr", unload=TRUE)

#Check this!!! Check why all = TRUE doesn´t work and if it is necessary after all
UniversePort <- merge(subset(Mastersub, select = -c(CompanyLvlProd)), UniversePort, by.x = "DebtTicker", by.y = "Co..Corp.TKR", all = FALSE)

# Sector exposure merging
UniversePort$piesector <- "Not Assessed" #label non-benchmarked sectors
UniversePort$piesector[UniversePort$Subgroup %in% OilGasBIC] <- "Oil&Gas"
UniversePort$piesector[UniversePort$Subgroup %in% CoalBIC] <- "Coal"
UniversePort$piesector[UniversePort$Subgroup %in% AutoBIC] <- "Automotive"
UniversePort$piesector[UniversePort$Subgroup %in% PowerBIC & UniversePort$Sector %in% "Power"] <- "Utility Power"
UniversePort$piesector[!(UniversePort$Subgroup %in% PowerBIC) & UniversePort$Sector %in% "Power"] <- "NonUtility Power"
UniversePort$piesector[!(UniversePort$Subgroup %in% OilGasBIC) & UniversePort$Sector %in% c("Oil&Gas")] <- "NonOG Production"
UniversePort$piesector[!(UniversePort$Subgroup %in% CoalBIC) & UniversePort$Sector %in% c("Coal")] <- "NonCoal Production"

# set weighting of companies that are duplicates (ff or auto and non-utility) to 0 for the non-utility part. THIS DIDN"T WORK WITHOUT EQY UniversePortN TICKERS, ALTERNATIVE BELOW
temp1 <- unique(subset(UniversePort, !piesector %in% c("Utility Power"), select = "DebtTicker"))
if(dim(temp1)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "NonUtility Power" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "NonUtility Power" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "NonUtility Power" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
}
temp2 <- unique(subset(UniversePort, Sector %in% c("Power"), select = "DebtTicker"))
if(dim(temp2)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
}
rm(temp1,temp2)

temp1 <- unique(subset(UniversePort, !piesector %in% c("Oil&Gas"), select = "DebtTicker"))
if(dim(temp1)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "NonUtility Power" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "NonOG Production" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "NonOG Production" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
}
temp2 <- unique(subset(UniversePort, Sector %in% c("Oil&Gas"), select = "DebtTicker"))
if(dim(temp2)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
}
rm(temp1,temp2)  

temp1 <- unique(subset(UniversePort, !piesector %in% c("Coal"), select = "DebtTicker"))
if(dim(temp1)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "NonUtility Power" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "NonCoal Production" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "NonCoal Production" & UniversePort$DebtTicker %in% temp1$DebtTicker] <- 0
}
temp2 <- unique(subset(UniversePort, Sector %in% c("Coal"), select = "DebtTicker"))
if(dim(temp2)[1]>0){
  #UniversePort$PortWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$SectorWeight[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
  UniversePort$Position[UniversePort$piesector == "Not Assessed" & UniversePort$DebtTicker %in% temp2$DebtTicker] <- 0
}
rm(temp1,temp2)  

UniversePort <- subset(UniversePort, UniversePort$Position != 0 & UniversePort$piesector %in% c("Utility Power", "Coal", "Automotive","Oil&Gas")  , select = -c(Sector))
# s <- UniversePort[duplicated(UniversePort[["Co..Corp.TKR"]], fromLast=TRUE), ]
# s <- subset(UniversePort, UniversePort$Co..Corp.TKR %in% s$Co..Corp.TKR)

CORPSecWts <- UniversePort
CORPSecWts <- merge(CORPSecWts, BICSectors, by.x = "Subgroup", by.y = "BIClvl2", all.x = TRUE)
CORPSecWts$Sector <- as.character(CORPSecWts$Sector)
CORPSecWts$Sector[is.na(CORPSecWts$Sector)] <- "NotCovered"
CORPSecWts <- ddply(CORPSecWts,.(DebtTicker, Sector), summarize,
                    SectorWeight=sum(SectorWeight, na.rm=TRUE))

MarketSecWts <- ddply(CORPSecWts,.(Sector), summarize,
                      SectorWeight=sum(SectorWeight, na.rm=TRUE))


#Continue with market portfolio
DebtData <- merge(MasterData, DebtData, by.x = "DebtTicker", by.y = "Co..Corp.TKR", all = TRUE)
DebtData$PowerDebt <- rowSums(DebtData[,c("Power.Generation", "Utilities")], na.rm=T)
DebtData$AutoDebt <- DebtData$Automobiles.Manufacturing
DebtData$OGDebt <-  rowSums(DebtData[,c("Exploration...Production","Integrated.Oils")], na.rm=T)
DebtData$CoalDebt <-  rowSums(DebtData[,c("Coal.Operations", "Metals...Mining")], na.rm=T)
DebtData$OtherDebt  <- DebtData$TotalCorpDebt - DebtData$PowerDebt - DebtData$AutoDebt - DebtData$CoalDebt - DebtData$OGDebt
DebtData <- subset(DebtData, !is.na(DebtData$Technology))

#Convert OG units to energy
#DebtData$CompanyLvlProd[DebtData$Technology == "Coal"] <- DebtData$CompanyLvlProd[DebtData$Technology == "Coal"]*24 #converts t to GJ
DebtData$Sector[DebtData$Technology == "Coal"] <- "Coal"
DebtData$Sector[DebtData$Technology == "Oil"] <- "Oil&Gas"
DebtData$Sector[DebtData$Technology == "Gas"] <- "Oil&Gas"
DebtData$CompanyLvlProd[DebtData$Technology == "Oil"] <- DebtData$CompanyLvlProd[DebtData$Technology == "Oil"]*6.12
DebtData$CompanyLvlProd[DebtData$Technology == "Gas"] <- DebtData$CompanyLvlProd[DebtData$Technology == "Gas"]*0.0372

DebtData[is.na(DebtData)] <- 0
DebtData <-subset(DebtData, !DebtData$DebtTicker == "")

#Sum debt for relevant sectos
DebtData$ClimateSecDebt[DebtData$Sector == "Power" & DebtData$PowerDebt != 0] <- DebtData$PowerDebt[DebtData$Sector == "Power" & DebtData$PowerDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Automotive" & DebtData$AutoDebt != 0] <- DebtData$AutoDebt[DebtData$Sector == "Automotive" & DebtData$AutoDebt != 0]
#DebtData$ClimateSecDebt[DebtData$Sector == "Fossil Fuels" & DebtData$TotalDebt != 0] <- DebtData$TotalDebt[DebtData$Sector == "Fossil Fuels" & DebtData$TotalDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Oil&Gas" & DebtData$OGDebt != 0] <- DebtData$OGDebt[DebtData$Sector == "Oil&Gas" & DebtData$OGDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Coal" & DebtData$CoalDebt != 0] <- DebtData$CoalDebt[DebtData$Sector == "Coal" & DebtData$CoalDebt != 0]

# DDply debt for each GD ID for each sector (Consolidate debt at GD ID level. This effective does it at the parent equity level, and this will be changed to the parent obiligor level (corp debt ticker) level later)  
# these needs to be added back in after BoE first run

# DebtDataCORP_global <-ddply(DebtData,.(DebtTicker, Sector, Year, Technology,  CompanyLvlProd), summarize,
#                       PowerDebt=sum(Utilities + Power.Generation, na.rm=TRUE),
#                       AutoDebt=sum(Automobiles.Manufacturing, na.rm=TRUE),
#                       OGDebt=sum(Exploration...Production + Integrated.Oils, na.rm=TRUE),
#                       CoalDebt=sum(Coal.Operations + Metals...Mining, na.rm=TRUE),
#                       OtherDebt=sum(Other, na.rm=TRUE),
#                       TotalDebt=sum(TotalCorpDebt, na.rm=TRUE))

DebtDataCORP_global <- ddply(DebtData,.(DebtTicker, Sector, Year, Technology, ClimateSecDebt,TotalCorpDebt), summarize,
                             CompanyLvlProd=sum(CompanyLvlProd, na.rm=TRUE)) # PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,


DebtDataCORP_global <-ddply(DebtDataCORP_global,.(DebtTicker, Sector, Year, Technology,  CompanyLvlProd), summarize,
                            ClimateSecDebt=sum(ClimateSecDebt, na.rm=TRUE),
                            TotalDebt=sum(TotalCorpDebt, na.rm=TRUE))

DebtDataCORP_global <- subset(DebtDataCORP_global, DebtDataCORP_global$CompanyLvlProd != 0)

DebtDataCORPSec_global <- ddply(DebtDataCORP_global,.(DebtTicker, Sector, Year,ClimateSecDebt, TotalDebt), summarize,
                                CompanyLvlSecProd = sum(CompanyLvlProd, na.rm=TRUE)) #PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

DebtDataCORP_global <- merge(DebtDataCORP_global,DebtDataCORPSec_global, by = c("DebtTicker", "Sector", "Year", "ClimateSecDebt","TotalDebt")) #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", 
DebtDataCORP_global$TechShare <- 1
DebtDataCORP_global$TechShare <- DebtDataCORP_global$CompanyLvlProd/DebtDataCORP_global$CompanyLvlSecProd


DebtDataCORP_global$Aggregation <- "Global"
DebtDataCORP_global$BenchmarkRegion <- "Global"

t<-subset(DebtDataCORP_global, DebtDataCORP_global$Technology == "Coal" & DebtDataCORP_global$Year == Startyear)
sum(t$CompanyLvlProd)


DebtData <- merge(DebtData, CountryISOList, by.x = "PlantLocation", by.y = "GDPlantLocation", all.x = TRUE)

for (j in 1:length(BenchmarkRegionList$BenchmarkRegion)){
  # print(j)
  DebtDataCORP_regional <- subset(DebtData, COUNTRY_ISO %in% BenchRegionLists[,names(BenchRegionLists) == BenchmarkRegionList$BenchmarkRegionColname[j]])
  DebtDataCORP_regional$BenchmarkRegion <- BenchmarkRegionList$BenchmarkRegion[j]
  if(j==1){
    DebtDataCORP <- DebtDataCORP_regional
  }else{
    DebtDataCORP <- rbind(DebtDataCORP, DebtDataCORP_regional)
  }
}

t<-subset(DebtDataCORP, DebtDataCORP$Technology == "Coal" & DebtDataCORP$Year == Startyear)
sum(t$CompanyLvlProd)


#####
DebtDataCORP <- ddply(DebtDataCORP,.(DebtTicker, Sector, Year, Technology, BenchmarkRegion, ClimateSecDebt, TotalDebt), summarize,
                      CompanyLvlProd=sum(CompanyLvlProd, na.rm=TRUE)) #PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

DebtDataCORP <- subset(DebtDataCORP, CompanyLvlProd != 0 & ((Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | 
                                                              (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                                                              (Sector == "Oil&Gas" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | 
                                                              (Sector == "Coal" & BenchmarkRegion %in% "Global")))

DebtDataCORPSec <- ddply(DebtDataCORP,.(DebtTicker, Sector, Year, BenchmarkRegion, ClimateSecDebt, TotalDebt), summarize,
                         CompanyLvlSecProd = sum(CompanyLvlProd, na.rm=TRUE)) # PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

#DebtDataCORPSec <- rename(aggregate(DebtDataCORP["CompanyLvlProd"], by=DebtDataCORP[,c("DebtTicker", "Sector", "Year", "TotalDebt", "BenchmarkRegion")], FUN = sum), c("CompanyLvlProd" = "CompanyLvlSecProd"))

DebtDataCORPSecRef <- subset(DebtDataCORPSec, Year == Startyear)
DebtDataCORPSecRef <- subset(DebtDataCORPSecRef, select = -c(Year))
DebtDataCORPSecGlobal <- ddply(DebtDataCORPSecRef,.(DebtTicker, Sector, ClimateSecDebt,TotalDebt), summarize,
                               CompanyLvlSecProdGlobal=sum(CompanyLvlSecProd, na.rm=TRUE))
DebtDataCORPSecRef <- merge(DebtDataCORPSecRef, DebtDataCORPSecGlobal, by = c("DebtTicker", "Sector","ClimateSecDebt", "TotalDebt"))
DebtDataCORPSecRef$RegionalWeightingFactorCompany <- DebtDataCORPSecRef$CompanyLvlSecProd / DebtDataCORPSecRef$CompanyLvlSecProdGlobal

DebtDataCORP <- merge(DebtDataCORP,DebtDataCORPSec, by = c("DebtTicker", "Sector", "BenchmarkRegion", "Year", "ClimateSecDebt", "TotalDebt")) # "PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", 
DebtDataCORP <- merge(DebtDataCORP,subset(DebtDataCORPSecRef, select = c("DebtTicker", "Sector", "BenchmarkRegion", "ClimateSecDebt", "TotalDebt", "RegionalWeightingFactorCompany")), by = c("DebtTicker", "Sector", "BenchmarkRegion","ClimateSecDebt", "TotalDebt")) #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt",

# Calaculate the technolgoy share/fuel mix
DebtDataCORP$TechShare <- 1
DebtDataCORP$Aggregation <- "GlobalAggregate"
DebtDataCORP$TechShare<- DebtDataCORP$CompanyLvlProd/DebtDataCORP$CompanyLvlSecProd

DebtDataCORP_global[setdiff(colnames(DebtDataCORP),colnames(DebtDataCORP_global))] <- NA
DebtDataCORP <- rbind(DebtDataCORP, DebtDataCORP_global)
DebtDataCORP[is.na(DebtDataCORP)] <- 0

# Calculate the production per $1 of corp bond debt issued
DebtDataCORP$DebtIntensity <- 0
DebtDataCORP$DebtIntensity[DebtDataCORP$ClimateSecDebt != 0] <- DebtDataCORP$CompanyLvlProd[DebtDataCORP$ClimateSecDebt != 0]/DebtDataCORP$ClimateSecDebt[DebtDataCORP$ClimateSecDebt != 0]

# Filter the market for companies that don't have debt production
# DebtDataCORP <- subset(DebtDataCORP, DebtDataCORP$DebtIntensity != 0)
DebtDataCORP <- subset(DebtDataCORP, DebtDataCORP$DebtIntensity != 0) 

# Calculate the weight of debt for teach technolgoy relative to the total corp bond debt market
DebtDataCORP$Wt <- DebtDataCORP$ClimateSecDebt/TotalDebt

# Allocate production to the 'portfolio' by the portfolio wt approach
DebtDataCORP$WtProduction <- DebtDataCORP$CompanyLvlProd * DebtDataCORP$Wt

# Allocate a portion of the technolgy mix based off the technolgies debt wt in the market
DebtDataCORP$WtTechShare <- DebtDataCORP$TechShare * DebtDataCORP$Wt


##calculate company level change (growth) in production to be used for new metric that scales the portfolio wt by te companies planned production
CompGrowthRefVal <- subset(DebtDataCORP, DebtDataCORP$Year == Startyear, select = c("DebtTicker","Sector","BenchmarkRegion","Technology","CompanyLvlProd","Aggregation"))
CompGrowthRefVal <- rename(CompGrowthRefVal, c("CompanyLvlProd" = "RefCompanyLvlProd"))

DebtDataCORP <- merge(DebtDataCORP, CompGrowthRefVal, by =  c("DebtTicker","Sector","BenchmarkRegion","Technology","Aggregation"), all.x = TRUE)
DebtDataCORP$RefCompanyLvlGrowth[!is.na(DebtDataCORP$RefCompanyLvlProd)] <- (DebtDataCORP$CompanyLvlProd[!is.na(DebtDataCORP$RefCompanyLvlProd)]-DebtDataCORP$RefCompanyLvlProd[!is.na(DebtDataCORP$RefCompanyLvlProd)])/DebtDataCORP$RefCompanyLvlProd[!is.na(DebtDataCORP$RefCompanyLvlProd)]


# Merge with IEA targets (only by year =  start year!)
MarketData <- subset(DebtDataCORP, DebtDataCORP$Year == Startyear & Aggregation == "GlobalAggregate", select = c("DebtTicker", "Sector", "Technology","Year", "ClimateSecDebt", "TotalDebt", "CompanyLvlProd", "CompanyLvlSecProd","RegionalWeightingFactorCompany", "BenchmarkRegion"))
MarketData <- rename(MarketData, c("CompanyLvlProd" = "RefCompanyLvlProd", "CompanyLvlSecProd" = "RefCompanyLvlSecProd"))
#MarketData$BenchmarkRegion <- "Global" 

MarketRef <- subset(MarketData, MarketData$Year == Startyear, select = -c(Year))

# Technology share of the technology in this benchmarkregion
MarketRef$MarketTechShareRef <- MarketRef$RefCompanyLvlProd/MarketRef$RefCompanyLvlSecProd
# Technology share of the technology in of the global portfolio
MarketRef$RegionWtMarketTechShareRef <- MarketRef$MarketTechShareRef * MarketRef$RegionalWeightingFactorCompany

## Calculate
MarketRef[is.na(MarketRef)] <- 0
MarketRef <- subset(MarketRef, ClimateSecDebt != 0)

MarketRef$Wt <- MarketRef$ClimateSecDebt/TotalDebt
MarketRef$RegionWtMarketWtCompanies <-  MarketRef$Wt * MarketRef$RegionalWeightingFactorCompany

CompanyWeightingData <- unique(subset(MarketRef, select = c("DebtTicker","Sector","BenchmarkRegion","Wt","RegionalWeightingFactorCompany","RegionWtMarketWtCompanies")))

MarketRef$CompWtProjMarketTechShare <- MarketRef$RegionWtMarketTechShareRef * MarketRef$Wt

saveMarketRef <- MarketRef
MarketRef <- saveMarketRef

RegionalMarketWeights <- CompanyWeightingData
RegionalMarketWeights <- ddply(RegionalMarketWeights,.(Sector,BenchmarkRegion),summarise, MarketWeightRegionWeight = sum(RegionWtMarketWtCompanies,na.rm = TRUE))

MarketRef <- ddply(MarketRef,.(Sector, Technology, BenchmarkRegion), summarize,
                   RefMarketLvlProd = sum(RefCompanyLvlProd, na.rm=TRUE),
                   # EquityIntensity=sum(EquityIntensity, na.rm=TRUE),
                   Wt=sum(Wt, na.rm=TRUE),
                   # WtProduction=sum(WtProduction, na.rm=TRUE),
                   RefCarstensMetric = sum(CompWtProjMarketTechShare, na.rm=TRUE))

MarketRefSec <- ddply(MarketRef,.(Sector, BenchmarkRegion), summarize,
                      RefMarketLvlSecProd  = sum(RefMarketLvlProd, na.rm=TRUE),
                      RefCarstensMetricSec = sum(RefCarstensMetric, na.rm=TRUE))

MarketRef <- merge(MarketRef, MarketRefSec, by= c("Sector", "BenchmarkRegion"), all.x = TRUE)

# Convert IEA sectors for fossel fuels
IEATargetssub$Sector[IEATargetssub$Technology == "Coal"] <- "Coal"
IEATargetssub$Sector[IEATargetssub$Technology == "Oil"] <- "Oil&Gas"
IEATargetssub$Sector[IEATargetssub$Technology == "Gas"] <- "Oil&Gas"

MarketRef <- merge(MarketRef,IEATargetssub, by = c("BenchmarkRegion","Sector","Technology"), all.x = TRUE)

### Calculate projected production values
## Relative to the market and including current assets and future plans 
MarketRef$ProjMarketProd <-  MarketRef$RefMarketLvlProd + MarketRef$RefMarketLvlSecProd * MarketRef$FairSharePerc
MarketRef$ProjMarketProd[which(MarketRef$Direction == "declining")] <-  MarketRef$RefMarketLvlProd[which(MarketRef$Direction == "declining")] * (1 + MarketRef$FairSharePerc[which(MarketRef$Direction == "declining")])
MarketRef$Benchmark_CarstensMetric <-  MarketRef$RefCarstensMetric +  MarketRef$RefCarstensMetricSec * MarketRef$FairSharePerc
MarketRef$Benchmark_CarstensMetric[which(MarketRef$Direction == "declining")] <-  MarketRef$RefCarstensMetric[which(MarketRef$Direction == "declining")] * (1 + MarketRef$FairSharePerc[which(MarketRef$Direction == "declining")])
MarketRef$WtProjMarketProduction <- MarketRef$ProjMarketProd * MarketRef$Wt

#new metric
#MarketRef$Intensification <- ProjMarketProd

MarketRefSec <- ddply(MarketRef,.(Sector, Year, Scenario, BenchmarkRegion), summarize, 
                      ProjMarketProdSec = sum(ProjMarketProd, na.rm=TRUE), 
                      WtProjMarketProductionSec = sum(WtProjMarketProduction, na.rm = TRUE),
                      Benchmark_CarstensMetricSec = sum(Benchmark_CarstensMetric,na.rm = TRUE))

MarketRef <- merge(MarketRef, MarketRefSec, by= c("Sector", "BenchmarkRegion", "Year","Scenario"), all.x = TRUE)

MarketRef <- merge(MarketRef, MarketSecWts, by = "Sector",all.x = TRUE, all.y = FALSE)

MarketRef$WtTechShareTechShare <- MarketRef$Benchmark_CarstensMetric / MarketRef$Benchmark_CarstensMetricSec

MarketRef <- rename(MarketRef, c("Wt" = "WtMarket", "SectorWeight" = "SecWtMarket",	"WtTechShareTechShare" = "WtTechShareTechShareMarket")) #"SectorWeight" = "SectorWeightMarket"

#Changed this as it created duplicates without unique (due to SCenarios)
CompGrowthRefVal <- unique(subset(MarketRef, MarketRef$Year == Startyear & !is.na(MarketRef$ProjMarketProd), select = c("Sector","BenchmarkRegion","Technology","ProjMarketProd")))
CompGrowthRefVal <- rename(CompGrowthRefVal, c("ProjMarketProd" = "RefProjMarketProd"))

MarketRef<- merge(MarketRef, CompGrowthRefVal, by =  c("Sector","BenchmarkRegion","Technology"), all.x = TRUE)
MarketRef$RefCompanyLvlGrowth[!is.na(MarketRef$RefProjMarketProd)] <- (MarketRef$CompanyLvlProd[!is.na(MarketRef$RefProjMarketProd)]-MarketRef$RefProjMarketProd[!is.na(MarketRef$RefProjMarketProd)])/MarketRef$RefProjMarketProd[!is.na(MarketRef$RefProjMarketProd)]

RegionalBenchmark <- MarketRef

MarketBenchmarkGlobal <- merge(MarketRef,RegionalMarketWeights, by = c("Sector","BenchmarkRegion"),all.x = TRUE)
MarketBenchmarkGlobal$PortfolioSpecific_RegWt_CarstensMetric <- MarketBenchmarkGlobal$Benchmark_CarstensMetric / MarketBenchmarkGlobal$Benchmark_CarstensMetricSec * MarketBenchmarkGlobal$MarketWeightRegionWeight
MarketBenchmarkGlobalAggregate <- aggregate(MarketBenchmarkGlobal["PortfolioSpecific_RegWt_CarstensMetric"], by=MarketBenchmarkGlobal[,c("Sector","Year","Technology","Scenario")], FUN=sum)
MarketBenchmarkGlobalAggregate$BenchmarkRegion <- "GlobalAggregate"
MarketBenchmarkGlobalAggregate <- rename(MarketBenchmarkGlobalAggregate, c("PortfolioSpecific_RegWt_CarstensMetric" = "Benchmark_CarstensMetric"))


# Calculate Market Global Aggregate benchmark - compare to global benchmark using old code.
MarketRefSave <- MarketBenchmarkGlobalAggregate
MarketRefSave$PortName <- "Market_Benchmark"

write.csv(MarketData, paste0("Data/DebtMarketData_", Date, ".csv"), row.names = FALSE, na = "")

MasterDataDebt <- subset(DebtDataCORP, select = -c(WtProduction))

##cross validation checks
write.csv(DebtDataCORP, "DebtDataCORP.csv", row.names = FALSE, na = "")
write.csv(MarketRefSave, "MarketRefSave.csv", row.names = FALSE, na = "")
write.csv(UniversePort, "UniversePort.csv", row.names = FALSE, na = "")

x <- read.csv(paste0(FolderLocation,"Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
x <- subset(x, select = -c(Total))
x <- melt(x, id.vars = c("Co..Corp.TKR"), variable.name = "Subgroup", value.name = "Position")
x <- subset(x, !is.na(x$Position))
x <- subset(x, !is.na(x$Position) & x$Co..Corp.TKR != "Total")

write.csv(x, "DebtDataBBG.csv", row.names = FALSE, na = "")

y <- subset(MasterData, MasterData$Year %in% c("2016","2021")) #this is the same as doing it from MasterDatasub
y <- ddply(y,.(DebtTicker, Sector, Technology, Year), summarize, CompanyLvlProd = sum(CompanyLvlProd))
y <- dcast(y, DebtTicker + Sector + Technology ~ Year, value.var = "CompanyLvlProd")

write.csv(y, "DebtALD.csv", row.names = FALSE, na = "")



# ------
# g) Read in Portfolio data
# ------

#Get List of Portfolios either from folder or from 1 file (PortfolioN-check format) depending on Bulk-Methode
#Clean Holdingds data
MissingISIN <- subset(PortfolioAllPorts, ISIN == "")
# Some bonds don't have icines.. 
# PortfolioAllPorts <- subset(PortfolioAllPorts, ISIN != "")
saveAllPorts <- PortfolioAllPorts
#PortfolioAllPorts <- saveAllPorts ## reset input

if("Cnty.of.Dom" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Cnty.of.Dom" = "CNTRY_OF_DOMICILE"))
}else if("Country.ISO.Code" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Country.ISO.Code" = "CNTRY_OF_DOMICILE"))
}

## Merge PortfolioNs with BBG Data for those ISINs
# BBGPORTOutput <- subset(BBG_Data, !ISIN %in% c("#N/A N/A","") | !is.na(BBG_Data$ISIN)) # use this when using BBG-Data-Bind_v2
BBGPORTOutput <- unique(subset(BBGPORTOutput, !(ISIN %in% c("#N/A N/A","",NA) | is.na(BBGPORTOutput$ISIN)), c("Ticker" , "Subgroup", "ISIN", "CNTRY_OF_DOMICILE")))
#PortfolioAllPorts <-subset(PortfolioAllPorts, select = -c(Ticker))
PortfolioAllPorts <- merge(PortfolioAllPorts, BBGPORTOutput, by = c("ISIN"), all.x = TRUE,all.y=FALSE)

CorpDebtTicker <- colsplit(PortfolioAllPorts$Ticker, pattern = " ", names = c("COMPANY_CORP_TICKER",2,3))[1]
PortfolioAllPorts <- cbind(PortfolioAllPorts,CorpDebtTicker)

PortGovBanksSupraNat <- subset(PortfolioAllPorts, PortfolioAllPorts$Ticker %in% GovBanksSupraNat$Co..Corp.TKR)
PortfolioAllPorts <- subset(PortfolioAllPorts, !PortfolioAllPorts$Ticker %in% GovBanksSupraNat$Co..Corp.TKR)
# PortfolioAllPorts <- subset(PortfolioAllPorts, select = -(Fund.Size))

MissingBBGInfo <- unique(subset(PortfolioAllPorts, is.na(Subgroup), select = "ISIN"))
MissingBBGInfo$QTY <- 1
MissingBBGInfo$Date <- BBGDataDate


#Create metaportfolio
#PortfolioAllPorts <- rename(PortfolioAllPorts, c("BrandName" = "InvestorName", "FundName" = "PortfolioName"))
MetaPort <- PortfolioAllPorts
MetaPort$InvestorName <- "MetaPortfolio"
MetaPort$PortfolioName <- "MetaPortfolio"

PortfolioAllPorts <- rbind(PortfolioAllPorts,MetaPort)




##  Prepare PortfolioN and Investor names by cleaning

if(!"Position" %in% colnames(PortfolioAllPorts)){
  if("ValueUSD" %in% colnames(PortfolioAllPorts)){
    PortfolioAllPorts$Position <- PortfolioAllPorts$ValueUSD 
  }else{
    print("Check portfolio input: No position input or value given")
  }
}

#Get rid of NA´s and negative or NAN values in Number of shares
PortfolioAllPorts$Position <- as.numeric(PortfolioAllPorts$Position)
# PortfolioAllPorts$Position <- as.numeric(PortfolioAllPorts$Position)
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
# PortfolioAllPorts$InvestorName[PortfolioAllPorts$InvestorName %in% PortfolioAllPorts$PortfolioName] <- paste(PortfolioAllPorts$InvestorName[PortfolioAllPorts$InvestorName %in% PortfolioAllPorts$PortfolioName],"_Investor",sep = "")

PortfolioAllPorts<-subset(PortfolioAllPorts, select = c("COMPANY_CORP_TICKER", "Subgroup", "Position", "InvestorName", "PortfolioName", "Ticker", "ISIN", "CNTRY_OF_DOMICILE"))

# Create portfolio for bond universe
UniversePort <- rename(UniversePort, c("DebtTicker"= "COMPANY_CORP_TICKER"))
UniversePort <- subset(UniversePort, select = -c(SectorWeight,piesector))
UniversePort$InvestorName <- "GlobalBondUniverse"
UniversePort$PortfolioName <- "GlobalBondUniverse"
UniversePort$Ticker <- "GlobalBondUniverse"
UniversePort$ISIN <- "GlobalBondUniverse"
UniversePort$CNTRY_OF_DOMICILE <- "GlobalBondUniverse"
#UniversePort$Type <- "GlobalBondUniverse"
#UniversePort$CompanyName <- "GlobalBondUniverse"

PortfolioAllPorts <- rbind(PortfolioAllPorts, UniversePort)

PortfolioNameCount <- data.frame(table(PortfolioAllPorts$PortfolioName))
InvestorList <- data.frame(InvestorName = unique(PortfolioAllPorts$InvestorName))
InvestorList$Type <- "Investor"
InvestorList$PortfolioName <- NA
UniquePortList <- data.frame (unique(subset(PortfolioAllPorts, select = c("PortfolioName","InvestorName"))))
UniquePortList$Type <- "Portfolio"

ListAllPorts <- rbind(InvestorList,UniquePortList)
ListAllPorts <- subset(ListAllPorts, !ListAllPorts$InvestorName %in% c("MetaPortfolio_Investor") | ListAllPorts$Type == "Investor")
ListAllPorts <- subset(ListAllPorts, !ListAllPorts$InvestorName %in% c("GlobalBondUniverse") | ListAllPorts$Type != "Portfolio")

#--------------
# |------------------------------|
# |----- Portfolio Analysis -----|
# |------------------------------|
for (i in  1:length(ListAllPorts$PortfolioName)){
  
  # for (i in  1:length(UniquePortList$PortfolioName)){
  tryCatch({
    print(i)
    if (ListAllPorts$Type[i] == "Portfolio"){ 
      Portfolio = subset(PortfolioAllPorts, PortfolioName == ListAllPorts$PortfolioName[i] & InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- paste0(ListAllPorts$PortfolioName[i],"_",ListAllPorts$InvestorName[i])
    }else{
      Portfolio = subset(PortfolioAllPorts, InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- ListAllPorts$InvestorName[i]
      
    }
    
    Portfolio <- subset(Portfolio, select = c("COMPANY_CORP_TICKER", "Position", "Subgroup" , "Ticker", "ISIN", "CNTRY_OF_DOMICILE"))
    TotalPortfolioAUM <- sum(Portfolio$Position,na.rm = TRUE)
    
    #sum position over isn and ticker in case of multiples
    Portfolio <- ddply(Portfolio,.(COMPANY_CORP_TICKER, Subgroup, Ticker, ISIN, CNTRY_OF_DOMICILE), summarize, Position = sum(Position))
    
    
    ##if there is no price information or if the asset is outside of the region
    Portfolio$Position <- as.numeric(Portfolio$Position)
    if(sum(Portfolio$Position, na.rm = TRUE) != 0){
      # Subset the Portfolio by securities that cannot be assessed, i.e. with missing price or country information
      Portfolio$AUM <- Portfolio$Position
      PortAUM<-sum(Portfolio$AUM, na.rm = TRUE)
      PortfolioAUMAssessed <- sum(subset(Portfolio, Portfolio$COMPANY_CORP_TICKER != "")$AUM, na.rm = TRUE)
      if(eval(ListAllPorts$InvestorName[i]) == "GlobalBondUniverse") { PortAUM<- TotalDebt }
      #Calculate PortWeight at security level
      if(sum(Portfolio$AUM, na.rm = TRUE) != 0){
        Portfolio$IssLvlPortWeight<-Portfolio$AUM/PortAUM
        
        # Add sector to the Portfolio. There are multiple companies to each corp ticker so there are then in some instance multiple sector classifications, this part of the code trys to take the main setor, but it's not perfect... will not be a problem once we go to issuer level
        
        SectorProduction <- unique(subset(MasterDataDebt, select = c("DebtTicker","Sector")))
        Portfolio <- merge(Portfolio, SectorProduction, by.x = "COMPANY_CORP_TICKER",by.y = "DebtTicker", all.x = TRUE, all.y = FALSE)
        # #sort by 'Sector' to get Automotive and FF first, then power, then deduplicate to deal with non-utility power
        # Portfolio <- Portfolio[order(Portfolio$Sector,decreasing = FALSE),]
        # #if(eval(ListAllPorts$InvestorName[i]) != "GlobalBondUniverse") { Portfolio <- Portfolio[!duplicated(Portfolio$ISIN),] }
        # # remove duplicated tickers due to multi subsector classifications
        # PortSub <- unique(subset(Portfolio, select = c("Sector", "COMPANY_CORP_TICKER")))
        # PortSubdubs <- PortSub[duplicated(PortSub$COMPANY_CORP_TICKER),]
        # PortSub <- subset(PortSub, PortSub$COMPANY_CORP_TICKER %in% PortSubdubs$COMPANY_CORP_TICKER &  PortSub$Sector == "Power")
        # if(dim(PortSub)[1]>0) {Portfolio2 <- subset(Portfolio, paste0(as.character(Portfolio$COMPANY_CORP_TICKER), as.character(Portfolio$Sector)) != paste0(PortSub$COMPANY_CORP_TICKER, PortSub$Sector))}
        # rm(PortSub,PortSubdubs)
        # 
        #Meta-Analysis for piechart & Moodys Risk map
        # Sector exposure merging
        Portfolio$piesector <- "Not Assessed" #label non-benchmarked sectors
        Portfolio$piesector[Portfolio$Subgroup %in% OilGasBIC] <- "Oil&Gas"
        Portfolio$piesector[Portfolio$Subgroup %in% CoalBIC] <- "Coal"
        Portfolio$piesector[Portfolio$Subgroup %in% AutoBIC] <- "Automotive"
        Portfolio$piesector[Portfolio$Subgroup %in% PowerBIC & Portfolio$Sector %in% "Power"] <- "Utility Power"
        Portfolio$piesector[!(Portfolio$Subgroup %in% PowerBIC) & Portfolio$Sector %in% "Power"] <- "NonUtility Power"
        Portfolio$piesector[!(Portfolio$Subgroup %in% OilGasBIC) & Portfolio$Sector %in% "Oil&Gas"] <- "NonOG Production"
        
        # set weighting of companies that are duplicates (ff or auto and non-utility) to 0 for the non-utility part. THIS DIDN"T WORK WITHOUT EQY PortfolioN TICKERS, ALTERNATIVE BELOW
        temp1 <- unique(subset(Portfolio, !piesector %in% c("Utility Power"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp1)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "NonUtility Power" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "NonUtility Power" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "NonUtility Power" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
        }
        temp2 <- unique(subset(Portfolio, Sector %in% c("Power"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp2)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
        }
        rm(temp1,temp2)
        
        temp1 <- unique(subset(Portfolio, !piesector %in% c("Oil&Gas"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp1)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "NonUtility Power" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "NonOG Production" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "NonOG Production" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
        }
        temp2 <- unique(subset(Portfolio, Sector %in% c("Oil&Gas"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp2)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
        }
        rm(temp1,temp2)  
        
        temp1 <- unique(subset(Portfolio, !piesector %in% c("Coal"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp1)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "NonUtility Power" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "NonOG Production" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "NonOG Production" & Portfolio$COMPANY_CORP_TICKER %in% temp1$COMPANY_CORP_TICKER] <- 0
        }
        temp2 <- unique(subset(Portfolio, Sector %in% c("Coal"), select = "COMPANY_CORP_TICKER"))
        if(dim(temp2)[1]>0){
          #Portfolio$PortWeight[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$AUM[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
          Portfolio$Position[Portfolio$piesector == "Not Assessed" & Portfolio$COMPANY_CORP_TICKER %in% temp2$COMPANY_CORP_TICKER] <- 0
        }
        rm(temp1,temp2) 
        
        Portfolio$piesector[Portfolio$Subgroup %in% FuturesecsBIC] <- Portfolio$Subgroup[Portfolio$Subgroup %in% FuturesecsBIC]
        
        PortfolioSectorWeights <- ddply(unique(subset(Portfolio, select = c("COMPANY_CORP_TICKER", "Sector","AUM"))),.(Sector),summarize, SectorAUM = sum(AUM,na.rm = TRUE))
        PortfolioSectorWeights$SectorWeight <- PortfolioSectorWeights$SectorAUM / PortAUM
        
        # Moody's Risk map merging
        BICSMoodysRisk <- read.csv(paste0(FolderLocation,"Data/BICS_to_MoodysRisk_Bridge.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
        BICSMoodysRisk <- unique(subset(BICSMoodysRisk,select = c("INDUSTRY_SUBGROUP","MoodysRiskLvl")))
        Portfolio <- merge(Portfolio, BICSMoodysRisk, by.x = c("Subgroup"), by.y = "INDUSTRY_SUBGROUP", all.x = TRUE, all.y = FALSE)
        Portfolio$MoodysRiskLvl[is.na(Portfolio$MoodysRiskLvl)] <- 5
        
        ## Need to aggregate to piesector level and corp level, not just corp level, if you want to later split the companies that have OG production and are classified as OGs
        
        # Aggregate to CORP level
        PortfolioSub <- ddply(subset(Portfolio,Position!=0),.(COMPANY_CORP_TICKER, piesector), summarize, Position = sum(Position))
        # Calculate new portoflio weights at the Corp Debt ticker level
        PortAUM2 <- sum(PortfolioSub$Position)
        #if (round(PortAUM,1) != round(PortAUM2,1)) break 
        #rm(PortAUM2)
        
        PortfolioSub$PortWeightDebtlvl <- PortfolioSub$Position/PortAUM
        
        #Combine with Asset level data
        PortfolioSub$piesector[PortfolioSub$piesector %in% "Utility Power"] <- "Power"
        ReducedListDebt <- merge(subset(MasterDataDebt,Aggregation == "Global"), PortfolioSub, by.x = c("DebtTicker","Sector"), by.y = c("COMPANY_CORP_TICKER", "piesector"), all.x=FALSE, all.y=FALSE)
        #ReducedListDebt2 <- merge(subset(MasterDataDebt), PortfolioSub, by.x = c("DebtTicker","Sector"), by.y = c("COMPANY_CORP_TICKER", "piesector"), all.x=FALSE, all.y=FALSE)
        
        # Combine with Asset level data
        # first aggregate to CORP to deal with multiple CORPs to one EQY_PortfolioN
        # ReducedListDebtpre <- ddply(subset(PortfolioSub, select = -c(EQY_PortfolioN_TICKER)),.(COMPANY_CORP_TICKER,CNTRY_OF_DOMICILE),summarize,Position = sum(Position,na.rm=TRUE))
        # ReducedListDebt<- merge(MasterDataDebt, ReducedListDebtpre, by.x = c("Sub_COMPANY_CORP_TICKER","CNTRY_OF_DOMICILE"), by.y = c("COMPANY_CORP_TICKER","CNTRY_OF_DOMICILE"), all.x=FALSE, all.y=FALSE)
        # ReducedListDebt <-unique(ReducedListDebt)
        PortAUMAnalyzed <- 0
        if(dim(ReducedListDebt)[1]>0){
          PortAUMAnalyzed <- sum(unique(subset(ReducedListDebt, select = c("Position", "DebtTicker")))[,"Position"], na.rm = TRUE)
          PortfolioRegionality <- merge(PortfolioSub,CompanyWeightingData, by.x = "COMPANY_CORP_TICKER", by.y = "DebtTicker", all.x = FALSE, all.y = FALSE)
          PortfolioRegionality$RegionalWtPortfolioWt <- PortfolioRegionality$PortWeightDebtlvl * PortfolioRegionality$RegionalWeightingFactorCompany
          PortfolioRegionSplit <- aggregate(PortfolioRegionality["RegionalWtPortfolioWt"],by = PortfolioRegionality[,c("Sector","BenchmarkRegion")], FUN = sum)
          
          # ReducedListDebt$DebtIntensity[is.na(ReducedListDebt$DebtIntensity)] <- 0
          ReducedListDebt$Position<-as.numeric(ReducedListDebt$Position)
          #calculate Portfolio production
          # ReducedListDebt$DebtProduction <- as.numeric(ReducedListDebt$DebtIntensity) * ReducedListDebt$Position #Production = Production/AmtOut * Position
          # An option for the  fossil fuels market for the contribution to debt method, the allocation of production could to be weighted by the percentage of the portfolio that is exposed to fossil fuel production, if the debt intensity is calaculated at company level 
          #ReducedListDebt$DebtProduction[ReducedListDebt$Sector == "Fossil Fuels"] <- as.numeric(ReducedListDebt$DebtIntensity[ReducedListDebt$Sector == "Fossil Fuels"]) * ReducedListDebt$Position[ReducedListDebt$Sector == "Fossil Fuels"]  *  ReducedListDebt$PortWeightDebtlvl[ReducedListDebt$Sector == "Fossil Fuels"]
          ReducedListDebt$WtProduction <- ReducedListDebt$CompanyLvlProd * ReducedListDebt$PortWeightDebtlvl
          ReducedListDebt$Intensification <- ReducedListDebt$Wt*ReducedListDebt$RefCompanyLvlGrowth
          #check to ensure that Position isn't greater than total debt. If so scale production down to CompanyLvlProd
          ReducedListDebt$Production_unalt <- ReducedListDebt$Production
          ReducedListDebt$Production[ReducedListDebt$Production > ReducedListDebt$CompanyLvlProd] <- ReducedListDebt$CompanyLvlProd[ReducedListDebt$Production > ReducedListDebt$CompanyLvlProd]
          ReducedListDebt$badprodflag <- 0
          ReducedListDebt$badprodflag[ReducedListDebt$Production != ReducedListDebt$Production_unalt] <- 1
          
          # Minimise data frame size by restricting results to only a 10 year forcast
          ReducedListDebt <- subset (ReducedListDebt, Year <= (Startyear + 10))
          # ReducedListDebt <- merge(ReducedListDebt, CountryISOList, by.x = "PlantLocation", by.y = "GDPlantLocation", all.x = TRUE)
          # ReducedListDebt <- rename(ReducedListDebt, c("COUNTRY_ISO" = "PlantLocation_ISO"))
          # # If there is no plant location for the fossil fuel production the production is considered to have come the country of doimicle of the owner
          # ReducedListDebt$PlantLocation_ISO[is.na(ReducedListDebt$PlantLocation_ISO) & ReducedListDebt$Sector == "Fossil Fuels"] <-  ReducedListDebt$CNTRY_OF_DOMICILE[is.na(ReducedListDebt$PlantLocation_ISO) & ReducedListDebt$Sector == "Fossil Fuels"]  
          
          # # Sum over the technologies and locations
          # Portfoliomix <- ddply(ReducedListDebt, .(DebtTicker, Sector, Technology, Year), summarize, 
          #                                         DebtProduction = sum(DebtProduction,na.rm=TRUE),
          #                                         WtProduction = sum(WtProduction,na.rm=TRUE),
          #                                         PortWeightDebtlvl = sum(PortWeightDebtlvl, na.rm=TRUE)) #PlantLocation_ISO, CNTRY_OF_DOMICILE
          #Portfoliomix$BenchmarkRegion <- "Global"
          #PortMix <- Portfoliomix
          ##New appraoch, calculate hte chant in weight if the wight were to increase/decrase at the same rate as the company production 
          
          PortMix <- subset(ReducedListDebt,select = -c(TotalDebt, ClimateSecDebt))
          PortMix$BenchmarkRegion <- "Global"
          ##Create a seperate rows for the debt methods
          #PortMix <- melt(PortMix, id.vars = c("DebtTicker", "Sector", "Technology", "BenchmarkRegion", "Year"), variable.name = "Method", value.name = "Production")
          
          # make sure all technologies and sectors are in the database (to avoid errors at a later stage)
          techlist <- data.frame("Technology" = c("Electric","Hybrid","ICE","OilCap","GasCap","CoalCap","RenewablesCap","HydroCap","NuclearCap", "Coal","Oil","Gas"))
          #regionlist <- data.frame("BenchmarkRegion" = c("Global","OECD","Non-OECD","EU28","U.S."))
          yearlist <- data.frame("Year" = unique(PortMix$Year))
          lineadd <- merge(techlist,yearlist)
          lineadd$Sector <- "Power"
          lineadd$Sector[lineadd$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
          lineadd$Sector[lineadd$Technology %in% c("Coal")] <- "Coal" 
          lineadd$Sector[lineadd$Technology %in%  c("Electric","Hybrid","ICE")] <-  "Automotive"
          PortMix <- subset(PortMix, Technology %in% c("Electric","Hybrid","ICE","OilCap","GasCap","CoalCap","RenewablesCap","HydroCap","NuclearCap", "Coal","Oil","Gas"))
          PortMix <- merge(PortMix,lineadd, by = c("Sector","Technology","Year"), all.y = TRUE, all.x = TRUE)
          PortMix$WtProduction[is.na(PortMix$WtProduction)] <- 0
          
          #calaculate the sector prodution and the techshare
          Sectorref <- ddply(PortMix,.(DebtTicker,BenchmarkRegion, Sector, Year),summarize,
                             WtSectorProd = sum(WtProduction,na.rm=TRUE))
          
          Sectorref2 <- ddply(PortMix,.(Sector, Year),summarize,
                              SectorWeight = sum(PortWeightDebtlvl,na.rm=TRUE))
          
          techlist2 <- unique(subset(PortMix, select = c("DebtTicker","Sector","Technology", "Year")))
          Sectorref <- merge(Sectorref,techlist2, by = c("Sector", "Year", "DebtTicker"), all.x = TRUE, all.y = TRUE)
          # #calaculate the sector prodution and the techshare
          # Sectorref <- ddply(subset(PortMix, Sector %in% c("Automotive","Power")),.(DebtTicker,BenchmarkRegion, Sector, Year),summarize, SectorProd = sum(WtProduction,na.rm=TRUE)) #Method
          # techlist2 <- unique(subset(PortMix, Sector %in% c("Automotive","Power"), select = c("DebtTicker","Sector","Technology", "Year")))
          # Sectorref <- merge(Sectorref,techlist2, by = c("Sector", "Year", "DebtTicker"), all.x = TRUE, all.y = TRUE)
          # sectorrefff <- ddply(subset(PortMix,!Sector %in% c("Automotive","Power")),.(DebtTicker,BenchmarkRegion, Technology, Year),summarize,SectorProd = sum(WtProduction,na.rm=TRUE)) #Method
          # if(dim(sectorrefff)[1]>0){sectorrefff$Sector <- "Fossil Fuels"}
          # Sectorref <- rbind(Sectorref,sectorrefff)
          PortMix <- merge(PortMix,Sectorref, by = c("DebtTicker","BenchmarkRegion", "Sector", "Technology",  "Year"), all.x=TRUE, all.y=FALSE) #Method
          # PortMix$PortWtTechShare <- PortMix$WtProduction/PortMix$SectorProd
          PortMix$PortWtTechShare <- PortMix$WtProduction/PortMix$WtSectorProd * PortMix$PortWeightDebtlvl
          
          # Calculate reference values for scnerio projection
          # Calculate the reference values (for the technology as well as for the sector in the start year/initial year) and merge it with the portfolio production mix data
          Sectorref <- subset(PortMix, Year == Startyear, select =  c("DebtTicker","BenchmarkRegion", "Sector", "Technology", "WtProduction", "WtSectorProd"))
          Sectorref <- rename(Sectorref, c("WtProduction" = "RefWtProduction", "WtSectorProd" = "RefWtSectorProd"))
          PortMix <- merge(PortMix, Sectorref, by = c("DebtTicker","BenchmarkRegion", "Sector", "Technology"), all.x=TRUE, all.y=FALSE) #Method
          PortMix1 <- PortMix
          PortMix1$CarstensMetric <-  PortMix1$PortWeightDebtlvl * PortMix1$TechShare
          
          PortMix1 <- ddply(subset(PortMix1, PortMix1$DebtTicker != "TotalDebt"),.(Sector, Year, Technology), summarize,
                            CompanyLvlProd = sum(CompanyLvlProd, na.rm=TRUE),
                            # DebtIntensity=sum(DebtIntensity, na.rm=TRUE),
                            Wt=sum(PortWeightDebtlvl, na.rm=TRUE),
                            WtProduction=sum(WtProduction, na.rm=TRUE),
                            PortWtTechShare = sum(PortWtTechShare, na.rm=TRUE),
                            Intensification = sum(Intensification,na.rm=TRUE),
                            CarstensMetric = sum(CarstensMetric, na.rm=TRUE))
          
          PortMix1Sec <- ddply(PortMix1,.(Sector, Year), summarize,
                               CompanyLvlProdSec=sum(CompanyLvlProd, na.rm=TRUE),
                               WtProductionSec = sum(WtProduction, na.rm=TRUE),
                               PortWtTechShareSec=sum(PortWtTechShare, na.rm=TRUE))
          
          PortMix1 <- datacompletion(PortMix1)
          PortMix1 <- merge(PortMix1, PortMix1Sec, by = c("Sector", "Year"), all.x = TRUE)
          PortMix1$CompanyLvlProdSec [is.na(PortMix1$CompanyLvlProdSec)] <- 0  
          PortMix1$WtProductionSec [is.na(PortMix1$WtProductionSec)] <- 0  
          PortMix1$PortWtTechShareSec [is.na(PortMix1$PortWtTechShareSec)] <- 0  
          PortMix1 <- merge(PortMix1, subset(PortfolioSectorWeights, select = c("Sector","SectorWeight")), by = "Sector", all.x = TRUE, all.y = FALSE)
          
          PortMix1$WtTechShareTechShare <- PortMix1$PortWtTechShare/PortMix1$PortWtTechShareSec
          # PortMix1$WtTechShareTechShare2 <- PortMix1$CarstensMetric / PortMix1$SectorWeight
          PortMix1$PortTechShare <- PortMix1$CompanyLvlProd/PortMix1$CompanyLvlProdSec
          PortMix1$WtProductionTechShare <- PortMix1$WtProduction/PortMix1$WtProductionSec
          
          PortMix1$BenchmarkRegion <- "GlobalAggregate"
          
          
          PortfolioRegionSplit <- Regiondatacompletion(PortfolioRegionSplit)
          PortfolioBenchmark <- merge(PortfolioRegionSplit, subset(RegionalBenchmark, select = c("Sector","BenchmarkRegion", "SecWtMarket","Year","Scenario","Technology","Benchmark_CarstensMetric","Benchmark_CarstensMetricSec","RefMarketLvlProd","ProjMarketProd","ProjMarketProdSec","WtProjMarketProduction","WtTechShareTechShareMarket")), by = c("Sector","BenchmarkRegion"), all.x = TRUE)
          PortfolioBenchmark$PortfolioBenchmark_CarstensMetric <- PortfolioBenchmark$Benchmark_CarstensMetric / PortfolioBenchmark$Benchmark_CarstensMetricSec * PortfolioBenchmark$RegionalWtPortfolioWt
          PortfolioBenchmark$RegWtProjMarketProd <- PortfolioBenchmark$ProjMarketProd * PortfolioBenchmark$RegionalWtPortfolioWt
          PortfolioBenchmark$RegWtRefMarketLvlProd <- PortfolioBenchmark$RefMarketLvlProd * PortfolioBenchmark$RegionalWtPortfolioWt
          
          # PortfolioBenchmark <- merge(PortfolioBenchmark,unique(subset(PortMix1, Year == Startyear,select = c("Technology","Wt"))), by = "Technology", all.x = TRUE) # that actually makes no sense but maybe we need to calculate the Wt
          PortfolioBenchmark$Benchmark_WtTechShare <- PortfolioBenchmark$RegWtProjMarketProd/PortfolioBenchmark$ProjMarketProdSec
          PortfolioBenchmark$Benchmark_WtTechShareTechShare <- PortfolioBenchmark$WtTechShareTechShareMarket * PortfolioBenchmark$RegionalWtPortfolioWt
          
          PortBenchmark <- ddply(PortfolioBenchmark,.(Sector, Year, Scenario, Technology,  SecWtMarket), summarize,
                                 Benchmark_CarstensMetric=sum(PortfolioBenchmark_CarstensMetric, na.rm=TRUE),
                                 RegWtProjMarketProd = sum(RegWtProjMarketProd,na.rm = TRUE),
                                 RegWtRefMarketLvlProd = sum(RegWtRefMarketLvlProd,na.rm = TRUE),
                                 Benchmark_WtTechShare = sum(Benchmark_WtTechShare,na.rm = TRUE),
                                 Benchmark_WtTechShareTechShare = sum(Benchmark_WtTechShareTechShare ,na.rm = TRUE))
          
          PortBenchmarkSec <- ddply(PortBenchmark,.(Sector, Year, Scenario), summarize,
                                    RegWtProjMarketProdSec = sum(RegWtProjMarketProd,na.rm = TRUE),
                                    Benchmark_WtTechShareSec = sum(Benchmark_WtTechShare,na.rm = TRUE),
                                    Benchmark_WtTechShareTechShareSec = sum(Benchmark_WtTechShareTechShare ,na.rm = TRUE))
          
          PortBenchmark <- merge(PortBenchmark, PortBenchmarkSec, by= c("Sector", "Year", "Scenario"))
          
          PortBenchmark$Benchmark_OGC <- PortBenchmark$RegWtProjMarketProd / PortBenchmark$RegWtRefMarketLvlProd * 100
          PortBenchmark$Benchmark_WtTechShareTechShare_corrupted <- PortBenchmark$Benchmark_WtTechShare / PortBenchmark$Benchmark_WtTechShareSec
          PortBenchmark$Benchmark_WtTechShareTechShare <- PortBenchmark$Benchmark_WtTechShareTechShare / PortBenchmark$Benchmark_WtTechShareTechShareSec
          
          #  aggColnames <- c(colnames(PortfolioBenchmark)[!colnames(MarketBenchmarkGlobal) %in% c("PortfolioBenchmark_CarstensMetric","BenchmarkRegion")])
          # PortBenchmark <- aggregate(PortfolioBenchmark["PortfolioBenchmark_CarstensMetric"], by = PortfolioBenchmark[,c("Sector",  "Year", "Scenario", "Technology")], FUN = sum)
          # 
          
          PortBenchmark$BenchmarkRegion <- "GlobalAggregate"
          #PortBenchmark <- rename(PortBenchmark, c("PortfolioBenchmark_CarstensMetric" = "Benchmark_CarstensMetric"))
          
          Combin <- merge(PortMix1, PortBenchmark, by =c("BenchmarkRegion", "Sector", "Technology", "Year"))  
          
          OGCMetrikRef <- subset(Combin, Year == Startyear)
          # OGCMetrikRef$OGCMetrik_PortfolioRef <- OGCMetrikRef$SectorWeight / OGCMetrikRef$SecWtMarket * 100
          OGCMetrikRef$OGCMetrik_PortfolioRef <- (OGCMetrikRef$SectorWeight * OGCMetrikRef$PortTechShare) / (OGCMetrikRef$SecWtMarket * OGCMetrikRef$RegWtProjMarketProd / OGCMetrikRef$RegWtProjMarketProdSec) * 100
          OGCMetrikRef <- rename(OGCMetrikRef, c("WtProduction" = "WtProductionRef"))
          
          Combin <- merge(Combin,subset(OGCMetrikRef, select = c("BenchmarkRegion", "Sector", "Technology", "Scenario", "OGCMetrik_PortfolioRef", "WtProductionRef")), by = c("BenchmarkRegion", "Sector", "Technology", "Scenario"), all.x = TRUE)
          Combin$OGCMetrik_Portfolio <- Combin$OGCMetrik_PortfolioRef * Combin$WtProduction / Combin$WtProductionRef
          Combin$OGCMetrik_Portfolio[is.nan(Combin$OGCMetrik_Portfolio)] <- 0
          
          # Calculate Exposure percentages for technologies with direct 'green' subsititues. This uses the difference in Technology mixes
          Combin$Exposure_WtTechShareTechShare <- (Combin$WtTechShareTechShare - Combin$Benchmark_WtTechShareTechShare) / Combin$Benchmark_WtTechShareTechShare
          Combin$Exposure_CarstensMetric <- (Combin$CarstensMetric - Combin$Benchmark_CarstensMetric) / Combin$Benchmark_CarstensMetric
          Combin$Exposure_OGCMetrik <- (Combin$OGCMetrik_Portfolio - Combin$Benchmark_OGC) / Combin$Benchmark_OGC
          
          Combin$PortfolioAUMTotal <- TotalPortfolioAUM
          Combin$PortfolioAUMAnalyzed <- PortfolioAUMAssessed
          Combin$PortfolioAUM2diiSectors <- PortAUMAnalyzed
          
          
          #Combin$TrajectoryExposure <- (Combin$TechShare - Combin$ScenarioShare) / Combin$ScenarioShare
          # For fossil fuels where the sector requires shrinkage
          # For the contribution to debt method, you need to benchmark based on intensity rather than absolute values for Fossil fuels
          #Combin$MarketExposure[Combin$Sector == "Fossil Fuels"] <- (Combin$Production[Combin$Sector == "Fossil Fuels"] - Combin$ProjMarketProd[Combin$Sector == "Fossil Fuels"]) / Combin$ProjMarketProd[Combin$Sector == "Fossil Fuels"] 
          #Combin$TrajectoryExposure[Combin$Sector == "Fossil Fuels"] <- (Combin$Production[Combin$Sector == "Fossil Fuels"] - Combin$ScenarioTrajectoryProd[Combin$Sector == "Fossil Fuels"]) / Combin$ScenarioTrajectoryProd[Combin$Sector == "Fossil Fuels"]
          #Combin$AUMExposure <- (Combin$Production - Combin$TargetProductionAUMIntensity) / Combin$TargetProductionAUMIntensity
          
          # # Calculate Exposure percentages for technologies with direct 'green' subsititues. This uses the difference in Technology mixes
          # Combin$MarketExposure <- (Combin$TechShare - Combin$ProjMarketTechShare) / Combin$ProjMarketTechShare
          # Combin$TrajectoryExposure <- (Combin$TechShare - Combin$ScenarioShare) / Combin$ScenarioShare
          # # For fossil fuels where the sector requires shrinkage
          # # For the contribution to debt method, you need to benchmark based on intensity rather than absolute values for Fossil fuels
          # Combin$MarketExposure[Combin$Sector == "Fossil Fuels"] <- (Combin$Production[Combin$Sector == "Fossil Fuels"] - Combin$ProjMarketProd[Combin$Sector == "Fossil Fuels"]) / Combin$ProjMarketProd[Combin$Sector == "Fossil Fuels"] 
          # Combin$TrajectoryExposure[Combin$Sector == "Fossil Fuels"] <- (Combin$Production[Combin$Sector == "Fossil Fuels"] - Combin$ScenarioTrajectoryProd[Combin$Sector == "Fossil Fuels"]) / Combin$ScenarioTrajectoryProd[Combin$Sector == "Fossil Fuels"]
          # #Combin$AUMExposure <- (Combin$Production - Combin$TargetProductionAUMIntensity) / Combin$TargetProductionAUMIntensity
          
          #implement AUM in sector values
          Combin$PortName <- as.character(PortfolioName)
          Combin$InvestorName <- ListAllPorts$InvestorName[i]
          Combin$Type <- ListAllPorts$Type[i]
          ReducedListDebt$Type <- ListAllPorts$Type[i]
          ReducedListDebt$PortName <- PortfolioName
          Portfolio$Type <- ListAllPorts$Type[i]
          Portfolio$PortName <- PortfolioName
          PortfolioList <- data.frame(PortfolioName = PortfolioName, Type = ListAllPorts$Type[i])
          Portfolio$InvestorName <- ListAllPorts$InvestorName[i]
          Portfolio$PortfolioAUMAnalyzed <- PortfolioAUMAssessed
          Portfolio$PortfolioAUM2diiSectors <- PortAUMAnalyzed
          
          
          if (exists("CombinAll")){
            CombinAll <- rbind(CombinAll,Combin)
            ReducedListDebtAll <- rbind(ReducedListDebtAll,subset(ReducedListDebt,Year %in% c(Startyear,Startyear+5)))
            PortfolioAll <- rbind(PortfolioAll,Portfolio)
            PortfolioListAll <- rbind(PortfolioListAll,PortfolioList)
          }else{
            CombinAll <- Combin
            ReducedListDebtAll <- subset(ReducedListDebt,Year %in% c(Startyear,Startyear+5))
            PortfolioAll <- Portfolio
            PortfolioListAll <- PortfolioList
          }  
          #rm(AUMmix)
        }
      }
    }
  })
}


# Create a folder for portolio results and go to that folder, 
#Definitely need to check for these

OutputFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/03_Results/01_BatchResults")
AssessmentDate <- "2016Q4"

BatchFolder <- paste0(OutputFolder,"/", BatchName,"/")
if(!dir.exists(file.path(BatchFolder))){dir.create(file.path(BatchFolder), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
BatchFolder <- paste0(OutputFolder,"/", BatchName,"/",AssessmentDate,"/")
if(!dir.exists(file.path(BatchFolder))){dir.create(file.path(BatchFolder), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  

# If there is already existing data, then copy all png,pdf and csv files into the next RunDirectory
# flist <- list.files(BatchFolder,pattern=c("\\.csv$"), full.names = TRUE)
# dlist <- list.dirs(BatchFolder, recursive = FALSE, full.names = FALSE)

# if(length(flist)>0){
#   nextrun <- 1
#   if (length(dlist)>0){nextrun <- as.numeric(gsub("Run","",dlist[length(dlist)]))+1}
#   RunDirectory <- paste0(BatchFolder,"Run",nextrun,"/")
#   dir.create(file.path(RunDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")
#   if(length(flist)>0){
#     for (file in flist){file.copy(file, RunDirectory)}}
# }

setwd(BatchFolder)


#only for Swiss!!!
if (length(grep("Swiss",BatchName))[1] == 1){
  SwissLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/02_Swiss/") #Output-folder for the results
  ParticipantList <- read.csv(paste0(SwissLocation,"/ParticipantsOverview.csv"),strip.white = TRUE, stringsAsFactors = FALSE)
  CombinAll <- merge(CombinAll,ParticipantList, by = "InvestorName", all.x = TRUE)
  PortfolioAll <- merge(PortfolioAll,ParticipantList, by = "InvestorName", all.x = TRUE)
}

MarketRefSave$CarstensMetric <- MarketRefSave$Benchmark_CarstensMetric
MarketRefSave[, setdiff(colnames(CombinAll),colnames(MarketRefSave))] <- NA
MarketRefSave$Type <- "Listed Market"

CombinAll <- rbind(CombinAll, MarketRefSave)
CombinAllsave <- CombinAll

## for not subset the results by global global..
#CombinAll <- subset(CombinAll, CombinAll$BenchmarkRegion == "Global")

# BatchName <- "Swiss_Aggregated"

#Create a folder for portolio results and go to that folder, 
# dir.create(file.path(paste0(OutputFolder,BatchName,"/")), showWarnings = TRUE, recursive = FALSE, mode = "0777")
# setwd(BatchFolder)

# Add one port in only
# resultslist <- addcompanydebt(BatchName, BatchFolder, CombinAll, ReducedListDebtAll,PortfolioAll,"GEPABU")
# CombinAll <- resultslist[[1]]
# ReducedListDebtAll <- resultslist[[2]]
# PortfolioAll <- resultslist[[3]]


# write.csv(CombinAll,paste(BatchName,"_DebtAnalysisResults.csv",sep = ""),row.names = FALSE)
# write.csv(ReducedListDebtAll,paste(BatchName,"_DebtProductionCompanies_Snapshot",Startyear+5,".csv",sep = ""),row.names = FALSE)
# write.csv(PortfolioAll,paste(BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv",sep = ""),row.names = FALSE)
# write.csv(DebtDataEconomy,paste("BondMarket_DebtAnalysisResults_", Date, ".csv",sep = ""),row.names = FALSE)  
# write.csv(MissingBBGInfo, paste0(BatchName,"_",dim(MissingBBGInfo)[1],"ISINs_wo_BBG_Info_from_Port.csv"), row.names = FALSE)
# Sub450Scenario <- subset(CombinAll, Scenario == "450S")
# write.csv(Sub450Scenario,paste(BatchName,"_DebtAnalysisResults-450S-only.csv",sep = ""),row.names = FALSE, na = "")




write.csv(CombinAll,paste(BatchName,"_DebtAnalysisResults.csv",sep = ""),row.names = FALSE)
write.csv(ReducedListDebtAll,paste(BatchName,"_DebtProductionCompanies_Snapshot",Startyear+5,".csv",sep = ""),row.names = FALSE)
write.csv(PortfolioAll,paste(BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv",sep = ""),row.names = FALSE)
# write.csv(DebtDataEconomy,paste("BondMarket_DebtAnalysisResults_", Date, ".csv",sep = ""),row.names = FALSE)  
write.csv(MissingBBGInfo, paste0(BatchName,"_",dim(MissingBBGInfo)[1],"ISINs_wo_BBG_Info_from_Port.csv"), row.names = FALSE)
Sub450Scenario <- subset(CombinAll, Scenario == "450S")
write.csv(Sub450Scenario,paste(BatchName,"_DebtAnalysisResults-450S-only.csv",sep = ""),row.names = FALSE, na = "")




# DebtData_existing <- read.csv("C:/Users/Work/Dropbox (2° Investing)/PortCheck/03_Results/01_BatchResults/Swiss/2016Q4/Swiss_DebtAnalysisResults.csv", strip.white = TRUE, stringsAsFactors = TRUE)
# Test1 <- subset(CombinAll,!CombinAll$PortName %in% DebtData_existing$PortName)
# Test1 <- rbind(Test1,DebtData_existing)
# write.csv(Test1, "DebtDataAll.csv",row.names = FALSE)
