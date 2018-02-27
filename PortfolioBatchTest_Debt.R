
### Based off the DebtCode _v17_BOE with incorporations of v15 and updates for Parameter File


# ------
# Load packages
# ------
library(grid)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(ggplot2)
library(png)
library(tidyr)



#------------
# Set up 2DII Dev Environment Folders and Locations
#------------

if (!exists("TWODII.CONSTS")) {
  ### 2dii-init should be run once, before any/all 2 Degrees R code.  Check for this.
  print("/// WARNING: 2DII DEV NAMES AND PATHS NOT INITIALIZED.  Run Common/2dii-init.R and try again.")
} 

#------------
# Set up PortCheck-Specific Constants and Functions
#------------

### this file sourced at top of all PortCheck scripts
### this defines any project constants and functions
### and will also source an override file if it exists
source(paste0(PORTCHECK.CODE.PATH, "proj-init.R"))
print("*** Starting DataImport.R Script")
print(show.consts())

#------------
# Read in Parameter File and Set Variables
#------------

### define these vars so we know they will be important
### makes the code easier to understand - they're not a surprise later
BatchName <- NA
BenchmarkRegionchoose <- NA
CompanyDomicileRegion <- NA
Indexchoose <- NA
Scenario <- NA
Startyear <- NA
ComparisonFile <- NA
ReportTemplate <- NA

ParameterFile <- ReadParameterFile(PROC.DATA.PATH)
### fill up those variables
SetParameters(ParameterFile)              # Sets BatchName, Scenario, BenchmarkRegion etc. 
print("*** STARTING SCRIPT with PARAMETERS:")
print(ParameterFile)

SamplePortfolio <- FALSE


#-------------
# Set Input / OUtput Locations Based on parameter File Input
#------------

### Finish setting up paths based on what was in the Parameter file
BATCH.PATH <- paste0(PROJ.PATH,BatchName,"/")
if(!dir.exists(file.path(BATCH.PATH))){dir.create(file.path(BATCH.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  

PORT.FIN.DATA.PATH <- paste0(FIN.DATA.PATH,ParameterFile$DateofFinancialData,"/PORT/")

#-------------
# All Input parameters & make the code interactive
#------------
# Please select the input parameters here
# BBGDataYear <- "2016" #The date of which the financial data is from - this is used to select the FFperc of BBGDataYear
BBGDataDate <- "30/12/2016"
# MarketDataDate <- "06_07_2016"
# AssessmentDate <- "2016Q4"
# Date <- Sys.Date() #get todays date for appending to file names to track historical calcualtion results
# BatchName <- "insuranceCorpBondsData" #PortfolioData file name
# BBGPORTOutput <- "FinancialDataUnique" # this would need to use the BBG-Data-Bind_v2 Code beforehand which merges PORT look-ups with API-look-ups as well
# FinancialDataDate <- "2017-04-19" # just in case the financial data got created by BBG-Data-Bind_v2 - Should create a function to look this up automotically and takes the most recent file (e.g. by using list function, then cut first 10 digits and sort by "date")
# BBGPORTOutput <- "2016Q4_corpBonds_characteristics_as30122016"

#------------
# Read in all files
#------------
BICSMoodysRisk <- read.csv(paste0(PROC.DATA.PATH,"/BICS_to_MoodysRisk_Bridge.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
OGCDebtData <- read.csv(paste0(PROC.DATA.PATH,"/OGDebtMaster_201704_UpdatedCoal.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
AutoDebtData <- read.csv(paste0(PROC.DATA.PATH,"/AutoDebtMaster_2016.csv"),stringsAsFactors=FALSE,strip.white=TRUE) # this is just the equity roll up, matched to corporate debt ticker
DebtToEquityBridge <- read.csv(paste0(PROC.DATA.PATH,"/EquityToDebtBridge_2017-10-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
PowerDebtData <- read.csv(paste0(PROC.DATA.PATH,"/2017-08-28powermasterDebt.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
ALDBridge <- read.csv(paste0(PROC.DATA.PATH,"/ALDEquityBridge_2017-10-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
BBGPORTOutput <- read.csv(paste0(PORT.FIN.DATA.PATH,BBGPORTOutput,".csv"),stringsAsFactors=FALSE)

PortfolioAllPorts <- read.csv(paste0(PORTS.PATH,ParameterFile$ProjektName,"/",BatchName,"/",BatchName,"Port_Bonds.csv"),stringsAsFactors=FALSE, strip.white = TRUE)

BenchRegionLists <- read.csv(paste0(PROC.DATA.PATH,"/BenchRegions.csv"))
CompanyDomicileRegion <- read.csv(paste0(PROC.DATA.PATH,"/IndexRegions.csv"))
CountryISOList <- read.csv(paste0(PROC.DATA.PATH,"/CountryISOCodes.csv"), stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c(""))
IEATargetsNew <- read.csv(paste0(PROC.DATA.PATH,"/IEATargets_WEO2016+ETP2017.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
DebtData <- read.csv(paste0(PROC.DATA.PATH,"/Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)

#-------------
# Set workdrive
#-------------
# setwd(FolderLocation)

# |------------------------------|
# |------   Data Loading   ------|
# |------------------------------|

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

# OGCDebtData <- read.csv(paste0(PROC.DATA.PATH,"/OGDebtMaster_201704_UpdatedCoal.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
OGCDebtData$Sector <- "Fossil Fuels"
OGCDebtData <- subset(OGCDebtData, select = -c(RollUpType,GDCompanyType))
OGCDebtData <- rename(OGCDebtData, c("Region" ="PlantLocation"))

# AutoDebtData <- read.csv(paste0(PROC.DATA.PATH,"/AutoDebtMaster_2016.csv"),stringsAsFactors=FALSE,strip.white=TRUE) # this is just the equity roll up, matched to corporate debt ticker
# DebtToEquityBridge <- read.csv(paste0(PROC.DATA.PATH,"/EquityToDebtBridge_2017-10-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
AutoDebtData <- merge(AutoDebtData, DebtToEquityBridge, by.x = "EQY_FUND_TICKER", by.y = "EquityTicker", all.x = TRUE)
AutoDebtData <- subset(AutoDebtData, !is.na(AutoDebtData$DebtTicker), select = -c(EQY_FUND_TICKER))

# PowerDebtData <- read.csv(paste0(PROC.DATA.PATH,"/2017-08-28powermasterDebt.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
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

MasterData <- rename(MasterData, c("Production" = "CompanyLvlProd"))

#Trim Masterdata to startyear
MasterData<-subset(MasterData, MasterData$Year >= Startyear)

# ------
# b) Read in Asset level Data Bridges
# ------
# ALDBridge <- read.csv(paste0(PROC.DATA.PATH,"/ALDEquityBridge_2017-10-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)

# ------
# c) Read in stored company level financial data (Data retrieved from API and PORT function) 
# ------
BBGPORTOutput <- BBGPORTOutput[!duplicated(BBGPORTOutput$ISIN),]

# ------
# d) Read in portoflio holdings (Portfolio holdings data given by Regulator or Instituion)
# ------
# PortfolioAllPorts <- read.csv(paste0(PORTS.PATH,ParameterFile$ProjektName,"/",BatchName,"/",BatchName,"Port_Bonds.csv"),stringsAsFactors=FALSE, strip.white = TRUE)


# ------
# e) #Define sector claissification based of BIC subgroup. Need to consoludiate SERCH BIC level 2 calssfication with whatever we were using for moodies.
# ------
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
# ------
# Import Regionsfile
# RegionLists <- read.csv(paste0(FolderLocation,"Data/RegionLists.csv"), na.strings = c(""))

# Create a List of all existing Benchmark-Region and all assessed CompanyLocation-Regions
# BenchRegionLists <- read.csv(paste0(PROC.DATA.PATH,"/BenchRegions.csv"))
BenchRegionLists[is.na(BenchRegionLists)] <- ""
BenchRegionLists <- rename(BenchRegionLists, c("BenchRegions" = "BenchmarkRegions", "BenchRegions_ISO_colnames" = "BenchmarkRegions_ISO_colnames"))
BenchmarkRegionList <- data.frame(BenchmarkRegion = BenchRegionLists$BenchmarkRegions[!is.na(BenchRegionLists$BenchmarkRegions) & BenchRegionLists$BenchmarkRegions != ""], BenchmarkRegionColname = BenchRegionLists$BenchmarkRegions_ISO_colnames[!is.na(BenchRegionLists$BenchmarkRegions_ISO_colnames) & BenchRegionLists$BenchmarkRegions_ISO_colnames != ""])
# CompanyDomicileRegion <- read.csv(paste0(PROC.DATA.PATH,"/IndexRegions.csv"))
CompanyDomicileRegion <- rename(CompanyDomicileRegion, c("IndexUniverse" = "CompanyDomicileRegion", "IndexUniverseColname" = "CompanyDomicileRegionColname")) 
#CompanyDomicileRegion[is.na(CompanyDomicileRegion)] <- ""
CompanyDomicileRegionList <- data.frame(CompanyDomicileRegion = CompanyDomicileRegion$CompanyDomicileRegion[!is.na(CompanyDomicileRegion$CompanyDomicileRegion) & CompanyDomicileRegion$CompanyDomicileRegion != ""], CompanyDomicileRegionColname = CompanyDomicileRegion$CompanyDomicileRegionColname[!is.na(CompanyDomicileRegion$CompanyDomicileRegionColname) & CompanyDomicileRegion$CompanyDomicileRegionColname != ""])

# Read countryname-conversion file to abbreviation
# CountryISOList <- read.csv(paste0(PROC.DATA.PATH,"/CountryISOCodes.csv"), stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c(""))
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

TechnologyList <- c("Electric","Hybrid","ICE","GasCap","CoalCap","OilCap","RenewablesCap","HydroCap","NuclearCap", "Coal","Oil","Gas")
SectorList <- c("Automotive","Coal","Power","Oil&Gas")


AllLists <- list(TechList = TechnologyList, PowerBenchmarkRegionOECD = PowerBenchmarkRegionOECD, PowerBenchmarkRegionNonOECD = PowerBenchmarkRegionNonOECD, PowerBenchmarkRegionGlobal = PowerBenchmarkRegionGlobal,
                 FossilFuelBenchmarkRegions = FossilFuelBenchmarkRegions, FossilFuelBenchmarkRegionsOECD = FossilFuelBenchmarkRegionsOECD, FossilFuelBenchmarkRegionsNonOECD = FossilFuelBenchmarkRegionsNonOECD,
                 MutualExclusiveCompanyDomicileRegions = MutualExclusiveCompanyDomicileRegions)

# techlist <- data.frame("Technology" = AllLists$TechList)
# #regionlist <- data.frame("BenchmarkRegion" = c("Global","OECD","Non-OECD","EU28","U.S."))
# yearlist <- data.frame("Year" = unique(PortMix$Year))
# # yearlist <- data.frame("Year" = Startyear:(Startyear+10))
# lineadd <- merge(techlist,yearlist)
# lineadd$Sector <- "Power"
# lineadd$Sector[lineadd$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
# lineadd$Sector[lineadd$Technology %in% c("Coal")] <- "Coal" 
# lineadd$Sector[lineadd$Technology %in%  c("Electric","Hybrid","ICE")] <-  "Automotive"

# ------
# g) Read in scenario data
# ------
# Calculate fair share ratios
# calculate the targets. Set this to be switched off if reload is set to 0

# IEATargetsNew <- read.csv(paste0(PROC.DATA.PATH,"/IEATargets_WEO2016+ETP2017.csv"),stringsAsFactors=FALSE, strip.white = TRUE)
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

# Convert IEA sectors for fossel fuels
IEATargetssub$Sector[IEATargetssub$Technology == "Coal"] <- "Coal"
IEATargetssub$Sector[IEATargetssub$Technology == "Oil"] <- "Oil&Gas"
IEATargetssub$Sector[IEATargetssub$Technology == "Gas"] <- "Oil&Gas"

# Clean IEATargets
IEATargetssub[IEATargetssub == "NaN"] <- 0

rm(IEATargets,IEATargetsPower,IEATargetsNew,IEATargetswide,RETargets,refvalIEA,EndRefValIEA )

# ------
# 1.  Processing of DebtData
# ------

# Debt Data is all of the DebtData from Bloomberg
# Processing includes:
# - cleaning to remove negative values
# - adding sector breakdown
# - aggregating to debt ticker level
# - adding benchmark region
# - adding some weighting factors
#
# Output: DataDebtCORP
# Used as the basis for Market and Portfolio Debt. 


### Initial cleaning ###

# Rename Total row as to not confuse it with TOTAL SA...
DebtData$Co..Corp.TKR[DebtData$Co..Corp.TKR == "Total"] <- "TotalDebt"

GovBanksSupraNat <- subset(DebtData, (DebtData$Government.Development.Banks !=0 | DebtData$Supranationals != 0) & DebtData$Co..Corp.TKR != "TotalDebt") 
DebtData <- subset(DebtData, !DebtData$Co..Corp.TKR %in% GovBanksSupraNat$Co..Corp.TKR)
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

### Universe Port ###

# Create portfolio for bond universe to be rbound with PortfolioAllPorts. have to have a bond for every sector classifcation... so need to melt.
UniversePort <- subset(DebtData, select = -c(TotalCorpDebt), DebtData$Co..Corp.TKR != "TotalDebt")
UniversePort <- melt(UniversePort, id.vars = c("Co..Corp.TKR"), variable.name = "Subgroup", value.name = "Position")
UniversePort <- subset(UniversePort, UniversePort$Position != 0)
UniversePort$SectorWeight <- UniversePort$Position/sum(UniversePort$Position, na.rm = TRUE)

# Creat FF subset of MasterData
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

UniversePort <- subset(UniversePort, UniversePort$Position != 0 & UniversePort$piesector %in% c("Utility Power", "Coal", "Automotive","Oil&Gas"), select = -c(Sector))

### Weighting ###

CORPSecWts <- UniversePort
CORPSecWts <- merge(CORPSecWts, BICSectors, by.x = "Subgroup", by.y = "BIClvl2", all.x = TRUE)
CORPSecWts$Sector <- as.character(CORPSecWts$Sector)
CORPSecWts$Sector[is.na(CORPSecWts$Sector)] <- "NotCovered"
CORPSecWts <- ddply(CORPSecWts,.(DebtTicker, Sector), summarize,
                    SectorWeight=sum(SectorWeight, na.rm=TRUE))

MarketSecWts <- ddply(CORPSecWts,.(Sector), summarize,
                      SectorWeight=sum(SectorWeight, na.rm=TRUE))

### CorpSecWts -- AUM % weight of bond uni of each Ticker/BICS Sector matched to ALD
### basically, maps company/sector AUM % of bond universe issuance to that company's ALD sector production 
### maps the company/sector share of the bond universe AUM to the company/sector amount of production
### MarketSecWts -- AUM % weight of bond uni each BICS Sector matched to ALD
### DebtData: Ticker / sector 1 debt, sector 2 debt, other debt, total debt - one row per ticker
### MasterData: Ticker / Sector / Tech / Year / Location - definitely multiple rows for each Ticker#Continue with market portfolio
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
DebtData <- subset(DebtData, !DebtData$DebtTicker == "")

#Sum debt for relevant sectos
DebtData$ClimateSecDebt[DebtData$Sector == "Power" & DebtData$PowerDebt != 0] <- DebtData$PowerDebt[DebtData$Sector == "Power" & DebtData$PowerDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Automotive" & DebtData$AutoDebt != 0] <- DebtData$AutoDebt[DebtData$Sector == "Automotive" & DebtData$AutoDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Oil&Gas" & DebtData$OGDebt != 0] <- DebtData$OGDebt[DebtData$Sector == "Oil&Gas" & DebtData$OGDebt != 0]
DebtData$ClimateSecDebt[DebtData$Sector == "Coal" & DebtData$CoalDebt != 0] <- DebtData$CoalDebt[DebtData$Sector == "Coal" & DebtData$CoalDebt != 0]

#Aggregate total global production by technology and debt ticker
DebtDataCORP_global <- ddply(DebtData,.(DebtTicker, Sector, Year, Technology, ClimateSecDebt,TotalCorpDebt), summarize,
                             CompanyLvlProd=sum(CompanyLvlProd, na.rm=TRUE)) # PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

#Aggregate over Climate sector debt in case a debt ticker has multiple subsector classifcations (This seems to have already been taken care of, as the dataframe length doesn't change)
DebtDataCORP_global <-ddply(DebtDataCORP_global,.(DebtTicker, Sector, Year, Technology,  CompanyLvlProd), summarize,
                            ClimateSecDebt=sum(ClimateSecDebt, na.rm=TRUE),
                            TotalDebt=sum(TotalCorpDebt, na.rm=TRUE))

#Remove entries with zero production
DebtDataCORP_global <- subset(DebtDataCORP_global, DebtDataCORP_global$CompanyLvlProd != 0)

#Calculate global sector production per debt ticker
DebtDataCORPSec_global <- ddply(DebtDataCORP_global,.(DebtTicker, Sector, Year, ClimateSecDebt, TotalDebt), summarize,
                                CompanyLvlSecProd = sum(CompanyLvlProd, na.rm=TRUE)) #PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

# Merge sector and technology values and Calculate tech share
### Adds CompanySecLvlProd
DebtDataCORP_global <- merge(DebtDataCORP_global,DebtDataCORPSec_global, by = c("DebtTicker", "Sector", "Year", "ClimateSecDebt","TotalDebt")) #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", 
# ALL AT THE GLOBAL LEVEL - ignores REGIONS and locations
DebtDataCORP_global$TechShare <- 1
DebtDataCORP_global$TechShare <- DebtDataCORP_global$CompanyLvlProd/DebtDataCORP_global$CompanyLvlSecProd

DebtDataCORP_global$Aggregation <- "Global"
DebtDataCORP_global$BenchmarkRegion <- "Global"
DebtDataCORP_global$RegionalWeightingFactorCompany <- 1

### NOW TO ADD BENCHMARK REGIONS
#Merge with ISO codes then put the production in each country into it's resepective benchmark region
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

#Aggregate production of each technology from each debt ticker at the benchmark region level
# this aggregates over individual plant location countries to Behnchmark Region
DebtDataCORP <- ddply(DebtDataCORP,.(DebtTicker, Sector, Year, Technology, BenchmarkRegion, ClimateSecDebt, TotalDebt), summarize,
                      CompanyLvlProd=sum(CompanyLvlProd, na.rm=TRUE)) #PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

DebtDataCORP <- subset(DebtDataCORP, CompanyLvlProd != 0 & ((Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | 
                                                              (Sector == "Automotive" & BenchmarkRegion == "Global") | 
                                                              (Sector == "Oil&Gas" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | 
                                                              (Sector == "Coal" & BenchmarkRegion %in% "Global")))

#Aggregate production from each sector and from each debt ticker at the benchmark region level
DebtDataCORPSec <- ddply(DebtDataCORP,.(DebtTicker, Sector, Year, BenchmarkRegion, ClimateSecDebt, TotalDebt), summarize,
                         CompanyLvlSecProd = sum(CompanyLvlProd, na.rm=TRUE)) # PowerDebt, AutoDebt, OGDebt, CoalDebt, OtherDebt,

DebtDataCORPSecRef <- subset(DebtDataCORPSec)
DebtDataCORPSecGlobal <- ddply(DebtDataCORPSecRef,.(DebtTicker, Sector, ClimateSecDebt,TotalDebt, Year), summarize,
                               CompanyLvlSecProdGlobal=sum(CompanyLvlSecProd, na.rm=TRUE))
DebtDataCORPSecRef <- merge(DebtDataCORPSecRef, DebtDataCORPSecGlobal, by = c("DebtTicker", "Sector", "ClimateSecDebt", "TotalDebt", "Year"))

#Calculate the of production/capacity in each region. This will be use to distrubte the AUM/expsosure to each region and the relative misalignment with the market in that region.
DebtDataCORPSecRef$RegionalWeightingFactorCompany <- DebtDataCORPSecRef$CompanyLvlSecProd / DebtDataCORPSecRef$CompanyLvlSecProdGlobal

DebtDataCORP <- merge(DebtDataCORP,DebtDataCORPSec, by = c("DebtTicker", "Sector", "BenchmarkRegion", "Year", "ClimateSecDebt", "TotalDebt")) # "PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", 
DebtDataCORP <- merge(DebtDataCORP,subset(DebtDataCORPSecRef, select = c("DebtTicker", "Sector", "BenchmarkRegion","Year", "ClimateSecDebt", "TotalDebt", "RegionalWeightingFactorCompany")), by = c("DebtTicker", "Sector", "BenchmarkRegion","Year","ClimateSecDebt", "TotalDebt")) #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt", #"PowerDebt","AutoDebt", "OGDebt", "CoalDebt", "OtherDebt",

DebtDataCORPsave <- DebtDataCORP
DebtDataCORP <- DebtDataCORPsave

# Calculate the technology share/fuel mix
DebtDataCORP$TechShare <- 1
DebtDataCORP$Aggregation <- "GlobalAggregate"
DebtDataCORP$TechShare <- DebtDataCORP$CompanyLvlProd/DebtDataCORP$CompanyLvlSecProd

# Merge back with  values aggregated at the global level
DebtDataCORP_global[setdiff(colnames(DebtDataCORP),colnames(DebtDataCORP_global))] <- NA
DebtDataCORP <- rbind(DebtDataCORP, DebtDataCORP_global)
DebtDataCORP[is.na(DebtDataCORP)] <- 0

# Calculate the production per $1 of corp bond debt issued
DebtDataCORP$DebtIntensity <- 0
DebtDataCORP$DebtIntensity[DebtDataCORP$ClimateSecDebt != 0] <- DebtDataCORP$CompanyLvlProd[DebtDataCORP$ClimateSecDebt != 0]/DebtDataCORP$ClimateSecDebt[DebtDataCORP$ClimateSecDebt != 0]

# Filter the market for companies that don't have debt production
DebtDataCORP <- subset(DebtDataCORP, DebtDataCORP$DebtIntensity != 0) 

# Calculate the weight of debt for teach technolgoy relative to the total corp bond debt market
DebtDataCORP$Wt <- DebtDataCORP$ClimateSecDebt/TotalDebt

# Allocate production to the 'portfolio' by the portfolio wt approach
DebtDataCORP$WtProduction <- DebtDataCORP$CompanyLvlProd * DebtDataCORP$Wt

# Allocate a portion of the technolgy mix based off the technolgies debt wt in the market
DebtDataCORP$WtTechShare <- DebtDataCORP$TechShare * DebtDataCORP$Wt

# Calculate Carsten Metric. 
# For global it's the debt ticker's wt (debt/total debt) multiplied by the techshare, 
# For regions: it's the debt ticker's global wt multiplied by the regional waiting factor multiplied by the techshare
DebtDataCORP$Market_CarstensMetric <- DebtDataCORP$RegionalWeightingFactorCompany*DebtDataCORP$Wt*DebtDataCORP$TechShare

# ## Calculate company level change (growth) in production to be used for new metric that scales the portfolio wt by the companies planned production
MasterDataDebt <- subset(DebtDataCORP, select = -c(WtProduction))
MasterDataDebt <- rename(MasterDataDebt, c("Wt" = "CORPD_Global_Wt"))

rm(DebtDataCORPSecRef,DebtDataCORP_global,DebtDataCORPSec_global,DebtDataCORP_regional,DebtDataCORPSec,DebtDataCORPSec_global,DebtDataCORPSecGlobal)

# ------
# 2.  Creation of Market Reference Portfolio
# ------

# Create market data or read it in
# For debt the market data is different as the market is different. It's no longer the listed universe, but the debt universe.
#if (CalculateMarketData == 1){

### MarketRef- subset of the DebtDataCORP as per the market. 
### Not aggregated by DebtTicker

MarketRef <- subset(DebtDataCORP, DebtDataCORP$Year == Startyear & Aggregation == "GlobalAggregate", 
                    select = c("DebtTicker", "Sector", "Technology","Year", "ClimateSecDebt", "TotalDebt",
                               "CompanyLvlProd", "CompanyLvlSecProd","RegionalWeightingFactorCompany", "BenchmarkRegion", "TechShare", "Wt"))

MarketRef <- rename(MarketRef, c("CompanyLvlProd" = "RefCompanyLvlProd", "CompanyLvlSecProd" = "RefCompanyLvlSecProd", "TechShare"="MarketTechShareRef", "Wt" = "WtMarket"))

MarketRef <- subset(MarketRef, MarketRef$Year == Startyear, select = -c(Year))

# Technology share of the technology in of the global portfolio
MarketRef$RegionWtMarketTechShareRef <- MarketRef$MarketTechShareRef * MarketRef$RegionalWeightingFactorCompany

## Calculate
MarketRef[is.na(MarketRef)] <- 0
MarketRef <- subset(MarketRef, ClimateSecDebt != 0)

# Wt = The Sectoral Weight within the Debt
MarketRef$RegionWtMarketWtCompanies <-  MarketRef$WtMarket * MarketRef$RegionalWeightingFactorCompany

CompanyWeightingData <- unique(subset(MarketRef, select = c("DebtTicker","Sector","BenchmarkRegion","WtMarket","RegionalWeightingFactorCompany","RegionWtMarketWtCompanies")))

MarketRef$CompWtProjMarketTechShare <- MarketRef$RegionWtMarketTechShareRef * MarketRef$WtMarket

####### Matches v15 up until here ############
# colnames(MarketRef)

#-------
## Add lines for all technologies in each benchregion for each company. This is to insure that fair share addtions are added to cmpanyes who have existing production in each respective benchregion
DebtTickerList <- unique(subset(MarketRef, select= c("DebtTicker")))

# Call function to add lines on the require subset, then add the MarketRef data back
t <- subset(MarketRef, select = c(DebtTicker, Sector,Technology, BenchmarkRegion)) #  RefCompanyLvlProd, RefCompanyLvlSecProd, MarketTechShareRef
t1 <- datacompletionSub(t)

# remove unneccessarly combinations of sector and benchregion
t2 <- subset(t1, ((Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) |
                    (Sector == "Automotive" & BenchmarkRegion == "Global") |
                    (Sector == "Oil&Gas" & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) |
                    (Sector == "Coal" & BenchmarkRegion %in% "Global")))

# Add Global level debt ticker data back to completed dataframe
t3 <- unique(subset(MarketRef, select = c("DebtTicker", "TotalDebt")))
t4 <- merge(t2,t3, by = "DebtTicker")

# Add Sector level data back
t5 <- unique(subset(MarketRef, select = c("DebtTicker", "Sector", "ClimateSecDebt", "WtMarket")))
t6 <- merge(t4,t5, by = c("DebtTicker", "Sector"))

# Add Regional level data back
t7 <- unique(subset(MarketRef, select = c("DebtTicker", "Sector", "BenchmarkRegion", "ClimateSecDebt", "WtMarket", "RegionalWeightingFactorCompany", "RefCompanyLvlSecProd")))
t8 <- merge(t6,t7, by = c("DebtTicker", "Sector", "BenchmarkRegion", "ClimateSecDebt", "WtMarket"))

# Add Technology level data back
t9 <- unique(subset(MarketRef, select = c("DebtTicker", "Sector", "BenchmarkRegion", "Technology", "ClimateSecDebt", "WtMarket", "RefCompanyLvlProd", "MarketTechShareRef","CompWtProjMarketTechShare")))
t10 <- merge(t8,t9, by = c("DebtTicker", "Sector", "BenchmarkRegion", "Technology", "ClimateSecDebt", "WtMarket"), all = TRUE)
t10[is.na(t10)] <- 0

MarketRef <- t10
rm(t,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
## Remove debt tickers/companies that do not have actove bond issuance in the revelant sector that has asset level data
MarketRef[is.na(MarketRef)] <- 0
MarketRef <- subset(MarketRef, ClimateSecDebt != 0)
colnames(MarketRef)

#----------


RegionalMarketWeights <- CompanyWeightingData
RegionalMarketWeights <- ddply(RegionalMarketWeights,.(Sector,BenchmarkRegion),summarise, 
                               MarketWeightRegionWeight = sum(RegionWtMarketWtCompanies,na.rm = TRUE))

####### Add in Aggregated MarketRef ##########

### MarketRefAgg- subset of the Debt data as per the market. 
### Aggregated by DebtTicker

# Technology Aggregation of Debt Tickers
MarketRefTechAgg <- ddply(MarketRef,.(Sector, Technology, BenchmarkRegion), summarize,
                          RefMarketLvlProd = sum(RefCompanyLvlProd, na.rm=TRUE),
                          # EquityIntensity=sum(EquityIntensity, na.rm=TRUE),
                          Wt=sum(WtMarket, na.rm=TRUE),
                          # WtProduction=sum(WtProduction, na.rm=TRUE),
                          RefCarstensMetric = sum(CompWtProjMarketTechShare, na.rm=TRUE))

# Sector Aggregation of Debt Tickers
MarketRefSecAgg <- ddply(MarketRefTechAgg,.(Sector, BenchmarkRegion), summarize,
                         RefMarketLvlSecProd  = sum(RefMarketLvlProd, na.rm=TRUE),
                         RefCarstensMetricSec = sum(RefCarstensMetric, na.rm=TRUE))

# Combination Tech and Sector data frames with the IEA Targets
MarketRefAgg <- merge(MarketRefTechAgg, MarketRefSecAgg, by= c("Sector", "BenchmarkRegion"), all.x = TRUE)
MarketRefAgg <- merge(MarketRefAgg,IEATargetssub, by = c("BenchmarkRegion","Sector","Technology"), all.x = TRUE)

### Calculate projected production values
## Relative to the market and including current assets and future plans 
MarketRefAgg$ProjMarketProd <-  MarketRefAgg$RefMarketLvlProd + MarketRefAgg$RefMarketLvlSecProd * MarketRefAgg$FairSharePerc
MarketRefAgg$ProjMarketProd[which(MarketRefAgg$Direction == "declining")] <-  MarketRefAgg$RefMarketLvlProd[which(MarketRefAgg$Direction == "declining")] * (1 + MarketRefAgg$FairSharePerc[which(MarketRefAgg$Direction == "declining")])

MarketRefAgg$Benchmark_CarstensMetric <-  MarketRefAgg$RefCarstensMetric +  MarketRefAgg$RefCarstensMetricSec * MarketRefAgg$FairSharePerc
MarketRefAgg$Benchmark_CarstensMetric[which(MarketRefAgg$Direction == "declining")] <-  MarketRefAgg$RefCarstensMetric[which(MarketRefAgg$Direction == "declining")] * (1 + MarketRefAgg$FairSharePerc[which(MarketRefAgg$Direction == "declining")])
MarketRefAgg$WtProjMarketProduction <- MarketRefAgg$ProjMarketProd * MarketRefAgg$Wt

MarketRefSecAgg <- ddply(MarketRefAgg,.(Sector, Year, Scenario, BenchmarkRegion), summarize, 
                      ProjMarketProdSec = sum(ProjMarketProd, na.rm=TRUE), 
                      WtProjMarketProductionSec = sum(WtProjMarketProduction, na.rm = TRUE),
                      Benchmark_CarstensMetricSec = sum(Benchmark_CarstensMetric,na.rm = TRUE))

MarketRefAgg <- merge(MarketRefAgg, MarketRefSecAgg, by= c("Sector", "BenchmarkRegion", "Year","Scenario"), all.x = TRUE)

MarketRefAgg <- merge(MarketRefAgg, MarketSecWts, by = "Sector",all.x = TRUE, all.y = FALSE)

MarketRefAgg$WtTechShareTechShare <- MarketRefAgg$Benchmark_CarstensMetric / MarketRefAgg$Benchmark_CarstensMetricSec

MarketRefAgg <- rename(MarketRefAgg, c("Wt" = "WtMarket", "SectorWeight" = "SecWtMarket",	"WtTechShareTechShare" = "WtTechShareTechShareMarket")) #"SectorWeight" = "SectorWeightMarket"

### Add in the reference value (Start year)
CompGrowthRefVal <- unique(subset(MarketRefAgg, MarketRefAgg$Year == Startyear & !is.na(MarketRefAgg$ProjMarketProd), select = c("Sector","BenchmarkRegion","Technology","ProjMarketProd")))
CompGrowthRefVal <- rename(CompGrowthRefVal, c("ProjMarketProd" = "RefProjMarketProd"))

MarketRefAgg<- merge(MarketRefAgg, CompGrowthRefVal, by =  c("Sector","BenchmarkRegion","Technology"), all.x = TRUE)
MarketRefAgg$RefCompanyLvlGrowth[!is.na(MarketRefAgg$RefProjMarketProd)] <- (MarketRefAgg$CompanyLvlProd[!is.na(MarketRefAgg$RefProjMarketProd)]-MarketRefAgg$RefProjMarketProd[!is.na(MarketRefAgg$RefProjMarketProd)])/MarketRefAgg$RefProjMarketProd[!is.na(MarketRefAgg$RefProjMarketProd)]

RegionalBenchmarkAgg <- MarketRefAgg

# MarketBenchmarkGlobal <- merge(MarketRefAgg,RegionalMarketWeights, by = c("Sector","BenchmarkRegion"),all.x = TRUE)
# MarketBenchmarkGlobal$PortfolioSpecific_RegWt_CarstensMetric <- MarketBenchmarkGlobal$Benchmark_CarstensMetric / MarketBenchmarkGlobal$Benchmark_CarstensMetricSec * MarketBenchmarkGlobal$MarketWeightRegionWeight
# MarketBenchmarkGlobalAggregate <- aggregate(MarketBenchmarkGlobal["PortfolioSpecific_RegWt_CarstensMetric"], by=MarketBenchmarkGlobal[,c("Sector","Year","Technology","Scenario")], FUN=sum)
# MarketBenchmarkGlobalAggregate$BenchmarkRegion <- "GlobalAggregate"
# MarketBenchmarkGlobalAggregate <- rename(MarketBenchmarkGlobalAggregate, c("PortfolioSpecific_RegWt_CarstensMetric" = "Benchmark_CarstensMetric"))
# 
# 
# # Calculate Market Global Aggregate benchmark - compare to global benchmark using old code.
# MarketRefAggSave <- MarketBenchmarkGlobalAggregate
# MarketRefAggSave$PortName <- "Market_Benchmark"


########## End of Agg ###########
# Aggregation previously used to merge with PortfolioResults. 
# New version is not aggregated by DebtTickers; just regionally. 



#Merge IEA targets the fair share percentage
MarketRef <- merge(MarketRef,IEATargetssub, by = c("BenchmarkRegion","Sector","Technology"), all.x = TRUE)

### Calculate projected production values
### Relative to the market and including current assets and future plans
MarketRef$ProjCompProd <-  MarketRef$RefCompanyLvlProd + MarketRef$RefCompanyLvlSecProd * MarketRef$FairSharePerc
MarketRef$ProjCompProd[which(MarketRef$Direction == "declining")] <-  MarketRef$RefCompanyLvlProd[which(MarketRef$Direction == "declining")] * (1 + MarketRef$FairSharePerc[which(MarketRef$Direction == "declining")])

### Clean negative production values??
MarketRef$ProjCompProd[MarketRef$ProjCompProd < 0]  <- 0 

#Recalculate the RegionalWeightingFactorCompany for the projected values, and the technology share

#Aggregate sector production for tech share
MarketRefSec <- ddply(MarketRef,.(DebtTicker, Sector, Year, Scenario, BenchmarkRegion, ClimateSecDebt, TotalDebt), summarize, 
                      ProjCompProdSec = sum(ProjCompProd, na.rm=TRUE))

#Aggregate global production for RegionalWeightingFactorCompany
MarketRefSecGlobal <- ddply(MarketRefSec,.(DebtTicker, Sector, Year, Scenario, ClimateSecDebt, TotalDebt), summarize,
                            ProjCompProdSecGlobal = sum(ProjCompProdSec, na.rm=TRUE))

#Merge both back to MasterRef
MarketRef <- merge(MarketRef, MarketRefSec, by= c("DebtTicker","Sector", "BenchmarkRegion", "Year", "Scenario", "ClimateSecDebt","TotalDebt"), all.x = TRUE)
MarketRef <- merge(MarketRef, MarketRefSecGlobal, by = c("DebtTicker","Sector", "Year", "Scenario", "ClimateSecDebt","TotalDebt"))

MarketRef$ProjCompProdTechShare <- MarketRef$ProjCompProd/MarketRef$ProjCompProdSec
# MarketRed$ProjCompProdTechShareRef <- MarketRef

MarketRef$RegionalWeightingFactorCompany <- MarketRef$ProjCompProdSec / MarketRef$ProjCompProdSecGlobal
MarketRef$ProjCarstenMetric <- MarketRef$ProjCompProdTechShare * MarketRef$RegionalWeightingFactorCompany * MarketRef$WtMarket

# MarketRef$WtTechShareTechShare <- MarketRef$ProjCarstenMetric / MarketRef$ProjCarstenMetricSec


# MarketRef$ProjWtTechShareTechShare <-  MarketRef$ProjCompProdTechShare * MarketRef$RegionalWeightingFactorCompany

#MarketRef <- merge(MarketRef, MarketSecWts, by = "Sector", all.x = TRUE, all.y = FALSE)

# This reconfirmation, sometimes R won't autmoatically class a vector as a factor and this can cause issues when you try to subset.
MarketRef$BenchmarkRegion <- as.factor(MarketRef$BenchmarkRegion)

saveMarketRef <- MarketRef
MarketRef <- saveMarketRef

# ## For Coal we will just use the constant sector wt as the benchmark.
# MarketRef$Benchmark_CarstensMetric[MarketRef$Sector == "Coal"] <- MarketRef$WtMarket[MarketRef$Sector == "Coal"]
# MarketRef$Benchmark_CarstensMetricSec[MarketRef$Sector == "Coal"] <- MarketRef$WtMarket[MarketRef$Sector == "Coal"]

# RegionalBenchmark <- MarketRef

RegionalBenchmarkTech <- ddply(MarketRef,.(BenchmarkRegion, Sector, Technology, Year, Scenario), summarize, # to add the sum/aggregated FairSharePerc you have to also have to weight it appropriately
                               # Wt=sum(WtMarket, na.rm=TRUE),
                               # WtProduction=sum(WtProduction, na.rm=TRUE),
                               ProjCompProd=sum(ProjCompProd, na.rm=TRUE),
                               ProjCarstenMetric=sum(ProjCarstenMetric, na.rm=TRUE))

RegionalBenchmarkSec <- ddply(MarketRef,.(BenchmarkRegion, Sector, Year, Scenario), summarize,
                              # Wt=sum(WtMarket, na.rm=TRUE),
                              # WtProduction=sum(WtProduction, na.rm=TRUE),
                              ProjCompProdSec=sum(ProjCompProd, na.rm=TRUE),
                              ProjCarstenMetricSec=sum(ProjCarstenMetric, na.rm=TRUE))

# Calculate the Aggregated Regional Sector Weights. This is done separately just to avoid confusion... or create it, one or the other. If you're reading this then maybe it's the later
# Aggregation happens at this point.
MarketRefsub <- unique(subset(MarketRef, select = c(DebtTicker, BenchmarkRegion, Sector, Year, Scenario, WtMarket, RegionalWeightingFactorCompany)))
MarketRefsub$WtRegion<-MarketRefsub$WtMarket*MarketRefsub$RegionalWeightingFactorCompany

RegionalSectorWeights <-  ddply(MarketRefsub,.(BenchmarkRegion, Sector, Year, Scenario), summarize,
                                ProjWtRegion=sum(WtRegion, na.rm=TRUE))

#The below commented-out code double checks that the sector weights add up to the orginal sectoral weights. It worked, can be coded to print error if it doesn't work, but didn't have time to write that. 

# GlobalSectorWeights <- ddply(RegionalSectorWeights,.(Sector, Year, Scenario), summarize,
#                              WtRegion=sum(WtRegion, na.rm=TRUE))
# 
# CrossRefSectorWts <- unique(subset(MarketRef, select = c("DebtTicker","Sector", "ClimateSecDebt")))
# summary(as.factor(CrossRefSectorWts$Sector))
# CrossRefSectorWts <-ddply(CrossRefSectorWts,.(Sector), summarize,
#                           Wt=sum(ClimateSecDebt, na.rm=TRUE)/TotalDebt)

RegionalBenchmark <- merge(RegionalBenchmarkTech, RegionalBenchmarkSec, by = c("BenchmarkRegion", "Sector", "Scenario", "Year"))
RegionalBenchmark <- merge(RegionalBenchmark, RegionalSectorWeights, by = c("BenchmarkRegion", "Sector", "Scenario", "Year"))

### THIS IS NOT THE SAME AS THE TECHNOLOGY SHARE USED WITH THE CARSTEN METRIC! SO YOU CANNOT CALCULATE THE CARSTEN METRIC FROM MULTIPLE THIS BY THE GLOBAL SECTOR WEIGHTS!
### THIS IS DUE TO THE DIFFERENT GEOGRAPHICAL DISTRIBTION OF ASSETS BY COMPANIES AS WELL AS DIVERGENT REGIONAL ENERGY TRANSITION PATHWAYS
RegionalBenchmark$MarketTechShare <- RegionalBenchmark$ProjCompProd/RegionalBenchmark$ProjCompProdSec

### NEW ###
RegionalBenchmark$ProjWtTechShareTechShare <- RegionalBenchmark$ProjCarstenMetric/RegionalBenchmark$ProjCarstenMetricSec
###########

## Repeat the same process for the Global level aggregration
GlobalBenchmarkTech <-  ddply(MarketRef,.(Sector, Technology, Year, Scenario), summarize,  # to add the sum/aggregated FairSharePerc you have to also have to weight it appropriately
                              # Wt=sum(WtMarket, na.rm=TRUE),
                              # WtProduction=sum(WtProduction, na.rm=TRUE),
                              ProjCompProd=sum(ProjCompProd, na.rm=TRUE),
                              ProjCarstenMetric=sum(ProjCarstenMetric, na.rm=TRUE))

GlobalBenchmarkSec <- ddply(MarketRef,.(Sector, Year, Scenario), summarize,
                            # Wt=sum(WtMarket, na.rm=TRUE),
                            # WtProduction=sum(WtProduction, na.rm=TRUE),
                            ProjCompProdSec=sum(ProjCompProd, na.rm=TRUE),
                            ProjCarstenMetricSec = sum(ProjCarstenMetric, na.rm = TRUE))

GlobalSectorWeights <- ddply(MarketRefsub,.(Sector, Year, Scenario), summarize,
                             ProjWtRegion=sum(WtRegion, na.rm=TRUE))

GlobalBenchmark <- merge(GlobalBenchmarkTech,GlobalBenchmarkSec, by = c("Sector", "Scenario", "Year"))
GlobalBenchmark <- merge(GlobalBenchmark,GlobalSectorWeights, by = c("Sector", "Scenario", "Year"))

GlobalBenchmark$MarketTechShare <- GlobalBenchmark$ProjCompProd/GlobalBenchmark$ProjCompProdSec

### NEW ###
GlobalBenchmark$ProjWtTechShareTechShare <- GlobalBenchmark$ProjCarstenMetric/GlobalBenchmark$ProjCarstenMetricSec
###########

GlobalBenchmark$BenchmarkRegion <- "GlobalAggregate"

##What to name this dataframe??
DebtBenchmarkDataframe <- rbind(GlobalBenchmark,RegionalBenchmark)

## Order Columns for ease of reading
DebtBenchmarkDataframe <- subset(DebtBenchmarkDataframe, select = c("BenchmarkRegion", "Sector", "Scenario", "Year", "Technology", "ProjCompProd", "ProjCompProdSec", "MarketTechShare", "ProjWtRegion", "ProjCarstenMetric", "ProjWtTechShareTechShare"))

## Rename for clarifty when combind with portfolio  level data
DebtBenchmarkDataframe <- rename(DebtBenchmarkDataframe, c("ProjCompProd" = "ProjMarketProd", "ProjCompProdSec" = "ProjMarketProdSec", "ProjWtRegion" = "ProjWtRegion_Market", "ProjCarstenMetric" = "CarstenMetric_ProjMarket", "ProjWtTechShareTechShare"= "WtTechShareTechShare_ProjMarket"))

# MarketBenchmarkGlobal <- merge(MarketRef,RegionalMarketWeights, by = c("Sector","BenchmarkRegion"),all.x = TRUE)
# MarketBenchmarkGlobal$PortfolioSpecific_RegWt_CarstensMetric <- (MarketBenchmarkGlobal$ProjMarketProd/MarketBenchmarkGlobal$ProjMarketProdSec) * MarketBenchmarkGlobal$MarketWeightRegionWeight
# MarketBenchmarkGlobalAggregate <- aggregate(MarketBenchmarkGlobal["PortfolioSpecific_RegWt_CarstensMetric"], by=MarketBenchmarkGlobal[,c("Sector","Year","Technology","Scenario")], FUN=sum)
# MarketBenchmarkGlobalAggregate$BenchmarkRegion <- "GlobalAggregate"
# MarketBenchmarkGlobalAggregate <- rename(MarketBenchmarkGlobalAggregate, c("PortfolioSpecific_RegWt_CarstensMetric" = "Benchmark_CarstensMetric"))

# Calculate Market Global Aggregate benchmark - compare to global benchmark using old code.
# MarketRefSave <- MarketBenchmarkGlobalAggregate
# MarketRefSave$PortName <- "Market_Benchmark"

# write.csv(MarketRef, paste0("Data/DebtMarketRef_", Date, ".csv"), row.names = FALSE, na = "")
# write.csv(DebtBenchmarkDataframe, paste0("Data/DebtBenchmarkDataframe_", Date, ".csv"), row.names = FALSE, na = "")
BICSMoodysRisk <- unique(subset(BICSMoodysRisk,select = c("INDUSTRY_SUBGROUP","MoodysRiskLvl")))

# ------
# g) Read in Portfolio data
# ------

#Get List of Portfolios either from folder or from 1 file (PortfolioN-check format) depending on Bulk-Method
#Clean Holdings data
MissingISIN <- subset(PortfolioAllPorts, ISIN == "")
# Some bonds don't have icines.. 
# PortfolioAllPorts <- subset(PortfolioAllPorts, ISIN != "")
saveAllPorts <- PortfolioAllPorts
# PortfolioAllPorts <- saveAllPorts ## reset input


if("Cnty.of.Dom" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Cnty.of.Dom" = "CNTRY_OF_DOMICILE"))
}else if("Country.ISO.Code" %in% colnames(BBGPORTOutput)) {
  BBGPORTOutput <- rename(BBGPORTOutput, c("Country.ISO.Code" = "CNTRY_OF_DOMICILE"))
}

## Merge PortfolioNs with BBG Data for those ISINs
BBGPORTOutput <- unique(subset(BBGPORTOutput, !(ISIN %in% c("#N/A N/A","",NA) | is.na(BBGPORTOutput$ISIN)), c("Ticker" , "Subgroup", "ISIN", "CNTRY_OF_DOMICILE","Mty")))
#PortfolioAllPorts <-subset(PortfolioAllPorts, select = -c(Ticker))
PortfolioAllPorts <- merge(PortfolioAllPorts, BBGPORTOutput, by = c("ISIN"), all.x = TRUE,all.y=FALSE)


CorpDebtTicker <- colsplit(PortfolioAllPorts$Ticker, pattern = " ", names = c("COMPANY_CORP_TICKER",2,3))[1]
PortfolioAllPorts <- cbind(PortfolioAllPorts,CorpDebtTicker)

PortGovBanksSupraNat <- subset(PortfolioAllPorts, PortfolioAllPorts$Ticker %in% GovBanksSupraNat$Co..Corp.TKR)
PortfolioAllPorts <- subset(PortfolioAllPorts, !PortfolioAllPorts$Ticker %in% GovBanksSupraNat$Co..Corp.TKR)

MissingBBGInfo <- unique(subset(PortfolioAllPorts, is.na(Subgroup), select = "ISIN"))
MissingBBGInfo$QTY <- 1
MissingBBGInfo$Date <- BBGDataDate

#Create metaportfolio
PortfolioAllPorts <- rename(PortfolioAllPorts, c("FirmName" = "InvestorName", "Portfolio" = "PortfolioName","Amount" = "Position"))
MetaPort <- PortfolioAllPorts
MetaPort$InvestorName <- "MetaPortfolio"
MetaPort$PortfolioName <- "MetaPortfolio"

PortfolioAllPorts <- rbind(PortfolioAllPorts,MetaPort)

PortfolioAllPorts$PortfolioName <- paste0(PortfolioAllPorts$PortfolioName, PortfolioAllPorts$Linked)

PortfolioAllPorts <- subset(PortfolioAllPorts, select = -c(Linked,IDcodeHeader))

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

PortfolioAllPorts<-subset(PortfolioAllPorts, select = c("COMPANY_CORP_TICKER", "Subgroup", "Position", "InvestorName", "PortfolioName", "Ticker", "ISIN", "CNTRY_OF_DOMICILE","Mty"))

# Create portfolio for bond universe
UniversePort <- rename(UniversePort, c("DebtTicker"= "COMPANY_CORP_TICKER"))
UniversePort <- subset(UniversePort, select = -c(SectorWeight,piesector))
UniversePort$InvestorName <- "GlobalBondUniverse"
UniversePort$PortfolioName <- "GlobalBondUniverse"
UniversePort$Ticker <- "GlobalBondUniverse"
UniversePort$ISIN <- "GlobalBondUniverse"
UniversePort$CNTRY_OF_DOMICILE <- "GlobalBondUniverse"
UniversePort$Mty <- "30000101"
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
ListAllPorts <- subset(ListAllPorts, !ListAllPorts$InvestorName %in% c("MetaPortfolio") | ListAllPorts$Type != "Portfolio")

# |------------------------------|
# |----- Portfolio Analysis -----|
# |------------------------------|
for (i in  1:length(ListAllPorts$PortfolioName)){
  tryCatch({
    print(i)
    if (ListAllPorts$Type[i] == "Portfolio"){ 
      Portfolio = subset(PortfolioAllPorts, PortfolioName == ListAllPorts$PortfolioName[i] & InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- paste0(ListAllPorts$PortfolioName[i],"_",ListAllPorts$InvestorName[i])
    }else{
      Portfolio = subset(PortfolioAllPorts, InvestorName == ListAllPorts$InvestorName[i])
      PortfolioName <- ListAllPorts$InvestorName[i]
      
    }
    
    Portfolio <- subset(Portfolio, select = c("COMPANY_CORP_TICKER", "Position", "Subgroup" , "Ticker", "ISIN", "CNTRY_OF_DOMICILE","Mty"))
    TotalPortfolioAUM <- sum(Portfolio$Position,na.rm = TRUE)
    if(eval(ListAllPorts$InvestorName[i]) == "GlobalBondUniverse") { TotalPortfolioAUM <- TotalDebt }
    
    #sum position over isn and ticker in case of multiples
    Portfolio <- ddply(Portfolio,.(COMPANY_CORP_TICKER, Subgroup, Ticker, ISIN, CNTRY_OF_DOMICILE,Mty), summarize, Position = sum(Position))
    
    
    ### if there is no price information or if the asset is outside of the region ###
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
        Portfolio <- merge(Portfolio, BICSMoodysRisk, by.x = c("Subgroup"), by.y = "INDUSTRY_SUBGROUP", all.x = TRUE, all.y = FALSE)
        Portfolio$MoodysRiskLvl[is.na(Portfolio$MoodysRiskLvl)] <- 5
        
        ## Need to aggregate to piesector level and corp level, not just corp level, if you want to later split the companies that have OG production and are classified as OGs
        
        # Aggregate to CORP level
        PortfolioSub <- ddply(subset(Portfolio,Position!=0),.(COMPANY_CORP_TICKER, piesector), summarize, Position = sum(Position))
        
        # Calculate new portoflio weights at the Corp Debt ticker level
        PortAUM2 <- sum(PortfolioSub$Position)
        #if (round(PortAUM,1) != round(PortAUM2,1)) break 
        rm(PortAUM2)
        
        PortfolioSub$PortWeightDebtlvl <- PortfolioSub$Position/PortAUM
        PortfolioSub$piesector[PortfolioSub$piesector %in% "Utility Power"] <- "Power"
        
        #####  Combine with Asset level data ######
        
        ReducedListDebt <- merge(subset(MasterDataDebt,Aggregation == "GlobalAggregate"), PortfolioSub, by.x = c("DebtTicker","Sector"), by.y = c("COMPANY_CORP_TICKER", "piesector"), all.x=FALSE, all.y=FALSE)
        
        ### If there is ALD within the portfplio then proceed with the analysis ###
        PortAUMAnalyzed <- 0
        if(dim(ReducedListDebt)[1]>0){
          PortAUMAnalyzed <- sum(unique(subset(ReducedListDebt, select = c("Position", "DebtTicker")))[,"Position"], na.rm = TRUE)
	        
          # Portfolio Regionality
          PortfolioRegionality <- merge(PortfolioSub,CompanyWeightingData, by.x = "COMPANY_CORP_TICKER", by.y = "DebtTicker", all.x = FALSE, all.y = FALSE)
          PortfolioRegionality$RegionalWtPortfolioWt <- PortfolioRegionality$PortWeightDebtlvl * PortfolioRegionality$RegionalWeightingFactorCompany
          PortfolioRegionSplit <- aggregate(PortfolioRegionality["RegionalWtPortfolioWt"],by = PortfolioRegionality[,c("Sector","BenchmarkRegion")], FUN = sum)
          
          
          
	        ReducedListDebt$WtProduction <- ReducedListDebt$CompanyLvlProd * ReducedListDebt$PortWeightDebtlvl
          	    
          # Minimise data frame size by restricting results to only a 10 year forcast
          ReducedListDebt <- subset (ReducedListDebt, Year <= (Startyear + 10))
          
          # Allocate production to the portfolio based on Portfolio Wt 
          
          # Calculate Carsten Metric
          ReducedListDebt$CarstenMetric_Port <- ReducedListDebt$PortWeightDebtlvl * ReducedListDebt$TechShare * ReducedListDebt$RegionalWeightingFactorCompany
          
          # Aggregate Global and regional portfolio values
          PortMix <- subset(ReducedListDebt,select = -c(TotalDebt, ClimateSecDebt))
          
          # make sure all technologies and sectors are in the database (to avoid errors at a later stage) 
          # MAYBE CHANGE THIS TO USE THE COMPLETEDATA FUNCTION AT SOME STAGE
          techlist <- data.frame("Technology" = AllLists$TechList)
          yearlist <- data.frame("Year" = unique(PortMix$Year))
          lineadd <- merge(techlist,yearlist)
          lineadd$Sector <- "Power"
          lineadd$Sector[lineadd$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
          lineadd$Sector[lineadd$Technology %in% c("Coal")] <- "Coal" 
          lineadd$Sector[lineadd$Technology %in%  c("Electric","Hybrid","ICE")] <-  "Automotive"
          ### the datacompletion function does this, however is fairly specific. 
          PortMix <- subset(PortMix, Technology %in% TechnologyList)
          PortMix <- merge(PortMix,lineadd, by = c("Sector","Technology","Year"), all.y = TRUE, all.x = TRUE)
          
          #calaculate the sector prodution and the techshare
          Sectorref <- ddply(PortMix,.(DebtTicker,BenchmarkRegion, Sector, Year),summarize,
                             WtSectorProd = sum(WtProduction,na.rm=TRUE))
          
          Sectorref2 <- ddply(PortMix,.(Sector, Year),summarize,
                              SectorWeight = sum(PortWeightDebtlvl,na.rm=TRUE))
          
          techlist2 <- unique(subset(PortMix, select = c("DebtTicker","Sector","Technology", "Year")))
          Sectorref <- merge(Sectorref,techlist2, by = c("Sector", "Year", "DebtTicker"), all.x = TRUE, all.y = TRUE)
          PortMix <- merge(PortMix,Sectorref, by = c("DebtTicker","BenchmarkRegion", "Sector", "Technology",  "Year"), all.x=TRUE, all.y=FALSE) #Method
          
          
          # fill blanks of newly add rows
          PortMix$WtProduction[is.na(PortMix$WtProduction)] <- 0
          PortMix$CarstenMetric_Port[is.na(PortMix$CarstenMetric_Port)] <- 0
          
          PortMix$PortWtTechShare <- PortMix$WtProduction/PortMix$WtSectorProd * PortMix$PortWeightDebtlvl
          
          
          ## Aggregate Portfolio at regional level
          PortMixRegionalTech <- ddply(PortMix,.(BenchmarkRegion, Sector, Technology, Year), summarize,
                                       CompanyLvlProd = sum(CompanyLvlProd, na.rm=TRUE),
                                       Wt = sum(PortWeightDebtlvl, na.rm=TRUE),
                                       WtProduction = sum(WtProduction, na.rm=TRUE),
                                       PortWtTechShare = sum(PortWtTechShare, na.rm=TRUE),
                                       CarstenMetric_Port = sum(CarstenMetric_Port, na.rm=TRUE))
          
          PortMixRegionalSec <- ddply(PortMix,.(BenchmarkRegion, Sector, Year), summarize,
                                      CompanyLvlProdSec = sum(CompanyLvlProd, na.rm=TRUE),
                                      WtProductionSec = sum(WtProduction, na.rm=TRUE),
                                      PortWtTechShareSec=sum(PortWtTechShare, na.rm=TRUE),
                                      CarstenMetric_PortSec = sum(CarstenMetric_Port, na.rm=TRUE))
          
          PortMixRegional <- merge(PortMixRegionalTech, PortMixRegionalSec, by = c("BenchmarkRegion", "Sector", "Year"))
          
          ## Not sure whether to calculate this and include it. Have to do it at global level too.
          
          ## Aggregate Portfolio at global level          
          PortMixGlobalTech <- ddply(PortMix,.(Sector, Year, Technology), summarize,
                                     CompanyLvlProd = sum(CompanyLvlProd, na.rm=TRUE),
                                     Wt = sum(PortWeightDebtlvl, na.rm=TRUE),
                                     WtProduction = sum(WtProduction, na.rm=TRUE),
                                     PortWtTechShare = sum(PortWtTechShare, na.rm=TRUE),
                                     CarstenMetric_Port = sum(CarstenMetric_Port, na.rm=TRUE))
          
          PortMixGlobalSec <- ddply(PortMix,.(Sector, Year), summarize,
                                    CompanyLvlProdSec = sum(CompanyLvlProd, na.rm=TRUE),
                                    WtProductionSec = sum(WtProduction, na.rm=TRUE),
				                            PortWtTechShareSec = sum(PortWtTechShare, na.rm=TRUE),
                                    CarstenMetric_PortSec = sum(CarstenMetric_Port, na.rm=TRUE))
          
          PortMixGlobal <- merge(PortMixGlobalTech, PortMixGlobalSec, by = c("Sector", "Year"))
          PortMixGlobal$BenchmarkRegion <- "GlobalAggregate"
          
          #Combine and remove redunant dataframes
          PortMix <- rbind(PortMixGlobal,PortMixRegional)
          rm(PortMixRegionalTech,PortMixRegionalSec,PortMixGlobalTech,PortMixGlobalSec)
          
          #Complete dataframe to insure successful merging
          PortMix <- datacompletion(PortMix)
          
          PortMix$CompanyLvlProdSec [is.na(PortMix$CompanyLvlProdSec)] <- 0
          PortMix$WtProductionSec [is.na(PortMix$WtProductionSec)] <- 0
          PortMix$CarstenMetric_PortSec [is.na(PortMix$CarstenMetric_PortSec)] <- 0
	        PortMix$PortWtTechShareSec [is.na(PortMix$PortWtTechShareSec)] <- 0
          
          # Calculate regional and global techshares. These are not the techshare used to calculate the Carsten metric. 
	        # They're the raw techshares of companies the portfolio is invested in without the effect 
          # of the portfolio portfolio wt distrubtion over different companies different regional distrubtion of physical assets.
          PortMix$PortCompanyTechShare <- PortMix$CompanyLvlProd/PortMix$CompanyLvlProdSec
          PortMix$PortWtTechShare <- PortMix$WtProduction/PortMix$WtProductionSec
          
          # Rename datafelds as prepreation for mering with the market benchmark" data
          PortMix <- rename(PortMix, c("Wt" = "PortWt","CompanyLvlProd" = "PortCompanyLvlProd", "CompanyLvlProdSec" = "PortCompanyLvlProdSec", "WtProduction" = "PortWtProduction", "WtProductionSec" = "PortWtProductionSec"))
          
          ###### New Aggregation lines ###########
# 	        PortfolioRegionSplit <- Regiondatacompletion(PortfolioRegionSplit)
# 
#           PortfolioBenchmark <- merge(PortfolioRegionSplit, subset(RegionalBenchmark, select = c("Sector","BenchmarkRegion", "Year","Scenario","Technology")), by = c("Sector","BenchmarkRegion"), all.x = TRUE)
#           # "SecWtMarket","Benchmark_CarstensMetric","Benchmark_CarstensMetricSec","RefMarketLvlProd","ProjMarketProd","ProjMarketProdSec","WtProjMarketProduction","WtTechShareTechShareMarket"
#           PortfolioBenchmark$PortfolioBenchmark_CarstensMetric <- PortfolioBenchmark$Benchmark_CarstensMetric / PortfolioBenchmark$Benchmark_CarstensMetricSec * PortfolioBenchmark$RegionalWtPortfolioWt
#           PortfolioBenchmark$RegWtProjMarketProd <- PortfolioBenchmark$ProjMarketProd * PortfolioBenchmark$RegionalWtPortfolioWt
#           PortfolioBenchmark$RegWtRefMarketLvlProd <- PortfolioBenchmark$RefMarketLvlProd * PortfolioBenchmark$RegionalWtPortfolioWt
# 
#           # PortfolioBenchmark <- merge(PortfolioBenchmark,unique(subset(PortMix1, Year == Startyear,select = c("Technology","Wt"))), by = "Technology", all.x = TRUE) # that actually makes no sense but maybe we need to calculate the Wt
#           PortfolioBenchmark$Benchmark_WtTechShare <- PortfolioBenchmark$RegWtProjMarketProd/PortfolioBenchmark$ProjMarketProdSec
#           PortfolioBenchmark$Benchmark_WtTechShareTechShare <- PortfolioBenchmark$WtTechShareTechShareMarket * PortfolioBenchmark$RegionalWtPortfolioWt
# 
#           PortBenchmark <- ddply(PortfolioBenchmark,.(Sector, Year, Scenario, Technology,  SecWtMarket), summarize,
#                                Benchmark_CarstensMetric=sum(PortfolioBenchmark_CarstensMetric, na.rm=TRUE),
#                                RegWtProjMarketProd = sum(RegWtProjMarketProd,na.rm = TRUE),
#                                RegWtRefMarketLvlProd = sum(RegWtRefMarketLvlProd,na.rm = TRUE),
#                                Benchmark_WtTechShare = sum(Benchmark_WtTechShare,na.rm = TRUE),
#                                Benchmark_WtTechShareTechShare = sum(Benchmark_WtTechShareTechShare ,na.rm = TRUE))
# 
#           PortBenchmarkSec <- ddply(PortBenchmark,.(Sector, Year, Scenario), summarize,
#                                  RegWtProjMarketProdSec = sum(RegWtProjMarketProd,na.rm = TRUE),
#                                  Benchmark_WtTechShareSec = sum(Benchmark_WtTechShare,na.rm = TRUE),
#                                  Benchmark_WtTechShareTechShareSec = sum(Benchmark_WtTechShareTechShare ,na.rm = TRUE))
# 
#           PortBenchmark <- merge(PortBenchmark, PortBenchmarkSec, by= c("Sector", "Year", "Scenario"))
# 
#           PortBenchmark$Benchmark_OGC <- PortBenchmark$RegWtProjMarketProd / PortBenchmark$RegWtRefMarketLvlProd * 100
#           PortBenchmark$Benchmark_WtTechShareTechShare_corrupted <- PortBenchmark$Benchmark_WtTechShare / PortBenchmark$Benchmark_WtTechShareSec
#           PortBenchmark$Benchmark_WtTechShareTechShare <- PortBenchmark$Benchmark_WtTechShareTechShare / PortBenchmark$Benchmark_WtTechShareTechShareSec
# 
#           PortBenchmark$BenchmarkRegion <- "GlobalAggregate"
          
          
          ############
          
          ### The regional and market benchmarks are bound to the Portfolio Data here 
          ### DebtBenchmarkDataframe was created prior to this loop. 
          ### However can Benchmark_WtTechShareTS be created previously or is it dependent on the portfolio?
          ### However DebtBenchmarkDataframe and the PortBenchmark aren't the same. Merge with PortBenchmark to ensure coverage. 
          
          ### Combin here is not aggregated past debt ticker level
          Combin <- merge(PortMix, DebtBenchmarkDataframe, by = c("BenchmarkRegion", "Sector", "Technology", "Year"))  
          
          # Calaculate OGC Metric only for GlobalAgg
          ### CHANGED "CarstenMetric_PortSec" to "CarstenMetric_Port"
          
          OGCMetric <- subset(Combin, BenchmarkRegion == "GlobalAggregate", select = c("BenchmarkRegion", "Sector", "Technology", "Scenario", "Year", "PortWtProduction", "CarstenMetric_Port", "ProjWtRegion_Market", "ProjMarketProd"))
          # Selct reference year values so you can calculate the aggregate production allocated to the porfolio by the portfolio wt method, and the markets production
          OGCMetricRef <- subset(Combin, Year == Startyear & BenchmarkRegion == "GlobalAggregate", select = c("BenchmarkRegion", "Sector", "Technology", "Scenario", "PortWtProduction", "CarstenMetric_Port", "ProjWtRegion_Market", "ProjMarketProd"))
          OGCMetricRef <- rename(OGCMetricRef, c("PortWtProduction" = "RefPortWtProduction", "CarstenMetric_Port" = "Ref_PortSecWt", "ProjMarketProd" = "RefProjMarketProd", "ProjWtRegion_Market" = "RefProjWtRegion_Market"))
          OGCMetric <- merge(OGCMetric, OGCMetricRef, by = c("BenchmarkRegion", "Sector", "Technology", "Scenario"))
          # Calaculate the OGC sector weight change if it were to change at the same rate as the production profile of companies (but when allocated the portfolio wt)
          OGCMetric$ProjOGCSectorWt_Port <- OGCMetric$Ref_PortSecWt + ((OGCMetric$PortWtProduction - OGCMetric$RefPortWtProduction)/OGCMetric$RefPortWtProduction * OGCMetric$Ref_PortSecWt)
          OGCMetric$ProjOGCSectorWt_Market <- OGCMetric$RefProjWtRegion_Market + ((OGCMetric$ProjMarketProd - OGCMetric$RefProjMarketProd)/OGCMetric$RefProjMarketProd * OGCMetric$RefProjWtRegion_Market)
          OGCMetric$OGCMetric_Port <- 100 * (OGCMetric$ProjOGCSectorWt_Port/OGCMetric$RefProjWtRegion_Market)
          OGCMetric$OGCMetric_ProjMarket <- 100 * (OGCMetric$ProjOGCSectorWt_Market/OGCMetric$RefProjWtRegion_Market)
          
          #Merge back with combin
          Combin <- merge(Combin, OGCMetric, by = c("BenchmarkRegion", "Sector", "Technology", "Scenario", "Year", "PortWtProduction", "CarstenMetric_PortSec", "ProjWtRegion_Market", "ProjMarketProd"))
           
          # OGCMetrikRef$OGCMetrik_PortfolioRef <- OGCMetrikRef$SectorWeight / OGCMetrikRef$SecWtMarket * 100
          # OGCMetrikRef$OGCMetrik_PortfolioRef <- (OGCMetrikRef$SectorWeight * OGCMetrikRef$PortTechShare) / (OGCMetrikRef$SecWtMarket * OGCMetrikRef$RegWtProjMarketProd / OGCMetrikRef$RegWtProjMarketProdSec) * 100
          # OGCMetrikRef <- rename(OGCMetrikRef, c("WtProduction" = "WtProductionRef"))
          # 
          # Combin <- merge(Combin,subset(OGCMetrikRef, select = c("BenchmarkRegion", "Sector", "Technology", "Scenario", "OGCMetrik_PortfolioRef", "WtProductionRef")), by = c("BenchmarkRegion", "Sector", "Technology", "Scenario"), all.x = TRUE)
          # Combin$OGCMetrik_Portfolio <- Combin$OGCMetrik_PortfolioRef * Combin$WtProduction / Combin$WtProductionRef
          # Combin$OGCMetrik_Portfolio[is.nan(Combin$OGCMetrik_Portfolio)] <- 0
          # 
          # Calculate Exposure percentages for technologies with direct 'green' subsititues. This uses the difference in Technology mixes
          # Combin$Exposure_WtTechShareTechShare <- (Combin$WtTechShareTechShare - Combin$Benchmark_WtTechShareTechShare) / Combin$Benchmark_WtTechShareTechShare
          Combin$Exposure_CarstensMetric <- (Combin$CarstenMetric_Port - Combin$CarstenMetric_ProjMarket) / Combin$CarstenMetric_ProjMarket
          Combin$Exposure_OGCMetric <- (Combin$OGCMetric_Port - Combin$OGCMetric_ProjMarket) / Combin$OGCMetric_ProjMarket
          
          #Combin$Exposure_OGCMetrik <- (Combin$OGCMetrik_Portfolio - Combin$Benchmark_OGC) / Combin$Benchmark_OGC
          
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
          
          # implement AUM in sector values
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
        }
      }
    }
  })
}


# Create a folder for portolio results and go to that folder, 
#Definitely need to check for these
BatchFolder <- paste0(OutputFolder,"/", BatchName,"/")
if(!dir.exists(file.path(BatchFolder))){dir.create(file.path(BatchFolder), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
BatchFolder <- paste0(OutputFolder,"/", BatchName,"/",AssessmentDate,"/")
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

CombinAllsave <- CombinAll

write.csv(CombinAll,paste(BatchFolder,BatchName,"_DebtAnalysisResults.csv",sep = ""),row.names = FALSE)
write.csv(ReducedListDebtAll,paste(BatchFolder,BatchName,"_DebtProductionCompanies_Snapshot",Startyear+5,".csv",sep = ""),row.names = FALSE)
write.csv(PortfolioAll,paste(BatchFolder,BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv",sep = ""),row.names = FALSE)

write.csv(MissingBBGInfo, paste0(BatchFolder,BatchName,"_",dim(MissingBBGInfo)[1],"ISINs_wo_BBG_Info_from_Port.csv"), row.names = FALSE)

Sub450Scenario <- subset(CombinAll, Scenario == "450S")
write.csv(Sub450Scenario,paste(BatchFolder,BatchName,"_DebtAnalysisResults-450S-only.csv",sep = ""),row.names = FALSE, na = "")

