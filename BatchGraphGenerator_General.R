#2DP Graph Creater
# units of all data in units of cars, MW power capacity, BBL oil, CF gas, and tons coal

#Version Control
# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 05 - 12 |        CM        |         1            | 

# ------
# Load packages
# ------


# rm(list=ls())

library(grid)
library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(gridExtra)
library(scales)
library(stringr)
library(extrafont)
library(tidyr)
library(knitr)
library(png)
library(RColorBrewer)
library(matrixStats)


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

ParameterFile <- ReadParameterFile(PROC.DATA.PATH)
### fill up those variables
SetParameters(ParameterFile)              # Sets BatchName, Scenario, BenchmarkRegion etc. 
print("*** STARTING SCRIPT with PARAMETERS:")
print(ParameterFile)


#-------------
# Set Input / OUtput Locations Based on parameter File Input
#------------

### Finish setting up paths based on what was in the Parameter file
# FinancialDataFolder <- paste0(FIN.DATA.PATH,ParameterFile$DateofFinancialData,"/PORT/")
TEMPLATE.PATH <- paste0(CODE.PATH,"03_ReportingCode/01_ReportTemplates/")
PROC.DATA.PATH <- paste0(DATA.PATH,"/01_ProcessedData/")
PROJ.PATH <- paste0(PORTS.PATH,ParameterFile$ProjektName,"/")
BATCH.PATH <- paste0(PROJ.PATH,BatchName,"/")
#BATCH.PATH <- BATCH.PATH
#PortfolioLocation <- PROJ.PATH

if(!dir.exists(file.path(BATCH.PATH))){dir.create(file.path(BATCH.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  


# ------
# Read in Results and Inputs
# ------- 

# Read in Parameter File
ComparisonFile <- ParameterFile$ComparisonFile                        # Defines whether the comparative graphs are to be produced
ReportTemplate <- ParameterFile$ReportStyle
ImportNewComparisonList <- FALSE

# Set Results Location
REPORT.PATH <- paste0(RESULTS.PATH,"/05_Reports/",ProjectName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
REPORT.PATH <- paste0(RESULTS.PATH,"/05_Reports/",ProjectName,"/",BatchName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
BATCH.RES.PATH <- paste0(RESULTS.PATH,"/01_BatchResults/",BatchName,"/",BatchToTest,"/")
# setwd(BATCH.RES.PATH)

# Get Equity Batch Results
if (file.exists(paste0(BATCH.RES.PATH,BatchName,"/",BatchToTest,"/",BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"))){
  EQBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"),stringsAsFactors=FALSE,strip.white=TRUE)
}else{
  EQBatchTest <- read.csv(paste(BATCH.RES.PATH,BatchName,"_EquityAnalysisResults-450S-only.csv",sep=""),stringsAsFactors=FALSE)
}
EQBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_PortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE)
EQCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_CompanysProduction_Snapshot.csv"),stringsAsFactors = FALSE)

# Get Debt Batch Results
CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors=FALSE)
CBBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE)
CBCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtProductionCompanies_Snapshot2022.csv"),stringsAsFactors = FALSE)

# External Data Read In
setwd(paste0(DATA.PATH,"/01_ProcessedData/"))
Index <- read.csv("2017-07-20_IndexAnalysis5_EquityAnalysisResults.csv",stringsAsFactors = FALSE)
AllIEATargets <- read.csv("IEATargets2016_AllRegions.csv", stringsAsFactors=FALSE)
AllCompanyData <- read.csv("CompanyLevelData_2017-08-21.csv",stringsAsFactors = FALSE)
BBGData_CompanyLevel <- read.csv(paste0("CompanylvlBBGData",ParameterFile$DateofFinancialData,".csv"),stringsAsFactors = FALSE)
MasterData <- read.csv(paste0("MasterData",ParameterFile$DateofFinancialData,".csv"))
OGData <- read.csv(paste0("01_SectorMasters/",ParameterFile$GDYearMonth,"/OGMaster_",ParameterFile$GDYearMonth,".csv"),stringsAsFactors = FALSE)
BenchmarkRegionList <- read.csv("BenchRegions.csv")
IndexUniverses <- read.csv("IndexRegions.csv")
OGCarbonBudget <- read.csv(paste0(DATA.PATH,"/04_Other/CarbonCapexUpstream.csv"),stringsAsFactors = FALSE)

# Batch related Portfolio & Fund-Data Results
PortfolioBreakdown <- read.csv(paste0(PORTS.PATH,ProjectName,"/",BatchName,"/",BatchName,"Portfolio_Overview_Piechart.csv"),stringsAsFactors = FALSE)
PortfolioBreakdown$InvestorNameLong <- PortfolioBreakdown$InvestorName
PortfolioBreakdown$PortfolioNameLong <- PortfolioBreakdown$PortfolioName

PortfolioBreakdown$InvestorName<- gsub("[ _.-]","",PortfolioBreakdown$InvestorName)
PortfolioBreakdown$PortfolioName<- gsub("[ _.-]","",PortfolioBreakdown$PortfolioName)
PortfolioBreakdown$InvestorName<- gsub("[()]","",PortfolioBreakdown$InvestorName)
PortfolioBreakdown$PortfolioName<- gsub("[()]","",PortfolioBreakdown$PortfolioName)

PortfolioBreakdown$PortName <- paste0(PortfolioBreakdown$PortfolioName,"_",PortfolioBreakdown$InvestorName)
PortfolioBreakdown$PortName <- gsub(" ","",PortfolioBreakdown$PortName)
TestList<-PortfolioBreakdown

# Funds within the Portfolios
FundList <- read.csv(paste0(PORTS.PATH,ProjectName,"/",BatchName,"/",BatchName,"Port_ListofFunds.csv"),stringsAsFactors = FALSE)
if (exists("FundList")){
  FundList$InvestorName <- gsub(".","",FundList$InvestorName, fixed = TRUE)
  FundList$PortfolioName<- gsub(".","",FundList$PortfolioName, fixed = TRUE)
}else{
  FundList <- "No Funds"
}

# Remove Listed Market if it's there
EQBatchTest <- EQBatchTest[EQBatchTest$InvestorName != c("ListedMarket", "MetaPortfolio"),]
CBBatchTest <- CBBatchTest[CBBatchTest$InvestorName != c("ListedMarket", "MetaPortfolio"),]

# EQBatchTest_PortSnapshots <- EQBatchTest_PortSnapshots[EQBatchTest_PortSnapshots$InvestorName != c("ListedMarket", "MetaPortfolio"),]
# CBBatchTest_PortSnapshots <- CBBatchTest_PortSnapshots[CBBatchTest_PortSnapshots$InvestorName != c("ListedMarket", "MetaPortfolio"),]

# Add Company Names to BatchTest_PortSnapshot -  should be superceded with changes to EQY Code
if (!"Name" %in% colnames(EQBatchTest_PortSnapshots)){
  CompanyNames <- unique(subset(AllCompanyData, select=c("EQY_FUND_TICKER","Name")))
  EQBatchTest_PortSnapshots <- merge(CompanyNames, EQBatchTest_PortSnapshots, by = "EQY_FUND_TICKER", all.y = TRUE)}

# For the Funds Heat Map
FundsDataAll <- read.csv(paste0(RESULTS.PATH,"/01_BatchResults/Swiss_FundData/2016Q4/Swiss_FundData_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"), stringsAsFactors = FALSE)

# Add Funds for comparison charts 
# Add a code to update the Index results
if (ComparisonFile == "FundComparison"){
  FundLocation<-paste0(RESULTS.PATH,"/01_BatchResults/",ComparisonFile,"/2016Q4/")

  if (ImportNewComparisonList == TRUE){
    
    EQComparisonBatchTestLong <- read.csv(paste0(FundLocation,ComparisonFile,"_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"),stringsAsFactors = FALSE)
    EQComparisonPortSSLong <- read.csv(paste0(FundLocation,ComparisonFile,"_PortfolioData_Snapshot2017.csv"),stringsAsFactors = FALSE)
    CBComparisonBatchTestLong <- read.csv(paste0(FundLocation,ComparisonFile,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors = FALSE)
    CBComparisonPortSSLong <- read.csv(paste0(FundLocation,ComparisonFile,"_DebtPortfolioData_Snapshot",Startyear,".csv"),stringsAsFactors = FALSE)
    
    comparisonNumber <- 100
    
    ComparisonList <- comparisonlist(comparisonNumber, EQComparisonBatchTestLong,CBComparisonBatchTestLong)
    
    EQComparisonBatchTest <- EQComparisonBatchTestLong[EQComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    EQComparisonPortSS <- EQComparisonPortSSLong[EQComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    CBComparisonBatchTest <- CBComparisonBatchTestLong[CBComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    CBComparisonPortSS <-EQComparisonPortSSLong[CBComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    
    write.csv(EQComparisonBatchTest,paste0(FundLocation, "EQComparisonBatchTest.csv"),row.names = F)
    write.csv(EQComparisonPortSS,paste0(FundLocation, "EQComparisonPortSS.csv"),row.names = F)
    write.csv(CBComparisonBatchTest,paste0(FundLocation, "CBComparisonBatchTest.csv"),row.names = F)
    write.csv(CBComparisonPortSS,paste0(FundLocation, "CBComparisonPortSS.csv"),row.names = F)
    
  }else{
    EQComparisonBatchTest<- read.csv(paste0(FundLocation,"EQComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    EQComparisonPortSS <- read.csv(paste0(FundLocation,"EQComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonBatchTest<- read.csv(paste0(FundLocation,"CBComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonPortSS <- read.csv(paste0(FundLocation,"CBComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
  }
}

if (ComparisonFile == "Swiss"){
  FundLocation<-paste0(RESULTS.PATH,"/01_BatchResults/","Swiss","/2016Q4/")
  
  if (ImportNewComparisonList == TRUE){
    
    EQComparisonBatchTestLong <- read.csv(paste0(FundLocation,ComparisonFile,"_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"),stringsAsFactors = FALSE)
    EQComparisonPortSSLong <- read.csv(paste0(FundLocation,ComparisonFile,"_PortfolioData_Snapshot2017.csv"),stringsAsFactors = FALSE)
    CBComparisonBatchTestLong <- read.csv(paste0(FundLocation,ComparisonFile,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors = FALSE)
    CBComparisonPortSSLong <- read.csv(paste0(FundLocation,ComparisonFile,"_DebtPortfolioData_Snapshot",Startyear,".csv"),stringsAsFactors = FALSE)
    
    comparisonNumber <- length(unique(EQComparisonBatchTestLong$PortName))
    
    ComparisonList <- comparisonlist(comparisonNumber, EQComparisonBatchTestLong,CBComparisonBatchTestLong)
    
    EQComparisonBatchTest <- EQComparisonBatchTestLong[EQComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    EQComparisonPortSS <- EQComparisonPortSSLong[EQComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    CBComparisonBatchTest <- CBComparisonBatchTestLong[CBComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    CBComparisonPortSS <-EQComparisonPortSSLong[CBComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    
    write.csv(EQComparisonBatchTest,paste0(FundLocation, "EQComparisonBatchTest.csv"),row.names = F)
    write.csv(EQComparisonPortSS,paste0(FundLocation, "EQComparisonPortSS.csv"),row.names = F)
    write.csv(CBComparisonBatchTest,paste0(FundLocation, "CBComparisonBatchTest.csv"),row.names = F)
    write.csv(CBComparisonPortSS,paste0(FundLocation, "CBComparisonPortSS.csv"),row.names = F)
    
  }else{
    EQComparisonBatchTest<- read.csv(paste0(FundLocation,"EQComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    EQComparisonPortSS <- read.csv(paste0(FundLocation,"EQComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonBatchTest<- read.csv(paste0(FundLocation,"CBComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonPortSS <- read.csv(paste0(FundLocation,"CBComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
  }
}

# ------
# Bench Regions and Indicies and Sector Classifications
# ------
# Get Bench Regions
RegionCountries <- data.frame(BenchmarkRegionList$Global, BenchmarkRegionList$OECD)
RegionCountries <- rename(RegionCountries, c("BenchmarkRegionList.Global"="Global","BenchmarkRegionList.OECD"="OECD"))

IndexUniverses[is.na(IndexUniverses)] <- ""
IndexUniversesList <- data.frame(IndexUniverse = IndexUniverses$IndexUniverse[!is.na(IndexUniverses$IndexUniverse) & IndexUniverses$IndexUniverse != ""], IndexUniversesColname = IndexUniverses$IndexUniverseColname[!is.na(IndexUniverses$IndexUniverseColname) & IndexUniverses$IndexUniverseColname != ""])

# Select the Regional and Index Benchmarks
if (Indexchoose == "MSCIWorld"){Indexchoose <- "MSCIWorld_MSCI"}
IndexData <- subset(Index, BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & PortName == Indexchoose) 
if (BenchmarkRegionchoose != "Global"){
  IndexAutoGlobal <- subset(Index, BenchmarkRegion == "Global" & Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & PortName ==Indexchoose & Sector == "Automotive")
  IndexData <- rbind(IndexData,IndexAutoGlobal)
}
if (Indexchoose == "MSCIWorld_MSCI"){Indexchoose <- "MSCIWorld"}

IEATargetsAll <- subset(AllIEATargets, BenchmarkRegion == "Global" &Year %in% c(Startyear,Startyear+5,Startyear+10,Startyear+15)  & Scenario == Scenariochoose, select = c("Sector","Technology","AnnualvalIEAtech","Year")) 
IEATargetsAll <- IEATargetsAll[!IEATargetsAll$Technology %in% "OilCap",]


# Bind Sector Classification from BBG - ICB Subsector Name
CleanedBBGData <- cleanBBGData(BBGData_CompanyLevel,AllCompanyData,Startyear,Scenariochoose,CompanyDomicileRegionchoose,BenchmarkRegionchoose)
Companies <- CleanedBBGData[[1]]
UtilityCompanies <- CleanedBBGData[[2]]
AutoCompanies <- CleanedBBGData[[3]]
OilData <- cleanOGData(OGData,AllCompanyData,Startyear)

# Other Sector Data
OSTargets <- read.csv(paste0(DATA.PATH,"/04_Other/SDA_Targets.csv"), stringsAsFactors = FALSE)
OSdata <- read.csv(paste0(DATA.PATH,"/00_RawData/99_SampleDataSets/OSmaster_2017-10-07.csv"), stringsAsFactors = FALSE)
ShippingData <- read.csv(paste0(DATA.PATH,"/00_RawData/07_Shipping/ShippingData.csv"), stringsAsFactors = FALSE,strip.white = TRUE)
EQ_OS_WEM <- matchOS(OSdata, EQBatchTest_PortSnapshots)
write.csv(EQ_OS_WEM,paste0(BatchName,"_EQ_OtherSectorOutput.csv"),row.names = FALSE)
CB_OS_WEM <- matchOS(OSdata, CBBatchTest_PortSnapshots)
write.csv(CB_OS_WEM,paste0(BatchName,"_CB_OtherSectorOutput.csv"),row.names = FALSE)

# ---------
# Comparison Companies
# ---------
# Provides the ranking table for each company, Global benchmarkregion
# Currently takes the BatchTest inputs, but this should be changed to the results for specified companies

# Batch Test Results 
# Only require the exposure, AUM, ranking needs to happen compared to the Comparison Results
# Fund Comparison vs BatchComparison (ie - comparison internally within the batch with differentiation between Investors and Portfolio)
# Otherwise allows for differentiation within a Batch for Investors vs Portfolios

# For Swiss Comparison ONLY
if (ComparisonFile ==  "Swiss"){
  EQComparisonBatchTest <- subset(EQComparisonBatchTest, EQComparisonBatchTest$Type == "Brand")
  CBComparisonBatchTest <- subset(CBComparisonBatchTest, CBComparisonBatchTest$Type == "Investor")
}

if (ComparisonFile %in%  c("FundComparison","Swiss")){
  EQComparisonResults <- company_comparison("EQ",EQComparisonBatchTest, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  EQComparisonExposures <- EQComparisonResults[[2]]
  EQComparisonAUMs <- EQComparisonResults[[4]]
  EQComparisonCoverageWeights <- CoverageWeight_data("EQ","Brand",EQComparisonPortSS, EQComparisonBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
  EQWMCoverageWeight <- EQComparisonCoverageWeights[EQComparisonCoverageWeights$PortName %in% "WeightedResults",]
  
  CBComparisonResults <- company_comparison("CB",CBComparisonBatchTest, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  CBComparisonExposures <- CBComparisonResults[[2]]
  CBComparisonAUMs <- CBComparisonResults[[4]]
  CBComparisonCoverageWeights <- CoverageWeight_data("CB","Investor",CBComparisonPortSS, CBComparisonBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
  CBWMCoverageWeight <- CBComparisonCoverageWeights[CBComparisonCoverageWeights$PortName %in% "WeightedResults",]
  
  EQBatchResults <- company_comparison("EQ",EQBatchTest, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  EQExposures <- EQBatchResults[[2]]
  EQAUMs <- EQBatchResults[[4]]
  EQCoverageWeights <- CoverageWeight_data("EQ","Investor",EQBatchTest_PortSnapshots, EQBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
  
  CBBatchResults <- company_comparison("CB",CBBatchTest, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  CBExposures <- CBBatchResults[[2]]
  CBAUMs <- CBBatchResults[[4]]
  CBCoverageWeights <- CoverageWeight_data("CB","Investor",CBBatchTest_PortSnapshots, CBBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
}else{
  PortfolioList <- TestList$PortName[TestList$PortfolioType %in% c("Investor","Portfolio")]
  InvestorList <- TestList$PortName[TestList$PortfolioType %in% c("Investor","InvestorMPs")]
  EQPortfolioResultsRaw <- EQBatchTest[EQBatchTest$PortName %in% PortfolioList,]
  EQInvestorResultsRaw <- EQBatchTest[EQBatchTest$PortName %in% InvestorList,]
  EQInvestorResultsRaw <- EQBatchTest[EQBatchTest$Type %in% "Investor",]
  
  EQPortfolioResults <- company_comparison("EQ",EQPortfolioResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  EQPortfolioRanks <- EQPortfolioResults[[1]]
  EQPortfolioExposures <- EQPortfolioResults[[2]]
  EQPortfolioAUMs <- EQPortfolioResults[[4]]
  
  EQInvestorResults <- company_comparison("EQ",EQInvestorResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  EQInvestorRanks <- EQInvestorResults[[1]]
  EQInvestorExposures <- EQInvestorResults[[2]]
  EQInvestorAUMs <- EQInvestorResults[[4]]
  
  EQPortfolioCoverageWeights <- CoverageWeight_data("EQ","Portfolio",EQBatchTest_PortSnapshots, EQBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
  EQInvestorCoverageWeights <- CoverageWeight_data("EQ","Investor",EQBatchTest_PortSnapshots, EQBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
  EQWMPortfolioCoverageWeight <- EQPortfolioCoverageWeights[EQPortfolioCoverageWeights$PortName %in% "WeightedResults",]
  EQWMInvestorCoverageWeight <- EQInvestorCoverageWeights[EQInvestorCoverageWeights$PortName %in% "WeightedResults",]
  
  if(nrow(CBPortSnapshot)>0){
    CBPortfolioResultsRaw <- CBBatchTest[CBBatchTest$PortName %in% PortfolioList,]
    CBInvestorResultsRaw <- CBBatchTest[CBBatchTest$PortName %in% InvestorList,]
    CBInvestorResultsRaw <- CBBatchTest[CBBatchTest$Type == "Investor",]
    
    CBPortfolioResults <- company_comparison("CB",CBPortfolioResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
    CBPortfolioRanks <- CBPortfolioResults[[1]]
    CBPortfolioExposures <- CBPortfolioResults[[2]]
    CBPortfolioAUMs <- CBPortfolioResults[[4]]
    
    CBInvestorResults <- company_comparison("CB",CBInvestorResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
    CBInvestorRanks <- CBInvestorResults[[1]]
    CBInvestorExposures <- CBInvestorResults[[2]]
    CBInvestorAUMs <- CBInvestorResults[[4]]
    
    CBInvestorCoverageWeights <- CoverageWeight_data("CB","Investor",CBBatchTest_PortSnapshots, CBBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
    CBPortfolioCoverageWeights <- CoverageWeight_data("CB","Portfolio",CBBatchTest_PortSnapshots, CBBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
    CBWMPortfolioCoverageWeight <- CBPortfolioCoverageWeights[CBPortfolioCoverageWeights$PortName %in% "WeightedResults",]  
    CBWMInvestorCoverageWeight <- CBInvestorCoverageWeights[CBInvestorCoverageWeights$PortName %in% "WeightedResults",]  }
}


# ------
# Graph Inputs
# ---------
SetGraphInputs()
figuredirectory <- paste0(GIT.PATH,"Templates/ReportGraphics/Icons/")

ResultsLocFolder <- ResultsLocation

# ------
# Translation and Report Inputs
# ---------
template <- (readLines(paste0(GIT.PATH,"Templates/",ReportTemplate,".tex"),encoding="UTF-8"))

GraphTranslation <- read.csv(paste0(TEMPLATE.PATH,"/GraphTranslation_V4.csv"), stringsAsFactors = FALSE)
ReportTranslation <- read.csv(paste0(TEMPLATE.PATH,"/GeneralReportTranslation_V1.csv"), stringsAsFactors = FALSE)
if (length(grep("Swiss",ReportTemplate))==1){ReportTranslation <- read.csv(paste0(TEMPLATE.PATH,"/SwissReportTranslation_V12.csv"), stringsAsFactors = FALSE)}

GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)

#--------
# Meta Analysis Charts
#--------

b<- c("Pensionskassen")
ToTest2 <- which(TestList$PortfolioName %in% b)

#-------
# Loop through Portfolios
#--------
for (i in 1:nrow(TestList)){
  # ------
  # Setting Directory and Getting Results
  # ------
  PortfolioNameLong <- TestList[i,"PortfolioNameLong"]
  TestType <- TestList[i,"PortfolioType"]
  InvestorNameLong <-  TestList[i,"InvestorNameLong"]
  InvestorName <-  TestList[i,"InvestorName"]
  PortfolioName <- TestList[i,"PortfolioName"]
  PortName <- TestList[i,"PortName"]
  if(TestType %in% c("Investor","InvestorMPs")){
    ReportName <- PortfolioNameLong
    PortName <- InvestorName
    # ReportName <- InvestorNameLong
  }else{
    ReportName <- paste0(InvestorNameLong,": ", PortfolioNameLong)
  }
  
  if (PortName == "AggregiertesPortfolio_AggregiertesPortfolio"){PortName <- "AggregiertesPortfolio"}
  if (PortName == "Helvetia_Helvetia"){PortName <- "Helvetia"}
  
  print(paste0(PortfolioNameLong, "; ",InvestorNameLong,"; ",i))
  
  InvestorDirectory <- paste0(ResultsLocFolder,InvestorName,"/")  
  PortfolioDirectory <- paste0(InvestorDirectory,PortfolioName,"/")
  RegionDirectory <-PortfolioDirectory
  
  #Definitely need to check for these
  if(!dir.exists(file.path(InvestorDirectory))){dir.create(file.path(InvestorDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
  if(!dir.exists(file.path(PortfolioDirectory))){dir.create(file.path(PortfolioDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  
  setwd(RegionDirectory)
  
  #--------Load Inputs-----------
  PortData <- PortfolioBreakdown[PortfolioBreakdown$InvestorName %in% InvestorName & PortfolioBreakdown$PortfolioName %in% PortfolioName & PortfolioBreakdown$HoldingType == "All",]
  
  # Batch Results
  EQCombin <- EQBatchTest[EQBatchTest$PortName == PortName,]
  EQPortSnapshot <- EQBatchTest_PortSnapshots[EQBatchTest_PortSnapshots$PortName == PortName,]
  EQCompProdSnapshot <- EQCompProdSnapshots[EQCompProdSnapshots$PortName == PortName,]
  CBCombin <- CBBatchTest[CBBatchTest$PortName == PortName,]
  CBPortSnapshot <- CBBatchTest_PortSnapshots[CBBatchTest_PortSnapshots$PortName == PortName,]
  CBCompProdSnapshot <- CBCompProdSnapshots[CBCompProdSnapshots$PortName == PortName,]
  
  
  if (ComparisonFile %in% c("Swiss","FundComparison")){
    # Comparative Results
    EQExposure <- EQExposures[EQExposures$PortName %in% PortName,]
    CBExposure <- CBExposures[CBExposures$PortName %in% PortName,]  
    EQAUMData <- EQAUMs[EQAUMs$PortName %in% PortName,]
    CBAUMData <- CBAUMs[CBAUMs$PortName %in% PortName,]  
    
    EQCoverageWeight <- EQCoverageWeights[EQCoverageWeights$PortName == PortName,]
    if (CBCoverageWeights != "NoResults"){
      CBCoverageWeight <- CBCoverageWeights[CBCoverageWeights$PortName == PortName,]
    }else{CBCoverageWeight <- "NoResults"}
    
    # EQComparisonExposures <- subset(EQComparisonExposures, EQComparisonExposures$PortName != "Helvetia")
    # CBComparisonExposures <- subset(CBComparisonExposures, CBComparisonExposures$PortName != "Helvetia")
    if (PortName %in% EQComparisonExposures$PortName){
      EQComparisonExposures <- EQComparisonExposures[which(EQComparisonExposures$PortName != PortName),]
    }
    
    if (PortName %in% CBComparisonExposures$PortName){
      CBComparisonExposures <- CBComparisonExposures[which(CBComparisonExposures$PortName != PortName),]
    }
    
    EQRanks <- RankPortfolios(EQComparisonExposures, EQExposure, PortName)
    CBRanks <- RankPortfolios(CBComparisonExposures, CBExposure, PortName)
    
    EQExposureRange <- rbind(EQExposure,EQComparisonExposures) 
    CBExposureRange <- rbind(CBExposure,CBComparisonExposures) 
    EQAUMDatarange <- rbind(EQAUMData,EQComparisonAUMs)
    CBAUMDatarange <- rbind(CBAUMData,CBComparisonAUMs)
    
  }else{
    
    if(TestType == "Portfolio"){
      EQRanks <- EQPortfolioRanks
      EQExposures <- EQPortfolioExposures
      EQAUMData <- EQPortfolioAUMs
      EQWMCoverageWeight <- EQWMPortfolioCoverageWeight
      EQCoverageWeight <- EQPortfolioCoverageWeights[EQPortfolioCoverageWeights$PortName %in% PortfolioNameLong,]  
      
      if (nrow(CBPortSnapshot) >0 ){
        CBRanks <- CBPortfolioRanks
        CBExposures <- CBPortfolioExposures
        CBAUMData <- CBPortfolioAUMs
        CBWMCoverageWeight <- CBWMPortfolioCoverageWeight
        CBCoverageWeight <- CBPortfolioCoverageWeights[CBPortfolioCoverageWeights$PortName %in% PortfolioNameLong,]}
    }else{
      EQRanks <- EQInvestorRanks
      EQExposures <- EQInvestorExposures
      EQAUMData <- EQInvestorAUMs
      EQWMCoverageWeight <- EQWMInvestorCoverageWeight
      EQCoverageWeight <- EQInvestorCoverageWeights[EQInvestorCoverageWeights$PortName %in% PortfolioNameLong,]
      
      if (nrow(CBPortSnapshot) >0 ){
        CBRanks <- CBInvestorRanks
        CBExposures <- CBInvestorExposures
        CBAUMData <- CBInvestorAUMs
        CBWMCoverageWeight <- CBWMInvestorCoverageWeight
        CBCoverageWeight <- CBInvestorCoverageWeights[CBInvestorCoverageWeights$PortName %in% PortfolioNameLong,]}
    }
    
    EQExposureRange <-EQExposures
    CBExposureRange <- CBExposures
    EQAUMDatarange <- EQAUMData
    CBAUMDatarange <- CBAUMData
  }
  
  # Fund Results
  FundsInPort <- Portfunds(20,FundList,FundsDataAll, PortfolioName,InvestorName, TestType)
  if (typeof(FundsInPort)=="list"){
    if (nrow(FundsInPort) == 0){FundsInPort = "NoFunds"}  }
  
  # Sectors with Production
  # Used to check whether a Line Graph, Ranking, and BarChart should be printed
  EQSectorProd <- SectorProduction(EQCombin,"EQ")
  CBSectorProd <- SectorProduction(CBCombin,"CB")
  
  # PortfolioNameLong <<- "PORTEFEUILLE MOYEN"  # I don't think this works. 
  
  #------ Specify Language and Load Report ------ 
  Languagechoose =  ParameterFile$Languageselect
  Languagechoose <- "EN"
  GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
  RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)
  
  LanguageDirectory <- paste0(RegionDirectory,Languagechoose,"/")
  if(!dir.exists(file.path(LanguageDirectory))){dir.create(file.path(LanguageDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  setwd(LanguageDirectory)
  
  if (nrow(EQCombin)+nrow(CBCombin) >0){ 
    tryCatch({
      
      # -------
      # Swiss Charts
      # -------
      # Page 4
      plot_0 <- port_pie("00", PortData)
      # 
      plot_1 <- pie_chart("01","EQ",EQPortSnapshot, PortfolioName, CompanyDomicileRegionchoose)
      plot_2 <- pie_chart("02","CB",CBPortSnapshot, PortfolioName, CompanyDomicileRegionchoose)
      
      # Page 8
      if (SectorPrint("Power",EQSectorProd)==1){
        plot_3 <- stacked_bar_chart("03","EQ",EQCombin,EQWMCoverageWeight,"Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortfolioName)
        plot_4 <- mini_line_chart("04","EQ",EQCombin,"RenewablesCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_5 <- mini_line_chart("05","EQ",EQCombin,"CoalCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_6 <- mini_line_chart("06","EQ",EQCombin,"GasCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_7 <- ranking_chart_alignment("07","EQ",Startyear,"Power", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)
        # plot_7 <- ranking_chart_alignment("07","EQ",Startyear,"Power", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,InvestorName)
      }
      
      # Page 9
      if (SectorPrint("Power",CBSectorProd)==1){
        plot_08 <- stacked_bar_chart("08","CB",CBCombin,CBWMCoverageWeight,"Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortfolioName)
        plot_09 <- mini_line_chart("09","CB",CBCombin,"RenewablesCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_10 <- mini_line_chart(10,"CB", CBCombin,"CoalCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_11 <- mini_line_chart(11,"CB",CBCombin,"GasCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_12 <- ranking_chart_alignment(12,"CB",Startyear,"Power", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)
      }
      # Page 10
      if (SectorPrint("Automotive",EQSectorProd)==1){
        plot_13 <- stacked_bar_chart(13,"EQ",EQCombin,EQWMCoverageWeight,"Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortfolioName)
        plot_14 <- mini_line_chart(14,"EQ",EQCombin,"ICE","Automotive",BenchmarkRegionchoose,  CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_15 <- mini_line_chart(15,"EQ",EQCombin,"Electric","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_16 <- mini_line_chart(16,"EQ",EQCombin,"Hybrid","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_17 <- ranking_chart_alignment(17,"EQ",Startyear,"Automotive",EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)
        # plot_17 <- ranking_chart_alignment(17,"EQ",Startyear,"Automotive",EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,InvestorName)
      }
      # Page 11
      if (SectorPrint("Automotive",CBSectorProd)==1){
        plot_18 <- stacked_bar_chart(18,"CB",CBCombin,CBWMCoverageWeight,"Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortfolioName)
        plot_19 <- mini_line_chart(19,"CB",CBCombin,"ICE","Automotive",BenchmarkRegionchoose,  CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_20 <- mini_line_chart(20,"CB",CBCombin,"Electric","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_21 <- mini_line_chart(21,"CB",CBCombin,"Hybrid","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_22 <- ranking_chart_alignment(22,"CB",Startyear,"Automotive", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)
      }
      # Page 12
      plot_23 <- stacked_bar_chart(23,"EQ",EQCombin,EQWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortfolioName)
      plot_24 <- mini_line_chart(24,"EQ",EQCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_25 <- mini_line_chart(25,"EQ",EQCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_26 <- mini_line_chart(26,"EQ",EQCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_27 <- ranking_chart_alignment(27,"EQ",Startyear,"Fossil Fuels", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)
      # plot_27 <- ranking_chart_alignment(27,"EQ",Startyear,"Fossil Fuels", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,InvestorName)
      
      # Page 13
      if (nrow(CBPortSnapshot)>0){
        plot_28 <- stacked_bar_chart(28,"CB",CBCombin,CBWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortfolioName)
        plot_29 <- mini_line_chart(29,"CB",CBCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_30 <- mini_line_chart(30,"CB",CBCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_31 <- mini_line_chart(31,"CB",CBCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_32 <- ranking_chart_alignment(32,"CB",Startyear,"Fossil Fuels", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)}
      
      # Page 14
      plot_33 <- ranking_chart_alignment(33,"EQ",Startyear,"All", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)
      # plot_33 <- ranking_chart_alignment(33,"EQ",Startyear,"All", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,InvestorName)
      
      # Page 15
      if (nrow(CBPortSnapshot)>0){
        plot_34 <- ranking_chart_alignment(34,"CB",Startyear,"All", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)
        }
      
      # Page 16
      plot_35 <- fundmap_chart(35,FundsInPort, Startyear, Scenariochoose, PortfolioName)
      
      # Page 17
      plot_36 <- other_sector_chart(36, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Cement",PortName)
      plot_37 <- other_sector_chart(37, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Steel",PortName)
      plot_38 <- other_sector_chart(38, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Aviation",PortName)
      plot_39 <- shipping_chart(39, EQPortSnapshot,CBPortSnapshot,ShippingData, SectorToPlot="Shipping",PortfolioName)
      OtherSectors <- data.frame("Cement"=plot_36,"Steel"=plot_37,"Aviation"=plot_38,"Shipping"=plot_39)
      
      # Page 21
      if(nrow(EQCombin)==0){
        plot_43 <- flat_wheel_chart(43,10,"CB",CBPortSnapshot,CBCombin, UtilityCompanies,SectorToPlot = "Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }else{
        plot_43 <- flat_wheel_chart(43,10,"EQ",EQPortSnapshot,EQCombin, UtilityCompanies,SectorToPlot = "Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }
      
      # Page 22
      if(nrow(EQCombin)==0){
        plot_44 <- flat_wheel_chart(44,10,"CB",CBPortSnapshot,CBCombin, AutoCompanies,SectorToPlot = "Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }else{
        plot_44 <- flat_wheel_chart(44,10,"EQ",EQPortSnapshot,EQCombin, AutoCompanies,SectorToPlot = "Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }
      
      if (nrow(EQCombin)==0){
        plot_46 <- flat_wheel_chart(46,20,"CB",CBPortSnapshot,CBCompProdSnapshot, OGCarbonBudget,SectorToPlot = "OG",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }else{
        plot_46 <- flat_wheel_chart(46,20,"EQ",EQPortSnapshot,EQCompProdSnapshot, OGCarbonBudget,SectorToPlot = "OG",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortfolioName)
      }
      
      # Page 24
      if(nrow(EQCombin)!=0){
        plot_51 <- renewablesadditions_chart(51,"EQ",EQCombin, EQPortSnapshot, Scenariochoose, MasterData, AllIEATargets, RegionCountries,PortfolioName)
      }
      
      figurelist <- list.files(getwd(),pattern=c("\\.png$"), full.names = FALSE)
      writeLines(figurelist,"FigureList.txt")
      
      # -------
      # PDF CREATION
      # -------
      EQReportData<-report_data("EQ",EQCombin, EQExposureRange,EQAUMDatarange,EQRanks,EQPortSnapshot,CompanyDomicileRegionchoose, BenchmarkRegionchoose,Scenariochoose, Startyear,PortfolioName)
      CBReportData<-report_data("CB",CBCombin, CBExposureRange,CBAUMDatarange,CBRanks,CBPortSnapshot,CompanyDomicileRegionchoose, BenchmarkRegionchoose,Scenariochoose, Startyear,PortfolioName)
      
      report(PortfolioName,ReportName, InvestorName, template, RT,EQReportData,CBReportData,FundsInPort,OtherSectors,EQSectorProd,CBSectorProd,Languagechoose)
    })}else{
      print (paste0(PortfolioNameLong," has no Equity and Bond Data"))
    }
  
  write.csv(EQCompProdSnapshot, paste0("EQCompProdSnapshot_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  write.csv(EQPortSnapshot,paste0("EQPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  write.csv(CBCompProdSnapshot,paste0("CBCompProdSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
}

