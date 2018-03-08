#2DP Graph Creater
# units of all data in units of cars, MW power capacity, BBL oil, CF gas, and tons coal

#Version Control
# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 05 - 12 |        CM        |         1            | 

# ------
# Load packages
# ------


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
TEMPLATE.PATH <- paste0(CODE.PATH,"03_ReportingCode/01_ReportTemplates/")
PROC.DATA.PATH <- paste0(DATA.PATH,"/01_ProcessedData/")
PROJ.PATH <- paste0(PORTS.PATH,ParameterFile$ProjektName,"/")
BATCH.PATH <- paste0(PROJ.PATH,BatchName,"/")
if(!dir.exists(file.path(BATCH.PATH))){dir.create(file.path(BATCH.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  

# ------
# Read in Results and Inputs
# ------- 
ImportNewComparisonList <- F

### Set Results Location
REPORT.PATH <- paste0(RESULTS.PATH,"05_Reports/",ProjectName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
REPORT.PATH <- paste0(RESULTS.PATH,"05_Reports/",ProjectName,"/",BatchName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
BATCH.RES.PATH <- paste0(RESULTS.PATH,"01_BatchResults/",BatchName,"/",BatchToTest,"/")

### Get Equity Batch Results
if (file.exists(paste0(BATCH.RES.PATH,BatchName,"/",BatchToTest,"/",BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"))){
  EQBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"),stringsAsFactors=FALSE,strip.white=TRUE)
}else{
  EQBatchTest <- read.csv(paste(BATCH.RES.PATH,BatchName,"_EquityAnalysisResults-450S-only.csv",sep=""),stringsAsFactors=FALSE,strip.white = T)
}
EQBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_PortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE,strip.white = T)
EQCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_CompanysProduction_Snapshot.csv"),stringsAsFactors = FALSE,strip.white = T)

### Get Debt Batch Results
CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors=FALSE,strip.white = T)
CBBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE,strip.white = T)
CBCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtProductionCompanies_Snapshot2022.csv"),stringsAsFactors = FALSE,strip.white = T)

### External Data Read In
setwd(PROC.DATA.PATH)
Index <- read.csv("2017-07-20_IndexAnalysis5_EquityAnalysisResults.csv",stringsAsFactors = FALSE)
AllIEATargets <- read.csv("IEATargets2016_AllRegions.csv", stringsAsFactors=FALSE)
AllCompanyData <- read.csv("CompanyLevelData_2017-08-21.csv",stringsAsFactors = FALSE)
BBGData_CompanyLevel <- read.csv(paste0("CompanylvlBBGData",ParameterFile$DateofFinancialData,".csv"),stringsAsFactors = FALSE)
MasterData <- read.csv(paste0("MasterData",ParameterFile$DateofFinancialData,".csv"))
OGData <- read.csv(paste0("01_SectorMasters/",ParameterFile$GDYearMonth,"/OGMaster_",ParameterFile$GDYearMonth,".csv"),stringsAsFactors = FALSE)
BenchmarkRegionList <- read.csv("BenchRegions.csv")
IndexUniverses <- read.csv("IndexRegions.csv")
OGCarbonBudget <- read.csv(paste0(DATA.PATH,"/04_Other/CarbonCapexUpstream.csv"),stringsAsFactors = FALSE)

### Batch related Portfolio & Fund-Data Results
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

### Funds within the Portfolios
FundList <- read.csv(paste0(PORTS.PATH,ProjectName,"/",BatchName,"/",BatchName,"Port_ListofFunds.csv"),stringsAsFactors = FALSE)
if (exists("FundList")){
  FundList$InvestorName <- gsub(".","",FundList$InvestorName, fixed = TRUE)
  FundList$PortfolioName<- gsub(".","",FundList$PortfolioName, fixed = TRUE)
}else{
  FundList <- "No Funds"
}

### Remove Listed Market if it's there
EQBatchTest <- EQBatchTest[EQBatchTest$InvestorName != c("MetaPortfolio"),]    #"ListedMarket"
CBBatchTest <- CBBatchTest[CBBatchTest$InvestorName != c("MetaPortfolio"),]     #"ListedMarket", 

### Add Company Names to BatchTest_PortSnapshot -  should be superceded with changes to EQY Code
if (!"Name" %in% colnames(EQBatchTest_PortSnapshots)){
  CompanyNames <- unique(subset(AllCompanyData, select=c("EQY_FUND_TICKER","Name")))
  EQBatchTest_PortSnapshots <- merge(CompanyNames, EQBatchTest_PortSnapshots, by = "EQY_FUND_TICKER", all.y = TRUE)}

### Funds Heat Map Data
### This should be updated
FundsDataAll <- read.csv(paste0(RESULTS.PATH,"/01_BatchResults/Swiss_FundData/2016Q4/Swiss_FundData_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"), stringsAsFactors = FALSE)

### Add Comparative Portfolios 
### Add a code to update the Index results
if (ComparisonFile %in% c("FundComparison","Swiss")){
  COMPARISON.PATH <- paste0(RESULTS.PATH,"/01_BatchResults/",ComparisonFile,"/2016Q4/")
  
  if (ImportNewComparisonList == TRUE){
    
    EQComparisonBatchTestLong <- read.csv(paste0(COMPARISON.PATH,ComparisonFile,"_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"),stringsAsFactors = FALSE)
    EQComparisonPortSSLong <- read.csv(paste0(COMPARISON.PATH,ComparisonFile,"_PortfolioData_Snapshot2017.csv"),stringsAsFactors = FALSE)
    CBComparisonBatchTestLong <- read.csv(paste0(COMPARISON.PATH,ComparisonFile,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors = FALSE)
    CBComparisonPortSSLong <- read.csv(paste0(COMPARISON.PATH,ComparisonFile,"_DebtPortfolioData_Snapshot",Startyear,".csv"),stringsAsFactors = FALSE)
    
    comparisonNumber <- 100
    if (ComparisonFile == "Swiss"){comparisonNumber <- length(unique(EQComparisonBatchTestLong$PortName))}
    
    ComparisonList <- comparisonlist(comparisonNumber, EQComparisonBatchTestLong,CBComparisonBatchTestLong)
    
    EQComparisonBatchTest <- EQComparisonBatchTestLong[EQComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    EQComparisonPortSS <- EQComparisonPortSSLong[EQComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="EQ"],] 
    CBComparisonBatchTest <- CBComparisonBatchTestLong[CBComparisonBatchTestLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    CBComparisonPortSS <-CBComparisonPortSSLong[CBComparisonPortSSLong$PortName %in% ComparisonList$PortName[ComparisonList$Type =="CB"],] 
    
    write.csv(EQComparisonBatchTest,paste0(COMPARISON.PATH, "EQComparisonBatchTest.csv"),row.names = F)
    write.csv(EQComparisonPortSS,paste0(COMPARISON.PATH, "EQComparisonPortSS.csv"),row.names = F)
    write.csv(CBComparisonBatchTest,paste0(COMPARISON.PATH, "CBComparisonBatchTest.csv"),row.names = F)
    write.csv(CBComparisonPortSS,paste0(COMPARISON.PATH, "CBComparisonPortSS.csv"),row.names = F)
    
  }else{
    EQComparisonBatchTest<- read.csv(paste0(COMPARISON.PATH,"EQComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    EQComparisonPortSS <- read.csv(paste0(COMPARISON.PATH,"EQComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonBatchTest<- read.csv(paste0(COMPARISON.PATH,"CBComparisonBatchTest.csv"),strip.white = T,stringsAsFactors = F)
    CBComparisonPortSS <- read.csv(paste0(COMPARISON.PATH,"CBComparisonPortSS.csv"),strip.white = T,stringsAsFactors = F)
  }
}


### Hangover from the Fund/Brand 
### If true - changes Fund to Portfolio; Brand to Investor
EQComparisonBatchTest <- ChangeFundPort(EQComparisonBatchTest)
CBComparisonBatchTest <- ChangeFundPort(CBComparisonBatchTest)
EQComparisonPortSS <- ChangeFundPort(EQComparisonPortSS)
CBComparisonPortSS <- ChangeFundPort(CBComparisonPortSS)

# ------
# Bench Regions and Indicies and Sector Classifications
# ------
### Get Bench Regions
RegionCountries <- data.frame(BenchmarkRegionList$Global, BenchmarkRegionList$OECD)
RegionCountries <- rename(RegionCountries, c("BenchmarkRegionList.Global"="Global","BenchmarkRegionList.OECD"="OECD"))

IndexUniverses[is.na(IndexUniverses)] <- ""
IndexUniversesList <- data.frame(IndexUniverse = IndexUniverses$IndexUniverse[!is.na(IndexUniverses$IndexUniverse) & IndexUniverses$IndexUniverse != ""], IndexUniversesColname = IndexUniverses$IndexUniverseColname[!is.na(IndexUniverses$IndexUniverseColname) & IndexUniverses$IndexUniverseColname != ""])

### Select the Regional and Index Benchmarks
if (Indexchoose == "MSCIWorld"){Indexchoose <- "MSCIWorld_MSCI"}
IndexData <- subset(Index, BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & PortName == Indexchoose) 
if (BenchmarkRegionchoose != "Global"){
  IndexAutoGlobal <- subset(Index, BenchmarkRegion == "Global" & Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & PortName ==Indexchoose & Sector == "Automotive")
  IndexData <- rbind(IndexData,IndexAutoGlobal)
}
if (Indexchoose == "MSCIWorld_MSCI"){Indexchoose <- "MSCIWorld"}

IEATargetsAll <- subset(AllIEATargets, BenchmarkRegion == "Global" &Year %in% c(Startyear, Startyear+5, Startyear+10,Startyear+15)  & Scenario == Scenariochoose, select = c("Sector","Technology","AnnualvalIEAtech","Year")) 
IEATargetsAll <- IEATargetsAll[!IEATargetsAll$Technology %in% "OilCap",]

IEATargets246 <- subset(AllIEATargets, BenchmarkRegion == "Global" &Year %in% Startyear:(Startyear+5)  & Scenario %in% c("450S","NPS","CPS"), select = c("Sector","Technology","Scenario","Year","AnnualvalIEAtech")) 


### Bind Sector Classification from BBG - ICB Subsector Name
CleanedBBGData <- cleanBBGData(BBGData_CompanyLevel,AllCompanyData,Startyear,Scenariochoose,CompanyDomicileRegionchoose,BenchmarkRegionchoose)
Companies <- CleanedBBGData[[1]]
UtilityCompanies <- CleanedBBGData[[2]]
AutoCompanies <- CleanedBBGData[[3]]
OilData <- cleanOGData(OGData,AllCompanyData,Startyear)

### Other Sector Data
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


###### NEW SECTION

# Define this is the parameter file. CompareBatch or Name of File to compare to.  
ComparisonType <- ComparisonFile

# EQBatchTest is read in - if you want additional files to compare to, these are then bound to the original results
EQBatchTest$ComparisonType <- "BatchResults"
CBBatchTest$ComparisonType <- "BatchResults"

# If there is a comparison file (ie the results are not being compared to themselves, the results are bound here)
if (ComparisonType != "CompareBatch"){
  
  if (nrow(EQBatchTest) >0){
    EQComparisonBatchTest$ComparisonType <- "ComparisonResults"
    EQBatchTest <-AddMissingColumns(EQBatchTest,EQComparisonBatchTest)
    EQComparisonBatchTest <-AddMissingColumns(EQComparisonBatchTest,EQBatchTest)
    EQBatchTest <- rbind(EQBatchTest, EQComparisonBatchTest)
    
    EQBatchTest_PortSnapshots <- rbind(EQBatchTest_PortSnapshots,EQComparisonPortSS)
  }
}

if (ComparisonType != "CompareBatch"){
  if (nrow(CBBatchTest) >0){
    CBComparisonBatchTest$ComparisonType <- "ComparisonResults"
    CBBatchTest <-AddMissingColumns(CBBatchTest,CBComparisonBatchTest)
    CBComparisonBatchTest <-AddMissingColumns(CBComparisonBatchTest,CBBatchTest)
    CBBatchTest <- rbind(CBBatchTest, CBComparisonBatchTest)
    
    CBBatchTest_PortSnapshots <-AddMissingColumns(CBBatchTest_PortSnapshots,CBComparisonPortSS)
    CBComparisonPortSS <-AddMissingColumns(CBComparisonPortSS,CBBatchTest_PortSnapshots)
    CBBatchTest_PortSnapshots <- rbind(CBBatchTest_PortSnapshots,CBComparisonPortSS)
  } 
}


### Calculates Exposure, AUM, Coverage Weight and Weighted Mean of Coverage Weight 

if (nrow(EQBatchTest) >0){
  EQResults <- company.comparison("EQ",EQBatchTest,EQBatchTest_PortSnapshots, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  EQExposures <- EQResults[[1]]
  EQAUMs <- EQResults[[2]]
  EQCoverageWeights <- EQResults[[3]]
  EQWMCoverageWeights <- EQResults[[4]]
}

if (nrow(CBBatchTest) >0){
  CBResults <- company.comparison("CB",CBBatchTest, BatchTest_PortSnapshots,Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  CBExposures <- CBResults[[1]]
  CBAUMs <- CBResults[[2]]
  CBCoverageWeights <- CBResults[[3]]
  CBWMCoverageWeights <- CBResults[[4]]
}

###### END NEW SECTION


# ------
# Graph Inputs
# ---------
SetGraphInputs()
figuredirectory <- paste0(GIT.PATH,"Templates/ReportGraphics/Icons/")

# ResultsLocFolder <- ResultsLocation

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

# b<- c("Pensionskassen")
# ToTest2 <- which(TestList$PortfolioName %in% b)

#-------
# Loop through Portfolios
#--------
for (i in 1:nrow(TestList)){
  
  ### Specify the Names from the Test List
  PortfolioNameLong <- TestList[i,"PortfolioNameLong"]
  TestType <- TestList[i,"PortfolioType"]
  InvestorNameLong <-  TestList[i,"InvestorNameLong"]
  InvestorName <-  TestList[i,"InvestorName"]
  PortfolioName <- TestList[i,"PortfolioName"]
  PortName <- TestList[i,"PortName"]
  if(TestType %in% c("Investor","InvestorMPs")){
    ReportName <- PortfolioNameLong
    TestType <- "Investor"
    PortName <-  InvestorName
    # # if there is only one Portfolio
    # if (PortfolioName == InvestorName){
    #   PortName <-  InvestorName
    # }    
  }else{
    ReportName <- paste0(InvestorNameLong,": ", PortfolioNameLong)
  }
  
  TestType <- "Portfolio"
  
  print(paste0(PortfolioNameLong, "; ",InvestorNameLong,"; ",i, " of ",nrow(TestList)))
  
  ### Loads Summary Inputs
  PortData <- PortfolioBreakdown[PortfolioBreakdown$InvestorName %in% InvestorName & PortfolioBreakdown$PortfolioName %in% PortfolioName & PortfolioBreakdown$HoldingType == "All",]
  
  
  ### Inputs to print a Sample Template
  if (SamplePortfolio == TRUE){
    PortfolioName <- "SamplePortfolio"
    InvestorName <- "Sample Investor"
    ReportName <- "Sample Report"
    REPORT.PATH <- paste0(RESULTS.PATH,"05_Reports/06_Sample/")
  }  
  
  ### Creates working directory
  INVESTOR.PATH <- paste0(REPORT.PATH,InvestorName,"/")  
  PORTFOLIO.PATH <- paste0(INVESTOR.PATH,PortfolioName,"/")
  if(!dir.exists(file.path(INVESTOR.PATH))){dir.create(file.path(INVESTOR.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
  if(!dir.exists(file.path(PORTFOLIO.PATH))){dir.create(file.path(PORTFOLIO.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}


  
  ### Subsets results for this portfolio
  EQCombin <- EQBatchTest[EQBatchTest$PortName == PortName & EQBatchTest$ComparisonType == "BatchResults",]
  EQPortSnapshot <- EQBatchTest_PortSnapshots[EQBatchTest_PortSnapshots$PortName == PortName,]
  EQCompProdSnapshot <- EQCompProdSnapshots[EQCompProdSnapshots$PortName == PortName,]
  CBCombin <- CBBatchTest[CBBatchTest$PortName == PortName & CBBatchTest$ComparisonType == "BatchResults",]
  CBPortSnapshot <- CBBatchTest_PortSnapshots[CBBatchTest_PortSnapshots$PortName == PortName,]
  CBCompProdSnapshot <- CBCompProdSnapshots[CBCompProdSnapshots$PortName == PortName,]
  
  
  
  ##### NEW SECTION
  ### Selects the current portfolio from the Comparative Results
  EQComparisonExposures <- EQExposures[which(EQExposures$Type == TestType & EQExposures$ComparisonType == "ComparisonResults" & EQExposures$PortName != PortName),]
  EQComparisonAUMs <- EQAUMs[which(EQAUMs$ComparisonType == "ComparisonResults"),]
  EQWMCoverageWeight <- EQWMCoverageWeights[which(EQWMCoverageWeights$Type == TestType),]
  
  if (EQCoverageWeights != "NoResults"){
    EQExposure <- EQExposures[which(EQExposures$PortName == PortName & EQExposures$ComparisonType == "BatchResults"),]
    EQAUMData <- EQAUMs[which(EQAUMs$PortName == PortName & EQAUMs$ComparisonType == "BatchResults"),]
    EQCoverageWeight <- EQCoverageWeights[which(EQCoverageWeights$PortName == PortName & EQCoverageWeights$ComparisonType == "BatchResults"),]
    EQRanks <- RankPortfolios(EQComparisonExposures, EQExposure, PortName)
    EQExposureRange <- rbind(EQExposure,EQComparisonExposures) 
    EQAUMDatarange <- rbind(EQAUMData,EQComparisonAUMs)
  }
  
  CBComparisonExposures <- CBExposures[which(CBExposures$Type == TestType & CBExposures$ComparisonType == "ComparisonResults" & CBExposures$PortName != PortName),]
  CBComparisonAUMs <- CBAUMs[which(CBAUMs$ComparisonType == "ComparisonResults"),]
  CBWMCoverageWeight <- CBWMCoverageWeights[which(CBWMCoverageWeights$Type == TestType),]
  ### Could add additional filter in at this point. 
  
  if (CBCoverageWeights != "NoResults"){  
    CBExposure <- CBExposures[CBExposures$PortName == PortName & CBExposures$ComparisonType == "BatchResults",]  
    CBAUMData <- CBAUMs[which(CBAUMs$PortName == PortName & CBAUMs$ComparisonType == "BatchResults"),] 
    CBCoverageWeight <- CBCoverageWeights[which(CBCoverageWeights$PortName == PortName & CBCoverageWeights$ComparisonType == "BatchResults"),]
    CBRanks <- RankPortfolios(CBComparisonExposures, CBExposure, PortName)
    CBExposureRange <- rbind(CBExposure,CBComparisonExposures) 
    CBAUMDatarange <- rbind(CBAUMData,CBComparisonAUMs) 
  } 
  
  ##### NEW SECTION END  

  
  ### Fund Results
  ### Choose the top 20 funds in the results to present
  FundsInPort <- Portfunds(20,FundList,FundsDataAll, PortfolioName,InvestorName, TestType)
  if (typeof(FundsInPort) == "list"){ if (nrow(FundsInPort) == 0){FundsInPort = "NoFunds"}  }
  
  FundsHeatMapData <- heatmap_data(FundsInPort,NULL,"Funds","")
  # Required Batch Results
  PortHeatMapData <- heatmap_data(EQCombin, CBCombin,"Port",PortName)
  ### Sectors with Production
  ### Used to check whether a Line Graph, Ranking, and BarChart should be printed
  EQSectorProd <- SectorProduction(EQCombin,"EQ")
  CBSectorProd <- SectorProduction(CBCombin,"CB")
  
  ### Specify Language and Load Report 
  Languagechoose <-  ParameterFile$Languageselect
  # Languagechoose <- "FR"
  GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
  RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)
  
  LANGUAGE.PATH <- paste0(PORTFOLIO.PATH,Languagechoose,"/")
  if(!dir.exists(file.path(LANGUAGE.PATH))){dir.create(file.path(LANGUAGE.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  setwd(LANGUAGE.PATH)
  
  ### Loops through graphs and report generation
  
  if (nrow(EQCombin)+nrow(CBCombin) >0){ 
    tryCatch({
      
      # Overall Heat Map
      
      # fundmap_chart("99",PortHeatMapData, Startyear, Scenariochoose, PortfolioName)
      
      # StackedBarProdData <-stacked_bar_chart_data ("EQ", EQCombin,EQWMCoverageWeight)
        
      # stacked_bar_chart_new(99,"Power",StackedBarProdData,"EQ")

      ########################
      ### GENERAL TEMPLATE ###
      ########################
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
      }
      # Page 11
      if (SectorPrint("Automotive",CBSectorProd)==1){
        plot_18 <- stacked_bar_chart(18,"CB",CBCombin,CBWMCoverageWeight,"Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortfolioName)
        plot_19 <- mini_line_chart(19,"CB",CBCombin,"ICE","Automotive",BenchmarkRegionchoose,  CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_20 <- mini_line_chart(20,"CB",CBCombin,"Electric","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_21 <- mini_line_chart(21,"CB",CBCombin,"Hybrid","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_22 <- ranking_chart_alignment(22,"CB",Startyear,"Automotive", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)}

      # Page 12
      plot_23 <- stacked_bar_chart(23,"EQ",EQCombin,EQWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortfolioName)
      plot_24 <- mini_line_chart(24,"EQ",EQCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_25 <- mini_line_chart(25,"EQ",EQCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_26 <- mini_line_chart(26,"EQ",EQCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
      plot_27 <- ranking_chart_alignment(27,"EQ",Startyear,"Fossil Fuels", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)

      # Page 13
      if (SectorPrint("Fossil Fuels",CBSectorProd)==1){
        plot_28 <- stacked_bar_chart(28,"CB",CBCombin,CBWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortfolioName)
        plot_29 <- mini_line_chart(29,"CB",CBCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_30 <- mini_line_chart(30,"CB",CBCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_31 <- mini_line_chart(31,"CB",CBCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_32 <- ranking_chart_alignment(32,"CB",Startyear,"Fossil Fuels", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)}

      # Page 14
      plot_33 <- ranking_chart_alignment(33,"EQ",Startyear,"All", EQExposureRange, EQAUMDatarange,EQRanks,figuredirectory,PortName)

      # Page 15
      if (nrow(CBPortSnapshot)>0){
        plot_34 <- ranking_chart_alignment(34,"CB",Startyear,"All", CBExposureRange, CBAUMDatarange,CBRanks,figuredirectory,PortName)
      }

      # Page 16
      plot_35 <- fundmap_chart(35,FundsHeatMapData, Startyear, Scenariochoose, PortfolioName)

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
  write.csv(CBPortSnapshot,paste0("CBPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
}

