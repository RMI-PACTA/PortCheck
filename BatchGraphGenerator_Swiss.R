#This project was funded by the European Commission through LIFE program under grant: LIFE16 GIC/FR/000061 - PACTA)
#2DP Graph Creater
# units of all data in units of cars, MW power capacity, BBL oil, CF gas, and tons coal

#Version Control
# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 05 - 12 |        CM        |         1            | 

# ------
# Load packages
# ------


rm(list=ls())

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


# ------
# Read in Results and Inputs
# ------- 
UserName <- sub("/.*","",sub(".*Users/","",getwd()))
ResultsLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/03_Results/")
DataLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/")
CodeLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/01_Code/03_ReportingCode/")
GitHubLocation <- paste0("C:/Users/",UserName,"/Documents/GitHub/")

source(paste0(GitHubLocation, "PortCheck/CodeFunctions.R"))

# Read in Parameter File
PortfolioDataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/")
ParameterFileName <- choose.files(default = paste0(PortfolioDataFolder,"99_ParameterFiles/","*_ParameterFile.csv"),caption = "Select a parameter file", multi = FALSE)
ParameterFileInfo <- file.info(ParameterFileName, extra_cols = TRUE)
ParameterFile <- read.csv(ParameterFileName, stringsAsFactors = FALSE, strip.white = TRUE)
BatchName <- ParameterFile$BatchName
Languagechoose <- ParameterFile$Language
BatchToTest <- ParameterFile$AssessmentDate
ComparisonFile <- ParameterFile$ComparisonFile                        # Defines whether the comparative graphs are to be produced

Startyear <- ParameterFile$Startyear                                  # Date when the analysis starts - time horizon is always 5 years starting from here: e.g. if Startyear is 2016 the analysis will go to 2021
CompanyDomicileRegionchoose <- ParameterFile$CompanyDomicileRegion    # "MSCIWorld" # Select one of the following: "Global","MSCIWorld", "MSCIACWI", "STOXX600", "SP500" (for the S&P 500) or "MSCIEM" (for emerging markets)
Indexchoose <- ParameterFile$Index                                    # "MSCIWorld_MSCI"
BenchmarkRegionchoose <-ParameterFile$BenchmarkRegion                 # "OECD"      # OECD, DEV, Global, GlobalAggregate, NonOECD, EU, US, JP, ...      Need an IndexGeography for each of these
Scenariochoose <- ParameterFile$Scenario                              # "450S" # 450S, NPS, NPS+ or CPS

# Get Equity Batch Results
setwd(ResultsLocation)

if (file.exists(paste0(ResultsLocation,"01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"))){
  EQBatchTest <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_EquityAnalysisResults_",Scenariochoose,"_",BenchmarkRegionchoose,"_",CompanyDomicileRegionchoose,".csv"),stringsAsFactors=FALSE,strip.white=TRUE)
}else{
  EQBatchTest <- read.csv(paste("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_EquityAnalysisResults-450S-only.csv",sep=""),stringsAsFactors=FALSE)
}
EQBatchTest_PortSnapshots <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_PortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE)
EQCompProdSnapshots <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_CompanysProduction_Snapshot.csv"),stringsAsFactors = FALSE)

# Get Debt Batch Results
CBBatchTest <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors=FALSE)
CBBatchTest_PortSnapshots <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE)
CBCompProdSnapshots <- read.csv(paste0("01_BatchResults/",BatchName,"/",BatchToTest,"/",BatchName,"_DebtProductionCompanies_Snapshot2022.csv"),stringsAsFactors = FALSE)

# External Data Results
setwd(DataLocation)
Index <- read.csv("2017-07-20_IndexAnalysis5_EquityAnalysisResults.csv",stringsAsFactors = FALSE)
AllIEATargets <- read.csv("IEATargets2016_AllRegions.csv", stringsAsFactors=FALSE)
AllCompanyData <- read.csv("CompanyLevelData_2017-08-21.csv",stringsAsFactors = FALSE)
BBGData_CompanyLevel <- read.csv(paste0("CompanylvlBBGData",ParameterFile$DateofFinancialData,".csv"),stringsAsFactors = FALSE)
MasterData <- read.csv(paste0("MasterData",ParameterFile$DateofFinancialData,".csv"))
OGData <- read.csv(paste0("01_SectorMasters/",ParameterFile$GDYearMonth,"/OGMaster_",ParameterFile$GDYearMonth,".csv"),stringsAsFactors = FALSE)
BenchmarkRegionList <- read.csv("BenchRegions.csv")
IndexUniverses <- read.csv("IndexRegions.csv")

PortfolioList <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/",ParameterFile$ProjektName,"/",BatchName,"_PortfolioList.csv"),stringsAsFactors = FALSE)
PortfolioBreakdown <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/",ParameterFile$ProjektName,"/",BatchName,"Portfolio_Overview_Piechart.csv"),stringsAsFactors = FALSE)
OGCarbonBudget <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/04_Other/CarbonCapexUpstream.csv"),stringsAsFactors = FALSE)
FundList <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/02_Swiss/Swiss_BatchAllPort_ListofFunds.csv"),stringsAsFactors = FALSE)
FundList$InvestorName <- gsub(".","",FundList$InvestorName, fixed = TRUE)
FundList$PortfolioName<- gsub(".","",FundList$PortfolioName, fixed = TRUE)

PortfolioBreakdown$InvestorName<- gsub("[ _.-]","",PortfolioBreakdown$InvestorName)
PortfolioBreakdown$PortfolioName<- gsub("[ _.-]","",PortfolioBreakdown$PortfolioName)

# Add Company Names to BatchTest_PortSnapshot
# CompanyNames <- unique(subset(AllCompanyData, select=c("EQY_FUND_TICKER","Name")))
# EQBatchTest_PortSnapshots <- merge(CompanyNames, EQBatchTest_PortSnapshots, by = "EQY_FUND_TICKER", all.y = TRUE)

# For the Funds comparison chart
FundsDataAll <- read.csv(paste0(ResultsLocation,"01_BatchResults/Swiss_FundData/2016Q4/Swiss_FundData_EquityAnalysisResults_450S_GlobalAggregate_Global.csv"), stringsAsFactors = FALSE)

# ------
# Portfolio List
# ------
EQPortsToTest <- unique(EQBatchTest[,c("PortName", "Type")])
CBPortsToTest <- unique(CBBatchTest[,c("PortName", "Type")])
PortsToTest <- unique(rbind(EQPortsToTest,CBPortsToTest))

# List of all Investors and Portfolios, short and long
TestList <- FundPortcheck(PortsToTest,PortfolioList)
write.csv(TestList,paste0(ResultsLocation,"04_Swiss/TestList.csv"),row.names = FALSE)
# TestList<- read.csv(paste0(ResultsLocation,"04_Swiss/TestList.csv"),stringsAsFactors = F)

# Investor Information including Type (PK, V; In Switzerland, Outside)
InvestorInfo <- read.csv(paste0(ResultsLocation,"04_Swiss/FolderNAME_CompanyNAME.csv"))#,stringsasFactors = FALSE)

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


#Bind Sector Classification from BBG - ICB Subsector Name
CleanedBBGData <- cleanBBGData(BBGData_CompanyLevel,AllCompanyData,Startyear,Scenariochoose,CompanyDomicileRegionchoose,BenchmarkRegionchoose)
Companies <- CleanedBBGData[[1]]
UtilityCompanies <- CleanedBBGData[[2]]
AutoCompanies <- CleanedBBGData[[3]]
OilData <- cleanOGData(OGData,AllCompanyData,Startyear)

OSTargets <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/04_Other/SDA_Targets.csv"), stringsAsFactors = FALSE)
OSdata <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/00_RawData/99_SampleDataSets/OSmaster_2017-10-07.csv"), stringsAsFactors = FALSE)
ShippingData <- read.csv(paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/00_RawData/07_Shipping/ShippingData.csv"), stringsAsFactors = FALSE,strip.white = TRUE)
EQ_OS_WEM <- matchOS(OSdata, EQBatchTest_PortSnapshots)
write.csv(EQ_OS_WEM,paste0(BatchName,"_EQ_OtherSectorOutput.csv"),row.names = FALSE)
CB_OS_WEM <- matchOS(OSdata, CBBatchTest_PortSnapshots)
write.csv(CB_OS_WEM,paste0(BatchName,"_CB_OtherSectorOutput.csv"),row.names = FALSE)

# ---------
# Comparison Companies
# ---------
# Provides the ranking table for each company, Global benchmarkregion
# Currently takes the BatchTest inputs, but this should be changed to the results for specified companies
PortfolioList <- TestList$PortName[TestList$Type %in% c("Investor","Portfolio")]
InvestorList <- TestList$PortName[TestList$Type %in% c("Investor","InvestorMPs")]
EQPortfolioResultsRaw <- EQBatchTest[EQBatchTest$PortName %in% PortfolioList,]
EQInvestorResultsRaw <- EQBatchTest[EQBatchTest$PortName %in% InvestorList,]
# 
# EQPortfolioResultsRaw <- EQBatchTest
# EQInvestorResultsRaw <- EQBatchTest

EQPortfolioResults <- company_comparison("EQ",EQPortfolioResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
EQPortfolioRanks <- EQPortfolioResults[[1]]
EQPortfolioExposures <- EQPortfolioResults[[2]]
EQPortfolioAUMs <- EQPortfolioResults[[4]]

EQInvestorResults <- company_comparison("EQ",EQInvestorResultsRaw, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
EQInvestorRanks <- EQInvestorResults[[1]]
EQInvestorExposures <- EQInvestorResults[[2]]
EQInvestorAUMs <- EQInvestorResults[[4]]

PKEQInvestors <- filterports(EQInvestorResultsRaw, InvestorInfo, "PK")
PKEQInvestorResults <- company_comparison("EQ",PKEQInvestors, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
PKEQInvestorRanks <- PKEQInvestorResults[[1]]
PKEQInvestorExposures <- PKEQInvestorResults[[2]]
PKEQInvestorAUMs <- PKEQInvestorResults[[4]]

VEQInvestors <- filterports(EQInvestorResultsRaw, InvestorInfo, "V")
VEQInvestorResults <- company_comparison("EQ",VEQInvestors, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
VEQInvestorRanks <- VEQInvestorResults[[1]]
VEQInvestorExposures <- VEQInvestorResults[[2]]
VEQInvestorAUMs <- VEQInvestorResults[[4]]

EQPortfolioCoverageWeights <- CoverageWeight_data("EQ","Fund",EQBatchTest_PortSnapshots, EQBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
EQInvestorCoverageWeights <- CoverageWeight_data("EQ","Brand",EQBatchTest_PortSnapshots, EQBatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose)
EQWMInvestorCoverageWeight <- EQPortfolioCoverageWeights[EQPortfolioCoverageWeights$PortName %in% "WeightedResults",]
EQWMPortfolioCoverageWeight <- EQInvestorCoverageWeights[EQInvestorCoverageWeights$PortName %in% "WeightedResults",]

CBPortfolioResultsRaw <- CBBatchTest[CBBatchTest$PortName %in% PortfolioList,]
CBInvestorResultsRaw <- CBBatchTest[CBBatchTest$PortName %in% InvestorList,]
# 
# CBPortfolioResultsRaw <- CBBatchTest
# CBInvestorResultsRaw <- CBBatchTest


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
CBWMInvestorCoverageWeight <- CBInvestorCoverageWeights[CBInvestorCoverageWeights$PortName %in% "WeightedResults",]  

# ------
# Graph Inputs
# ---------
#Saturated colours
RenewablesColour = "#b3de69"
HydroColour = "#428bbd"
NuclearColour = "#827ab8"
GasCapColour="grey75"
CoalCapColour = "#252525"
ElectricColour= "#69c454"
HybridColour= "#00b7be"
ICEColour= "#2F4F4F"   #"#ed1c24" #"#f93620"
GasProdColour = "#ffb861"
OilProdColour = "#c88450"
CoalProdColour = "#835834"

YourportColour = "#265b9b"   #"#2e4b6e"  #"#17224D"
IndexColour =  "grey85"
Tar2DColourBar = "#b3de69"
Tar2DColour = "#a1c75e"
goodexpColour = "#1F891F"
badexpColour = "#ed1c24" #"#fb8072"
ReqCapColour = "grey55"
CurrCapColour = "grey75"
AxisColour = "#17375e" #"#274F80"

ColourPalette <- data.frame(Sector = c("Power","Power","Power","Power","Power","Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels"),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Electric","Hybrid","ICE","Gas","Oil","Coal"),Colours =c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,ElectricColour,HybridColour,ICEColour,GasProdColour,OilProdColour,CoalProdColour))
figuredirectory <- paste0(CodeLocation,"01_ReportTemplates/ReportGraphics/Icons/")

textsize = 8
ppi <- 600 #resolution of plots

ResultsLocFolder <- "02_CompanyResults/"
if (BatchName %in% c("Swiss","SwissAll")){ResultsLocFolder <- "04_Swiss/CompanyResults/"}

# ------
# Translation and Report Inputs
# ---------
GraphTranslation <- read.csv(paste0(CodeLocation,"01_ReportTemplates/GraphTranslation_V4.csv"), stringsAsFactors = FALSE)
ReportTranslation <- read.csv(paste0(CodeLocation,"01_ReportTemplates/SwissReportTranslation_V12.csv"), stringsAsFactors = FALSE)
template <- (readLines(paste0(GitHubLocation,"Templates/SwissTemplateInput_v2.tex"),encoding="UTF-8"))
GT <- preptranslations("Graph",GraphTranslation, Languagechoose, Startyear)
RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)



#------
# Select Portfolio to Run
#------

portnames <- c("Novartis")

ToTest2 <- which(TestList$InvestorName %in% portnames)
# ToTest2 <- c(107)

i=146

#-------
# Loop through Portfolios
#--------
for (i in ToTest2){
# for (i in c(6:6)){  

  # ------
  # Setting Directory and Getting Results
  # ------
  PortfolioNameLong <- TestList[i,1]
  TestType <- TestList[i,2]
  InvestorName <-  TestList[i,3]
  PortfolioName <- TestList[i,4]
  PortGraphName <- TestList[i,6]
  InvestorLongName <- TestList[i,5]
  ReportName <-TestList[i,7]
  
  InvestorDirectory <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/03_Results/",ResultsLocFolder,InvestorName,"/")  
  PortfolioDirectory <- paste0(InvestorDirectory,PortfolioName,"/")
  RegionDirectory <-PortfolioDirectory
    
  # TestDirectory <- paste0(PortfolioDirectory,Sys.Date(),"_",BatchName,"/")
  # RegionDirectory <- paste0(PortfolioDirectory,substr(CompanyDomicileRegionchoose,0,9),"_",substr(BenchmarkRegionchoose,0,9),"/")
  
  InvestorInfo$ShortName <- gsub(".","",gsub(" ","",InvestorInfo$RefName), fixed = TRUE)
  PortSet <- InvestorInfo[InvestorInfo$ShortName == InvestorName,]
  
  if(!dir.exists(file.path(InvestorDirectory))){dir.create(file.path(InvestorDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
  if(!dir.exists(file.path(PortfolioDirectory))){dir.create(file.path(PortfolioDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  # if(!dir.exists(file.path(TestDirectory))){dir.create(file.path(TestDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  # if(!dir.exists(file.path(RegionDirectory))){dir.create(file.path(RegionDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  
  
  # PortGraphName <- "Sample Portfolio"
  # ReportName <- "Sample Portfolio"
  # RegionDirectory <- paste0(ResultsLocation,"/04_Swiss/SAMPLE REPORTS/")
  
  
  setwd(RegionDirectory)
  
  #____________Load Inputs________________________
  EQCombin <- subset(EQBatchTest, PortName == PortfolioNameLong)
  EQPortSnapshot <- subset(EQBatchTest_PortSnapshots, PortName == PortfolioNameLong)
  EQCompProdSnapshot <- subset(EQCompProdSnapshots, PortName == PortfolioNameLong) 
  CBCombin <- subset(CBBatchTest, PortName == PortfolioNameLong)
  CBPortSnapshot <- subset(CBBatchTest_PortSnapshots, PortName == PortfolioNameLong)
  CBCompProdSnapshot <- subset(CBCompProdSnapshots, PortName == PortfolioNameLong) 
  
  PortData <- PortfolioBreakdown[PortfolioBreakdown$InvestorName %in% InvestorName & PortfolioBreakdown$PortfolioName %in% PortfolioName & PortfolioBreakdown$HoldingType == "All",]
  
  print(PortfolioNameLong)
  
  write.csv(EQCombin, paste0("EQCombin_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  write.csv(EQCompProdSnapshot, paste0("EQCompProdSnapshot_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  write.csv(EQPortSnapshot,paste0("EQPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  write.csv(CBCombin, paste0("CBCombin_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  
  write.csv(CBPortSnapshot,paste0("CBPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  write.csv(CBCompProdSnapshot,paste0("CBProdSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  
  # EQExposures <- exposure_data("EQ",EQPortSnapshot, EQCombin, BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose, Startyear,PortfolioName)
  # EQCoverageWeight <- EQCoverageWeights[EQCoverageWeights$PortName %in% PortfolioNameLong,]
  # CBCoverageWeight <- CBCoverageWeights[CBCoverageWeights$PortName %in% PortfolioNameLong,]
  
  # if (ComparisonFile != FALSE){
    if(TestType == "Portfolio"){
      EQRanks <- EQPortfolioRanks
      EQExposures <- EQPortfolioExposures
      EQAUMData <- EQPortfolioAUMs
      CBRanks <- CBPortfolioRanks
      CBExposures <- CBPortfolioExposures
      CBAUMData <- CBPortfolioAUMs
      EQWMCoverageWeight <- EQWMPortfolioCoverageWeight
      CBWMCoverageWeight <- CBWMPortfolioCoverageWeight
      EQCoverageWeight <- EQPortfolioCoverageWeights[EQPortfolioCoverageWeights$PortName %in% PortfolioNameLong,]
      CBCoverageWeight <- CBPortfolioCoverageWeights[CBPortfolioCoverageWeights$PortName %in% PortfolioNameLong,]
    }else{
        EQRanks <- EQInvestorRanks
        EQExposures <- EQInvestorExposures
        EQAUMData <- EQInvestorAUMs
        CBRanks <- CBInvestorRanks
        CBExposures <- CBInvestorExposures
        CBAUMData <- CBInvestorAUMs
        EQWMCoverageWeight <- EQWMInvestorCoverageWeight
        CBWMCoverageWeight <- CBWMInvestorCoverageWeight
        EQCoverageWeight <- EQInvestorCoverageWeights[EQInvestorCoverageWeights$PortName %in% PortfolioNameLong,]
        CBCoverageWeight <- CBInvestorCoverageWeights[CBInvestorCoverageWeights$PortName %in% PortfolioNameLong,]}
  
  FundsInPort <- Portfunds(20,FundList,FundsDataAll, PortfolioName,InvestorName)
  if (typeof(FundsInPort)=="list"){
    if (nrow(FundsInPort) == 0){FundsInPort = "NoFunds"}  }
    
  # Set Language
  # Run through charts for all languages
  LanguageSet <- PortSet[,5:7]
  # LanguageSet$DE<-1
  # LanguageSet$EN<-1
  # LanguageSet$FR<-0
  

  for (lc in 1:3){
    if (LanguageSet[1,lc] == 1){
      Languagechoose =  colnames(LanguageSet)[lc]
      GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
      RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)

      LanguageDirectory <- paste0(RegionDirectory,Languagechoose,"/")
      if(!dir.exists(file.path(LanguageDirectory))){dir.create(file.path(LanguageDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
      setwd(LanguageDirectory)

      # -------
      # Equity Charts
      # -------
      tryCatch({

        # -------
        # Swiss Charts
        # -------
        # Page 4
        plot_0 <- port_pie("00", PortData)
        # #
        plot_1 <- pie_chart("01","EQ",EQPortSnapshot, PortfolioName, CompanyDomicileRegionchoose)
        plot_2 <- pie_chart("02","CB",CBPortSnapshot, PortfolioName, CompanyDomicileRegionchoose)
        # # # # #
        # # # # # # # Page 8
        plot_3 <- stacked_bar_chart("03","EQ",EQCombin,EQWMCoverageWeight,"Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortGraphName)
        plot_4 <- mini_line_chart("04","EQ",EQCombin,"RenewablesCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_5 <- mini_line_chart("05","EQ",EQCombin,"CoalCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_6 <- mini_line_chart("06","EQ",EQCombin,"GasCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_7 <- ranking_chart_alignment("07","EQ",Startyear,"Power", EQExposures, EQAUMData,EQRanks,figuredirectory,PortfolioNameLong)
        # # #
        # # # # Page 9
        plot_08 <- stacked_bar_chart("08","CB",CBCombin,CBWMCoverageWeight,"Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortGraphName)
        plot_09 <- mini_line_chart("09","CB",CBCombin,"RenewablesCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_10 <- mini_line_chart(10,"CB", CBCombin,"CoalCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_11 <- mini_line_chart(11,"CB",CBCombin,"GasCap","Power", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_12 <- ranking_chart_alignment(12,"CB",Startyear,"Power", CBExposures, CBAUMData,CBRanks,figuredirectory,PortfolioNameLong)
        # #
        # # # Page 10
        plot_13 <- stacked_bar_chart(13,"EQ",EQCombin,EQWMCoverageWeight,"Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortGraphName)
        plot_14 <- mini_line_chart(14,"EQ",EQCombin,"ICE","Automotive",BenchmarkRegionchoose,  CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_15 <- mini_line_chart(15,"EQ",EQCombin,"Electric","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_16 <- mini_line_chart(16,"EQ",EQCombin,"Hybrid","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_17 <- ranking_chart_alignment(17,"EQ",Startyear,"Automotive", EQExposures, EQAUMData,EQRanks,figuredirectory,PortfolioNameLong)
        # #
        # # # Page 11
        plot_18 <- stacked_bar_chart(18,"CB",CBCombin,CBWMCoverageWeight,"Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortGraphName)
        plot_19 <- mini_line_chart(19,"CB",CBCombin,"ICE","Automotive",BenchmarkRegionchoose,  CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_20 <- mini_line_chart(20,"CB",CBCombin,"Electric","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_21 <- mini_line_chart(21,"CB",CBCombin,"Hybrid","Automotive", BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, figuredirectory,PortfolioName)
        plot_22 <- ranking_chart_alignment(22,"CB",Startyear,"Automotive", CBExposures, CBAUMData,CBRanks,figuredirectory,PortfolioNameLong)
        # #
        # # # Page 12
        plot_23 <- stacked_bar_chart(23,"EQ",EQCombin,EQWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName,PortGraphName)
        plot_24 <- mini_line_chart(24,"EQ",EQCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_25 <- mini_line_chart(25,"EQ",EQCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_26 <- mini_line_chart(26,"EQ",EQCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_27 <- ranking_chart_alignment(27,"EQ",Startyear,"Fossil Fuels", EQExposures, EQAUMData,EQRanks,figuredirectory,PortfolioNameLong)
        # #
        # #
        # # # Page 13
        plot_28 <- stacked_bar_chart(28,"CB",CBCombin,CBWMCoverageWeight,"Fossil Fuels",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear, PortfolioName,PortGraphName)
        plot_29 <- mini_line_chart(29,"CB",CBCombin,"Oil","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_30 <- mini_line_chart(30,"CB",CBCombin,"Gas","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_31 <- mini_line_chart(31,"CB",CBCombin,"Coal","Fossil Fuels", BenchmarkRegionchoose,CompanyDomicileRegionchoose,Scenariochoose, figuredirectory,PortfolioName)
        plot_32 <- ranking_chart_alignment(32,"CB",Startyear,"Fossil Fuels", CBExposures, CBAUMData,CBRanks,figuredirectory,PortfolioNameLong)
        # #
        # # Page 14
        plot_33 <- ranking_chart_alignment(33,"EQ",Startyear,"All", EQExposures, EQAUMData,EQRanks,figuredirectory,PortfolioNameLong)

        # # Page 15
        plot_34 <- ranking_chart_alignment(34,"CB",Startyear,"All", CBExposures, CBAUMData,CBRanks,figuredirectory,PortfolioNameLong)
        # 
        # # Page 16
        plot_35 <- fundmap_chart(35,FundsInPort, Startyear, Scenariochoose, PortfolioName)

        # Page 17
        plot_36 <- other_sector_chart(36, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Cement",PortfolioName)
        plot_37 <- other_sector_chart(37, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Steel",PortfolioName)
        plot_38 <- other_sector_chart(38, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot = "Aviation",PortfolioName)
        plot_39 <- shipping_chart(39, EQPortSnapshot,CBPortSnapshot,ShippingData, SectorToPlot="Shipping",PortfolioName)
        OtherSectors <- data.frame("Cement"=plot_36,"Steel"=plot_37,"Aviation"=plot_38,"Shipping"=plot_39)

        # Page 21
        if(nrow(EQCombin)==0){
        plot_43 <- flat_wheel_chart(43,10,"CB",CBPortSnapshot,CBCombin, UtilityCompanies,SectorToPlot = "Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
        }else{
        plot_43 <- flat_wheel_chart(43,10,"EQ",EQPortSnapshot,EQCombin, UtilityCompanies,SectorToPlot = "Power",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
        }
        # #
        # # # Page 22
        if(nrow(EQCombin)==0){
          plot_44 <- flat_wheel_chart(44,10,"CB",CBPortSnapshot,CBCombin, AutoCompanies,SectorToPlot = "Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
          }else{
           plot_44 <- flat_wheel_chart(44,10,"EQ",EQPortSnapshot,EQCombin, AutoCompanies,SectorToPlot = "Automotive",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
          }
        # 
        if (nrow(EQCombin)==0){
          plot_46 <- flat_wheel_chart(46,20,"CB",CBPortSnapshot,CBCompProdSnapshot, OGCarbonBudget,SectorToPlot = "OG",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
        }else{
          plot_46 <- flat_wheel_chart(46,20,"EQ",EQPortSnapshot,EQCompProdSnapshot, OGCarbonBudget,SectorToPlot = "OG",BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexData,Indexchoose,PortfolioName,PortGraphName)
        }
        # 
        # # Page 24
        if(nrow(EQCombin)!=0){
          plot_51 <- renewablesadditions_chart(51,"EQ",EQCombin, EQPortSnapshot, Scenariochoose, MasterData, AllIEATargets, RegionCountries,PortfolioName)
        }else{plot_51<-0}

        mini_chart_list <- data.frame("EQCaptionRenewablesCap"=plot_4,"EQCaptionGasCap"=plot_6,"EQCaptionCoalCap"=plot_5,"CBCaptionRenewablesCap"=plot_09,"CBCaptionGasCap"=plot_11,"CBCaptionCoalCap"=plot_10,"EQCaptionICE"=plot_14,"EQCaptionElectric"=plot_15,"EQCaptionHybrid"=plot_16,"CBCaptionICE"=plot_19,"CBCaptionElectric"=plot_20,"CBCaptionHybrid"=plot_21,"EQCaptionOilProd"=plot_24,"EQCaptionGasProd"=plot_25,"EQCaptionCoalProd"=plot_26,"CBCaptionOilProd"=plot_29,"CBCaptionGasProd"=plot_30,"CBCaptionCoalProd"=plot_31)

        figurelist <- list.files(getwd(),pattern=c("\\.png$"), full.names = FALSE)
        writeLines(figurelist,"FigureList.txt")


        # -------
        # PDF CREATION
        # -------
        EQReportData<-report_data("EQ",EQCombin, EQExposures,EQAUMData,EQRanks,EQPortSnapshot,CompanyDomicileRegionchoose, BenchmarkRegionchoose,Scenariochoose, Startyear,PortfolioNameLong)
        CBReportData<-report_data("CB",CBCombin, CBExposures,CBAUMData,CBRanks,CBPortSnapshot,CompanyDomicileRegionchoose, BenchmarkRegionchoose,Scenariochoose, Startyear,PortfolioNameLong)

        report(PortfolioName,ReportName, InvestorName, template, RT,EQReportData,CBReportData,FundsInPort,OtherSectors,mini_chart_list,RenewAdds=plot_51,Languagechoose)
        
        
      })
  }}
}
# ------
