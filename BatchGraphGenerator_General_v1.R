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
# library(dplyr)
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
InvestorName <- "California Insurers"

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
### Set Results Location
REPORT.PATH <- paste0(RESULTS.PATH,"05_Reports/",ProjectName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
REPORT.PATH <- paste0(RESULTS.PATH,"05_Reports/",ProjectName,"/",BatchName,"/")
if(!dir.exists(file.path(REPORT.PATH))){dir.create(file.path(REPORT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
BATCH.RES.PATH <- paste0(RESULTS.PATH,"01_BatchResults/",BatchName,"/",BatchToTest,"/")

### Get Debt Batch Results
CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtAnalysisResults-450S-only.csv"),stringsAsFactors=FALSE,strip.white = T)
CBBatchTest <- subset(CBBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
print(paste0("Debt Analysis Results: ", nrow(CBBatchTest), " rows."))

CBBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtPortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE,strip.white = T)
CBBatchTest_PortSnapshots <- subset(CBBatchTest_PortSnapshots, Type == "Portfolio")
print(paste0("Debt Portfolio Results: ", nrow(CBBatchTest_PortSnapshots), " rows."))

CBCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_DebtProductionCompanies_Snapshot2023.csv"),stringsAsFactors = FALSE,strip.white = T)
CBCompProdSnapshots <- subset(CBCompProdSnapshots, Type == "Portfolio" & Aggregation == BenchmarkRegionchoose)
print(paste0("Debt Company Production Results: ", nrow(CBCompProdSnapshots), " rows."))
CBALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"-ALD-Agg-Production.csv"),stringsAsFactors=FALSE,strip.white = T)


### Get Equity Batch Results
EQBatchTest <- read.csv(paste(BATCH.RES.PATH,BatchName,"_PortfolioWeightedAnalysisResults-450S-only.csv",sep=""),stringsAsFactors=FALSE,strip.white = T)
EQBatchTest <- subset(EQBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
print(paste0("Equity Analysis Results: ", nrow(EQBatchTest), " rows."))

EQBatchTest_PortSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_PortfolioData_Snapshot",Startyear,".csv"), stringsAsFactors=FALSE,strip.white = T)
EQBatchTest_PortSnapshots <- subset(EQBatchTest_PortSnapshots, Type == "Portfolio")
print(paste0("Equity Portfolio Snapshot: ", nrow(EQBatchTest_PortSnapshots), " rows."))

EQCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_ProductionCompanies_Snapshot2023.csv"),stringsAsFactors = FALSE,strip.white = T)
EQCompProdSnapshots <- subset(EQCompProdSnapshots, Type == "Portfolio" & Aggregation == BenchmarkRegionchoose)
print(paste0("Equity Company Production Snapshot: ", nrow(EQCompProdSnapshots), " rows."))

#EQALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"-ALD-Agg-Production.csv"),stringsAsFactors=FALSE,strip.white = T)

#Process (remove later)
EQBatchTest$PortName <- gsub(paste0("_",InvestorName),"",EQBatchTest$PortName)
EQCompProdSnapshots$PortName <- gsub(paste0("_",InvestorName),"",EQCompProdSnapshots$PortName)

CBBatchTest$PortName <- gsub(paste0("_",InvestorName),"",CBBatchTest$PortName)
CBCompProdSnapshots$PortName <- gsub(paste0("_",InvestorName),"",CBCompProdSnapshots$PortName)

# Comparison Flags
EQBatchTest$Type[EQBatchTest$InvestorName != "California Insurers"] <- "Market"
EQBatchTest$Type[EQBatchTest$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"
CBBatchTest$Type[CBBatchTest$InvestorName != "California Insurers"] <- "Market"
CBBatchTest$Type[CBBatchTest$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"
EQBatchTest_PortSnapshots$Type[EQBatchTest_PortSnapshots$InvestorName != "California Insurers"] <- "Market"
EQBatchTest_PortSnapshots$Type[EQBatchTest_PortSnapshots$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"
CBBatchTest_PortSnapshots$Type[CBBatchTest_PortSnapshots$InvestorName != "California Insurers"] <- "Market"
CBBatchTest_PortSnapshots$Type[CBBatchTest_PortSnapshots$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"
EQCompProdSnapshots$Type[EQCompProdSnapshots$InvestorName != "California Insurers"] <- "Market"
EQCompProdSnapshots$Type[EQCompProdSnapshots$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"
CBCompProdSnapshots$Type[CBCompProdSnapshots$InvestorName != "California Insurers"] <- "Market"
CBCompProdSnapshots$Type[CBCompProdSnapshots$PortName %in% c("MetaPort","MetaPortfolio")] <- "MetaPortfolio"


### External Data Read In
setwd(PROC.DATA.PATH)
AllIEATargets <- read.csv("IEATargets2016_AllRegions.csv", stringsAsFactors=FALSE)
OGData <- read.csv(paste0("01_SectorMasters/",ParameterFile$GDYearMonth,"/OGMaster_",ParameterFile$GDYearMonth,".csv"),stringsAsFactors = FALSE)
BenchmarkRegionList <- read.csv("BenchRegions.csv")
IndexUniverses <- read.csv("IndexRegions.csv")

### Batch related Portfolio & Fund-Data Results
EquityList <- unique(subset(EQBatchTest, select=c("PortName","InvestorName", "Type")))
EquityList$HasEquity <- TRUE
DebtList <- unique(subset(CBBatchTest, select=c("PortName","InvestorName", "Type")))
DebtList$HasDebt <- TRUE
TestList <- merge(EquityList,DebtList,by=c("PortName","InvestorName","Type"), all=T)
TestList[is.na(TestList)] <- FALSE
print(paste0("Test List: ", nrow(TestList), " rows."))


# ------
# Bench Regions and Indicies and Sector Classifications
# ------
### Get Bench Regions
RegionCountries <- data.frame(BenchmarkRegionList$Global, BenchmarkRegionList$OECD)
RegionCountries <- rename(RegionCountries, c("BenchmarkRegionList.Global"="Global","BenchmarkRegionList.OECD"="OECD"))

#IndexUniverses[is.na(IndexUniverses)] <- ""
IndexUniversesList <- data.frame(IndexUniverse = IndexUniverses$IndexUniverse[!is.na(IndexUniverses$IndexUniverse) & IndexUniverses$IndexUniverse != ""], 
                                 IndexUniversesColname = IndexUniverses$IndexUniverseColname[!is.na(IndexUniverses$IndexUniverseColname) & 
                                                                                               IndexUniverses$IndexUniverseColname != ""])

IEATargetsAll <- subset(AllIEATargets, BenchmarkRegion == "Global" &Year %in% c(Startyear, Startyear+5, Startyear+10,Startyear+15)  & 
                          Scenario == Scenariochoose, select = c("Sector","Technology","AnnualvalIEAtech","Year")) 
IEATargetsAll <- IEATargetsAll[!IEATargetsAll$Technology %in% "OilCap",]

OilData <- cleanOGData(OGData,Startyear)

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

template <- (readLines(paste0(GIT.PATH,"Templates/","CATemplateInput_v1.tex"),encoding="UTF-8"))

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
  PortfolioNameLong <- TestList[i,"PortName"]
  TestType <- TestList[i,"Type"]
  InvestorNameLong <-  TestList[i,"InvestorName"]
  InvestorName <-  TestList[i,"InvestorName"]
  PortfolioName <- TestList[i,"PortName"]
  PortName <- TestList[i,"PortName"]
  
  if(TestType == "MetaPortfolio"){
    ReportName <- InvestorNameLong
  }else if (TestType == "Portfolio") {
    ReportName <- paste0(InvestorNameLong,": ", PortfolioNameLong)
  }else {
    return()
  }
  
  print(paste0(PortfolioNameLong, "; ",InvestorNameLong,"; ",i, " of ",nrow(TestList)))

  ### Subsets results for this portfolio
  EQCombin <- EQBatchTest[EQBatchTest$PortName == PortName,]
  EQCompProdSnapshot <- EQCompProdSnapshots[EQCompProdSnapshots$PortName == PortName,]
  CBCombin <- CBBatchTest[CBBatchTest$PortName == PortName,]
  CBCompProdSnapshot <- CBCompProdSnapshots[CBCompProdSnapshots$PortName == PortName,]

  
  
  ### Creates working directory
  INVESTOR.PATH <- paste0(REPORT.PATH,InvestorName,"/")  
  PORTFOLIO.PATH <- paste0(INVESTOR.PATH,PortfolioName,"/")
  if(!dir.exists(file.path(INVESTOR.PATH))){dir.create(file.path(INVESTOR.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
  if(!dir.exists(file.path(PORTFOLIO.PATH))){dir.create(file.path(PORTFOLIO.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  
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
      
      #######################
      ### CA TEMPLATE #######
      #######################
      portfolio_sectorshare("01")
      sector_techshare(2,"Summary","All") #Combined EQ/CB, needs to be updated
      # Graph246(3, "Summary", c("RenewablesCap","Electric","Hybrid")) #Can't actually plot all three?
      # Graph246(4, "Summary", "Fossil Fuels") #No fossil fuel combination?
      distribution_chart("05", "Risk Exposure", "CB")
      
      sector_techshare("06","EQ","All")
      sector_techshare("07","CB","All")
      
      distribution_chart("08", "Carsten's Metric", "EQ")
      distribution_chart("09", "Carsten's Metric", "CB")
      
      Graph246(10, "EQ", "CoalCap")
      Graph246(11, "EQ", "RenewablesCap")
      Graph246(12, "EQ", "GasCap")
      Graph246(13, "EQ", "NuclearCap") #246 currently not working
      Graph246(14, "EQ", "OilProd")
      Graph246(15, "EQ", "GasProd")
      Graph246(16, "EQ", "ICE")
      Graph246(17, "EQ", "EV")
      
      Graph246(18, "CB", "CoalCap")
      Graph246(19, "CB", "RenewablesCap")
      Graph246(20, "CB", "GasCap")
      Graph246(21, "CB", "NuclearCap")
      Graph246(22, "CB", "OilProd")
      Graph246(23, "CB", "GasProd")
      Graph246(24, "CB", "ICE")
      Graph246(25, "CB", "EV")
      
      ranking_chart_alignment(26, "EQ", "All", Startyear) #Carstens Metric
      ranking_chart_alignment(27, "CB", "All", Startyear) #Carstens Metric
      
      company_techshare(28, 20, "EQ", "Power")
      company_techshare(29, 20, "EQ", "Automotive")
      company_techshare(30, 20, "EQ", "Fossil Fuels")
      # company_techshare(31, 20, "EQ", "Oil")
      
      company_techshare(32, 20, "CB", "Power")
      company_techshare(33, 20, "CB", "Automotive")
      company_techshare(34, 20, "CB", "Fossil Fuels")
      # company_techshare(35, 20, "CB", "Oil")
      
      # Creates the list of figures that were printed. 
      # A better solution is possible, but this works. 
      # This list gets deleted after the report is printed. 
      # The plotnumber has to match the figure number in the report ie Fig01 etc. Therefore "01" is necessary. 
      figurelist <- list.files(getwd(),pattern=c("\\.png$"), full.names = FALSE)
      writeLines(figurelist,"FigureList.txt")
      
      
      # Prepares the data that goes into the report
      CAReportData()

      
      # Creates the report for California
      # I think it's necessary to seperate from the previous
      CAReport()
      
      
    })}else{
      print (paste0(PortfolioNameLong," has no Equity and Bond Data"))
    }
  
  write.csv(EQCompProdSnapshot, paste0("EQCompProdSnapshot_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  write.csv(EQPortSnapshot,paste0("EQPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  write.csv(CBCompProdSnapshot,paste0("CBCompProdSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  write.csv(CBPortSnapshot,paste0("CBPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
}

