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
library(dplyr)
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
library(extrafont)
font_import()   #Importing fonts may take a few minutes, depending on the number of fonts and the speed of the system.
loadfonts(device = "win")

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

ParameterFile <- ReadParameterFile(PORTS.PATH)
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

### ###########################################################################
### GET OVERVIEW RESULTS
### ###########################################################################

# Ports.Overview <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Port-Overview.csv"),stringsAsFactors=FALSE,strip.white = T)
# names(Ports.Overview) <- gsub("TwoD\\.", "", names(Ports.Overview))
# length(unique(Ports.Overview$Portfolio.Name)) ## Number of Insurers

eqnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-Names-TAJ-Update.csv"),stringsAsFactors = FALSE,strip.white = T)
#any(duplicated(eqnames$EQY_FUND_TICKER)) 
cbnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-Names-TAJ-Update.csv"),stringsAsFactors = FALSE,strip.white = T)
#any(duplicated(cbnames$COMPANY_CORP_TICKER)) 

Subgroup.Overview <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Port-Overview-Subgroup.csv"),stringsAsFactors=FALSE,strip.white = T)
names(Subgroup.Overview) <- gsub("TwoD\\.", "", names(Subgroup.Overview))
length(unique(Subgroup.Overview$Portfolio.Name)) ## Number of Insurers   672

### Get Debt Batch Results
CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
CBBatchTest <- subset(CBBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
print(paste0("Debt Analysis Results: ", nrow(CBBatchTest), " rows."))

CBCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Company-ALD-2023.csv"),stringsAsFactors = FALSE,strip.white = T)
CBCompProdSnapshots <- subset(CBCompProdSnapshots, Type == "Portfolio" & Aggregation == BenchmarkRegionchoose & Scenario == Scenariochoose)
print(paste0("Debt Company Production Results: ", nrow(CBCompProdSnapshots), " rows."))
CBCompProdSnapshots <- left_join(CBCompProdSnapshots, cbnames, by="COMPANY_CORP_TICKER")
CBCompProdSnapshots <- CBCompProdSnapshots %>% select(-Name) %>% rename(Name=Final.Name)

CBALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-ALD-BuildOut.csv"),stringsAsFactors=FALSE,strip.white = T)

Moodys <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Moodys.csv"),stringsAsFactors=FALSE,strip.white = T)

### Get Equity Batch Results
EQBatchTest <- read.csv(paste(BATCH.RES.PATH,BatchName,"_Equity-Port-ALD-Results-450S.csv",sep=""),stringsAsFactors=FALSE,strip.white = T)
EQBatchTest <- subset(EQBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
print(paste0("Equity Analysis Results: ", nrow(EQBatchTest), " rows."))

EQCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Company-ALD-2023.csv"),stringsAsFactors = FALSE,strip.white = T)
EQCompProdSnapshots <- subset(EQCompProdSnapshots, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose & Scenario == Scenariochoose)
print(paste0("Equity Company Production Snapshot: ", nrow(EQCompProdSnapshots), " rows."))
EQCompProdSnapshots <- left_join(EQCompProdSnapshots, eqnames, by="EQY_FUND_TICKER")
EQCompProdSnapshots <- EQCompProdSnapshots %>% select(-Name) %>% rename(Name=Final.Name)


EQALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-ALD-BuildOut.csv"),stringsAsFactors=FALSE,strip.white = T)   

### FOR THE NEW COMPANY EXPOSURE

EQCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Company-ALD.csv"),stringsAsFactors = FALSE,strip.white = T)
EQCompALD <- subset(EQCompALD, Scenario == Scenariochoose & Aggregation=="GlobalAggregate")

CBCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"-Debt-Port-Company-ALD-Short-ALL.csv"),stringsAsFactors = FALSE,strip.white = T)
CBCompALD <- subset(CBCompALD, Scenario == Scenariochoose & Aggregation=="GlobalAggregate")


# Comparison Flags
EQBatchTest$Type <- "Portfolio"
EQBatchTest$Type[EQBatchTest$InvestorName != "California Insurers"] <- "Market"
EQBatchTest$Type[EQBatchTest$PortName == "MetaPort"] <- "MetaPortfolio"

CBBatchTest$Type <- "Portfolio"
CBBatchTest$Type[CBBatchTest$InvestorName != "California Insurers"] <- "Market"
CBBatchTest$Type[CBBatchTest$PortName == "MetaPort"] <- "MetaPortfolio"

EQCompProdSnapshots$Type <- "Portfolio"
EQCompProdSnapshots$Type[EQCompProdSnapshots$InvestorName != "California Insurers"] <- "Market"
EQCompProdSnapshots$Type[EQCompProdSnapshots$PortName == "MetaPort"] <- "MetaPortfolio"

CBCompProdSnapshots$Type <- "Portfolio"
CBCompProdSnapshots$Type[CBCompProdSnapshots$InvestorName != "California Insurers"] <- "Market"
CBCompProdSnapshots$Type[CBCompProdSnapshots$PortName == "MetaPort"] <- "MetaPortfolio"


#Add All Lower Case of PortName
CBBatchTest$portname <- tolower(CBBatchTest$PortName)
EQBatchTest$portname <- tolower(EQBatchTest$PortName)
CBBatchTest$portname <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", CBBatchTest$portname, perl=TRUE)
EQBatchTest$portname <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", EQBatchTest$portname, perl=TRUE)


### External Data Read In
setwd(PROC.DATA.PATH)
AllIEATargets <- read.csv("IEATargets2016_AllRegions.csv", stringsAsFactors=FALSE)
OGData <- read.csv(paste0("01_SectorMasters/",ParameterFile$GDYearMonth,"/OGMaster_",ParameterFile$GDYearMonth,".csv"),stringsAsFactors = FALSE)
BenchmarkRegionList <- read.csv("BenchRegions.csv")
IndexUniverses <- read.csv("IndexRegions.csv")
CarbonCap <- read.csv(paste0(DATA.PATH,"04_Other/","CarbonCapexUpstream.csv"),stringsAsFactors=FALSE,strip.white = T)


### Batch related Portfolio & Fund-Data Results
TestList <- CreateTestList(EQBatchTest, CBBatchTest)
print(paste0("Test List: ", nrow(TestList), " rows."))


# ------
# Bench Regions and Indicies and Sector Classifications
# ------
### Get Bench Regions
RegionCountries <- data.frame(BenchmarkRegionList$Global, BenchmarkRegionList$OECD)
RegionCountries <- rename(RegionCountries, "Global" = BenchmarkRegionList.Global,"OECD" = BenchmarkRegionList.OECD)

#IndexUniverses[is.na(IndexUniverses)] <- ""
IndexUniversesList <- data.frame(IndexUniverse = IndexUniverses$IndexUniverse[!is.na(IndexUniverses$IndexUniverse) & IndexUniverses$IndexUniverse != ""], 
                                 IndexUniversesColname = IndexUniverses$IndexUniverseColname[!is.na(IndexUniverses$IndexUniverseColname) & 
                                                                                               IndexUniverses$IndexUniverseColname != ""])

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

insurer_template <- (readLines(paste0(GIT.PATH,"Templates/","CATemplateInput_v2-Insurer.tex"),encoding="UTF-8"))
meta_template <- (readLines(paste0(GIT.PATH,"Templates/","CATemplateInput_v2-MetaPort.tex"),encoding="UTF-8"))


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

#Asserts
unique(intersect(CBBatchTest$Technology, CBCompProdSnapshots$Technology))
unique(intersect(EQBatchTest$Technology, EQCompProdSnapshots$Technology))
unique(intersect(CBBatchTest$BenchmarkRegion, CBCompProdSnapshots$BenchmarkRegion))
unique(intersect(EQBatchTest$BenchmarkRegion, EQCompProdSnapshots$BenchmarkRegion))
unique(intersect(CBBatchTest$Scenario, CBCompProdSnapshots$Scenario))
unique(intersect(EQBatchTest$Scenario, EQCompProdSnapshots$Scenario))

# i=326
# i=490
#-------
# Loop through Portfolios
#--------
TOP<-c(501, 327,389, 494, 39, 355,379,318,42,20)
for (i in TOP){#, 500:510)){

  ### Specify the Names from the Test List
  
  PortSummary <- TestList[i,]
  
  TestType <- TestList[i,"Type"]
  PortfolioNameLong <- TestList[i,"PortName"]
  InvestorNameLong <-  TestList[i,"InvestorName"]
  InvestorName <-  gsub(" ", "", TestList[i,"InvestorName"])
  PortfolioName <- gsub("[[:punct:]]", "", TestList[i,"PortName"])
  PortfolioName <- gsub(" ", "", PortfolioName)
  HasEquity <- TestList[i,"HasEquity"]
  HasDebt <- TestList[i,"HasDebt"]
  HasCarbonBudget <- FALSE #Initially set to FALSE, but will be updated if a carbon budget chart is successfully created
  
  print(paste0(PortfolioNameLong, "; ",InvestorNameLong,"; ",i, " of ",nrow(TestList)))

  PortName <- PortfolioNameLong
  
  ### Subsets results for this portfolio
  EQCombin <- EQBatchTest[EQBatchTest$PortName == PortName,]
  EQCompProdSnapshot <- EQCompProdSnapshots[EQCompProdSnapshots$PortName == PortName,]
  CBCombin <- CBBatchTest[CBBatchTest$PortName == PortName,]
  CBCompProdSnapshot <- CBCompProdSnapshots[CBCompProdSnapshots$PortName == PortName,]
  
  if(TestType == "MetaPortfolio"){
    ReportName <- InvestorNameLong
    EQCombin$Type <- "Portfolio"
    EQCompProdSnapshot$Type <- "Portfolio"
    CBCombin$Type <- "Portfolio"
    template <- meta_template
    WithCompanyCharts <- FALSE
    # PortName <- "Meta Portfolio"
  }else if (TestType == "Portfolio") {
    ReportName <- paste0(InvestorNameLong,": ",PortfolioNameLong)
    # PortName <- "Your Portfolio"
    template <- insurer_template
    WithCompanyCharts <- TRUE
  }else {
    return()
  }
  
  ### Creates working directory
  INVESTOR.PATH <- paste0(REPORT.PATH,InvestorName,"/")  
  PORTFOLIO.PATH <- paste0(INVESTOR.PATH,PortfolioName,"/")
  if(!dir.exists(file.path(INVESTOR.PATH))){dir.create(file.path(INVESTOR.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
  if(!dir.exists(file.path(PORTFOLIO.PATH))){dir.create(file.path(PORTFOLIO.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  
  ### Specify Language and Load Report 
  Languagechoose <-  ParameterFile$Languageselect
  # Languagechoose <- "FR"
  GT <- preptranslations("Graph",GraphTranslation, Languagechoose,Startyear)
  # RT <- preptranslations("Report",ReportTranslation, Languagechoose, Startyear)
  
  LANGUAGE.PATH <- paste0(PORTFOLIO.PATH,Languagechoose,"/")
  if(!dir.exists(file.path(LANGUAGE.PATH))){dir.create(file.path(LANGUAGE.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}
  setwd(LANGUAGE.PATH)
  
  ### Loops through graphs and report generation
  
  PrintPlot = FALSE
  SecAnalysis <- SectorDataAnalysis()
   
  if (nrow(EQCombin)+nrow(CBCombin) >0){ 
    tryCatch({
      
      #######################
      ### CA TEMPLATE #######
      #######################
     
     
      #Introduction
      analysed_summary("01") #trish's overview "pie chart"
      Overview_portfolio_sector_stack("02")
      
      
      if (HasEquity) {
        carsten_metric_chart("04", "EQ")
        sector_techshare("09","EQ","All", Startyear+5)
        Fossil_Distribution("11", "EQ")
        
        
        #5 Year Trajectory
        Graph246_new("22", "EQ","CoalCap")
        Graph246_new("23", "EQ" ,"RenewablesCap")
        Graph246_new("24", "EQ" ,"GasCap")
        Graph246_new("25", "EQ", "NuclearCap")
        Graph246_new("26","EQ", "Oil")
        Graph246_new("27","EQ", "Gas")
        Graph246_new("28","EQ", "ICE")
        Graph246_new("29","EQ", "Electric")
        
      }
      
      if (HasDebt) {      
        carsten_metric_chart("05", "CB")
        sector_techshare("10","CB","All", Startyear+5)
        Fossil_Distribution("12", "CB")  
        # Risk_Distribution("13", "CB")
        
        #5 Year Trajectory
        Graph246_new("14", "CB","CoalCap")
        Graph246_new("15", "CB" ,"RenewablesCap")
        Graph246_new("16", "CB" ,"GasCap")
        Graph246_new("17", "CB", "NuclearCap")
        Graph246_new("18","CB", "Oil")
        Graph246_new("19","CB", "Gas")
        Graph246_new("20","CB", "ICE")
        Graph246_new("21","CB", "Electric")
      }
  
      if (HasDebt ) {
        if (PortSummary$HasPower.CB){
          company_techshare("32", 10, "CB", "Power")
        }
        if (PortSummary$HasAuto.CB) {
          company_techshare("34", 10, "CB", "Automotive")
        }
      }
      Oilshare("38", 7, "CB")
      company_og_buildout("36", 7, "CB")
      
      if (HasEquity) {
        if (PortSummary$HasPower.EQ){
          company_techshare("33", 10, "EQ", "Power")
        }
        if (PortSummary$HasAuto.EQ) {
          company_techshare("35", 10, "EQ", "Automotive")
        }
      }
      Oilshare("39", 7, "EQ")
      company_og_buildout("37", 7, "EQ")
      HasCarbonBudget <- carboninout("40", 7, "EQ")
      
      figurelist <- list.files(getwd(),pattern=c("\\.png$"), full.names = FALSE)
      writeLines(figurelist,"FigureList.txt")
     
      # Report Generation
      CAReport()
      
    },error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }else{
      print (paste0(PortfolioNameLong," has no Equity and Bond Data"))
    }
  
  # 
  # write.csv(EQCompProdSnapshot, paste0("EQCompProdSnapshot_",PortfolioNameLong,".csv"),row.names = FALSE, na="")
  # write.csv(EQPortSnapshot,paste0("EQPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  # write.csv(CBCompProdSnapshot,paste0("CBCompProdSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
  # write.csv(CBPortSnapshot,paste0("CBPortSnapshot_",PortfolioNameLong,".csv"), row.names = FALSE, na = "")
}
