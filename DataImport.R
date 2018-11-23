#This project was funded by the European Commission through LIFE program under grant: LIFE16 GIC/FR/000061 - PACTA)
# Fund-Look Through for portfolio data (Using output of Fund-Look-through_preparation)
# 17/04/21 Klaus Hagedorn

# rm(list=ls())

#Load packages
library(plyr)
library(reshape2)

#Version - Control

# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 04 - 21 |        KH        |          1           | First version - loading all fund holdings and aditional information and binding it with input data (ISINs)



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
Scenario <- NA
Startyear <- NA

ParameterFile <- ReadParameterFile(PROC.DATA.PATH)
### fill up those variables
SetParameters(ParameterFile)              # Sets BAtchName, Scenario, BenchmarkRegion etc. 
print("*** STARTING SCRIPT with PARAMETERS:")
print(ParameterFile)


#-------------
# Set Input / OUtput Locations Based on parameter File Input
#------------

### Finish setting up paths based on what was in the Parameter file
PORT.FIN.DATA.PATH <- paste0(FIN.DATA.PATH,ParameterFile$DateofFinancialData,"/PORT/")
# OutputLocation <- paste0(PORT.DATA.PATH,ParameterFile$ProjektName,"/")
# DataFolder <- paste0(DATA.PATH,"/01_ProcessedData/")

PROJECT.PATH <- paste0(PORTS.PATH,ParameterFile$ProjektName,"/")
if(!dir.exists(file.path(PROJECT.PATH))){dir.create(file.path(PROJECT.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  

BATCH.PATH <- paste0(PROJECT.PATH,BatchName,"/")
if(!dir.exists(file.path(BATCH.PATH))){dir.create(file.path(BATCH.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  



#-------------
# Read in all data (Fund data, portfolio data and financial data)
#------------
#-#Read in portfolio data (including Fund-ISINs)
PortfolioData <- read.csv(paste0(BATCH.PATH,BatchName,"_Input.csv"),stringsAsFactors=FALSE,strip.white=TRUE)

PortfolioData <- rename(PortfolioData, c("Num.Shares"= "NumberofShares","Market.Value"="MarketValue", "Portfolio.Name"="PortfolioName"),warn_missing = F)
# PortfolioData$InvestorName <- "California Insurances"

MissingColnames <- setdiff(c("MarketValue","Currency","NumberofShares"), colnames(PortfolioData))
if(length(MissingColnames) > 0){PortfolioData[,MissingColnames] <- NA}

PortfolioData$MarketValue <- as.numeric(PortfolioData$MarketValue)
PortfolioData$NumberofShares <- as.numeric(PortfolioData$NumberofShares)

PortInput <- PortfolioData

### Bind in Currencies 
ExchRates <- read.csv(paste0(PROC.DATA.PATH,"Currencies.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
MissingCurrencies <- data.frame(MissingCurrencies = setdiff(unique(PortfolioData$Currency), ExchRates$Currency_abbr))
PortfolioData <- merge(PortfolioData, subset(ExchRates, select = c("Currency_abbr","ExchangeRate_31122016")), by.x = "Currency", by.y = "Currency_abbr", all.x = TRUE, all.y = FALSE)
PortfolioData$ValueUSD <- PortfolioData$MarketValue * PortfolioData$ExchangeRate_31122016

PortSizeCheck0USD <- sum(PortfolioData$ValueUSD, na.rm = TRUE)

### Remove Negative Values
NegativeValues <- subset(PortfolioData, !(!(MarketValue < 0 & (is.na(NumberofShares) | NumberofShares == 0)) & !(NumberofShares < 0 & (is.na(MarketValue) | MarketValue == 0 ))))
NegativeValues <- ddply(NegativeValues,.(ISIN, Currency, PortfolioName, InvestorName),summarize, NumberofShares = sum(NumberofShares,na.rm = TRUE), MarketValue = sum(MarketValue,na.rm = TRUE), ValueUSD = sum(ValueUSD,na.rm = TRUE))

PortfolioData <- subset(PortfolioData, !(MarketValue <= 0 & (is.na(NumberofShares) | NumberofShares == 0)) & !(NumberofShares <= 0 & (is.na(MarketValue) | MarketValue == 0 )))

# PortfolioData <- subset(PortfolioData, !(MarketValue == 0 & is.na(NumberofShares)) & !(NumberofShares == 0 & is.na(MarketValue)))
PortfolioData <- ddply(PortfolioData,.(ISIN, Currency, PortfolioName, InvestorName),summarize, NumberofShares = sum(NumberofShares,na.rm = TRUE), MarketValue = sum(MarketValue,na.rm = TRUE), ValueUSD = sum(ValueUSD,na.rm = TRUE))
PortInputPositiveValuesOnly <- PortfolioData

PortSizeCheck0 <- sum(PortfolioData$MarketValue, na.rm = TRUE)

# 3) create overview file for meta analysis (a)AUM total, negative values total, AUM without ISIN vs with ISINs, AUM False ISINs (position without valueUSD vs positions with valueUSD (sum this) vs AUM assessable (b) Financial instrumetn split, (c) funds vs direct, etc.
PortInputNAISINs <- subset(PortfolioData, ISIN %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(ISIN))
PortfolioData$ISIN [PortfolioData$ISIN  %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(PortfolioData$ISIN) ] <- "NA_ISIN_Input"

PortSizeCheck01 <- sum(PortfolioData$ValueUSD, na.rm = TRUE)

if("Name" %in% colnames(PortfolioData)){PortfolioData <- rename(PortfolioData,c("Name" = "Name_InputPort"))}

### Read in fund look-through data
Fund_Data <- read.csv(paste0(FundDataLocation,"FundLookThroughData.csv"),stringsAsFactors=FALSE,strip.white=TRUE) 
Fund_Data_EQY <- read.csv(paste0(FundDataLocation,"FundLookThroughData_EQY.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
Fund_Data_CBonds <- read.csv(paste0(FundDataLocation,"FundLookThroughData_Bonds.csv"),stringsAsFactors=FALSE,strip.white=TRUE)

### Read in financial data
# BBG_Data <- read.csv(paste0(FinancialPROC.DATA.PATH,"FinancialData.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
BBG_Data <- read.csv(paste0(PORT.FIN.DATA.PATH,"FinancialData_20180131.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
BBG_Data <- rename(BBG_Data, c( "Mkt.Val..P." = "SharePrice"))
BBG_Data_sub <- subset(BBG_Data, ! is.na(BBG_Data$ISIN) & ISIN != "")
BBG_Data_sub <- BBG_Data_sub[!duplicated(BBG_Data_sub$ISIN),]

### Test if there are duplicates left in the financial database
ISINCount <- as.data.frame(table(BBG_Data_sub$ISIN))
BBG_Data_DupISINs <- subset(ISINCount, Freq > 1, select = "Var1")
BBG_Data_dups <- subset(BBG_Data, BBG_Data$ISIN %in% BBG_Data_DupISINs$Var1, select = c("ISIN","SharePrice"))

### Calculate the mean share price for duplicated holdings and replace in the unique BBG_Data dataframe
BBG_Data_dups <- aggregate(BBG_Data_dups["SharePrice"],by=list(BBG_Data_dups$ISIN), FUN=mean)
colnames(BBG_Data_dups) <- c("ISIN","SharePriceMean") 
BBG_Data_sub <- merge(BBG_Data_sub, BBG_Data_dups, by="ISIN",all.x = TRUE)
BBG_Data_sub$SharePrice[BBG_Data_sub$ISIN %in% BBG_Data_dups$ISIN]<- BBG_Data_sub$SharePriceMean[BBG_Data_sub$ISIN %in% BBG_Data_dups$ISIN]
BBG_Data_sub$SharePriceMean <- NULL


#------------
# Merge portfolio with financial data and prepare for fund-look-through (e.g. calculate USD-Value held of each security)
#------------

PortfolioData_w_BBG <- merge(BBG_Data_sub,PortfolioData, by = "ISIN", all.x = FALSE, all.y = TRUE)
PortfolioData_wo_BBG <- unique(subset(PortfolioData_w_BBG,is.na(Name), select = c("ISIN")))
PortfolioData_wo_BBG2 <- subset(PortfolioData_w_BBG, is.na(Name))

PortfolioData_w_BBG$ValueType <- "NumberofShares"
PortfolioData_w_BBG$ValueType[PortfolioData_w_BBG$MarketValue != 0 & !is.na(PortfolioData_w_BBG$MarketValue)] <- "MarketValue"

### Calculate ValueUSD (AUMs in each single fund)
PortfolioData_w_BBG$ValueUSD[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] <- PortfolioData_w_BBG$SharePrice[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] * PortfolioData_w_BBG$NumberofShares[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0]

Test <- subset(PortfolioData_w_BBG, is.na(ValueUSD))
# write.csv(Test, "InputWithoutBBGInformation_missingPorts.csv", row.names = FALSE)

# PortfolioData_w_BBG$ISIN[PortfolioData_w_BBG$ISIN %in% Test$ISIN & PortfolioData_w_BBG$ISIN != "NA_ISIN_Input" & (is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0)] <- "NA_ISIN_BBG_Data"
PortSizeCheck1 <- sum(PortfolioData_w_BBG$ValueUSD, na.rm = TRUE)

### Portfolio Meta Analysis
PortfolioEntries <- aggregate(ISIN ~ InvestorName + PortfolioName, data = PortfolioData_w_BBG, FUN = length)
PortfolioEntries <- rename(PortfolioEntries, c("ISIN" = "NrOfPosition"))
PortfolioSizes <- aggregate(PortfolioData_w_BBG["ValueUSD"], by  = PortfolioData_w_BBG[,c("InvestorName","PortfolioName")],FUN = sum, na.rm = TRUE)
PortfolioSizes <- rename(PortfolioSizes, c("ValueUSD" = "PortfolioSizeUSD"))
Test <- aggregate(ISIN ~ InvestorName + PortfolioName, data = Test, FUN = length)
Test <- rename(Test,c("ISIN" = "NrOfPositionsWOValueUSD"))
PortfolioSizes <- merge(merge(PortfolioSizes, PortfolioEntries, by = c("InvestorName","PortfolioName"), all.x = TRUE),Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)

#-------------
# # Merge Portfolio Data with fund data for the lookthrough and calculate owned holdings of Portfolios
#------------

### Merge Portfolio with Fund Dat
Portfolio_LookThrough <- merge(Fund_Data, subset(PortfolioData_w_BBG, select = c("ISIN", "ValueUSD", "PortfolioName","InvestorName")),  by.y = "ISIN", by.x = "FundISIN")

FundsInPort <- unique(Portfolio_LookThrough$FundISIN)

### If there are funds in the Portfolio - merge in BBG data
if (!is.null(nrow(FundsInPort))){
  Portfolio_LookThrough <- merge(Portfolio_LookThrough, unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name","SharePrice"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
  Portfolio_LookThrough$Position <- Portfolio_LookThrough$ValueUSD * Portfolio_LookThrough$value / 100
  Portfolio_LookThrough$Position[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] <- Portfolio_LookThrough$ValueUSD[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$value[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$SharePrice[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"]
  Portfolio_LookThrough <- subset(Portfolio_LookThrough, FundCoverage <= 100)
  Portfolio_LookThroughCovered <- subset(Portfolio_LookThrough, !is.na(Name))
  
  FundCoverage <- rename(aggregate(Portfolio_LookThroughCovered["Position"], by = Portfolio_LookThroughCovered[,c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage")],FUN = sum, na.rm = TRUE),c("FundCoverage" = "FundCoverageMS"))
  FundCoverage$FundCoverageBBG <- FundCoverage$Position / FundCoverage$ValueUSD
  FundCoveragePortfolioLevel <- ddply(FundCoverage,.(InvestorName,PortfolioName),summarize, USDinFunds = sum(ValueUSD,na.rm = TRUE), USDcovered = sum(Position,na.rm = TRUE))
  FundCoveragePortfolioLevel$Coverage <- FundCoveragePortfolioLevel$USDcovered / FundCoveragePortfolioLevel$USDinFunds
}




### Identify holdings with missing BBG info
PortfolioData_wo_BBG <- subset(PortfolioData_wo_BBG, !ISIN %in% Portfolio_LookThrough$FundISIN)
PortfolioData_wo_BBG2 <- subset(PortfolioData_wo_BBG2, !ISIN %in% Portfolio_LookThrough$FundISIN)
Test <- aggregate(ISIN ~ InvestorName + PortfolioName, data = PortfolioData_wo_BBG2, FUN = length)
Test <- rename(Test,c("ISIN" = "NrOfPositionsWOBBGInformation"))
PortfolioSizes <- merge(PortfolioSizes,Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)
Test <- aggregate(ValueUSD ~ InvestorName + PortfolioName, data = PortfolioData_wo_BBG2, FUN = sum, na.rm = TRUE)
Test <- rename(Test,c("ValueUSD" = "ValueUSDOfPositionsWOBBGWValueUSDInformation"))
PortfolioSizes <- merge(PortfolioSizes,Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)

PortfolioData_wo_BBG$Type <- "MissingISINsPortfolio"
MissingISINsLookThrough <- rename(subset(Portfolio_LookThrough, is.na(Position), select = "HoldingISIN"),c("HoldingISIN" = "ISIN"))

if(nrow(MissingISINsLookThrough) > 0){
  MissingISINsLookThrough$Type <- "MissingISINsFundsLookThrough"
  MissingISINs <- rbind(PortfolioData_wo_BBG, MissingISINsLookThrough)
}else{
  MissingISINs <- PortfolioData_wo_BBG
}

MissingISINs$QTY <- 1
MissingISINs$Date <- "31-12-2016"

### Identify Funds with missing BBG Data
PortfolioData_Funds <- subset(PortfolioData_w_BBG, ISIN %in% Portfolio_LookThrough$FundISIN)
FundsBBG <- subset(PortfolioData_w_BBG, Security.Type %in% c("ETF", "Closed End Fund", "Mutual Fund") | Sector == "Funds", select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name", "Group"))  

if (!is.null(nrow(FundsInPort))){
  FundMetaanalysis <- merge(FundsBBG, rename(subset(FundCoverage,select = c("InvestorName", "PortfolioName", "FundISIN", "FundCoverageMS", "Position", "FundCoverageBBG")), c("Position" = "PositionsCovered")), by.x = c("InvestorName", "PortfolioName", "ISIN"), by.y = c("InvestorName", "PortfolioName", "FundISIN"), all = TRUE)
  FundMetaanalysisPortLevel <- ddply(FundMetaanalysis,.(InvestorName,PortfolioName),summarize, FundsUSD = sum(ValueUSD,na.rm = TRUE), FundsCoveredUSD = sum(PositionsCovered, na.rm = TRUE))
  FundMetaanalysisPortLevel$Coverage <- FundMetaanalysisPortLevel$FundsCoveredUSD / FundMetaanalysisPortLevel$FundsUSD
  PortfolioMetaAnalysis <- merge(PortfolioSizes,FundMetaanalysisPortLevel, by = c("InvestorName", "PortfolioName"), all = TRUE)

  FundsCovered <- subset(PortfolioData_Funds, select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name"))
  FundsWithMissingBBGData <- subset(FundsCovered, is.na(ValueUSD), select = c("ISIN"))
  if(nrow(FundsWithMissingBBGData) > 0){
    FundsWithMissingBBGData$Type <- "FundswithMissingBBGData"
    FundsWithMissingBBGData$QTY <- 1
    FundsWithMissingBBGData$Date <- "31-12-2016"
    MissingISINs <- rbind(MissingISINs,FundsWithMissingBBGData)
    # write.csv(FundsWithMissingBBGData, "BBG-Look-up-needed.csv",row.names = FALSE)
  }
  
  
}else{
  PortfolioMetaAnalysis<- PortfolioSizes
}

### Identify Funds with Missing Holding Data
AllFunds <- unique(subset(PortfolioData_w_BBG, Sector == "Funds", select= c("ISIN")))
MissingFunds <- AllFunds[!AllFunds$ISIN %in% unique(Fund_Data$FundISIN),]
write.csv(MissingFunds,paste0(BatchName,"_MissingFunds.csv"), row.names = F)



PortfolioData_w_BBG_test <- subset(PortfolioData_w_BBG,!is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)

if(is.null(length(setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))))){
  print("ISINS GETTING LOST!! CHECK LOSTISINS")
  LOSTISINS <- setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))
}

PortfolioData_w_BBG <- subset(PortfolioData_w_BBG, !is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)

# Create the equity portfolio input files for the fund analysis
Portfolio <- subset(PortfolioData_w_BBG, select = c("ISIN", "Name", "Security.Type", "ValueUSD", "PortfolioName","InvestorName"))
Portfolio$HoldingType <- "Direct Holding"

if (!is.null(nrow(FundsInPort))){
  Portfolio_Funds <- subset(Portfolio_LookThrough, select = c("HoldingISIN", "Name", "Security.Type", "Position", "PortfolioName","InvestorName"))
  Portfolio_Funds <- rename(Portfolio_Funds, c("HoldingISIN" = "ISIN", "Position" = "ValueUSD"))
  # Portfolio_Funds_summed <- ddply(Portfolio_Funds,.(ISIN, Name, Security.Type, PortfolioName, InvestorName), summarise, Position = sum(Position, na.rm = TRUE))
  Portfolio_Funds_summed <- aggregate(Portfolio_Funds["ValueUSD"], by=Portfolio_Funds[,c("ISIN", "Name", "Security.Type", "PortfolioName", "InvestorName")], FUN=sum, na.rm = TRUE)
  Portfolio_Funds_summed$HoldingType <- "Fund Holding"
}

TotalPortfolio <- Portfolio
if (exists("Portfolio_Funds_summed")==TRUE){
  TotalPortfolio <- rbind(TotalPortfolio,Portfolio_Funds_summed)}
PortSizeCheck2 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)

TotalPortfolio <- merge(TotalPortfolio, subset(BBG_Data_sub, select = c("ISIN","ICB.Subsector.Name","Group", "Ticker", "Subgroup")), by = "ISIN", all.x = TRUE, all.y = FALSE)

Groups_notEQY <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Debt Fund","Multi-National","Commodity Fund", "Real Estate Fund","Alternative Fund","Money Market Fund", "","Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")

TotalPortfolio_EQY <- subset(TotalPortfolio, (!is.na(ICB.Subsector.Name) & ICB.Subsector.Name != "") | Security.Type == "Common Stock" | (Name != Ticker & !Group %in% Groups_notEQY & !is.na(Group)))
TotalPortfolio_EQY <- merge(TotalPortfolio_EQY, subset(BBG_Data_sub, select = c("ISIN","SharePrice")), by = "ISIN", all.x = TRUE, all.y = FALSE)
TotalPortfolio_EQY <- subset(TotalPortfolio_EQY , select = c("InvestorName", "PortfolioName", "ISIN", "ValueUSD"))


#------------
# Sort Debt Data
#------------
# Create the Cbond portfolio input files for the fund analysis
DebtData <- read.csv(paste0(PORT.FIN.DATA.PATH,"Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# Rename Total row as to not confuse it with TOTAL SA...
DebtData$Co..Corp.TKR[DebtData$Co..Corp.TKR == "Total"] <- "TotalDebt"
GovBanksSupraNat <- subset(DebtData, (DebtData$Government.Development.Banks !=0 | DebtData$Supranationals != 0) & DebtData$Co..Corp.TKR != "TotalDebt") 
DebtData <- subset(DebtData, !DebtData$Co..Corp.TKR %in% GovBanksSupraNat$Co..Corp.TKR)


# TotalPortfolio <- read.csv(paste0(FinancialDataFolder,"FinancialData.csv"),stringsAsFactors=FALSE,strip.white=TRUE)

CorpDebtTicker <- colsplit(TotalPortfolio$Ticker, pattern = " ", names = c("COMPANY_CORP_TICKER",2,3))[1]
TotalPortfolio <- cbind(TotalPortfolio,CorpDebtTicker)

Subgroups_notBonds <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Supranational Bank","US Municipals","FGLMC Single Family 30yr","FGLMC Single Family 15yr","GNMA Single Family 30yr","FNMA Single Family 30yr","FNMA Single Family 15yr","Export/Import Bank","Regional Authority","Regional Agencies", "Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")
TotalPortfolio_Bonds <- subset(TotalPortfolio, (Name == Ticker  | Group == "Debt Fund") & !Subgroup %in% Subgroups_notBonds)
TotalPortfolio_Bonds$LoanIndicator <- sub(".* ","",TotalPortfolio_Bonds$Ticker)
TotalPortfolio_Bonds$BondTest <- sapply(TotalPortfolio_Bonds$LoanIndicator, function(x) gregexpr("[[:punct:]]",x))
TotalPortfolio_Bonds <- subset(TotalPortfolio_Bonds, BondTest != "-1" | LoanIndicator == "PERP" | Group == "Debt Fund", select = c("InvestorName", "PortfolioName", "ISIN", "ValueUSD"))

TotalPortfolio$InstrumentType <- "Others"
TotalPortfolio$InstrumentType[TotalPortfolio$ISIN  %in% TotalPortfolio_EQY$ISIN] <- "Equity"
TotalPortfolio$InstrumentType[TotalPortfolio$ISIN  %in% TotalPortfolio_Bonds$ISIN] <- "Bonds"

PortSizeCheck3 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)

# Add the InstrumentType to the PortfolioData_w_BBG
PortfolioData_w_BBG$InstrumentType <- "Others"
PortfolioData_w_BBG$InstrumentType[PortfolioData_w_BBG$ISIN  %in% TotalPortfolio_EQY$ISIN] <- "Equity"
PortfolioData_w_BBG$InstrumentType[PortfolioData_w_BBG$ISIN  %in% TotalPortfolio_Bonds$ISIN] <- "Bonds"


#------------
# Data for the Overview Pie Chart
#------------
TotalPortfolio$ValueUSD <- as.numeric(TotalPortfolio$ValueUSD)
OverviewPiechartData <- aggregate(TotalPortfolio["ValueUSD"], by = TotalPortfolio[,c("InvestorName", "PortfolioName", "HoldingType", "InstrumentType")], FUN = sum, na.rm = TRUE)
OverviewPiechartDatawide <- dcast(OverviewPiechartData, InvestorName + PortfolioName  + HoldingType  ~ InstrumentType, value.var = "ValueUSD")

MissingColumns <-setdiff(c("Equity","Bonds","Others"),colnames(OverviewPiechartDatawide))
if(length(MissingColumns) > 0){OverviewPiechartDatawide[,MissingColumns] <- 0}

OverviewPiechartDatawideTotal <- ddply(OverviewPiechartDatawide,.(InvestorName, PortfolioName),summarize, Equity = sum(Equity,na.rm = TRUE),Bonds = sum(Bonds,na.rm = TRUE),Others = sum(Others,na.rm = TRUE))

OverviewPiechartDatawideTotal$HoldingType <- "All"
OverviewPiechartDatawide <- rbind(OverviewPiechartDatawide,OverviewPiechartDatawideTotal)
OverviewPiechartDataFinal <- merge(OverviewPiechartDatawide,subset(PortfolioSizes, select =c("InvestorName", "PortfolioName", "PortfolioSizeUSD", "ValueUSDOfPositionsWOBBGWValueUSDInformation")),by = c("InvestorName","PortfolioName"), all.x = TRUE)
OverviewPiechartDataFinal <- rename(OverviewPiechartDataFinal, c("Others" = "PositionsWithValue_Ignore4piechart"))
OverviewPiechartDataFinal$Others <- OverviewPiechartDataFinal$PortfolioSizeUSD - OverviewPiechartDataFinal$Equity - OverviewPiechartDataFinal$Bonds

OverviewPiechartDataFinal <- subset(OverviewPiechartDataFinal, HoldingType == "All")
InvestorLevelIDentification <- as.data.frame(table(OverviewPiechartDataFinal$InvestorName))
SinglePorts <- subset(InvestorLevelIDentification, Freq == 1)
OverviewPiechartDataFinal$PortfolioType <- "Portfolio"
OverviewPiechartDataFinal$PortfolioType[OverviewPiechartDataFinal$InvestorName %in% SinglePorts$Var1] <- "Investor"

#create file at investor level
OverviewPiechartDataFinalMPS <- ddply(subset(OverviewPiechartDataFinal, !InvestorName %in% SinglePorts$Var1),.(InvestorName, HoldingType),summarize,PortfolioSizeUSD = sum(PortfolioSizeUSD,na.rm = TRUE), Bonds = sum(Bonds,na.rm = TRUE), Equity = sum(Equity,na.rm = TRUE), Others = sum(Others,na.rm = TRUE))
if(nrow(OverviewPiechartDataFinalMPS)>0){
  OverviewPiechartDataFinalMPS$PortfolioName <- OverviewPiechartDataFinalMPS$InvestorName
  OverviewPiechartDataFinalMPS$PortfolioType <- "InvestorMPs"}

OverviewPiechartDataFinal <- OverviewPiechartDataFinal[,colnames(OverviewPiechartDataFinalMPS)]
OverviewPiechartDataFinal <- rbind(OverviewPiechartDataFinal, OverviewPiechartDataFinalMPS)



#------------
# Print Results To PortfolioData
#------------

### In general we should try avoid doing this, and use paths instead below - CHANGE
# setwd(paste0(BatchLocation))

write.csv(OverviewPiechartDataFinal,paste0(BATCH.PATH,BatchName,"Portfolio_Overview_Piechart.csv"),row.names = FALSE, na = "")
write.csv(PortfolioData_w_BBG,paste0(BATCH.PATH,BatchName,"PortfolioData_w_BBG-Info.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio,paste0(BATCH.PATH,BatchName,"Port.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio_EQY,paste0(BATCH.PATH,BatchName,"Port_EQY.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio_Bonds,paste0(BATCH.PATH,BatchName,"Port_Bonds.csv"),row.names = FALSE, na = "")
write.csv(PortfolioMetaAnalysis,paste0(BATCH.PATH,BatchName,"Portfolio_Metaanalysis.csv"),row.names = FALSE, na = "")

if(!is.null(nrow(MissingISINs))){
  write.csv(MissingISINs,paste0(BATCH.PATH,BatchName,"_",nrow(MissingISINs)-1,"_Missing_BBG-Data.csv"), row.names = FALSE, na = "")
}

if (!is.null(nrow(FundsInPort))){
  write.csv(FundCoverage,paste0(BATCH.PATH,BatchName,"Port_ListofFunds.csv"),row.names = FALSE, na = "")
}

# Temp save for bonds
UserName <- "Clare"
bondlocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/2DPORTFOLIO/PortfolioCheck/Data/Finance Reg Data/PortfolioData/")
write.csv(TotalPortfolio_Bonds,paste0(bondlocation,BatchName,"Port_Bonds.csv"),row.names = FALSE, na = "")


