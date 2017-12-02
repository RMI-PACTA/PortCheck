#Load packages
library(dplyr)
library(reshape2)


# --- DATE ---   |  --- Editor ---  | --- Version Name --- | --- Edits / Adds / Changes / Bugfixes ---
# 2017 - 04 - 21 |        KH        |          1           | First version - loading all fund holdings and aditional information and binding it with input data (ISINs)



### ###########################################################################
### Set up 2DII Env 
### ###########################################################################

if (!exists("TWODII.CONSTS")) {
  ### 2dii-init should be run once, before any/all 2 Degrees R code.  Check for this.
  print("/// WARNING: 2DII DEV NAMES AND PATHS NOT INITIALIZED.  Run Common/2dii-init.R and try again.")
} 

### ###########################################################################
### Set up Project Env 
### ###########################################################################

### this file sourced at top of all PortCheck scripts
### this defines any project constants and functions
### and will also source an override file if it exists
source(paste0(PORTCHECK.CODE.PATH, "proj-init.R"))
imp("Starting DataImport.R Script")


### ###########################################################################
### READ BATCH-SPECIFIC PARAMETERS 
### ###########################################################################

### declare these vars so we know they will be important
### makes the code easier to understand - they're not a surprise later

param.list <- c("BATCH.NAME","BENCHMARK.REGION",
                "COMPANYDOM.REGION","SCENARIO","START.YEAR",
                "FIN.DATA.DATE", "CLIENT.NAME")

### sets as NA initially
ignore <- sapply(param.list, function(x) assign(x, NA, envir = globalenv()))

### pick the file and set the params
ParameterFile <- ReadParameterFile()
SetParameters(ParameterFile)              

### save the values so we can see them
params <- sapply(param.list, function(x) eval(parse(text=x)))
imp("Loaded Parameters")
lp(params)


### ###########################################################################
### SET UP ANY OTHER VARS BASED ON PARAMS
### ###########################################################################

### Finish setting up paths based on what was in the Parameter file


### ClientName / BatchName / RunNumber -- Three levels of ways to save a POrtCheck output
CLIENT.PORTS.PATH <- paste0(PORTS.PATH,CLIENT.NAME,"/")
BATCH.PORTS.PATH <- paste0(CLIENT.PORTS.PATH,BATCH.NAME,"/")

### which financial data to use based on date
BATCH.FIN.DATA.PATH <- paste0(FIN.DATA.PATH,FIN.DATA.DATE,"/PORT/")

if(!dir.exists(file.path(BATCH.PORTS.PATH))){
  dir.create(file.path(BATCH.PORTS.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")
}  
lp(show.consts())


### ###########################################################################
### CONSTANTS USED IN THIS FILE
### ###########################################################################

### If you want to skip look through, set this to 0
FUND.LOOK.THROUGH <- 1

### financial data fields used
FIN.FIELDS <- c("ISIN","Ticker", "Security.Type", "SharePrice","ICB.Subsector.Name","Group", "Subgroup","Name")

### one way to classify asset class
GROUPS.NOT.EQUITY <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Debt Fund","Multi-National","Commodity Fund", "Real Estate Fund","Alternative Fund","Money Market Fund", "","Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")
Subgroups_notBonds <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Supranational Bank","US Municipals","FGLMC Single Family 30yr","FGLMC Single Family 15yr","GNMA Single Family 30yr","FNMA Single Family 30yr","FNMA Single Family 15yr","Export/Import Bank","Regional Authority","Regional Agencies", "Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")


### ###########################################################################
### LOAD EXTERNAL DATA
### ###########################################################################

ExchRates <- read.csv(paste0(PROC.DATA.PATH,"Currencies.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
names(ExchRates) <- c("Currency","Abbr","Ex.Rate")

### financial data from bloomberg
### when we call this function we assume NO DUPLICATE ISINs
Fin.Data <- load.financial.data()
assert(!any(duplicated(Fin.Data$ISIN)), "Duplicate ISINs in financial data")
### this select_ notation with ".dots" is jsut the dplyr way that you select fields that
### are stored in a vector
Fin.Data <- Fin.Data %>% select_(.dots=FIN.FIELDS)

### fund database
Fund.Data <- load.fund.data()
assert(!any(Fund.Data$FundCoverage > 100), "Fund Coverage > 100")


### ###########################################################################
### LOAD PORTFOLIO DATA
### ###########################################################################

port <- read.csv(paste0(BATCH.PORTS.PATH,BATCH.NAME,"_Input.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
check.rows <- nrow(port)
check.ISINs <- length(unique(port$ISIN))
### tag each row with a unique ID so we make sure we don't lose any
port$Import.ID <- rownames(port)
REQUIRED.PORT.COLS <- c("Import.ID", "InvestorName", "PortfolioName", "ISIN", 
                        "NumberofShares", "MarketValue", "Currency")

### check for required input fields
### if they don't exist, this script stops
if (length(setdiff(REQUIRED.PORT.COLS, names(port))) != 0) {
  warn("Missing Required Columns:")
  lp(paste0(REQUIRED.PORT.COLS))
  stop("XXX Missing Required Columns")
}

### save for later
init.cols <- names(port)
extra.cols <- setdiff(init.cols, REQUIRED.PORT.COLS)

### ###########################################################################
### MARK INVALID DATA
### ###########################################################################

### clean the data
### any non-numbers will be NA
port$MarketValue <- as.numeric(port$MarketValue)
port$NumberofShares <- as.numeric(port$NumberofShares)

### if we have neither Number of Shares nor MarketValue
port$err.noHolding <- ifelse(is.na(port$NumberofShares), 
                              ifelse(is.na(port$MarketValue), 1, 0), 0)
### missing currency
port$err.noCurr <- ifelse(is.na(port$Currency) | port$Currency=="", 1, 0)
### Get exchange rates
port <- left_join(port, ExchRates %>% select(Abbr, Ex.Rate),
                  by=c("Currency"="Abbr"))
assert(nrow(port)==check.rows, "Duplicates added when merging with currency rates")

### missing rate
port$err.noExRate <- ifelse(is.na(port$Ex.Rate), 1, 0)

### missing ISINs
port$err.noISIN <- ifelse(is.na(port$ISIN) | port$ISIN=="", 1,0)

### negative values
port$err.negMV <- ifelse(!is.na(port$MarketValue) & port$MarketValue < 0, 1, 0)
port$err.negShares <- ifelse(!is.na(port$NumberofShares) & port$NumberofShares < 0, 1, 0)


### ###########################################################################
### INITIAL CALCULATIONS
### ###########################################################################

### this could be NA based on either MarketValue or Rate
port$ValueUSD.Init <- port$MarketValue * port$Ex.Rate
check.valueUSD <- sum(port$ValueUSD.Init, na.rm=TRUE)


### ###########################################################################
### MERGE WITH FINANCIAL DATA
### ###########################################################################

port.fin <- left_join(port, Fin.Data, by=("ISIN"))
assert(nrow(port.fin)==check.rows, "Duplicates added when merging with financial data")

### one kind of invalid data
port.fin$err.noFinData <- ifelse(is.na(port.fin$Name), 1,0) 

### calculate a ValueUSD in case we don't have one
port.fin$ValueUSD.Calc <- port.fin$SharePrice * port.fin$NumberofShares
port.fin$ValueUSD <- ifelse(is.na(port.fin$ValueUSD.Init), 
                                  port.fin$ValueUSD.Calc, 
                                  port.fin$ValueUSD.Init)

### one more kind of invalid data
port.fin$err.noValue <- ifelse(is.na(port.fin$ValueUSD),1,0)

### add a column that sums the error columns - we can just check this one
### to see if row is invalid
### this says to select the error columns and put their sum into "err.invalid"
port.fin <- port.fin %>% mutate(err.invalid=rowSums(.[grep("err",names(port))]))

table(port.fin$err.invalid)
table(port.fin$err.noISIN)
table(port.fin$err.noValue)
table(port.fin$err.noValue, port.fin$err.noISIN)


### ###########################################################################
### CLASSIFY PORT
### ###########################################################################

GROUPS.DEBT <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,
                 "Municipal-City" ,"Municipal-County","Supranational Bank","US Municipals",
                 "FGLMC Single Family 30yr","FGLMC Single Family 15yr","GNMA Single Family 30yr",
                 "FNMA Single Family 30yr","FNMA Single Family 15yr","Export/Import Bank","Regional Authority",
                 "Regional Agencies", "Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine",
                 "WL Collat CMO Other","WL Collat CMO Sequential","Automobile ABS","WL Collateral CMO",
                 "Regional(state/provnc)","Commercial MBS")

SEC.TYPE.FUNDS <- c("ETF", "Closed End Fund", "Mutual Fund")


### if we didn't match with Financial Data, we can't make this decision 
### about what it is, we have no basis.  So those rows are NA
### If we have financial data, then we start with default of Unknown and classify
### based on "positive" signals from there
port.fin$Fin.Asset.Type <- ifelse(port.fin$err.noFin==1, NA, "Unknown")
### equity indicators - if these exist, we maark it as Equity
### if not, leave as "Unknown"
port.fin$Fin.Asset.Type <- ifelse(port.fin$ICB.Subsector.Name != "" | 
                                      port.fin$Security.Type=="Common Stock" |
                                        port.fin$Security.Type=="Global Depositary Receipt",  
                                      "EQUITY", port.fin$Fin.Asset.Type)
### debt indicators
port.fin$Fin.Asset.Type <- ifelse(port.fin$Name==port.fin$Ticker, 
                                      "DEBT", port.fin$Fin.Asset.Type)
port.fin$Fin.Asset.Type <- ifelse(port.fin$Group %in% GROUPS.DEBT, 
                                      "DEBT", port.fin$Fin.Asset.Type)

### fund indicators
port.fin$Fin.Asset.Type <- ifelse(port.fin$Security.Type %in% SEC.TYPE.FUNDS, 
                                      paste0(port.fin$Fin.Asset.Type, " FUND"), 
                                      port.fin$Fin.Asset.Type)

### mark that this is a direct holding
port.fin$Holding.Type <- "Direct Holding"


### ###########################################################################
### FUND LOOK THROUGH IF NEEDED
### ###########################################################################

non.fin.cols <- c("ISIN", subset(names(port.fin), !(names(port.fin) %in% FIN.FIELDS)))

if (FUND.LOOK.THROUGH == 1) {
  
  #All Instruments (R-pull with Port-Weight for both EQY & CBonds):
  #### inner join only returns the matching rows
  port.lt <- inner_join(port.fin %>% select_(.dots=non.fin.cols), Fund.Data, 
                                     by=c("ISIN"="FundISIN"))
  funds.check.rows <- nrow(port.lt)
  
  ### add financial data
  port.lt <- left_join(port.lt, fin.data, by=c("HoldingISIN"="ISIN"))
  assert(nrow(port.lt)==funds.check.rows, "Duplicates added when merging funds with financial data")

  ### Figure out the ValueUSD - that is the outut of the look-through
  ### based on "ValueUnit", two ways to possibly do this
  ### because Value could be either a PortWeight or SharesUSD
  port.lt$Fund.ValueUSD <- ifelse(port.lt$ValueUnit=="PortWeight",
                             port.lt$ValueUSD * port.lt$value / 100,
                             port.lt$ValueUSD * port.lt$value * port.lt$SharePrice)
  ### !!! or NA?? TAJ Don't understand what FundCoverage is.  Some is NA.
  port.lt$err.fundCov <- ifelse(port.lt$FundCoverage > 100, 1,0)
  port.lt$err.fundNoFinData <- ifelse(is.na(port.lt$Name), 1,0)
  
  matchFunds <- unique(port.lt$ISIN)
  port.fin.noMatchFunds <- subset(port.fin, !ISIN %in% matchFunds)
  assert(check.ISINs==(length(unique(port.fin.noMatchFunds$ISIN)) + length(matchFunds)))
  
  ### if we have matched fund holdings, use the holding ISIN and calcualted value
  port.lt$Holding.Type <- "Fund Holding"
  port.lt$ISIN <- port.lt$HoldingISIN
  port.lt$ValueUSD <- port.lt$Fund.ValueUSD
  port.lt <- port.lt %>% select(-HoldingISIN, -Fund.ValueUSD, -value, -ValueUnit, -FundCoverage)
  
  #setdiff(names(port.lt), names(port.fin))
  
  ### put them together -- port.lt has 2 additional fund error columns
  ### these cols will be NA for the port.fin rows
  port.all <- bind_rows(port.fin.noMatchFunds, port.lt)
  assert(length(unique(port$Import.ID))==length(unique(port.all$Import.ID)))

} else {
  
  ### no look through; assume already done
  
  
}

sum(port.all$ValueUSD, na.rm=TRUE)
sum(port$ValueUSD, na.rm=TRUE)


# 3) create overview file for meta analysis (a)AUM total, negative values total, AUM without ISIN vs with ISINs, AUM False ISINs (position without valueUSD vs positions with valueUSD (sum this) vs AUM assessable (b) Financial instrumetn split, (c) funds vs direct, etc.
#PortInputNAISINs <- subset(port, ISIN %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(ISIN))
#port$ISIN [port$ISIN  %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(port$ISIN) ] <- "NA_ISIN_Input"



#------------
# Merge portfolio with financial data and prepare for fund-look-through (e.g. calculate USD-Value held of each security)
#------------

# PortfolioData_w_BBG <- merge(BBG_Data_sub,PortfolioData, by = "ISIN", all.x = FALSE, all.y = TRUE)
# PortfolioData_wo_BBG <- unique(subset(PortfolioData_w_BBG,is.na(Name), select = c("ISIN")))
# PortfolioData_wo_BBG2 <- subset(PortfolioData_w_BBG,is.na(Name))

# PortfolioData_w_BBG$ValueType <- "NumberofShares"
# PortfolioData_w_BBG$ValueType[PortfolioData_w_BBG$MarketValue != 0 & !is.na(PortfolioData_w_BBG$MarketValue)] <- "MarketValue"

# # Calculate ValueUSD (AUMs in each single fund)
# PortfolioData_w_BBG$Position <- PortfolioData_w_BBG$ValueUSD
# PortfolioData_w_BBG$Position[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] <- PortfolioData_w_BBG$NumberofShares[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] 
# PortfolioData_w_BBG$ValueUSD[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] <- 
#   PortfolioData_w_BBG$SharePrice[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] * PortfolioData_w_BBG$NumberofShares[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0]
# 
# Test <- subset(PortfolioData_w_BBG, is.na(ValueUSD))
# write.csv(Test, "InputWithoutBBGInformation_missingPorts.csv", row.names = FALSE)

# PortfolioData_w_BBG$ISIN[PortfolioData_w_BBG$ISIN %in% Test$ISIN & PortfolioData_w_BBG$ISIN != "NA_ISIN_Input" & (is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0)] <- "NA_ISIN_BBG_Data"
# PortSizeCheck1 <- sum(PortfolioData_w_BBG$ValueUSD, na.rm = TRUE)

# Portfolio Meta Analysis
# PortfolioEntries <- aggregate(ISIN ~ InvestorName + PortfolioName, data = PortfolioData_w_BBG, FUN = length)
# PortfolioEntries <- rename(PortfolioEntries, c("ISIN" = "NrOfPosition"))
# PortfolioSizes <- aggregate(PortfolioData_w_BBG["ValueUSD"], by  = PortfolioData_w_BBG[,c("InvestorName","PortfolioName")],FUN = sum, na.rm = TRUE)
# PortfolioSizes <- rename(PortfolioSizes, c("ValueUSD" = "PortfolioSizeUSD"))
# Test <- aggregate(ISIN ~ InvestorName + PortfolioName, data = Test, FUN = length)
# Test <- rename(Test,c("ISIN" = "NrOfPositionsWOValueUSD"))
# PortfolioSizes <- merge(merge(PortfolioSizes, PortfolioEntries, by = c("InvestorName","PortfolioName"), all.x = TRUE),Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)

#-------------
# # Merge Portfolio Data with fund data for the lookthrough and calculate owned holdings of Portfolios
#------------
#All Instruments (R-pull with Port-Weight for both EQY & CBonds):
Portfolio_LookThrough <- merge(Fund_Data, subset(PortfolioData_w_BBG, select = c("ISIN", "ValueUSD", "PortfolioName","InvestorName")),  by.y = "ISIN", by.x = "FundISIN")
# FundCoverage <- rename(unique(subset(Portfolio_LookThrough, select = c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage"))), c("FundCoverage" = "FundCoverageMS"))
Portfolio_LookThrough <- merge(Portfolio_LookThrough, unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name","SharePrice"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
Portfolio_LookThrough$Position <- Portfolio_LookThrough$ValueUSD * Portfolio_LookThrough$value / 100
Portfolio_LookThrough$Position[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] <- Portfolio_LookThrough$ValueUSD[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$value[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$SharePrice[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"]
Portfolio_LookThrough <- subset(Portfolio_LookThrough, FundCoverage <= 100)
# Portfolio_LookThrough <- merge(subset(Portfolio_LookThrough, select = -c (value,ValueUnit)), unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
Portfolio_LookThroughCovered <- subset(Portfolio_LookThrough, !is.na(Name))
FundCoverage <- rename(aggregate(Portfolio_LookThroughCovered["Position"], by = Portfolio_LookThroughCovered[,c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage")],FUN = sum, na.rm = TRUE),c("FundCoverage" = "FundCoverageMS"))
FundCoverage$FundCoverageBBG <- FundCoverage$Position / FundCoverage$ValueUSD
FundCoveragePortfolioLevel <- ddply(FundCoverage,.(InvestorName,PortfolioName),summarize, USDinFunds = sum(ValueUSD,na.rm = TRUE), USDcovered = sum(Position,na.rm = TRUE))
FundCoveragePortfolioLevel$Coverage <- FundCoveragePortfolioLevel$USDcovered / FundCoveragePortfolioLevel$USDinFunds

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
if(nrow(MissingISINsLookThrough) > 0){MissingISINsLookThrough$Type <- "MissingISINsFundsLookThrough"

MissingISINs <- rbind(PortfolioData_wo_BBG, MissingISINsLookThrough)}else{MissingISINs <- PortfolioData_wo_BBG}

PortfolioData_Funds <- subset(PortfolioData_w_BBG, ISIN %in% Portfolio_LookThrough$FundISIN)
FundsBBG <- subset(PortfolioData_w_BBG, Security.Type %in% c("ETF", "Closed End Fund", "Mutual Fund") | Sector == "Funds", select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name", "Group"))  

FundMetaanalysis <- merge(FundsBBG, rename(subset(FundCoverage,select = c("InvestorName", "PortfolioName", "FundISIN", "FundCoverageMS", "Position", "FundCoverageBBG")), c("Position" = "PositionsCovered")), by.x = c("InvestorName", "PortfolioName", "ISIN"), by.y = c("InvestorName", "PortfolioName", "FundISIN"), all = TRUE)
FundMetaanalysisPortLevel <- ddply(FundMetaanalysis,.(InvestorName,PortfolioName),summarize, FundsUSD = sum(ValueUSD,na.rm = TRUE), FundsCoveredUSD = sum(PositionsCovered, na.rm = TRUE))
FundMetaanalysisPortLevel$Coverage <- FundMetaanalysisPortLevel$FundsCoveredUSD / FundMetaanalysisPortLevel$FundsUSD
PortfolioMetaAnalysis <- merge(PortfolioSizes,FundMetaanalysisPortLevel, by = c("InvestorName", "PortfolioName"), all = TRUE)

FundsCovered <- subset(PortfolioData_Funds, select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name"))

FundsWithMissingBBGData <- subset(FundsCovered, is.na(ValueUSD), select = c("ISIN"))
if(nrow(FundsWithMissingBBGData) > 0){
  FundsWithMissingBBGData$QTY <- 1
  FundsWithMissingBBGData$Date <- "31-12-2016"
  # write.csv(FundsWithMissingBBGData, "BBG-Look-up-needed.csv",row.names = FALSE)
}

PortfolioData_w_BBG_test <- subset(PortfolioData_w_BBG,!is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)

if(is.null(length(setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))))){
  print("ISINS GETTING LOST!! CHECK LOSTISINS")
  LOSTISINS <- setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))
}

PortfolioData_w_BBG <- subset(PortfolioData_w_BBG, !is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)

# Create the equity portfolio input files for the fund analysis
Portfolio <- subset(PortfolioData_w_BBG, select = c("ISIN", "Name", "Security.Type", "ValueUSD", "PortfolioName","InvestorName"))
Portfolio$HoldingType <- "Direct Holding"
Portfolio_Funds <- subset(Portfolio_LookThrough, select = c("HoldingISIN", "Name", "Security.Type", "Position", "PortfolioName","InvestorName"))
Portfolio_Funds <- rename(Portfolio_Funds, c("HoldingISIN" = "ISIN", "Position" = "ValueUSD"))
# Portfolio_Funds_summed <- ddply(Portfolio_Funds,.(ISIN, Name, Security.Type, PortfolioName, InvestorName), summarise, Position = sum(Position, na.rm = TRUE))
Portfolio_Funds_summed <- aggregate(Portfolio_Funds["ValueUSD"], by=Portfolio_Funds[,c("ISIN", "Name", "Security.Type", "PortfolioName", "InvestorName")], FUN=sum, na.rm = TRUE)
Portfolio_Funds_summed$HoldingType <- "Fund Holding"

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
DebtData <- read.csv(paste0(BATCH.FIN.DATA.PATH,"Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# Rename Total row as to not confuse it with TOTAL SA...
DebtData$Co..Corp.TKR[DebtData$Co..Corp.TKR == "Total"] <- "TotalDebt"
GovBanksSupraNat <- subset(DebtData, (DebtData$Government.Development.Banks !=0 | DebtData$Supranationals != 0) & DebtData$Co..Corp.TKR != "TotalDebt") 
DebtData <- subset(DebtData, !DebtData$Co..Corp.TKR %in% GovBanksSupraNat$Co..Corp.TKR)


# TotalPortfolio <- read.csv(paste0(BATCH.FIN.DATA.PATH,"FinancialData.csv"),stringsAsFactors=FALSE,strip.white=TRUE)

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
setwd(paste0(BatchLocation))

write.csv(OverviewPiechartDataFinal,paste0(BATCH.NAME,"Portfolio_Overview_Piechart.csv"),row.names = FALSE, na = "")
write.csv(MissingISINs,paste0(BATCH.NAME,"Missing_BBG-Data.csv"), row.names = FALSE, na = "")
write.csv(PortfolioData_w_BBG,paste0(BATCH.NAME,"PortfolioData_w_BBG-Info.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio,paste0(BATCH.NAME,"Port.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio_EQY,paste0(BATCH.NAME,"Port_EQY.csv"),row.names = FALSE, na = "")
write.csv(TotalPortfolio_Bonds,paste0(BATCH.NAME,"Port_Bonds.csv"),row.names = FALSE, na = "")
write.csv(FundCoverage,paste0(BATCH.NAME,"Port_ListofFunds.csv"),row.names = FALSE, na = "")
write.csv(PortfolioMetaAnalysis,paste0(BATCH.NAME,"Portfolio_Metaanalysis.csv"),row.names = FALSE, na = "")

# Temp save for bonds
UserName <- Clare
bondlocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/2DPORTFOLIO/PortfolioCheck/Data/Finance Reg Data/PortfolioData/")
write.csv(TotalPortfolio_Bonds,paste0(bondlocation,BATCH.NAME,"Port_Bonds.csv"),row.names = FALSE, na = "")


