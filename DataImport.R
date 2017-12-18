#Load packages
library(tidyr)
library(dplyr)
library(scales)


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
### PARAMETERS
### ###########################################################################

### declare these vars so we know they will be important
### makes the code easier to understand - they're not a surprise later

param.list <- c("BATCH.NAME","BENCHMARK.REGION",
                "COMPANYDOM.REGION","SCENARIO","START.YEAR",
                "FIN.DATA.DATE", "CLIENT.NAME")

### sets params as NA initially
ignore <- sapply(param.list, function(x) assign(x, NA, envir = globalenv()))
#params <- sapply(param.list, function(x) eval(parse(text=x)))

### pick the file and set the params
ParameterFile <- ReadParameterFile()
SetParameters(ParameterFile)              # Sets BATCH.NAME, SCENARIO, BenchmarkRegion etc. 
params <- sapply(param.list, function(x) eval(parse(text=x)))

imp("Loaded Parameters")
lp(params)

### If we want to skip look through, set to 0
FUND.LOOK.THROUGH <- 0


### ###########################################################################
### SET UP VARS BASED ON PARAMS
### ###########################################################################

### Finish setting up paths based on what was in the Parameter file - date of 
### financial data and batch name
BATCH.FIN.DATA.PATH <- paste0(FIN.DATA.PATH,FIN.DATA.DATE,"/PORT/")
CLIENT.PORTS.PATH <- paste0(PORTS.PATH,CLIENT.NAME,"/")
BATCH.PORTS.PATH <- paste0(CLIENT.PORTS.PATH,BATCH.NAME,"/")

if(!dir.exists(file.path(BATCH.PORTS.PATH))){
  dir.create(file.path(BATCH.PORTS.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")
}  
lp(show.consts())



### ###########################################################################
### CONSTANTS
### ###########################################################################
KEEP.BBG.OUTPUT.COLS <- c("Name", "SharePrice", "Cnty.of.Dom", "Security.Type","ICB.Subsector.Name","Sector", "Group","Subgroup","Ticker","TICKER_AND_EXCH_CODE", "COMPANY_CORP_TICKER")

UtilitiesICB <- c("Alternative Electricity","Conventional Electricity","Multiutilities")
OilGasICB <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production")
FuturesecsICB <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation")
AutoICB <- c("Automobiles","Commercial Vehicles & Trucks")

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



### ###########################################################################
### LOAD EXTERNAL DATA
### ###########################################################################

ExchRates <- read.csv(paste0(PROC.DATA.PATH,"Currencies.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
names(ExchRates) <- c("Currency","Abbr","Rate")

### financial data from bloomberg
fin.data <- load.CA.financial.data()
fin.data <- fin.data[!duplicated(fin.data$ISIN),]
fin.data$Mapped.Flag <- 1
dim(fin.data)

### fund database
#Fund_Data <- read.csv(paste0(FundDataLocation,"FundLookThroughData.csv"),stringsAsFactors=FALSE,strip.white=TRUE) 
#Fund_Data_EQY <- read.csv(paste0(FundDataLocation,"FundLookThroughData_EQY.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
#Fund_Data_CBonds <- read.csv(paste0(FundDataLocation,"FundLookThroughData_Bonds.csv"),stringsAsFactors=FALSE,strip.white=TRUE)


### ###########################################################################
### LOAD PORTFOLIO DATA
### ###########################################################################

### if its a bond portfolio, NumberofShares will be NA
### if its an equity portfolio

REQUIRED.PORT.COLS <- c("ISIN", "NumberofShares", "MarketValue", "Currency")
port <- read.csv(paste0(BATCH.PORTS.PATH,BATCH.NAME,"_Input.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
check.rows <- nrow(port)
cols.in <- names(port)

### check for required input fields
if (length(setdiff(REQUIRED.PORT.COLS, cols.in)) != 0) {
  warn("Missing Required Columns:")
  lp(paste0(REQUIRED.PORT.COLS))
  stop("XXX Missing Required Columns")
}

### clean the data
### any non-numbers will be NA
port$MarketValue <- as.numeric(port$MarketValue)
port$NumberofShares <- as.numeric(port$NumberofShares)


### ###########################################################################
### MARK INVALID DATA
### ###########################################################################

### if we have neither Number of Shares nor MarketValue
port$err.noHolding <- ifelse(is.na(port$NumberofShares), 
                              ifelse(is.na(port$MarketValue), 1, 0), 0)
### missing currency
port$err.noCurr <- ifelse(is.na(port$Currency) | port$Currency=="", 1, 0)
### Get exchange rates
port <- left_join(port, ExchRates %>% select(Abbr, Rate),
                  by=c("Currency"="Abbr"))
assert(nrow(port)==check.rows, "Duplicates added when merging with currency rates")

### missing rate
port$err.noExRate <- ifelse(is.na(port$Rate), 1, 0)

### missing ISINs
port$err.noISIN <- ifelse(is.na(port$ISIN) | port$ISIN=="", 1,0)

### negative values
port$err.negMV <- ifelse(!is.na(port$MarketValue) & port$MarketValue < 0, 1, 0)
port$err.negShares <- ifelse(!is.na(port$NumberofShares) & port$NumberofShares < 0, 1, 0)

### this could be NA based on either MarketValue or Rate
port$ValueUSD.Init <- port$MarketValue * port$Rate
check.valueUSD <- sum(port$ValueUSD.Init, na.rm=TRUE)


port %>% group_by(PortfolioName, Asset.Type) %>% 
  summarise(tot=sum(MarketValue), comma(tot)) %>% arrange(Asset.Type)


### ###########################################################################
### MERGE WIHT NEW FIN DATA to match more CUSIPS - HACK !!!
### ###########################################################################

### I am doing this because I had a bridging file from CUSIP--> ISIN
### which has more ISINs in it than the fin.data.
### this is temporary
### we should all use he same merging file so I won't have to do this

cusip.data <- fin.data[!duplicated(fin.data$CUSIP),]

port.fin <- left_join(port, cusip.data %>% select(CUSIP, ISIN), by ="CUSIP")
table(is.na(port.fin$ISIN.x), !is.na(port.fin$ISIN.y))
port.fin$ISIN <- ifelse(is.na(port.fin$ISIN.x), port.fin$ISIN.y, port.fin$ISIN.x)
port.fin <- port.fin %>% select(-ISIN.x, -ISIN.y)
assert(nrow(port.fin)==check.rows, "Duplicates added when merging with cUSIP data")

### update
port.fin$err.noISIN <- ifelse(is.na(port.fin$ISIN) | port.fin$ISIN=="", 1,0)

### at this point, we have all the ISINs we can get

### ###########################################################################
### MERGE WITH FINANCIAL DATA
### ###########################################################################

port.fin <- left_join(port.fin, fin.data, by=("ISIN"))
assert(nrow(port.fin)==check.rows, "Duplicates added when merging with financial data")
port.fin <- port.fin %>% rename(CUSIP=CUSIP.x)
port.fin <- port.fin %>% select(-CUSIP.y)


### mark what is mapped
port.fin$err.noBBG <- ifelse(is.na(port.fin$Mapped.Flag), 1,0) 

### pull out the company debt ticker
port.fin <- port.fin %>% separate(Ticker, sep=" ", into="COMPANY_CORP_TICKER", remove=FALSE, extra="drop")

### calculate a ValueUSD in case we don't have one
port.fin$ValueUSD.Calc <- port.fin$SharePrice * port.fin$NumberofShares
port.fin$ValueUSD <- ifelse(is.na(port.fin$ValueUSD.Init), 
                                  port.fin$ValueUSD.Calc, 
                                  port.fin$ValueUSD.Init)

### if we still don't have ValueUSD
port.fin$err.noValue <- ifelse(is.na(port.fin$ValueUSD),1,0)
### missing Country of Domicile
###!! hack for right now - we can get actual Country Doms from BBG
port.fin$Cnty.of.Dom <- ifelse(port.fin$err.noBBG==0 & is.na(port.fin$Cnty.of.Dom), "US", port.fin$Cnty.of.Dom)
port.fin$err.noCountry <- ifelse(is.na(port.fin$Cnty.of.Dom), 1, 0)
### missing SharePrice
##port.fin$err.noSharePrice <- ifelse(is.na(port.fin$SharePrice), 1, 0)


### Group Classification
GROUPS.GOVT <- c("Sovereign","Sovereign Agency", "Municipal", "Municipal-City","Municipal-County","Multi-National","Regional","Regional(state/provnc)",
                 "Export/Import Bank","Regional Authority","Regional Agencies")

GROUPS.ABS <- grep("(ABS|MBS|CMO)", unique(port.fin$Group), value = TRUE)
GROUPS.FUNDS <- grep("(Fund|ETF)", unique(port.fin$Group), value = TRUE)
SECTOR.MORT <- c("Mortgage Securities")

### PortCheck does not take government bonds
port.fin$err.Gov <- ifelse(port.fin$Group %in% GROUPS.GOVT | port.fin$Group1=="Government", 1, 0)
port.fin$err.Mort <- ifelse(port.fin$Sector %in% SECTOR.MORT, 1, 0)
port.fin$err.ABS <- ifelse(port.fin$Group %in% GROUPS.ABS, 1, 0)


### add a column that sums the error columns - we can just check this one
### to see if row is invalid
### this says to select the error columns and put their sum into "err.invalid"
#port.fin <- port.fin %>% mutate(err.invalid=rowSums(.[grep("err",names(port.fin))]))

table(port.fin$err.noBBG, port.fin$err.noISIN)
#subset(port.fin, err.noBBG==1 & err.noISIN==0)
#subset(port.fin, err.noBBG==0 & err.noCountry==1)
table(port.fin$err.Gov==1)
table(port.fin$err.ABS==1)


port.fin %>% group_by(PortfolioName, Asset.Type) %>% 
  summarise(tot=sum(MarketValue), comma(tot)) %>% arrange(Asset.Type)

port.fin %>% group_by(PortfolioName, Asset.Type, err.Gov) %>% 
  summarise(tot=sum(MarketValue), comma(tot)) 
port.fin %>% group_by(PortfolioName, Asset.Type, err.ABS) %>% 
  summarise(tot=sum(MarketValue), comma(tot)) 

### ###########################################################################
### MERGE WITH EQUITY BRIDGE
### ###########################################################################

# ###Change this to the new Bridge file
# EquityBridgeSub <- unique(EquityBridge)
# PortfolioAllPorts <- merge(PortfolioAllPorts,EquityBridgeSub, by.x = "Ticker", by.y = "TICKER_AND_EXCH_CODE",all.x = TRUE)
equity.bridge <- read.csv(paste0(PROC.DATA.PATH,"EquityBridge_2017-06-27.csv"),stringsAsFactors = FALSE, strip.white = TRUE)
equity.bridge <- equity.bridge[!duplicated(equity.bridge$TICKER_AND_EXCH_CODE),]
assert(nrow(port.fin)==check.rows, "Duplicates added when merging with financial data")

port.fin <- left_join(port.fin, equity.bridge, by=c("Ticker"="TICKER_AND_EXCH_CODE"))

#port.fin$err.noFundTicker <- ifelse(port.fin$Asset.Type=="EQUITY" & (is.na(port.fin$EQY_FUND_TICKER) | port.fin$EQY_FUND_TICKER==""), 1, 0)



### ###########################################################################
### FUND LOOK THROUGH IF NEEDED
### ###########################################################################

# if (FUND.LOOK.THROUGH == 1) {
#   
#   #All Instruments (R-pull with Port-Weight for both EQY & CBonds):
#   port.lookthru <- left_join(port.fin %>% select(InvestorName, PortfolioName, ISIN, ValueUSD), Fund_Data, 
#                                      by=c("ISIN"="FundISIN"))
#   ### assuming here that it is not NA in FundData ever
#   port.lookthru <- subset(port.lookthru, !is.na(ValueUnit))
#   
#   ### rest of BBG Data
#   port.lookthru <- 
#   
#   #Portfolio_LookThrough <- merge(Fund_Data, subset(PortfolioData_w_BBG, select = c("ISIN", "ValueUSD", "PortfolioName","InvestorName")),  by.y = "ISIN", by.x = "FundISIN")
#   # FundCoverage <- rename(unique(subset(Portfolio_LookThrough, select = c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage"))), c("FundCoverage" = "FundCoverageMS"))
#   #Portfolio_LookThrough <- merge(Portfolio_LookThrough, unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name","SharePrice"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
#   Portfolio_LookThrough$Position <- Portfolio_LookThrough$ValueUSD * Portfolio_LookThrough$value / 100
#   Portfolio_LookThrough$Position[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] <- Portfolio_LookThrough$ValueUSD[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$value[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$SharePrice[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"]
#   Portfolio_LookThrough <- subset(Portfolio_LookThrough, FundCoverage <= 100)
#   # Portfolio_LookThrough <- merge(subset(Portfolio_LookThrough, select = -c (value,ValueUnit)), unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
#   Portfolio_LookThroughCovered <- subset(Portfolio_LookThrough, !is.na(Name))
#   FundCoverage <- rename(aggregate(Portfolio_LookThroughCovered["Position"], by = Portfolio_LookThroughCovered[,c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage")],FUN = sum, na.rm = TRUE),c("FundCoverage" = "FundCoverageMS"))
#   FundCoverage$FundCoverageBBG <- FundCoverage$Position / FundCoverage$ValueUSD
#   FundCoveragePortfolioLevel <- ddply(FundCoverage,.(InvestorName,PortfolioName),summarize, USDinFunds = sum(ValueUSD,na.rm = TRUE), USDcovered = sum(Position,na.rm = TRUE))
#   FundCoveragePortfolioLevel$Coverage <- FundCoveragePortfolioLevel$USDcovered / FundCoveragePortfolioLevel$USDinFunds
# 
# }



### ###########################################################################
### PIE SECTORS
### ###########################################################################


#SUBGROUPS.NOT.BONDS <-c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Supranational Bank","US Municipals","FGLMC Single Family 30yr","FGLMC Single Family 15yr","GNMA Single Family 30yr","FNMA Single Family 30yr","FNMA Single Family 15yr","Export/Import Bank","Regional Authority","Regional Agencies", "Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")

# TotalPortfolio_Bonds <- subset(TotalPortfolio, (Name == Ticker  | Group == "Debt Fund") & !Subgroup %in% Subgroups_notBonds)
# TotalPortfolio_Bonds$LoanIndicator <- sub(".* ","",TotalPortfolio_Bonds$Ticker)
# TotalPortfolio_Bonds$BondTest <- sapply(TotalPortfolio_Bonds$LoanIndicator, function(x) gregexpr("[[:punct:]]",x))
# TotalPortfolio_Bonds <- subset(TotalPortfolio_Bonds, BondTest != "-1" | LoanIndicator == "PERP" | Group == "Debt Fund", select = c("InvestorName", "PortfolioName", "ISIN", "ValueUSD"))
# Groups_notEQY <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Debt Fund","Multi-National","Commodity Fund", "Real Estate Fund","Alternative Fund","Money Market Fund", "","Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")


### Neaten things up
REMOVE.COLS <- names(fin.data)[!names(fin.data) %in% KEEP.BBG.OUTPUT.COLS]
REMOVE.COLS <- REMOVE.COLS[!REMOVE.COLS %in% names(port)]
KEEP.COLS <- names(port.fin)[!names(port.fin) %in% REMOVE.COLS]
port.fin <- port.fin %>% select_(.dots=KEEP.COLS)
port.fin$Group.Clean <- ifelse(is.na(port.fin$Group), "Unknown",port.fin$Group)


### Some Useful Meta Groupings
port.fin$Instrument.Type <- port.fin$Asset.Type
port.fin$Instrument.Type <- ifelse(port.fin$Group.Clean %in% GROUPS.FUNDS, "Fund", port.fin$Instrument.Type)
#port.fin$Instrument.Type <- ifelse(port.fin$Group.Clean=="Private Equity", "Private Equity", port.fin$Instrument.Type)
#port.fin$Instrument.Type <- ifelse(port.fin$Group.Clean %in% c("REITS","Real Estate"), "Real Estate", port.fin$Instrument.Type)

port.fin$Instrument.SubType <- ifelse(port.fin$Instrument.Type=="BOND", "Corporate", "Unspecified")
port.fin$Instrument.SubType <- ifelse(port.fin$err.noBBG==1 & port.fin$Group1=="Government", "Government", port.fin$Instrument.SubType)
port.fin$Instrument.SubType <- ifelse(port.fin$Group.Clean %in% GROUPS.GOVT, "Government", port.fin$Instrument.SubType)
port.fin$Instrument.SubType <- ifelse(port.fin$Sector %in% SECTOR.MORT, "Mortgage", port.fin$Instrument.SubType)
port.fin$Instrument.SubType <- ifelse(port.fin$Group.Clean %in% GROUPS.ABS, "ABS", port.fin$Instrument.SubType)

#port.fin$Instrument.SubType <- ifelse(port.fin$Group.Clean=="Insurance", "Insurance", port.fin$Instrument.SubType)

port.fin$Instrument.Combo <- paste0(port.fin$Instrument.Type,"-",port.fin$Instrument.SubType)
port.fin$Instrument.PPN.Combo <- paste0(port.fin$Instrument.Type,"-",port.fin$PPN.flag)
port.fin$Instrument.HoldingType.Combo <- paste0(port.fin$Instrument.Type,"-",port.fin$HoldingType)
port.fin <- port.fin %>% select(-Group.Clean, -ValueUSD.Init, -ValueUSD.Calc)


### EQUITIES: ICB Classification
port.fin$pie.icb <- "Not Assessed" #label non-benchmarked sectors
port.fin$pie.icb[port.fin$ICB.Subsector.Name %in% OilGasICB] <- "Fossil Fuels"
port.fin$pie.icb[port.fin$ICB.Subsector.Name %in% AutoICB] <- "Automotive"
port.fin$pie.icb[which(port.fin$ICB.Subsector.Name %in% UtilitiesICB & port.fin$Sector %in% "Power")] <- "Utility Power"
port.fin$pie.icb[which(!port.fin$ICB.Subsector.Name %in% UtilitiesICB & port.fin$Sector %in% "Power")] <- "NonUtility Power"
port.fin$pie.icb[port.fin$ICB.Subsector.Name %in% FuturesecsICB] <- port.fin$ICB.Subsector.Name[port.fin$ICB.Subsector.Name %in% FuturesecsICB]

### BONDS: BICS Classification
port.fin$pie.bics <- "Not Assessed" #label non-benchmarked sectors
port.fin$pie.bics[port.fin$Subgroup %in% OilGasBIC] <- "Oil&Gas"
port.fin$pie.bics[port.fin$Subgroup %in% CoalBIC] <- "Coal"
port.fin$pie.bics[port.fin$Subgroup %in% AutoBIC] <- "Automotive"
port.fin$pie.bics[port.fin$Subgroup %in% PowerBIC & port.fin$Sector %in% "Utilities"] <- "Utility Power"
port.fin$pie.bics[!(port.fin$Subgroup %in% PowerBIC) & port.fin$Sector %in% "Utilities"] <- "NonUtility Power"
port.fin$pie.bics[!(port.fin$Subgroup %in% OilGasBIC) & port.fin$Sector %in% "Energy"] <- "NonOG Production"
port.fin$pie.bics[port.fin$Subgroup %in% FuturesecsBIC] <- port.fin$Subgroup[port.fin$Subgroup %in% FuturesecsBIC]


### Now check what will go into PortCheck
port.fin <- port.fin %>% mutate(err.invalid=rowSums(.[grep("err",names(port.fin))]))
port.fin$Valid.2DCheck <- ifelse(port.fin$err.invalid==0, 1, 0)
err.cols <- grep("err",names(port.fin), value=TRUE)
other.cols <- setdiff(names(port.fin), err.cols)
port.fin <- port.fin %>% select_(.dots=c(other.cols, err.cols))
port.fin$PortfolioName <- factor(port.fin$PortfolioName, levels=rev(c("State Compensation Insurance Fund of CA",
                                                                     "Wawanesa General Insurance Company",
                                                                     "Anchor General Insurance Company")), ordered=TRUE)

### ###########################################################################
### WRITE IT OUT
### ###########################################################################

port.fin %>% group_by(PortfolioName, Asset.Type, Valid.2DCheck) %>% 
  summarise(tot=sum(MarketValue), comma(tot)) %>% mutate(pct=percent(tot/sum(tot))) %>% arrange(Asset.Type)

by.inst.type <- port.fin %>% group_by(PortfolioName, Instrument.Type, Instrument.Combo) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% group_by(PortfolioName) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD))

ppn.by.inst.type <- port.fin %>% group_by(PortfolioName, PPN.flag) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% group_by(PortfolioName) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD))

mapped.by.inst.type <- port.fin %>% group_by(PortfolioName, Instrument.Type, Instrument.Combo, err.noBBG) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% group_by(PortfolioName) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD)) %>%
  arrange(PortfolioName, err.noBBG, Instrument.Type)

assessed.by.inst.type <- port.fin %>% group_by(PortfolioName, Asset.Type, Instrument.Type, Instrument.Combo, Valid.2DCheck) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% group_by(PortfolioName) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD)) %>%
  arrange(PortfolioName, Valid.2DCheck, Instrument.Type)

pie.icb <- port.fin %>% 
  group_by(Asset.Type, PortfolioName, pie.icb) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% 
  group_by(PortfolioName, Asset.Type) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD)) %>%
  arrange(Asset.Type, PortfolioName,pie.icb)

pie.bics <- port.fin %>% 
  group_by(Asset.Type, PortfolioName, pie.bics) %>%
  summarise(count=n(), ValueUSD=sum(ValueUSD), comma(ValueUSD)) %>% 
  group_by(PortfolioName, Asset.Type) %>%
  mutate(pct=ValueUSD/sum(ValueUSD), percent(pct), port.ValueUSD=sum(ValueUSD)) %>%
  arrange(Asset.Type, PortfolioName,pie.bics)

write.csv(port.fin, 
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "Port_ALL.csv"), row.names=FALSE)

write.csv(port.fin %>% filter(Asset.Type=="EQUITY"), 
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "Port_EQY.csv"), row.names=FALSE)
write.csv(port.fin %>% filter(Asset.Type=="BOND"), 
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "Port_BONDS.csv"), row.names=FALSE)

write.csv(by.inst.type %>% 
            select(PortfolioName, Instrument.Combo, ValueUSD) %>% 
            spread(key=Instrument.Combo, value=ValueUSD, fill=0), 
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "-Instrument-Overview.csv"), row.names=FALSE)

write.csv(ppn.by.inst.type %>% 
            select(PortfolioName, PPN.flag, ValueUSD) %>% 
            spread(key=PPN.flag, value=ValueUSD, fill=0),
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "-PPN-Overview.csv"), row.names=FALSE)

write.csv(pie.icb %>% 
            select(Asset.Type,PortfolioName, pie.icb, ValueUSD) %>% 
            spread(key=pie.icb, value=ValueUSD, fill=0),
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "-ICB-Pie.csv"), row.names=FALSE)
write.csv(pie.bics %>% 
            select(Asset.Type, PortfolioName, pie.bics, ValueUSD) %>% 
            spread(key=pie.bics, value=ValueUSD, fill=0),
          paste0(BATCH.PORTS.PATH, BATCH.NAME, "-BICS-Pie.csv"), row.names=FALSE)


write.csv(mapped.by.inst.type, paste0(BATCH.PORTS.PATH, BATCH.NAME, "-BBG-Mapped-Overview.csv"), row.names=FALSE)
write.csv(assessed.by.inst.type, paste0(BATCH.PORTS.PATH, BATCH.NAME, "-Assessed-Overview.csv"), row.names=FALSE)


### ###########################################################################
### CHECK OUT DATA
### ###########################################################################
# 
# ### these are what we should get in BBG ( the 0 0 quadrant)
# table(port.fin$err.noBBG, port.fin$err.noISIN)
# 
# # inst Type
# # inst subtype
# # holding type
# # ppn flag
# 
# base.stats <- port.fin %>% group_by(PortfolioName, Instrument.Type) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot))
# 
# base.stats <- port.fin %>% group_by(PortfolioName, Instrument.Type, Instrument.Combo) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot))
# 
# base.stats <- port.fin %>% group_by(PortfolioName, Instrument.PPN.Combo) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot))
# 
# base.stats <- port.fin %>% group_by(PortfolioName, Instrument.HoldingType.Combo) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot))
# 
# 
# adv.stats <- port.fin %>% group_by(PortfolioName, HoldingType, Instrument.Type) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot))
# 
# more.adv.stats <- (port.fin %>% group_by(PortfolioName, PPN.flag, HoldingType, Instrument.Type, Instrument.SubType) %>%
#   summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) %>%
#   arrange(PortfolioName, PPN.flag, HoldingType, Instrument.Type)) 
# 
# port.fin %>% group_by(PortfolioName, err.invalid, PPN.flag) %>%
#   summarise(count=n(), tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#   mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) 
# 
# 
# as.data.frame(port.fin %>% group_by(PortfolioName, Instrument.Type, mapped) %>%
#                 summarise(count=n(), tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#                 mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) %>%
#                 arrange(PortfolioName, mapped)) 
# 
# 
# as.data.frame(port.fin %>% group_by(PortfolioName, Instrument.Type, mapped) %>%
#                 summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#                 mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) %>%
#                 arrange(PortfolioName, mapped, Instrument.Type)) 
# 
# 
# as.data.frame(port.fin %>% group_by(PortfolioName, Instrument.Type, Instrument.SubType, mapped) %>%
#                 summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#                 mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) %>%
#                 arrange(PortfolioName, mapped, Instrument.Type)) 
# 
# as.data.frame(port.fin %>% group_by(PortfolioName, Instrument.Type, Instrument.SubType, mapped) %>%
#                 summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName, Instrument.Type) %>%
#                 mutate(pct=tot/sum(tot), percent(pct), port.tot=sum(tot)) %>%
#                 arrange(PortfolioName, Instrument.Type, mapped)) 
# 
# as.data.frame(port.fin %>% group_by(mapped, PortfolioName, PPN.flag, HoldingType, Instrument.Type, Instrument.SubType) %>%
#                 summarise(tot=sum(ValueUSD), comma(tot)) %>% group_by(PortfolioName) %>%
#                 mutate(pct=tot/sum(tot), percent(pct)) %>%
#                 arrange(mapped, PortfolioName, PPN.flag, HoldingType, Instrument.Type)) 
# 
# 
# # 3) create overview file for meta analysis (a)AUM total, negative values total, AUM without ISIN vs with ISINs, AUM False ISINs (position without valueUSD vs positions with valueUSD (sum this) vs AUM assessable (b) Financial instrumetn split, (c) funds vs direct, etc.
# #PortInputNAISINs <- subset(port, ISIN %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(ISIN))
# #port$ISIN [port$ISIN  %in% c("n.a. (diverse Cash)", "n.a. (diverse HFoFs)", "n.a. (diverse Hypotheken)", "n.a. (diverse Immobilien Ausland)", "n.a. (diverse Immobilien CH)", "n.a. (diverse PE)", "N/A", "-", "0") | is.na(port$ISIN) ] <- "NA_ISIN_Input"
# 
# 
# 
# #------------
# # Merge portfolio with financial data and prepare for fund-look-through (e.g. calculate USD-Value held of each security)
# #------------
# 
# # PortfolioData_w_BBG <- merge(BBG_Data_sub,PortfolioData, by = "ISIN", all.x = FALSE, all.y = TRUE)
# # PortfolioData_wo_BBG <- unique(subset(PortfolioData_w_BBG,is.na(Name), select = c("ISIN")))
# # PortfolioData_wo_BBG2 <- subset(PortfolioData_w_BBG,is.na(Name))
# 
# # PortfolioData_w_BBG$ValueType <- "NumberofShares"
# # PortfolioData_w_BBG$ValueType[PortfolioData_w_BBG$MarketValue != 0 & !is.na(PortfolioData_w_BBG$MarketValue)] <- "MarketValue"
# 
# # # Calculate ValueUSD (AUMs in each single fund)
# # PortfolioData_w_BBG$Position <- PortfolioData_w_BBG$ValueUSD
# # PortfolioData_w_BBG$Position[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] <- PortfolioData_w_BBG$NumberofShares[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] 
# # PortfolioData_w_BBG$ValueUSD[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] <- 
# #   PortfolioData_w_BBG$SharePrice[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0] * PortfolioData_w_BBG$NumberofShares[is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0]
# # 
# # Test <- subset(PortfolioData_w_BBG, is.na(ValueUSD))
# # write.csv(Test, "InputWithoutBBGInformation_missingPorts.csv", row.names = FALSE)
# 
# # PortfolioData_w_BBG$ISIN[PortfolioData_w_BBG$ISIN %in% Test$ISIN & PortfolioData_w_BBG$ISIN != "NA_ISIN_Input" & (is.na(PortfolioData_w_BBG$ValueUSD) | PortfolioData_w_BBG$ValueUSD == 0)] <- "NA_ISIN_BBG_Data"
# # PortSizeCheck1 <- sum(PortfolioData_w_BBG$ValueUSD, na.rm = TRUE)
# 
# # Portfolio Meta Analysis
# # PortfolioEntries <- aggregate(ISIN ~ InvestorName + PortfolioName, data = PortfolioData_w_BBG, FUN = length)
# # PortfolioEntries <- rename(PortfolioEntries, c("ISIN" = "NrOfPosition"))
# # PortfolioSizes <- aggregate(PortfolioData_w_BBG["ValueUSD"], by  = PortfolioData_w_BBG[,c("InvestorName","PortfolioName")],FUN = sum, na.rm = TRUE)
# # PortfolioSizes <- rename(PortfolioSizes, c("ValueUSD" = "PortfolioSizeUSD"))
# # Test <- aggregate(ISIN ~ InvestorName + PortfolioName, data = Test, FUN = length)
# # Test <- rename(Test,c("ISIN" = "NrOfPositionsWOValueUSD"))
# # PortfolioSizes <- merge(merge(PortfolioSizes, PortfolioEntries, by = c("InvestorName","PortfolioName"), all.x = TRUE),Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)
# 
# #-------------
# # # Merge Portfolio Data with fund data for the lookthrough and calculate owned holdings of Portfolios
# #------------
# #All Instruments (R-pull with Port-Weight for both EQY & CBonds):
# Portfolio_LookThrough <- merge(Fund_Data, subset(PortfolioData_w_BBG, select = c("ISIN", "ValueUSD", "PortfolioName","InvestorName")),  by.y = "ISIN", by.x = "FundISIN")
# # FundCoverage <- rename(unique(subset(Portfolio_LookThrough, select = c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage"))), c("FundCoverage" = "FundCoverageMS"))
# Portfolio_LookThrough <- merge(Portfolio_LookThrough, unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name","SharePrice"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
# Portfolio_LookThrough$Position <- Portfolio_LookThrough$ValueUSD * Portfolio_LookThrough$value / 100
# Portfolio_LookThrough$Position[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] <- Portfolio_LookThrough$ValueUSD[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$value[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"] * Portfolio_LookThrough$SharePrice[Portfolio_LookThrough$ValueUnit == "SharesPerUSD"]
# Portfolio_LookThrough <- subset(Portfolio_LookThrough, FundCoverage <= 100)
# # Portfolio_LookThrough <- merge(subset(Portfolio_LookThrough, select = -c (value,ValueUnit)), unique(subset(BBG_Data_sub, select = c("ISIN", "Security.Type", "Name"))),by.x = "HoldingISIN", by.y = "ISIN", all.x = TRUE, all.y = FALSE)
# Portfolio_LookThroughCovered <- subset(Portfolio_LookThrough, !is.na(Name))
# FundCoverage <- rename(aggregate(Portfolio_LookThroughCovered["Position"], by = Portfolio_LookThroughCovered[,c("InvestorName", "PortfolioName", "FundISIN", "ValueUSD", "FundCoverage")],FUN = sum, na.rm = TRUE),c("FundCoverage" = "FundCoverageMS"))
# FundCoverage$FundCoverageBBG <- FundCoverage$Position / FundCoverage$ValueUSD
# FundCoveragePortfolioLevel <- ddply(FundCoverage,.(InvestorName,PortfolioName),summarize, USDinFunds = sum(ValueUSD,na.rm = TRUE), USDcovered = sum(Position,na.rm = TRUE))
# FundCoveragePortfolioLevel$Coverage <- FundCoveragePortfolioLevel$USDcovered / FundCoveragePortfolioLevel$USDinFunds
# 
# PortfolioData_wo_BBG <- subset(PortfolioData_wo_BBG, !ISIN %in% Portfolio_LookThrough$FundISIN)
# PortfolioData_wo_BBG2 <- subset(PortfolioData_wo_BBG2, !ISIN %in% Portfolio_LookThrough$FundISIN)
# Test <- aggregate(ISIN ~ InvestorName + PortfolioName, data = PortfolioData_wo_BBG2, FUN = length)
# Test <- rename(Test,c("ISIN" = "NrOfPositionsWOBBGInformation"))
# PortfolioSizes <- merge(PortfolioSizes,Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)
# Test <- aggregate(ValueUSD ~ InvestorName + PortfolioName, data = PortfolioData_wo_BBG2, FUN = sum, na.rm = TRUE)
# Test <- rename(Test,c("ValueUSD" = "ValueUSDOfPositionsWOBBGWValueUSDInformation"))
# PortfolioSizes <- merge(PortfolioSizes,Test,by = c("InvestorName","PortfolioName"), all.x = TRUE)
# 
# PortfolioData_wo_BBG$Type <- "MissingISINsPortfolio"
# MissingISINsLookThrough <- rename(subset(Portfolio_LookThrough, is.na(Position), select = "HoldingISIN"),c("HoldingISIN" = "ISIN"))
# if(nrow(MissingISINsLookThrough) > 0){MissingISINsLookThrough$Type <- "MissingISINsFundsLookThrough"
# 
# MissingISINs <- rbind(PortfolioData_wo_BBG, MissingISINsLookThrough)}else{MissingISINs <- PortfolioData_wo_BBG}
# 
# PortfolioData_Funds <- subset(PortfolioData_w_BBG, ISIN %in% Portfolio_LookThrough$FundISIN)
# FundsBBG <- subset(PortfolioData_w_BBG, Security.Type %in% c("ETF", "Closed End Fund", "Mutual Fund") | Sector == "Funds", select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name", "Group"))  
# 
# FundMetaanalysis <- merge(FundsBBG, rename(subset(FundCoverage,select = c("InvestorName", "PortfolioName", "FundISIN", "FundCoverageMS", "Position", "FundCoverageBBG")), c("Position" = "PositionsCovered")), by.x = c("InvestorName", "PortfolioName", "ISIN"), by.y = c("InvestorName", "PortfolioName", "FundISIN"), all = TRUE)
# FundMetaanalysisPortLevel <- ddply(FundMetaanalysis,.(InvestorName,PortfolioName),summarize, FundsUSD = sum(ValueUSD,na.rm = TRUE), FundsCoveredUSD = sum(PositionsCovered, na.rm = TRUE))
# FundMetaanalysisPortLevel$Coverage <- FundMetaanalysisPortLevel$FundsCoveredUSD / FundMetaanalysisPortLevel$FundsUSD
# PortfolioMetaAnalysis <- merge(PortfolioSizes,FundMetaanalysisPortLevel, by = c("InvestorName", "PortfolioName"), all = TRUE)
# 
# FundsCovered <- subset(PortfolioData_Funds, select = c("ISIN", "SharePrice","PortfolioName", "InvestorName", "NumberofShares", "ValueUSD", "Security.Type", "ICB.Subsector.Name"))
# 
# FundsWithMissingBBGData <- subset(FundsCovered, is.na(ValueUSD), select = c("ISIN"))
# if(nrow(FundsWithMissingBBGData) > 0){
#   FundsWithMissingBBGData$QTY <- 1
#   FundsWithMissingBBGData$Date <- "31-12-2016"
#   # write.csv(FundsWithMissingBBGData, "BBG-Look-up-needed.csv",row.names = FALSE)
# }
# 
# PortfolioData_w_BBG_test <- subset(PortfolioData_w_BBG,!is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)
# 
# if(is.null(length(setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))))){
#   print("ISINS GETTING LOST!! CHECK LOSTISINS")
#   LOSTISINS <- setdiff(PortfolioData_w_BBG$ISIN,c(PortfolioData_Funds$ISIN,PortfolioData_wo_BBG$ISIN,PortfolioData_w_BBG_test$ISIN))
# }
# 
# PortfolioData_w_BBG <- subset(PortfolioData_w_BBG, !is.na(Name) & !ISIN %in% Portfolio_LookThrough$FundISIN)
# 
# # Create the equity portfolio input files for the fund analysis
# Portfolio <- subset(PortfolioData_w_BBG, select = c("ISIN", "Name", "Security.Type", "ValueUSD", "PortfolioName","InvestorName"))
# Portfolio$HoldingType <- "Direct Holding"
# Portfolio_Funds <- subset(Portfolio_LookThrough, select = c("HoldingISIN", "Name", "Security.Type", "Position", "PortfolioName","InvestorName"))
# Portfolio_Funds <- rename(Portfolio_Funds, c("HoldingISIN" = "ISIN", "Position" = "ValueUSD"))
# # Portfolio_Funds_summed <- ddply(Portfolio_Funds,.(ISIN, Name, Security.Type, PortfolioName, InvestorName), summarise, Position = sum(Position, na.rm = TRUE))
# Portfolio_Funds_summed <- aggregate(Portfolio_Funds["ValueUSD"], by=Portfolio_Funds[,c("ISIN", "Name", "Security.Type", "PortfolioName", "InvestorName")], FUN=sum, na.rm = TRUE)
# Portfolio_Funds_summed$HoldingType <- "Fund Holding"
# 
# TotalPortfolio <- Portfolio
# if (exists("Portfolio_Funds_summed")==TRUE){
# TotalPortfolio <- rbind(TotalPortfolio,Portfolio_Funds_summed)}
# PortSizeCheck2 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)
# 
# TotalPortfolio <- merge(TotalPortfolio, subset(BBG_Data_sub, select = c("ISIN","ICB.Subsector.Name","Group", "Ticker", "Subgroup")), by = "ISIN", all.x = TRUE, all.y = FALSE)
# 
# Groups_notEQY <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Debt Fund","Multi-National","Commodity Fund", "Real Estate Fund","Alternative Fund","Money Market Fund", "","Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")
# 
# TotalPortfolio_EQY <- subset(TotalPortfolio, (!is.na(ICB.Subsector.Name) & ICB.Subsector.Name != "") | Security.Type == "Common Stock" | (Name != Ticker & !Group %in% Groups_notEQY & !is.na(Group)))
# TotalPortfolio_EQY <- merge(TotalPortfolio_EQY, subset(BBG_Data_sub, select = c("ISIN","SharePrice")), by = "ISIN", all.x = TRUE, all.y = FALSE)
# TotalPortfolio_EQY <- subset(TotalPortfolio_EQY , select = c("InvestorName", "PortfolioName", "ISIN", "ValueUSD"))
# 
# 
# #------------
# # Sort Debt Data
# #------------
# # Create the Cbond portfolio input files for the fund analysis
# DebtData <- read.csv(paste0(BATCH.FIN.DATA.PATH,"Cbonds_Issuer&Subs_DebtTicker_BICS_2016Q4.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# # Rename Total row as to not confuse it with TOTAL SA...
# DebtData$Co..Corp.TKR[DebtData$Co..Corp.TKR == "Total"] <- "TotalDebt"
# GovBanksSupraNat <- subset(DebtData, (DebtData$Government.Development.Banks !=0 | DebtData$Supranationals != 0) & DebtData$Co..Corp.TKR != "TotalDebt") 
# DebtData <- subset(DebtData, !DebtData$Co..Corp.TKR %in% GovBanksSupraNat$Co..Corp.TKR)
# 
# 
# # TotalPortfolio <- read.csv(paste0(BATCH.FIN.DATA.PATH,"FinancialData.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
# 
# CorpDebtTicker <- colsplit(TotalPortfolio$Ticker, pattern = " ", names = c("COMPANY_CORP_TICKER",2,3))[1]
# TotalPortfolio <- cbind(TotalPortfolio,CorpDebtTicker)
# 
# Subgroups_notBonds <- c("Sovereign", "Agency CMBS", "Automobile ABS Other","CMBS Other","CMBS Subordinated" ,"Municipal-City" ,"Municipal-County","Supranational Bank","US Municipals","FGLMC Single Family 30yr","FGLMC Single Family 15yr","GNMA Single Family 30yr","FNMA Single Family 30yr","FNMA Single Family 15yr","Export/Import Bank","Regional Authority","Regional Agencies", "Other ABS","Sovereign","Sovereign Agency","WL Collat CMO Mezzanine","WL Collat CMO Other","WL Collat CMO Sequential")
# TotalPortfolio_Bonds <- subset(TotalPortfolio, (Name == Ticker  | Group == "Debt Fund") & !Subgroup %in% Subgroups_notBonds)
# TotalPortfolio_Bonds$LoanIndicator <- sub(".* ","",TotalPortfolio_Bonds$Ticker)
# TotalPortfolio_Bonds$BondTest <- sapply(TotalPortfolio_Bonds$LoanIndicator, function(x) gregexpr("[[:punct:]]",x))
# TotalPortfolio_Bonds <- subset(TotalPortfolio_Bonds, BondTest != "-1" | LoanIndicator == "PERP" | Group == "Debt Fund", select = c("InvestorName", "PortfolioName", "ISIN", "ValueUSD"))
# 
# TotalPortfolio$InstrumentType <- "Others"
# TotalPortfolio$InstrumentType[TotalPortfolio$ISIN  %in% TotalPortfolio_EQY$ISIN] <- "Equity"
# TotalPortfolio$InstrumentType[TotalPortfolio$ISIN  %in% TotalPortfolio_Bonds$ISIN] <- "Bonds"
# 
# PortSizeCheck3 <- sum(TotalPortfolio$ValueUSD, na.rm = TRUE)
# 
# # Add the InstrumentType to the PortfolioData_w_BBG
# PortfolioData_w_BBG$InstrumentType <- "Others"
# PortfolioData_w_BBG$InstrumentType[PortfolioData_w_BBG$ISIN  %in% TotalPortfolio_EQY$ISIN] <- "Equity"
# PortfolioData_w_BBG$InstrumentType[PortfolioData_w_BBG$ISIN  %in% TotalPortfolio_Bonds$ISIN] <- "Bonds"
# 
# 
# 
# 
# 
# #------------
# # Data for the Overview Pie Chart
# #------------
# TotalPortfolio$ValueUSD <- as.numeric(TotalPortfolio$ValueUSD)
# OverviewPiechartData <- aggregate(TotalPortfolio["ValueUSD"], by = TotalPortfolio[,c("InvestorName", "PortfolioName", "HoldingType", "InstrumentType")], FUN = sum, na.rm = TRUE)
# OverviewPiechartDatawide <- dcast(OverviewPiechartData, InvestorName + PortfolioName  + HoldingType  ~ InstrumentType, value.var = "ValueUSD")
# 
# MissingColumns <-setdiff(c("Equity","Bonds","Others"),colnames(OverviewPiechartDatawide))
# if(length(MissingColumns) > 0){OverviewPiechartDatawide[,MissingColumns] <- 0}
# 
# OverviewPiechartDatawideTotal <- ddply(OverviewPiechartDatawide,.(InvestorName, PortfolioName),summarize, Equity = sum(Equity,na.rm = TRUE),Bonds = sum(Bonds,na.rm = TRUE),Others = sum(Others,na.rm = TRUE))
#   
# OverviewPiechartDatawideTotal$HoldingType <- "All"
# OverviewPiechartDatawide <- rbind(OverviewPiechartDatawide,OverviewPiechartDatawideTotal)
# OverviewPiechartDataFinal <- merge(OverviewPiechartDatawide,subset(PortfolioSizes, select =c("InvestorName", "PortfolioName", "PortfolioSizeUSD", "ValueUSDOfPositionsWOBBGWValueUSDInformation")),by = c("InvestorName","PortfolioName"), all.x = TRUE)
# OverviewPiechartDataFinal <- rename(OverviewPiechartDataFinal, c("Others" = "PositionsWithValue_Ignore4piechart"))
# OverviewPiechartDataFinal$Others <- OverviewPiechartDataFinal$PortfolioSizeUSD - OverviewPiechartDataFinal$Equity - OverviewPiechartDataFinal$Bonds
# 
# OverviewPiechartDataFinal <- subset(OverviewPiechartDataFinal, HoldingType == "All")
# InvestorLevelIDentification <- as.data.frame(table(OverviewPiechartDataFinal$InvestorName))
# SinglePorts <- subset(InvestorLevelIDentification, Freq == 1)
# OverviewPiechartDataFinal$PortfolioType <- "Portfolio"
# OverviewPiechartDataFinal$PortfolioType[OverviewPiechartDataFinal$InvestorName %in% SinglePorts$Var1] <- "Investor"
# 
# #create file at investor level
# OverviewPiechartDataFinalMPS <- ddply(subset(OverviewPiechartDataFinal, !InvestorName %in% SinglePorts$Var1),.(InvestorName, HoldingType),summarize,PortfolioSizeUSD = sum(PortfolioSizeUSD,na.rm = TRUE), Bonds = sum(Bonds,na.rm = TRUE), Equity = sum(Equity,na.rm = TRUE), Others = sum(Others,na.rm = TRUE))
# if(nrow(OverviewPiechartDataFinalMPS)>0){
# OverviewPiechartDataFinalMPS$PortfolioName <- OverviewPiechartDataFinalMPS$InvestorName
# OverviewPiechartDataFinalMPS$PortfolioType <- "InvestorMPs"}
# 
# OverviewPiechartDataFinal <- OverviewPiechartDataFinal[,colnames(OverviewPiechartDataFinalMPS)]
# OverviewPiechartDataFinal <- rbind(OverviewPiechartDataFinal, OverviewPiechartDataFinalMPS)
# 
# 
# 
# #------------
# # Print Results To PortfolioData
# #------------
# 
# ### In general we should try avoid doing this, and use paths instead below - CHANGE
# setwd(paste0(BatchLocation))
# 
# write.csv(OverviewPiechartDataFinal,paste0(BATCH.NAME,"Portfolio_Overview_Piechart.csv"),row.names = FALSE, na = "")
# write.csv(MissingISINs,paste0(BATCH.NAME,"Missing_BBG-Data.csv"), row.names = FALSE, na = "")
# write.csv(PortfolioData_w_BBG,paste0(BATCH.NAME,"PortfolioData_w_BBG-Info.csv"),row.names = FALSE, na = "")
# write.csv(TotalPortfolio,paste0(BATCH.NAME,"Port.csv"),row.names = FALSE, na = "")
# write.csv(TotalPortfolio_EQY,paste0(BATCH.NAME,"Port_EQY.csv"),row.names = FALSE, na = "")
# write.csv(TotalPortfolio_Bonds,paste0(BATCH.NAME,"Port_Bonds.csv"),row.names = FALSE, na = "")
# write.csv(FundCoverage,paste0(BATCH.NAME,"Port_ListofFunds.csv"),row.names = FALSE, na = "")
# write.csv(PortfolioMetaAnalysis,paste0(BATCH.NAME,"Portfolio_Metaanalysis.csv"),row.names = FALSE, na = "")
# 
# # Temp save for bonds
# UserName <- Clare
# bondlocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/2° Investing Team/1. RESEARCH/1. Studies (projects)/2DPORTFOLIO/PortfolioCheck/Data/Finance Reg Data/PortfolioData/")
# write.csv(TotalPortfolio_Bonds,paste0(bondlocation,BATCH.NAME,"Port_Bonds.csv"),row.names = FALSE, na = "")
# 
# 
