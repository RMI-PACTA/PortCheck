library(dplyr)
library(reshape2)
source(paste0(PORTCHECK.CODE.PATH, "proj-init.R"))
print(show.consts())

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
############ Change Portfolio Name here ##################
PortfolioName<- "VARIABLE ANNUITY LIFE INSURANCE COMPANY (THE)"
#-------------
# Set Input / OUtput Locations Based on parameter File Input
#------------

### Finish setting up paths based on what was in the Parameter file
PROC.DATA.PATH <- paste0(DATA.PATH,"/01_ProcessedData/")
PROJ.PATH <- paste0(PORTS.PATH,ParameterFile$ProjektName,"/")
BATCH.PATH <- paste0(PROJ.PATH,BatchName,"/")
if(!dir.exists(file.path(BATCH.PATH))){dir.create(file.path(BATCH.PATH), showWarnings = TRUE, recursive = FALSE, mode = "0777")}  
BATCH.RES.PATH <- paste0(RESULTS.PATH,"01_BatchResults/",BatchName,"/",BatchToTest,"/")


eqnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-Names-TAJ-Update.csv"),stringsAsFactors = FALSE,strip.white = T)
cbnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-Names-TAJ-Update.csv"),stringsAsFactors = FALSE,strip.white = T)
Subgroup.Overview <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Port-Overview-Fin-Sector.csv"),stringsAsFactors=FALSE,strip.white = T)
names(Subgroup.Overview) <- gsub("TwoD\\.", "", names(Subgroup.Overview))
Subgroup.Overview<-Subgroup.Overview %>%
  filter(Portfolio.Name ==PortfolioName)%>%
  select(Portfolio.Name,Fin.Sector,Valid,ValueUSD,Asset.Type)
   
### Get Debt Batch Results
CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
CBBatchTest <- subset(CBBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose & PortName == PortfolioName)
CBBatchTest<-select(CBBatchTest,PortName,Year,CarstenMetric_Port,Sector,Technology,WtProduction,Scen.WtProduction.Market)
CBBatchTest$Type <-"Fixed Income"

CBCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Company-ALD-2023.csv"),stringsAsFactors = FALSE,strip.white = T)
CBCompProdSnapshots <- subset(CBCompProdSnapshots, Type == "Portfolio" & Aggregation == BenchmarkRegionchoose & Scenario == Scenariochoose & PortName == PortfolioName)
CBCompProdSnapshots <- left_join(CBCompProdSnapshots, cbnames, by="COMPANY_CORP_TICKER")
CBCompProdSnapshots <- CBCompProdSnapshots %>% select(-Name) %>% rename(Name=Final.Name)
CBCompProdSnapshots <-select(CBCompProdSnapshots,Year,Name,PortWeightEQYlvl, COMPANY_CORP_TICKER)
CBCompProdSnapshots$Type <-"Fixed Income"

CBALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-ALD-BuildOut.csv"),stringsAsFactors=FALSE,strip.white = T) %>%
  filter(Aggregation=="GlobalAggregate",BenchmarkRegion==BenchmarkRegionchoose,Scenario %in% c("450S","NPS","CPS"),PortName == PortfolioName)%>%
  select(Scenario, PortName, Technology,Sector,Year,WtProduction, Scen.WtProduction)
CBALDAggProd$Type <-'Fixed Income'
CBCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"-Debt-Port-Company-ALD-Short-ALL.csv"),stringsAsFactors = FALSE,strip.white = T)
CBCompALD <- subset(CBCompALD, Scenario == Scenariochoose & Aggregation==BenchmarkRegionchoose & Portfolio.Name == PortfolioName)
CBCompALD <- select(CBCompALD,COMPANY_CORP_TICKER,Portfolio.Name, Sector,Plan.WtTechProd,Scen.WtTechProd,Port.Sec.ClimateWt,Technology)
CBCompALD$Type <-"Fixed Income"

### Get Equity Batch Results
EQBatchTest <- read.csv(paste(BATCH.RES.PATH,BatchName,"_Equity-Port-ALD-Results-450S.csv",sep=""),stringsAsFactors=FALSE,strip.white = T)
EQBatchTest <- subset(EQBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose & PortName == PortfolioName)
EQBatchTest<-select(EQBatchTest,PortName,Year,CarstenMetric_Port,Sector,Technology,WtProduction,Scen.WtProduction.Market)
EQBatchTest$Type <-"Equity"
EQCompProdSnapshots <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Company-ALD-2023.csv"),stringsAsFactors = FALSE,strip.white = T)
EQCompProdSnapshots <- subset(EQCompProdSnapshots, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose & Scenario == Scenariochoose & PortName == PortfolioName)
EQCompProdSnapshots <- left_join(EQCompProdSnapshots, eqnames, by="EQY_FUND_TICKER")
EQCompProdSnapshots <- EQCompProdSnapshots %>% select(-Name) %>% rename(Name=Final.Name)
EQCompProdSnapshots <-select(EQCompProdSnapshots,Year,Name,PortWeightEQYlvl, EQY_FUND_TICKER)
EQCompProdSnapshots$Type <-"Equity"

EQALDAggProd<- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-ALD-BuildOut.csv"),stringsAsFactors=FALSE,strip.white = T)%>%
  filter(Aggregation==BenchmarkRegionchoose, BenchmarkRegion==BenchmarkRegionchoose,Scenario %in% c("450S","NPS","CPS"), PortName == PortfolioName)%>%
  select(Scenario, PortName, Technology,Sector,Year,WtProduction, Scen.WtProduction)
EQALDAggProd$Type <-'Equity'

EQCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Company-ALD.csv"),stringsAsFactors = FALSE,strip.white = T)
EQCompALD <- subset(EQCompALD, Scenario == Scenariochoose & Aggregation==BenchmarkRegionchoose & PortName == PortfolioName)
EQCompALD <- EQCompALD %>%
  select(EQY_FUND_TICKER,PortName, Sector,WtProduction,Scen.WtProduction,PortWeightEQYlvl,Technology) %>%
  rename("Plan.WtTechProd"=WtProduction,
         "Scen.WtTechProd"=Scen.WtProduction,
         "Port.Sec.ClimateWt"=PortWeightEQYlvl)

EQCompALD$Type <-"Equity"


port<-bind_rows(CBBatchTest,EQBatchTest)
Comp<-bind_rows(CBCompProdSnapshots,EQCompProdSnapshots)
builtout <- bind_rows(CBALDAggProd,EQALDAggProd)
COMP <- bind_rows(CBCompALD,EQCompALD)

write.csv(port, file = paste0(BATCH.RES.PATH, "CHECKING/Variable_Port.csv"),row.names = F)
write.csv(COMP, file = paste0(BATCH.RES.PATH, "CHECKING/Variable_COMP.csv"),row.names = F)
write.csv(builtout, file = paste0(BATCH.RES.PATH, "CHECKING/Variable_scenario.csv"),row.names = F)
