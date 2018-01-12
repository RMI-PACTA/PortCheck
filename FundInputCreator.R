# Create a Fund Report - Input file
# (ie. fund lookup only)


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



#----------
# Read in Data
#----------

TestName <- "HelvetiaFunds"

FundList <- read.csv(paste0(PORTS.PATH,"/06_Funds/FundISINsToTest/",TestName,".csv"),stringsAsFactors=FALSE,strip.white=TRUE)

Fund_Data <- read.csv(paste0(FundDataLocation,"FundLookThroughData.csv"),stringsAsFactors=FALSE,strip.white=TRUE) 

FundFound_Data <- merge(FundList, Fund_Data, by = "FundISIN")
FundFound_Data$MarketValue <- FundFound_Data$TestValue* FundFound_Data$value/100

FundDataPrint <- subset(FundFound_Data,select = c("FundISIN","HoldingISIN", "MarketValue" ))
FundDataPrint$InvestorName <- "Fund"
FundDataPrint <- rename(FundDataPrint, c("FundISIN"="PortfolioName", "HoldingISIN"="ISIN"))

write.csv(FundDataPrint,paste0(PORTS.PATH,"/06_Funds/FundISINsToTest/",TestName,"_Input.csv"),row.names = FALSE)

print(paste0("Funds Found: ", sum(FundFound_Data$value,na.rm = T)/100," of ", nrow(FundList)))



