

### Name of GitHub Code Repository
USER.NAME <- sub("/.*","",sub(".*Users/","",getwd()))
DROPBOX.PATH <- paste0("C:/Users/",USER.NAME,"/Dropbox (2° Investing)/")

### This is where we should each clone GitHub to.
### If your path is different, comment out the standard line and put in the correct 
### value for "CODE.PATH"...
### example: CODE.PATH <- "C:/SomeOtherPath/toSomeCode/"
CODE.PATH <- paste0(DROPBOX.PATH, "2° Investing Team/People/",USER.NAME,"/GitHub/")


### Path to 2DII R Code libraries
PORTCHECK.CODE.PATH <- paste0(CODE.PATH,"PortCheck/")
TEMPLATES.CODE.PATH <- paste0(CODE.PATH,"Templates/")
CAINS.CODE.PATH <- paste0(CODE.PATH, "CA-INS/")


### This is where the "reference" data is located
REF.DATA.PATH <- paste0(DROPBOX.PATH,"PortCheck/00_Data/")
FUND.DATA.PATH <- paste0(REF.DATA.PATH, "01_ProcessedData/02_FundData/")
FIN.DATA.PATH <- paste0(REF.DATA.PATH, "02_FinancialData/")


### Set this Variable - Ideally GitHub Project and PortCheck project Name are the same
PROJ.NAME <- "CA-INS" 

### Path to the code for THIS project - this is your working directory
PROJ.CODE.PATH <- paste0(CODE.PATH,PROJ.NAME,"/")

### This is where the Portfolios Are for this Project
PORT.DATA.PATH <- paste0("C:/Users/",USER.NAME,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/")


### Print this out on the command line for a visual check

print("*** 2DII Project Initialized ***")
consts <- grep("(PATH|NAME)", ls(), value=TRUE)
consts.val <- unname(sapply(consts, function(x) eval(parse(text=x))))
INIT.CONSTS <- data.frame(Constant=consts, Location=consts.val)
INIT.CONSTS


# FinancialDataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/02_FinancialData/2016Q4/PORT/")
# PortfolioLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/",ParameterFile$ProjektName,"/")
# OutputLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/02_PortfolioData/",ParameterFile$ProjektName,"/") #Output-folder for the results
# FundDataLocation <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/02_FundData/")
# DataFolder <- paste0("C:/Users/",UserName,"/Dropbox (2° Investing)/PortCheck/00_Data/01_ProcessedData/")
# 
# 
# BatchLocation <- paste0(PortfolioLocation,BatchName,"/")
