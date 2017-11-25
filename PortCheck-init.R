
### Set this Variable
PROJ.NAME <- "PortCheck"


### These are In here for backwards compatibility
### Eventually should change the code to use the upper case constants
DataFolder <- PROC.DATA.PATH
FundDataLocation <- FUND.DATA.PATH

### Uncomment if you want to use logging
### LOG.PATH <- paste0(PROJ.PATH,"SomeFolderWhereYouWriteLogs/")
### source(paste0(COMMON.CODE.PATH,"/log.R"))
### init.log()

print(paste0("*** ", PROJ.NAME, " Initialized ***"))
consts <- grep("(PATH|NAME)", ls(), value=TRUE)
consts.val <- unname(sapply(consts, function(x) eval(parse(text=x))))
PROJ.CONSTS <- data.frame(Constant=consts, Location=consts.val)
