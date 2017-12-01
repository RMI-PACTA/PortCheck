
### ###########################################################################
### PROJECT WORKING DIRECTORY
### ###########################################################################

### Set this Variable - Name of folder holding this project's code from Github 
PROJ.NAME <- "PortCheck"
PROJ.CODE.PATH <- paste0(GIT.PATH, PROJ.NAME, "/")


### ###########################################################################
### PROJECT CONSTANTS AND PATHS
### ###########################################################################

### Remember - 2dii-init has been run, so this scipt has access to 
### DATA.PATH, PORTS.PATH, etc...
### Define any other project-specific constants that might be useful here

PARAM.PATH <- paste0(PORTS.PATH, "99_ParameterFiles/")

### These are In here for backwards compatibility
### Eventually should change the code to use the upper case constants
DataFolder <- PROC.DATA.PATH
FundDataLocation <- FUND.DATA.PATH

Date <<- Sys.Date()
BBGPORTOutput <<- "FinancialData_20171127"



### ###########################################################################
### PROJECT FUNCTIONS
### ###########################################################################

### if there are shared functions, source them here
source(paste0(PROJ.CODE.PATH, "proj-functions.R"))


### ###########################################################################
### LOGGING
### ###########################################################################

### Uncomment if you want to use logging

LOG.PATH <- paste0(PORTCHECK.CODE.PATH)
source(paste0(COMMON.CODE.PATH,"/log.R"))
### init.log()


### ###########################################################################
### OVERRIDE IF NECESSARY
### ###########################################################################

if (file.exists("proj-init-overrides.R")) {
  source("proj-init-overrides.R")
}


### ###########################################################################
### PRINT OUT STATUS
### ###########################################################################

print(paste0("*** ", PROJ.NAME, " Initialized ***"))
print(show.consts())
