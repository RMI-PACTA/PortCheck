

ReadParameterFile <- function(){
  ParameterFileName <- choose.files(default = paste0(PARAM.PATH,"*_ParameterFile.csv"),
                                    caption = "Select a parameter file", multi = FALSE)
  ParameterFileInfo <- file.info(ParameterFileName, extra_cols = TRUE)
  ParameterFile <- read.csv(ParameterFileName, stringsAsFactors = FALSE, strip.white = TRUE)
  return(ParameterFile)
}

SetParameters <- function(ParameterFile){
  BENCHMARK.REGION <<- as.character(ParameterFile$BenchmarkRegion)
  COMPANYDOM.REGION <<- as.character(ParameterFile$CompanyDomicileRegion)
  Indexchoose <<- as.character(ParameterFile$Index) 
  SCENARIO <<- as.character(ParameterFile$Scenario)
  Scenariochoose <<- as.character(ParameterFile$Scenario)
  
  START.YEAR <<- as.numeric(format(Sys.time(), "%Y")) #as.numeric(ParameterFile$Startyear)
  BATCH.NAME <<- as.character(ParameterFile$BatchName)
  ComparisonFile <<- ParameterFile$ComparisonFile                        # Defines whether the comparative graphs are to be produced
  ReportTemplate <<- ParameterFile$ReportStyle
  CLIENT.NAME <<- ParameterFile$ProjektName
  BatchToTest <<- ParameterFile$AssessmentDate
  Languagechoose <<- ParameterFile$Languageselect
  
  FIN.DATA.DATE <<- ParameterFile$DateofFinancialData
  BBGDataDate <<- ParameterFile$DateofFinancialData
  AssessmentDate <<- ParameterFile$DateofFinancialData
}


load.financial.data <- function() {  
  #Read in financial data
  # BBG_Data <- read.csv(paste0(BATCH.FIN.DATA.PATH,"FinancialData.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
  BBG_Data <- read.csv(paste0(BATCH.FIN.DATA.PATH,"FinancialData_20170925.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
  BBG_Data <- plyr::rename(BBG_Data, c( "Mkt.Val..P." = "SharePrice"))
  BBG_Data_sub <- subset(BBG_Data, ! is.na(BBG_Data$ISIN) & ISIN != "")
  
  
  #Test if there are duplicates left in the financial database
  ISINCount <- as.data.frame(table(BBG_Data_sub$ISIN))
  DupsInBBGDataBETTERCHECK <- subset(ISINCount, Freq > 1, select = "Var1")
  BBG_Data_sub <- BBG_Data_sub[!duplicated(BBG_Data_sub),]
  BBG_Data_sub
  
}

is.equity() <- function() {
  
}