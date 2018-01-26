

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
  BBG_Data <- read.csv(paste0(BATCH.FIN.DATA.PATH,"FinancialData_20171127.csv"),stringsAsFactors=FALSE,strip.white=TRUE)
  BBG_Data <- plyr::rename(BBG_Data, c( "Mkt.Val..P." = "SharePrice"))
  BBG_Data <- subset(BBG_Data, !is.na(ISIN) & ISIN != "")
  BBG_Data <- BBG_Data[!duplicated(BBG_Data$ISIN),]
  
}

load.full.financial.data <- function() {  
  
  base.bridge <- load.financial.data()
  base.bridge <- base.bridge %>% select(Name, Ticker, ISIN, CUSIP, Security.Type, 
                 Sector, Group, Subgroup, ICB.Subsector.Name,
                 SharePrice, Cnty.of.Dom)
  base.bridge <- base.bridge[!duplicated(base.bridge$ISIN),]
  any(duplicated(base.bridge$ISIN))
  
  ###

  ###
  
  BBGPORTOutput <- read.csv(file=paste0(PROC.DATA.PATH, "BondPORTOutputMixedSource.csv"), 
                            stringsAsFactors = FALSE, encoding = "UTF-8") 
  BBGPORTOutput$ICB.Subsector.Name <- NA
  BBGPORTOutput$SharePrice <- 1
  
  any(duplicated(BBGPORTOutput$ISIN))
  
  #drop duplicate ISINs due to position-based variables
  BBGPORTOutput <- BBGPORTOutput[!duplicated(BBGPORTOutput$ISIN),]
  BBGPORTOutput <- subset(BBGPORTOutput, !(ISIN %in% c("#N/A N/A","",NA) | is.na(ISIN)))
  BBGPORTOutput <- subset(BBGPORTOutput, !(CUSIP %in% c("#N/A N/A","",NA) | is.na(CUSIP)))
  BBGPORTOutput <- subset(BBGPORTOutput, nchar(BBGPORTOutput$CUSIP) == 9)
  BBGPORTOutput$Cnty.of.Dom <- ifelse(is.na(BBGPORTOutput$Cnty.of.Dom), BBGPORTOutput$Country.ISO.Code, BBGPORTOutput$Cnty.of.Dom) 
  BBGPORTOutput <- BBGPORTOutput %>% select(Name, Ticker, ISIN, CUSIP, Security.Type, 
                                            Sector, Group, Subgroup,ICB.Subsector.Name,
                                            SharePrice, Cnty.of.Dom)
  any(duplicated(BBGPORTOutput$ISIN))
  new <- subset(BBGPORTOutput, !BBGPORTOutput$ISIN %in% base.bridge$ISIN)
  bridge <- bind_rows(base.bridge, new)
  
  
  ###
  
  uni <- read.csv(file=paste0(PROC.DATA.PATH, "2017-04-19FinancialDataUnique.csv"), 
                            stringsAsFactors = FALSE, encoding = "UTF-8") 
  uni$SharePrice <- 1
  uni$Sector <- NA
  uni <- uni[!duplicated(uni$ISIN),]
  uni$Cnty.of.Dom <- uni$Country.ISO.Code
  any(duplicated(uni$ISIN))
  
  uni <- uni %>% select(Name, Ticker, ISIN, CUSIP, Security.Type, 
             Sector, Group, Subgroup,ICB.Subsector.Name,
             SharePrice, Cnty.of.Dom)
  
 
  ###
  
  library(rio)
  cw.bridge <- import(paste0(PROC.DATA.PATH, "CW-Bond-Bridge.xlsx"))
  cw.bridge$SharePrice <- 1
  cw.bridge$Cnty.of.Dom <- NA
  cw.bridge$Security.Type <- cw.bridge$`Security Type` 
  cw.bridge$ICB.Subsector.Name <- cw.bridge$`ICB Subsector Name`
  
  cw.bridge <- cw.bridge %>% select(Name, Ticker, ISIN, CUSIP, Security.Type, 
                                    Sector, Group, Subgroup, ICB.Subsector.Name,
                                    SharePrice, Cnty.of.Dom)
  cw.bridge <- subset(cw.bridge, !is.na(CUSIP) & CUSIP != "")
  cw.bridge <- subset(cw.bridge, !is.na(ISIN) & ISIN != "")
  any(duplicated(cw.bridge$ISIN))
  
  ###
  
  uni <- left_join(uni, cw.bridge %>% select(ISIN, Sector), by=c("ISIN"))
  uni$Sector <- ifelse(is.na(uni$Sector.x), uni$Sector.y, uni$Sector.x)
  
  ###
  
  new <- subset(uni, !uni$ISIN %in% bridge$ISIN)
  bridge <- bind_rows(bridge, new)
  any(duplicated(bridge$ISIN))
  
  new <- subset(cw.bridge, !cw.bridge$ISIN %in% bridge$ISIN)
  bridge <- bind_rows(bridge, new)
  any(duplicated(bridge$ISIN))
  
  bridge %>% select(-Sector.x, -Sector.y)
}



load.fund.data <- function() {
  Fund_Data <- read.csv(paste0(FundDataLocation,"FundLookThroughData.csv"),stringsAsFactors=FALSE,strip.white=TRUE) 
  Fund_Data <- subset(Fund_Data, is.na(FundCoverage) |  FundCoverage <= 100)  
  Fund_Data
}
