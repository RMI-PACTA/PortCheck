#This project was funded by the European Commission through LIFE program under grant: LIFE16 GIC/FR/000061 - PACTA)
# Global Functions



#----
# Wraps labels for charts
#----
wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}


#-----
# Read Parameter File
#-----


ReadParameterFile <- function(PortfolioDataFolder){
  ParameterFileName <- choose.files(default = paste0(PortfolioDataFolder,"99_ParameterFiles/","*_ParameterFile.csv"),caption = "Select a parameter file", multi = FALSE)
  ParameterFileInfo <- file.info(ParameterFileName, extra_cols = TRUE)
  ParameterFile <- read.csv(ParameterFileName, stringsAsFactors = FALSE, strip.white = TRUE)
  return(ParameterFile)
}


SetParameters <- function(ParameterFile){
  BenchmarkRegionchoose <<- as.character(ParameterFile$BenchmarkRegion)
  CompanyDomicileRegionchoose <<- as.character(ParameterFile$CompanyDomicileRegion)
  Indexchoose <<- as.character(ParameterFile$Index) 
  Scenario <<- as.character(ParameterFile$Scenario)
  Scenariochoose <<- as.character(ParameterFile$Scenario)
  
  Startyear <<- as.numeric(ParameterFile$Startyear) #as.numeric(ParameterFile$Startyear)
  BatchName <<- as.character(ParameterFile$BatchName)
  ComparisonFile <<- ParameterFile$ComparisonFile                        # Defines whether the comparative graphs are to be produced
  ReportTemplate <<- ParameterFile$ReportStyle
  ProjectName <<- ParameterFile$ProjektName
  BatchToTest <<- ParameterFile$AssessmentDate
  Languagechoose <<- ParameterFile$Languageselect
  
  BBGDataDate <<- ParameterFile$DateofFinancialData
  AssessmentDate <<- ParameterFile$DateofFinancialData
}



#------
# Equity Functions
#------

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

datacompletion <- function (Data){
  Data <- subset(Data, Technology %in% AllLists$TechList)
  Data <- Data %>% complete(Technology = AllLists$TechList, Year = full_seq(c(Startyear,Startyear+10),1), fill = list(CompanyLvlProd = 0, Wt = 0, WtProduction = 0, CarstenMetric_Port = 0))
  Data$Sector <- "Power"
  Data$Sector[Data$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
  Data$Sector[Data$Technology %in% c("Coal")] <- "Coal" 
  Data$Sector[Data$Technology %in%c("Electric","Hybrid","ICE")] <-"Automotive"
  return(Data)
}

datacompletioneqy <- function (Data){
  Data <- subset(Data, Technology %in% AllLists$TechList)
  Data <- Data %>% complete(BenchmarkRegion = subset(BenchmarkRegionList, !BenchmarkRegion %in% c("NonOECDRest"), select = "BenchmarkRegion"), 
                            Year = full_seq(c(Startyear,Startyear+10),1), 
                            CompanyDomicileRegion = subset(CompanyDomicileRegionList, select = "CompanyDomicileRegion"), 
                            Technology = AllLists$TechList, 
                            fill = list(Production = 0))
  Data$Sector <- "Power"
  Data$Sector[Data$Technology %in% c("Coal","Oil","Gas")] <- "Fossil Fuels" 
  Data$Sector[Data$Technology %in%c("Electric","Hybrid","ICE")] <-"Automotive"
  # Data <- subset(Data,!is.na(CompanyDomicileRegion) & (Sector == "Power" | Sector == "Fossil Fuels" | (Sector == "Automotive" & BenchmarkRegion == "Global")))
  Data <- subset(Data,!is.na(CompanyDomicileRegion) & (Sector == "Power" | (Sector == "Fossil Fuels" & BenchmarkRegion %in% c(FossilFuelBenchmarkRegions,"Global")) | (Sector == "Automotive" & BenchmarkRegion == "Global")))
  
  return(Data)
}

datacompletiondebt <- function (Data){
  Data <- subset(Data, Technology %in% AllLists$TechList)
  Data <- Data %>% complete(Technology = AllLists$TechList, Year = full_seq(c(Startyear,Startyear+10),1), fill = list(CompanyLvlProd = 0, Wt = 0, WtProduction = 0, CarstenMetric_Port = 0))
  Data$Sector <- "Power"
  Data$Sector[Data$Technology %in% c("Oil","Gas")] <- "Oil&Gas" 
  Data$Sector[Data$Technology %in% c("Coal")] <- "Coal" 
  Data$Sector[Data$Technology %in%c("Electric","Hybrid","ICE")] <-"Automotive"
  return(Data)
}

Regiondatacompletion <- function (Data){
  Data <- Data %>% complete(BenchmarkRegion = subset(BenchmarkRegionList, !BenchmarkRegion %in% c("NonOECDRest"), select = "BenchmarkRegion"), 
                            Sector = SectorList, 
                            fill = list(RegionalWtPortfolioWt = 0))
  Data <- subset(Data, (Sector == "Power" & BenchmarkRegion %in% AllLists$PowerBenchmarkRegionGlobal) | (Sector %in% c("Oil&Gas") & BenchmarkRegion %in% AllLists$FossilFuelBenchmarkRegions) | (Sector %in% c("Automotive","Coal") & BenchmarkRegion == "Global"))
  Data$RegionalWtPortfolioWt[Data$Sector %in% c("Coal","Automotive")] <- 1
  return(Data)
}

datacompletionSub <- function (Data){
  Data <- subset(Data, Technology %in% AllLists$TechList)
  Data <- Data %>% complete(DebtTicker = DebtTickerList$DebtTicker,
                            BenchmarkRegion = subset(BenchmarkRegionList, !BenchmarkRegion %in% c("NonOECDRest"), select = "BenchmarkRegion"),
                            Technology = AllLists$TechList)
  Data$Sector <- "Power"
  Data$Sector[Data$Technology %in% c("Oil","Gas")] <- "Oil&Gas"
  Data$Sector[Data$Technology %in%c("Electric","Hybrid","ICE")] <-"Automotive"
  Data$Sector[Data$Technology %in% c("Coal")] <- "Coal" 
  #Data <- subset(Data,!is.na(CompanyDomicileRegion) & (Sector == "Power" | (Sector == "Fossil Fuels" & BenchmarkRegion %in% c(FossilFuelBenchmarkRegions,"Global")) | (Sector == "Automotive" & BenchmarkRegion == "Global")))
  return(Data)
}


MultipleDataBind <- function(List,Headers){
  for (k in 1:length(List)){
    Data <- read.csv(List[k],stringsAsFactors = FALSE,strip.white = TRUE, sep = ";", dec = ".", header = FALSE)
    colnames(Data) <- Headers
    Data$Source <- List[k]
    if (k==1){
      AllData <- Data
    }else{
      Data <- subset(Data, !FundISIN %in% AllData$FundISIN)
      if (length(colnames(Data)) < length(colnames(AllData))){
        colmiss <- setdiff(colnames(AllData),colnames(Data))
        Data[,colmiss] <- NA
      }else if (length(colnames(Data)) > length(colnames(AllData))){
        colmiss <- setdiff(colnames(Data),colnames(AllData))
        AllData[,colmiss] <- NA
      }
      AllData <- rbind(Data,AllData)
    }
  }
  return(AllData)
}

AddMissingColumns <- function(dfa, dfb){
  missingcols <- setdiff(colnames(dfb),colnames(dfa))
  tempdf <- data.frame(matrix(ncol = length(missingcols), nrow = nrow(dfa)))
  names(tempdf) <- missingcols
  dfa <- cbind(dfa,tempdf)
}

ChangeFundPort <- function(BatchTest){
  
  BatchTest$Type <- revalue(BatchTest$Type, c("Fund" = "Portfolio","Brand"="Investor"))
  return(BatchTest)
  
}

addcompanyeqy <- function(BatchName, BatchFolder, CombinAll, ReducedListAll,PortfolioAll,PortfolioListAll,InvestorToRemove){
  setwd(paste0(OutputFolder,"/Swiss/2016Q4"))
  CombinAllLong <- read.csv(paste(BatchName,"_EquityAnalysisResults.csv",sep = ""),stringsAsFactors = FALSE)
  ReducedListAllLong <- read.csv(paste(BatchName,"_CompanysProduction_Snapshot.csv",sep = ""),stringsAsFactors = FALSE)
  PortfolioAllLong <- read.csv(paste(BatchName,"_PortfolioData_Snapshot",Startyear,".csv",sep = ""),stringsAsFactors = FALSE)
  PortfolioListAllLong <- read.csv(paste(BatchName,"_ListPortfolioHoldings",Startyear,".csv",sep = ""),stringsAsFactors =  FALSE)
  
  CombinAllLong <- subset(CombinAllLong, InvestorName != InvestorToRemove)
  ReducedListAllLong <- subset(ReducedListAllLong, PortName != InvestorToRemove)
  PortfolioAllLong <- subset(PortfolioAllLong, PortName != InvestorToRemove)       
  PortfolioListAllLong <-subset(PortfolioListAllLong, PortfolioName != InvestorToRemove)   
  
  CombinAll <- rbind(CombinAllLong, CombinAll)
  ReducedListAll <- rbind(ReducedListAllLong, ReducedListAll)
  PortfolioAll <- rbind(PortfolioAllLong, PortfolioAll)
  PortfolioListAll <- rbind(PortfolioListAllLong, PortfolioListAll)
  
  results <- list(CombinAll,ReducedListAll, PortfolioAll,PortfolioListAll)
  
  return(results)
}



