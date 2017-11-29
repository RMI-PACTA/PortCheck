# Global Functions

 


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
  
  Startyear <<- as.numeric(format(Sys.time(), "%Y")) #as.numeric(ParameterFile$Startyear)
  BatchName <<- as.character(ParameterFile$BatchName)
  ComparisonFile <<- ParameterFile$ComparisonFile                        # Defines whether the comparative graphs are to be produced
  ReportTemplate <<- ParameterFile$ReportStyle
  ProjectName <<- ParameterFile$ProjektName
  BatchToTest <<- ParameterFile$AssessmentDate
}



#------
# Equity Functions
#------

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

datacompletion <- function (Data){
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

