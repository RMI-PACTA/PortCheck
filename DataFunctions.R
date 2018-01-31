# --------
# DATA CHECK FUNCTIONS
# --------
# ------------ RunSet -----------------------#
RunSet <- function(Directory){
  
  # If there is already existing data, then copy all png,pdf and csv files into the next RunDirectory
  
  pnglist <- list.files(Directory,pattern=c("\\.png$"), full.names = TRUE)
  pdflist <- list.files(Directory,pattern=c("\\.pdf$"), full.names = TRUE)
  csvlist <- list.files(Directory,pattern=c("\\.csv$"), full.names = TRUE)
  flist <- append(pnglist,csvlist)
  flist <- append(flist,pdflist)
  dlist <- list.dirs(Directory, recursive = FALSE, full.names = FALSE)
  
  if(length(flist)>0){
    nextrun <- 1
    if (length(dlist)>0){nextrun <- as.numeric(gsub("Run","",dlist[length(dlist)]))+1}
    RunDirectory <- paste0(Directory,"Run",nextrun,"/")
    dir.create(file.path(RunDirectory), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    if(length(flist)>0){
      for (file in flist){file.copy(file, RunDirectory)}}
  }
  
  return()
}

# ------------ Production Check ------------- #
production_check <- function(combin, Scenariochoose,Startyear,BenchmarkRegionchoose,CompanyDomicileRegionchoose){
  Results <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose,select= c("Sector","Production")) 
  # if (!"Automotive" %in% Results$Sector){
  #   ResultsAuto <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == "Global" & Sector== "Automotive" & CompanyDomicileRegion == CompanyDomicileRegionchoose,select= c("Sector","Production")) 
  #   Results <- rbind(Results,ResultsAuto)
  # }
  
  SectorProduction <- ddply(Results, .(Sector),summarise, Production  =sum(Production))
  return(SectorProduction)
}

#-------
# # ------------ Exposure Data ---------------- #
# exposure_data <- function(ChartType,PortSnapshot, combin, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, Startyear,PortfolioName){
#   
#   if (ChartType == "EQ"){
#     Results <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose)
#     # if (!"Automotive" %in% Results$Sector){
#     #   ResultsAuto <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == "Global" &Sector== "Automotive" & CompanyDomicileRegion == CompanyDomicileRegionchoose)
#     #   Results <- rbind(Results,ResultsAuto)}
#   }else{
#     Results <- subset(combin,Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose)
#     colnames(PortSnapshot)[colnames(PortSnapshot) %in% "IssLvlPortWeight"] <- "PortWeight"
#   }
#   
#   if   (ChartType == "EQ"){
#     Results <- subset(Results,select= c("Sector","Technology","MarketExposure","AUMExposure","Production","PortAUM")) 
#   }else{
#     Results <- subset(Results, select = c("Sector","Technology","MarketExposure","WtProduction")) #"AUMExposure",,"PortAUM"
#     colnames(Results)[colnames(Results) %in% "WtProduction"] <- "Production"
#   }
#   
#   
#   # SectorProduction <- ddply(Results, .(Sector),summarise, Production  =sum(Production))
#   
#   # Results <- subset(Results, !Technology %in% c("OilCap"))
#   
#   piesub_tech <- unique(subset(PortSnapshot,select=c("ISIN","piesector","PortWeight")))
#   piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
#   piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
#   pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
#   
#   techshares <- data.frame(piesector = c("Fossil Fuels", "Automotive","Utility Power"))
#   techshares <- merge(techshares,pieshares, by = "piesector", all.x = TRUE)
#   techshares$Portfolio_weight[is.na(techshares$Portfolio_weight)] <- 0
#   # techshares <- subset(pieshares, piesector %in% c("Fossil Fuels", "Automotive","Utility Power")) 
#   techshares$Portfolio_weight <- techshares$Portfolio_weight/sum(techshares$Portfolio_weight)
#   
#   Results$Production[Results$Technology == "Coal"]<- Results$Production[Results$Technology == "Coal"]*24
#   Results$Production[Results$Technology == "Oil"]<- Results$Production[Results$Technology == "Oil"]*6.12
#   Results$Production[Results$Technology == "Gas"]<- Results$Production[Results$Technology == "Gas"]*0.0372
#   
#   Results$CoverageWeight <- 1
#   Results$CoverageWeight[Results$Sector == "Fossil Fuels"]<-Results$Production[Results$Sector == "Fossil Fuels"]/sum(Results$Production[Results$Sector == "Fossil Fuels"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Fossil Fuels"]
#   Results$CoverageWeight[Results$Sector == "Automotive"]<-Results$Production[Results$Sector == "Automotive"]/sum(Results$Production[Results$Sector == "Automotive"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Automotive"]
#   Results$CoverageWeight[Results$Sector == "Power"]<-Results$Production[Results$Sector == "Power"]/sum(Results$Production[Results$Sector == "Power"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Utility Power"]
#   
#   Results$Exposure <- Results$MarketExposure*100
#   Results$Exposure[Results$Sector == "Fossil Fuels"] <- Results$AUMExposure[Results$Sector == "Fossil Fuels"]*100
#   
#   Results <- subset(Results, select =c("Technology","Sector","CoverageWeight","Exposure"))
#   
#   Technology<-c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap", "Electric", "Hybrid", "ICE","Gas","Oil", "Coal")
#   Sector<-c("Power","Power","Power","Power","Power","Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels")
#   TechOrder<-c(1,2,3,4,5,6,7,8,9,91,92)
#   Results$Sector <- NULL
#   tempdb<-cbind(Technology,Sector,TechOrder)
#   Results <- merge(tempdb,Results,by="Technology",all.x=TRUE,all.y=FALSE)
#   Results <-arrange(Results, TechOrder)  
#   Results$TechOrder <- NULL
#   
#   #if Exposure is >100% cut off for graphic
#   Results$ExposureChart<-Results$Exposure
#   Results$CoverageWeight[Results$CoverageWeight == 0] <- NA 
#   Results$ExposureChart[is.na(Results$CoverageWeight) & is.na(Results$ExposureChart)] <- 0
#   Results$CoverageWeight[is.na(Results$CoverageWeight) & !Results$Technology %in% c("Oil", "Gas","Coal")] <- 0.001 # add sliver for Jakob.
#   Results$CoverageWeight[is.na(Results$CoverageWeight)] <- 0
#   Results$ExposureChart[Results$Exposure > 100] <- 100 # Limits graphical display for really high values
#   Results$ExposureChart[Results$Exposure < -100] <- -100 # Limits graphical display for really low values
#   Results$CoverageWeight<-Results$CoverageWeight/sum(Results$CoverageWeight) #rescale results to accommodate sliver
#   
#   #calaculate the widths depending on th portfolio weights
#   tempdb<-subset(Results, CoverageWeight != 0)
#   tempdb$w <- cumsum(tempdb$CoverageWeight)
#   tempdb$wm <- tempdb$w - tempdb$CoverageWeight
#   Results <- merge(tempdb,Results,by=c("Technology","Exposure","ExposureChart","CoverageWeight","Sector"),all.x=TRUE,all.y=TRUE)
#   
#   #Results$w <- cumsum(Results$CoverageWeight)
#   # Results$wm <- ifelse(is.na(Results$CoverageWeight),0,Results$w - Results$CoverageWeight)
#   #sort dataframe
#   Results$Sector <- NULL
#   tempdb<-cbind(Technology,Sector,TechOrder)
#   Results <- merge(tempdb,Results,by="Technology",all.x=TRUE,all.y=FALSE)
#   Results <-arrange(Results, TechOrder)  
#   Results$TechOrder <- NULL
#   
#   #round to one decimal place
#   Results$Exposure <- round(Results$Exposure, digits = 1)
#   Results$Exposure[which(Results$Exposure == -100 & Results$Technology %in% c("Oil", "Gas","Coal"))] <- "N/A" 
#   Results$Exposure[is.na(Results$Exposure)] <- -100
#   Results$ExposureChart[Results$ExposureChart == 0] <- -100
#   Results$CoverageWeight <- round(Results$CoverageWeight, digits = 3) 
#   
#   return(Results)
# }
# 
#-------
# ------------ Average Coverage Weight Data -------- #
CoverageWeight_data <- function(ChartType,PortfolioType,BatchTest_PortSnapshots, BatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose){
  # This function returns the Coverage weight of either the average, or a specific Portfolio as specified in PortfolioName (or equal to Average)
  
  # BatchTest_PortSnapshots<-EQComparisonPortSS
  # BatchTest<-EQComparisonBatchTest
  # ChartType<- "EQ"
  # PortfolioType <- "Portfolio"
  # PortfolioType <- c("Portfolio","Investor","InvestorMPs")
  
  # PortfolioType <- c("Fund","Portfolio")
  
  if (ChartType == "EQ"){
    Results <- subset(BatchTest,Type %in% PortfolioType & Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose)#,select= c("PortName","Sector","Technology","MarketExposure","AUMExposure","Production","PortAUM"))
    Results <- subset(Results, !Technology %in% c("OilCap"))
    
    piesub_tech <- unique(subset(BatchTest_PortSnapshots,select=c("PortName","ISIN","piesector","PortWeight")))
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    pieshares <- ddply(piesub_tech, .(PortName,piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    pieshares$piesector[pieshares$piesector == "Utility Power"]<- "Power"
    pieshares <- subset(pieshares, pieshares$piesector %in% c("Fossil Fuels", "Automotive","Power"))
    
    df <- merge(Results,pieshares, by.x = c("PortName", "Sector"), by.y = c("PortName","piesector"), all.x = TRUE, all.y = FALSE)
    df$Portfolio_weight[is.na(df$Portfolio_weight)] <- 0
    
    df$SectorAUM <- df$PortAUM*df$Portfolio_weight
    
    SectorTotAUM <- unique(subset(df, select = c("PortName","Sector","SectorAUM")))
    SectorTotAUM <- ddply(SectorTotAUM, . (PortName),summarise, SectorTotAUM = sum(SectorAUM,na.rm = TRUE))
    df<-merge(df,SectorTotAUM, by="PortName")
    
    df$SectorCoverage <- df$SectorAUM/df$SectorTotAUM
    
    df$Production[df$Technology == "Coal"]<- df$Production[df$Technology == "Coal"]*24
    df$Production[df$Technology == "Oil"]<- df$Production[df$Technology == "Oil"]*6.12
    df$Production[df$Technology == "Gas"]<- df$Production[df$Technology == "Gas"]*0.0372
    
    ProdTotals <- ddply(df, .(PortName,Sector), summarise, SecProd = sum(Production, na.rm=TRUE))
    df <- merge(df, ProdTotals, by = c("PortName", "Sector"))
    
    df$TechShare <- df$Production/df$SecProd
    df$TechAUM <- df$TechShare*df$SectorAUM
    df$CoverageWeight <- df$TechAUM/df$SectorTotAUM
    
    df[is.na(df)]<- 0
    
    Results <- subset(df, select =c("PortName","Technology","Sector","SectorTotAUM","PortAUM","CoverageWeight"))
    
    # WeightedResults <- ddply(Results, .(Technology),summarise, CoverageWeight= weighted.mean(CoverageWeight,SectorTotAUM,na.rm = TRUE))
    # WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sum(WeightedResults$CoverageWeight)  
    # WeightedResults$PortName <- "WeightedResults" 
    # 
    # ResultsDF <- subset(Results, select = c("PortName","Technology","CoverageWeight"))
    # ResultsDF <- rbind(ResultsDF,WeightedResults)
    
  }else{
    
    dfall <- subset(BatchTest, Type %in% PortfolioType & Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose)
    dfall <- subset(dfall, !dfall$PortName %in% c("Market_Benchmark","GlobalBondUniverse","MetaPortfolio","MetaPortfolio_MetaPortfolio"))
    if (nrow(dfall)>0){
      # Fossil Fuels
      dfFF <- subset(dfall, Sector %in% c("Oil&Gas","Coal"))
      
      dfFF$TechShare <- dfFF$SectorWeight
      dfFF$TechShare[dfFF$Sector %in% "Oil&Gas"] <- dfFF$SectorWeight[dfFF$Sector %in% "Oil&Gas"]*dfFF$PortTechShare[dfFF$Sector %in% "Oil&Gas"]
      TSSUM <- ddply(dfFF, .(PortName), summarize, TSSUM = sum(TechShare, na.rm = TRUE))
      dfFF <- merge(dfFF,TSSUM, by= "PortName")
      dfFF$TechShare <-dfFF$TechShare/dfFF$TSSUM 
      
      dfFF <- subset(dfFF, select = c("Sector","PortName","Technology","TechShare"))
      # dfFF$Sector <- "Fossil Fuels"
      
      dfFF$TechShare[is.nan(dfFF$TechShare)] <- 0
      
      # Automotive, Power
      df <- subset(dfall, Sector %in% c("Automotive","Power"))
      df <- subset(df, select=c("PortName","Sector","Technology","WtTechShareTechShare"))
      df <- melt(df, id.vars = c("PortName","Sector","Technology"))
      df <- rename(df, c("value"="TechShare"))
      df$variable <- NULL
      
      df$TechShare[is.nan(df$TechShare)] <- 0
      df <- subset(df,!Technology %in% "OilCap")
      
      df <- subset(df,!Technology %in% "OilCap")
      powersums <- ddply(df, .(PortName,Sector), summarise, SectorTotal=sum(TechShare, na.rm=TRUE))
      df  <- merge(df, powersums, by = c("PortName","Sector"))
      
      df$TechShare <- df$TechShare/df$SectorTotal
      df$SectorTotal <- NULL
      
      
      # Combine and normalise
      
      dftot <- rbind(df,dfFF)
      sectorsums <- ddply(dftot, .(PortName,Sector), summarise, SectorTotal=sum(TechShare, na.rm=TRUE))
      dftot  <- merge(dftot, sectorsums, by = c("PortName","Sector"))
      
      dftot$TechShare <- dftot$TechShare/3
      dftot$SectorTotal <- NULL
      
      # Sector sum = 3, should equal 1?
      dfallsectors <- merge(dfall, dftot, by= c("PortName","Sector","Technology"))
      
      secweight <- ddply(dfallsectors, .(PortName, Sector),summarise, SectorWtTotal=sum(SectorWeight, na.rm=TRUE))
      secweightsum <- ddply(secweight, .(PortName),summarise, sumsecwts = sum(SectorWtTotal, na.rm = TRUE))
      
      sectors <- merge(secweight, secweightsum, by= "PortName")
      sectors$SectorWtTotal <- sectors$SectorWtTotal/sectors$sumsecwts
      sectors$sumsecwts<- NULL
      
      
      dfallsectors<- merge(dfallsectors,sectors, by= c("PortName", "Sector"))
      
      dfallsectors$TechShare <- dfallsectors$TechShare*dfallsectors$SectorWtTotal
      dfallsectors$SectorWtTotal <- NULL  
      dfallsectors <- rename(dfallsectors, c("TechShare"="CoverageWeight"))
      
      
      Results <- subset(dfallsectors, select = c("PortName","Technology","CoverageWeight","SectorWeight", "PortfolioAUMAnalyzed"))
      Results <- rename(Results, c("PortfolioAUMAnalyzed"="PortAUM"))
    }else{
      Results <- "NoResults"
    }
    
  }
  
  return(ResultsDF)
}

# ------------ List to Test Creation -------- # 
FundPortcheck <- function(PortsToTest,PortfolioList){
  PortsToTestUnique <- PortsToTest
  PortsToTestUnique$Type[PortsToTestUnique$Type == "Investor"] <- "Brand"
  PortsToTestUnique$Type[PortsToTestUnique$Type == "Portfolio"] <- "Fund"
  PortsToTestUnique <- unique(PortsToTestUnique)
  
  FundList <- subset(PortsToTestUnique, PortsToTestUnique$Type %in% "Fund")
  FundList$InvestorName <- gsub(".*[A-z ? 0-9 . -]_","",FundList$PortName)
  FundList$PortfolioName <- gsub("_.*[A-z ? 0-9 . -]","",FundList$PortName)
  FundList<- FundList[!FundList$InvestorName == FundList$PortfolioName,] 
  
  BrandList <- subset(PortsToTestUnique, PortsToTestUnique$Type %in% "Brand")
  BrandList$InvestorName <- BrandList$PortName
  BrandList$PortfolioName <- BrandList$PortName
  BrandList$Type[BrandList$InvestorName %in% FundList$InvestorName] <- "InvestorMPs"
  
  testlist <- rbind(BrandList,FundList)
  # testlist$PortName <- NULL
  
  InvestorMPs <- unique(data.frame("InvestorName"=(PortfolioList$InvestorName[!PortfolioList$InvestorName %in% PortfolioList$PortfolioName])))
  InvestorMPs$PortfolioName <- InvestorMPs$InvestorName
  
  PortfolioList <- rbind(PortfolioList,InvestorMPs)
  
  PortfolioList$InvestorRefName <- gsub("[ _ .-]","",PortfolioList$InvestorName) 
  PortfolioList$PortfolioRefName <- gsub("[ _ .-]","",PortfolioList$PortfolioName) 
  # PortfolioList$PortName <- paste0(PortfolioList$InvestorRefName,"_",PortfolioList$PortfolioRefName)
  
  testlist<- merge(PortfolioList, testlist, by.x = c("InvestorRefName","PortfolioRefName"),by.y=c("InvestorName","PortfolioName"))
  # testlist<- merge(PortfolioList, testlist, by.x = c("InvestorRefName","PortfolioRefName"),by.y=c("InvestorName","PortfolioName"),all = TRUE)
  
  testlist$Type[testlist$Type %in% "Brand"] <- "Investor"
  testlist$Type[testlist$Type %in% "Fund"] <- "Portfolio"
  
  testlist <- subset(testlist, select= c("PortName","Type","InvestorRefName","PortfolioRefName","InvestorName","PortfolioName"))
  testlist$PortfolioName <- gsub("_"," ",testlist$PortfolioName)
  
  testlist$ReportTitle <- testlist$InvestorName
  testlist$ReportTitle[testlist$Type %in% c("Portfolio")]<- paste0(testlist$ReportTitle[testlist$Type %in% c("Portfolio")],": ",testlist$PortfolioName[testlist$Type %in% c("Portfolio")])
  
  return(testlist)
}

# ----------- Filter Funds ------------------ #
filterfunds <- function(nofunds,FundsData){
  FundsDataShort <- unique(subset(FundsData, select=c("PortName","PortAUM")))
  FundsDataShort <- FundsDataShort[order(FundsDataShort, decreasing = TRUE),]
  Fundslist <- unique(FundsDataShort$PortName)[1:nofunds]
  FundsDataLong <- FundsData[FundsData$PortName %in% Fundslist,]
  return(FundsDataLong)
}

# ---------- Comparison List ---------------- #
comparisonlist <- function(noFunds, EQData,CBData){
  EQDataShort <- unique(subset(EQData, select=c("PortName","PortAUM")))
  EQDataShort <- subset(EQDataShort, !EQDataShort$PortName %in% c("ListedMarket_ListedMarket","Fund"))
  EQDataShort <- EQDataShort[order(EQDataShort, decreasing = TRUE),]
  EQlist <- as.data.frame(unique(EQDataShort$PortName)[1:noFunds])
  colnames(EQlist) <- "PortName" 
  EQlist$Type <- "EQ"
  
  CBDataShort <- unique(subset(CBData, select=c("PortName","PortfolioAUMAnalyzed")))
  CBDataShort <- subset(CBDataShort, !CBDataShort$PortName %in% c("ListedMarket_ListedMarket","Fund"))
  CBDataShort <- CBDataShort[order(CBDataShort, decreasing = TRUE),]
  CBlist <- as.data.frame(unique(CBDataShort$PortName)[1:noFunds])
  colnames(CBlist) <- "PortName" 
  CBlist$Type <- "CB"
  
  ComparisonList <- rbind(EQlist,CBlist)
  return(ComparisonList)
}

# ----------- Port Filter Funds ------------------ #
Portfunds <- function(maxno,FundList,FundsDataAll, PortfolioName,InvestorName, TestType){
  
  if (typeof(FundList)=="list"){
    FundsListData<- FundList
    FundsListData$InvestorName<- gsub(" ","",FundsListData$InvestorName)
    FundsListData$PortfolioName<- gsub(" ","",FundsListData$PortfolioName)
    
    if (TestType == "Portfolio"){
      FundsListPort <- FundsListData[FundsListData$InvestorName == InvestorName & FundsListData$PortfolioName == PortfolioName,]
    }
    
    if (TestType %in%  c("Investor", "InvestorMPs")){
      FundsListPort <- FundsListData[FundsListData$InvestorName == InvestorName,]
    }
    
    if (!TestType %in% c("Investor","Portfolio", "InvestorMPs")){
      print("Test Type no Investor or Portfolio; check PortFunds function for error" )
    }
    
    if (nrow(FundsListPort) >0 ){
      
      # FundsDataAll$ISIN <- str_split(FundsDataAll$PortName,"[01]_AllFunds",simplify = TRUE)[,1]
      FundsDataAll$ISIN <- substring(FundsDataAll$PortName,1,12)
      
      # FundsListPort$FundISINName <-  paste0(FundsListPort$FundISIN, "0_AllFunds")
      FundsListPort <- FundsListPort[rev(order(FundsListPort$ValueUSD)),]
      
      FundsListPort <- FundsListPort[1:min(maxno, nrow(FundsListPort)),]
      FundsDataPort <- FundsDataAll[FundsDataAll$ISIN %in% FundsListPort$FundISIN,]
      
    }else{
      FundsDataPort <- "NoFunds"
    }}else{
      FundsDataPort <- "NoFunds"
    }
  
  return(FundsDataPort) 
}

# ----------- Filter Ports ------------------ #
filterports <- function(PortData, PortfolioInfo, BrandType){
  
  ListToInclude <- PortfolioInfo[PortfolioInfo$PKV %in% BrandType & PortfolioInfo$Swiss %in% "Yes",]
  PortData <- subset(PortData, PortData$PortName %in% ListToInclude$RefName)
  
  return(PortData)
}

# ------------ Company Comparison Data ------ #
company_comparison <- function(ChartType,BatchTestComparison, Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose){
  
  # ChartType <- "CB"
  # BatchTestComparison <- CBBatchTest
  # BatchTestComparison <- EQBatchTest
  Results <- BatchTestComparison
  # Results <- subset(Results, Results$)
  
  if (ChartType == "EQ"){
    # AUMData<-Results[Results$PortName %in% Results$PortName,]
    AUMs <- unique(subset(Results, select=c("PortName","PortAUM"))) ## CORRECT
    # AUMs <- ddply(Results, .(PortName),summarise, AUM = sum(PortAUM, na.rm=FALSE))  # WRONG BUT USED IN SWISS
    
    Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    
    # Without Company Dom Region, for Stoxx600 
    # Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
    # After getting the AUM values, remove that vector from the dataframe
    Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
    Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
    
  }else{
    
    AUMs <- unique(subset(Results, select = c("PortName","PortfolioAUMAnalyzed")))
    AUMs <- rename(AUMs, c("PortfolioAUMAnalyzed" = "PortAUM"))
    
    Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose  ,select = c("PortName","Technology","BenchmarkRegion","Exposure_WtTechShareTechShare", "Exposure_OGCMetrik"))
    
    # After getting the AUM values, remove that vector from the dataframe
    
    Heatmap$MarketExposure <- as.numeric(Heatmap$Exposure_WtTechShareTechShare)
    Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- as.numeric(Heatmap$Exposure_OGCMetrik[Heatmap$Technology %in% c("Oil", "Gas", "Coal")])
    Heatmap <- Heatmap[,names(Heatmap) != 'Exposure_WtTechShareTechShare']
    Heatmap <- Heatmap[,names(Heatmap) != 'Exposure_OGCMetrik']
    
  }
  
  Heatmap$MarketExposure <- Heatmap$MarketExposure*100
  
  Heatmap <- subset(Heatmap, Heatmap$BenchmarkRegion %in% BenchmarkRegionchoose)
  Heatmap <- subset(Heatmap, select = c("PortName","Technology","MarketExposure"))
  
  ComparisonTable <- reshape(Heatmap, v.names = "MarketExposure",idvar=c("PortName"),timevar="Technology", direction= "wide")
  colnames(ComparisonTable) <- gsub("MarketExposure.","",colnames(ComparisonTable))
  ComparisonTable$OilCap <- NULL
  
  # issues with rank with positive and negative values, compare a positive table only
  ComparisonPositive <- ComparisonTable
  # ComparisonPositive$PortName <- NULL
  ComparisonPositive[2:12] <- ComparisonPositive[2:12]+max(abs(ComparisonPositive[2:12]),na.rm = TRUE)+1
  
  # Order the table for Green vs Brown Tech
  goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")  
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  
  CompGood <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName",goodtech)]
  CompBad <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","CoalCap","GasCap","ICE")]
  CompFF <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","Oil","Gas","Coal")]
  
  rownames(CompGood)<- CompGood$PortName
  rownames(CompBad)<- CompBad$PortName
  rownames(CompFF)<- CompFF$PortName
  
  if (length(unique(Heatmap$PortName)) > 1){
    # ranking
    # smallest number is number 1
    rankingbad <- data.frame(apply(CompBad[2:4],2,rank, ties.method="min",na.last="keep"))
    rankingff <- data.frame(apply(CompFF[2:4],2,rank, ties.method="min",na.last="keep"))
    rankinggood <- data.frame(apply(-CompGood[2:6],2,rank, ties.method="min",na.last="keep"))
    
    rankingbad$PortName <- row.names(rankingbad)
    rankinggood$PortName <- row.names(rankinggood)
    rankingff$PortName <- row.names(rankingff)
    
    rankingtable <- merge(rankingbad,rankingff, by="PortName")
    rankingtable <- merge(rankingtable, rankinggood,by="PortName")
    
    
    # colnames(rankingtable)[1] <- "PortName"
    rankingtable <- subset(rankingtable, select = colnames(ComparisonTable))
  }else{
    rankingtable <- data.frame(rep(1,11))
  }
  
  AverageTable <- melt(ComparisonTable, id.vars="PortName")
  AverageTable <- ddply(AverageTable,.(variable),summarise, value=mean(value,na.rm=TRUE))
  AverageTableHori<- dcast(melt(AverageTable,id.vars = "variable"), ...~variable)
  AverageTableHori$. <- NULL
  AverageTableHori$PortName <- "Average"
  
  
  results <- list(rankingtable,ComparisonTable, AUMs)#, Productions)
  
  return(results)
}

# ------------ Company Comparison Data ------ #
company.comparison <- function(ChartType,BatchTest, BatchTest_PortSnapshots,Startyear, Scenariochoose,BenchmarkRegionchoose,CompanyDomicileRegionchoose){
  
  # ChartType <- "CB"
  # BatchTest <- CBBatchTest
  # BatchTest <- EQBatchTest
  # BatchTest_PortSnapshots <- EQBatchTest_PortSnapshots
  
  ### AUMs
  Results <- BatchTest
  
  if (ChartType == "EQ"){
    # AUMData<-Results[Results$PortName %in% Results$PortName,]
    AUMs <- unique(subset(Results, select=c("PortName","ComparisonType","PortAUM"))) ## CORRECT
    # AUMs <- ddply(Results, .(PortName),summarise, AUM = sum(PortAUM, na.rm=FALSE))  # WRONG BUT USED IN SWISS
    
  }else{
    
    AUMs <- unique(subset(Results, select = c("PortName","ComparisonType","PortfolioAUMAnalyzed")))
    AUMs <- rename(AUMs, c("PortfolioAUMAnalyzed" = "PortAUM"))
  }   
  
  
  ### Exposures  
  if (ChartType == "EQ"){
    Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose ,select = c("PortName","ComparisonType","Type","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    # Without Company Dom Region, for Stoxx600 
    # Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
    # After getting the AUM values, remove that vector from the dataframe
    Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
    Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
  }else{
    Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose  ,select = c("PortName","ComparisonType","Type","Technology","BenchmarkRegion","Exposure_WtTechShareTechShare", "Exposure_OGCMetrik"))
    
    # After getting the AUM values, remove that vector from the dataframe
    
    Heatmap$MarketExposure <- as.numeric(Heatmap$Exposure_WtTechShareTechShare)
    Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- as.numeric(Heatmap$Exposure_OGCMetrik[Heatmap$Technology %in% c("Oil", "Gas", "Coal")])
    Heatmap <- Heatmap[,names(Heatmap) != 'Exposure_WtTechShareTechShare']
    Heatmap <- Heatmap[,names(Heatmap) != 'Exposure_OGCMetrik']
  }
  
  Heatmap$MarketExposure <- Heatmap$MarketExposure*100
  Heatmap <- subset(Heatmap, Heatmap$BenchmarkRegion %in% BenchmarkRegionchoose)
  Heatmap <- unique(subset(Heatmap, select = c("PortName","ComparisonType","Type","Technology","MarketExposure")))
  # Heatmap <- subset(Heatmap, select = c("PortName","Technology","MarketExposure"))
  
  ComparisonTable <- reshape(Heatmap, v.names = "MarketExposure",idvar=c("PortName","ComparisonType","Type"),timevar="Technology", direction= "wide")
  colnames(ComparisonTable) <- gsub("MarketExposure.","",colnames(ComparisonTable))
  ComparisonTable$OilCap <- NULL
  
  
  
  if (ChartType == "EQ"){
    CoverageWeight <- subset(BatchTest,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose)#,select= c("PortName","Sector","Technology","MarketExposure","AUMExposure","Production","PortAUM"))
    CoverageWeight <- subset(CoverageWeight, !Technology %in% c("OilCap"))
    
    piesub_tech <- unique(subset(BatchTest_PortSnapshots,select=c("PortName","ISIN","piesector","PortWeight")))
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    pieshares <- ddply(piesub_tech, .(PortName,piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    pieshares$piesector[pieshares$piesector == "Utility Power"]<- "Power"
    pieshares <- subset(pieshares, pieshares$piesector %in% c("Fossil Fuels", "Automotive","Power"))
    
    df <- merge(CoverageWeight,pieshares, by.x = c("PortName", "Sector"), by.y = c("PortName","piesector"), all.x = TRUE, all.y = FALSE)
    df$Portfolio_weight[is.na(df$Portfolio_weight)] <- 0
    
    df$SectorAUM <- df$PortAUM*df$Portfolio_weight
    
    SectorTotAUM <- unique(subset(df, select = c("PortName","Sector","SectorAUM")))
    SectorTotAUM <- ddply(SectorTotAUM, . (PortName),summarise, SectorTotAUM = sum(SectorAUM,na.rm = TRUE))
    df<-merge(df,SectorTotAUM, by="PortName")
    
    df$SectorCoverage <- df$SectorAUM/df$SectorTotAUM
    
    df$Production[df$Technology == "Coal"]<- df$Production[df$Technology == "Coal"]*24
    df$Production[df$Technology == "Oil"]<- df$Production[df$Technology == "Oil"]*6.12
    df$Production[df$Technology == "Gas"]<- df$Production[df$Technology == "Gas"]*0.0372
    
    ProdTotals <- ddply(df, .(PortName,Sector), summarise, SecProd = sum(Production, na.rm=TRUE))
    df <- merge(df, ProdTotals, by = c("PortName", "Sector"))
    
    df$TechShare <- df$Production/df$SecProd
    df$TechAUM <- df$TechShare*df$SectorAUM
    df$CoverageWeight <- df$TechAUM/df$SectorTotAUM
    
    df[is.na(df)]<- 0
    
    CoverageWeight <- unique(subset(df, select =c("PortName","Technology","Sector","SectorTotAUM","PortAUM","CoverageWeight","ComparisonType","Type")))
    
  }else{
    
    dfall <- unique(subset(BatchTest, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose))
    dfall <- subset(dfall, !dfall$PortName %in% c("Market_Benchmark","GlobalBondUniverse","MetaPortfolio","MetaPortfolio_MetaPortfolio"))
    if (nrow(dfall)>0){
      # Fossil Fuels
      dfFF <- subset(dfall, Sector %in% c("Oil&Gas","Coal"))
      
      dfFF$TechShare <- dfFF$SectorWeight
      dfFF$TechShare[dfFF$Sector %in% "Oil&Gas"] <- dfFF$SectorWeight[dfFF$Sector %in% "Oil&Gas"]*dfFF$PortTechShare[dfFF$Sector %in% "Oil&Gas"]
      TSSUM <- ddply(dfFF, .(PortName), summarize, TSSUM = sum(TechShare, na.rm = TRUE))
      dfFF <- merge(dfFF,TSSUM, by= "PortName")
      dfFF$TechShare <-dfFF$TechShare/dfFF$TSSUM 
      
      dfFF <- subset(dfFF, select = c("Sector","PortName","Technology","TechShare"))
      # dfFF$Sector <- "Fossil Fuels"
      
      dfFF$TechShare[is.nan(dfFF$TechShare)] <- 0
      
      # Automotive, Power
      df <- subset(dfall, Sector %in% c("Automotive","Power"))
      df <- subset(df, select=c("PortName","Sector","Technology","WtTechShareTechShare"))
      df <- melt(df, id.vars = c("PortName","Sector","Technology"))
      df <- rename(df, c("value"="TechShare"))
      df$variable <- NULL
      
      df$TechShare[is.nan(df$TechShare)] <- 0
      df <- subset(df,!Technology %in% "OilCap")
      
      df <- subset(df,!Technology %in% "OilCap")
      powersums <- ddply(df, .(PortName,Sector), summarise, SectorTotal=sum(TechShare, na.rm=TRUE))
      df  <- merge(df, powersums, by = c("PortName","Sector"))
      
      df$TechShare <- df$TechShare/df$SectorTotal
      df$SectorTotal <- NULL
      
      # Combine and normalise
      
      dftot <- rbind(df,dfFF)
      sectorsums <- ddply(dftot, .(PortName,Sector), summarise, SectorTotal=sum(TechShare, na.rm=TRUE))
      dftot  <- merge(dftot, sectorsums, by = c("PortName","Sector"))
      
      dftot$TechShare <- dftot$TechShare/3
      dftot$SectorTotal <- NULL
      
      # Sector sum = 3, should equal 1?
      dfallsectors <- merge(dfall, dftot, by= c("PortName","Sector","Technology"))
      
      secweight <- ddply(dfallsectors, .(PortName, Sector),summarise, SectorWtTotal=sum(SectorWeight, na.rm=TRUE))
      secweightsum <- ddply(secweight, .(PortName),summarise, sumsecwts = sum(SectorWtTotal, na.rm = TRUE))
      
      sectors <- merge(secweight, secweightsum, by= "PortName")
      sectors$SectorWtTotal <- sectors$SectorWtTotal/sectors$sumsecwts
      sectors$sumsecwts<- NULL
      
      
      dfallsectors<- merge(dfallsectors,sectors, by= c("PortName", "Sector"))
      
      dfallsectors$TechShare <- dfallsectors$TechShare*dfallsectors$SectorWtTotal
      dfallsectors$SectorWtTotal <- NULL  
      dfallsectors <- rename(dfallsectors, c("TechShare"="CoverageWeight"))
      
      
      CoverageWeight <- unique(subset(dfallsectors, select = c("PortName","Technology","CoverageWeight","SectorWeight", "PortfolioAUMAnalyzed","ComparisonType","Type")))
      CoverageWeight <- rename(CoverageWeight, c("PortfolioAUMAnalyzed"="PortAUM"))
    }else{
      CoverageWeight <- "NoCoverageWeight"
    }
  } 
  
  if (CoverageWeight != "NoCoverageWeight"){
    CoverageWeightComparison <- subset(CoverageWeight, ComparisonType == "ComparisonResults")
    WeightedCoverageWeight <- ddply(CoverageWeightComparison, .(Technology,ComparisonType,Type),summarise, CoverageWeight= weighted.mean(CoverageWeight,PortAUM,na.rm = TRUE))
    CalculatedCWs <- nrow(unique(subset(WeightedCoverageWeight, select= c("ComparisonType","Type"))) )
    WeightedCoverageWeight$CoverageWeight <- WeightedCoverageWeight$CoverageWeight/(sum(WeightedCoverageWeight$CoverageWeight)/CalculatedCWs)
    WeightedCoverageWeight$PortName <- "WeightedCoverageWeight"
    
    
    
    # CoverageWeightDF <- subset(CoverageWeight, select = c("PortName","ComparisonType","Technology","CoverageWeight"))
    # CoverageWeightDF <- rbind(CoverageWeightDF,WeightedCoverageWeight)
  }else{
    CoverageWeight<- "NoCoverageWeight"
  }
  
  
  
  
  results <- list(ComparisonTable, AUMs, CoverageWeight, WeightedCoverageWeight)
  
  return(results)
}

# -------- Seperate Ranking chart -----------
RankPortfolios <- function(ComparisonExposures, PortfolioExposures, PortName){
  
  # ComparisonExposures<- EQComparisonExposures
  # PortfolioExposures <- EQExposure
  
  ComparisonPositive <- rbind(ComparisonExposures,PortfolioExposures)
  TechList <- c("Electric","Hybrid","ICE","Coal","Oil","Gas","RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap")
  
  ComparisonPositive[colnames(ComparisonPositive) %in% TechList] <- ComparisonPositive[colnames(ComparisonPositive) %in% TechList]+max(abs(ComparisonPositive[colnames(ComparisonPositive) %in% TechList]),na.rm = TRUE)+1
  
  # Order the table for Green vs Brown Tech
  goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")  
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  
  CompGood <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName",goodtech)]
  CompBad <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","CoalCap","GasCap","ICE")]
  CompFF <- ComparisonPositive[colnames(ComparisonPositive) %in% c("PortName","Oil","Gas","Coal")]
  
  rownames(CompGood)<- CompGood$PortName
  rownames(CompBad)<- CompBad$PortName
  rownames(CompFF)<- CompFF$PortName
  
  
  # ranking
  # smallest number is number 1
  rankingbad <- data.frame(apply(CompBad[2:4],2,rank, ties.method="min",na.last="keep"))
  rankingff <- data.frame(apply(CompFF[2:4],2,rank, ties.method="min",na.last="keep"))
  rankinggood <- data.frame(apply(-CompGood[2:6],2,rank, ties.method="min",na.last="keep"))
  
  rankingbad$PortName <- row.names(rankingbad)
  rankinggood$PortName <- row.names(rankinggood)
  rankingff$PortName <- row.names(rankingff)
  
  rankingtable <- merge(rankingbad,rankingff, by="PortName")
  rankingtable <- merge(rankingtable, rankinggood,by="PortName")
  
  
  # colnames(rankingtable)[1] <- "PortName"
  rankingtable <- subset(rankingtable, select = c("PortName",TechList))
  # rankportfolio <- rankingtable[rankingtable$PortName == PortName,]
  
  return(rankingtable)
}

#----------- BBG Data Cleaning -------------- #
cleanBBGData <- function(BBGData_CompanyLevel,AllCompanyData,Startyear,Scenariochoose,CompanyDomicileRegionchoose,BenchmarkRegionchoose){
  
  BBGData_Companies <- subset(BBGData_CompanyLevel, select = c("EQY_FUND_TICKER", "ICB_SUBSECTOR_NAME"))
  UtilitiesICB <- c("Alternative Electricity","Conventional Electricity","Multiutilities")
  OilGasICB <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production")
  FuturesecsICB <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation")
  AutoICB <- c("Automobiles","Commercial Vehicles & Trucks")
  
  BBGData_Companies$ICB_SUBSECTOR_NAME[BBGData_Companies$ICB_SUBSECTOR_NAME %in% AutoICB] <- "Automotive"
  BBGData_Companies$ICB_SUBSECTOR_NAME[BBGData_Companies$ICB_SUBSECTOR_NAME %in% UtilitiesICB] <- "Power"
  BBGData_Companies$ICB_SUBSECTOR_NAME[!BBGData_Companies$ICB_SUBSECTOR_NAME %in% c("Automotive","Power")] <- "Other"
  if (length(grep("Aggregate",BenchmarkRegionchoose)) == 1){
    Companies <- subset(AllCompanyData, Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & BenchmarkRegion == sub("Aggregate","",BenchmarkRegionchoose))
  }else{
    Companies <- subset(AllCompanyData, Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & BenchmarkRegion == BenchmarkRegionchoose)
  }
  Companies <- join(Companies,BBGData_Companies, by = "EQY_FUND_TICKER" )
  UtilityCompanies <- subset(Companies,  ICB_SUBSECTOR_NAME == "Power"& Sector == "Power")
  AutoCompanies <- subset(AllCompanyData, Technology %in% c("Electric","Hybrid","ICE") & Year == Startyear+5 & CompanyDomicileRegion == CompanyDomicileRegionchoose & BenchmarkRegion == "Global")
  
  CleanedResults <- list(Companies,UtilityCompanies,AutoCompanies)
  return(CleanedResults)
  
}

#------------ Clean O&G Data ---------------- #
cleanOGData <- function(OGData,AllCompanyData,Startyear){
  techlist <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
  OilData <- subset(OGData, OGData$Technology %in% "Oil" & OGData$Year %in% (Startyear+5))  
  OilData$Resource.Type[!OilData$Resource.Type %in% techlist] <- "Other & Unknown"
  OilData$Resource.Type[OilData$Resource.Type %in% "Other"] <- "Other & Unknown"
  OilData <- rename(OilData, c("Ticker" = "EQY_FUND_TICKER"))
  OilCompanyData <- unique(subset(AllCompanyData, AllCompanyData$Sector %in% "Fossil Fuels" & AllCompanyData$Year %in% (Startyear+5), select = c("EQY_FUND_TICKER","Name")))
  Data <- merge(OilData,OilCompanyData, by = "EQY_FUND_TICKER", all.x=TRUE, all.y=FALSE)
  return(Data)
}

#----------- Prepare Graph Translations ----- #
preptranslations <- function(TranslationType,GraphTranslation, Languagechoose, Startyear){
  GT <- subset(GraphTranslation, select = c("TextLabel",Languagechoose))
  
  # GT <- subset(ReportTranslation, select = c("TextLabel",Languagechoose))
  
  GT <- setNames(data.frame(t(GT[,-1])), GT[,1])
  
  # a <- cat("\\%")
  GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("Startyear\\+5",as.character(Startyear+5),x) else x))  
  GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("Startyear",Startyear,x) else x))
  
  if (TranslationType == "Report"){
    GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("%",as.character("\\\\\\%"),x,fixed = TRUE) else x))
    GT <- as.data.frame(lapply(GT,function(x) if(is.character(x)|is.factor(x)) gsub("&",as.character("\\\\\\&"),x,fixed = TRUE) else x))
  }
  
  GT <- data.frame(lapply(GT, str_trim), stringsAsFactors = FALSE)
  GT <- data.frame(lapply(GT, as.character), stringsAsFactors=FALSE)
  
  
  return(GT)
  
}

# ---------- Match OS Data ------------------ #
matchOS <- function(OSdata, PortSnapshot){
  
  OSdatared <- unique(subset(OSdata, select = c("Issuer","Sector","ISIN","Weighted.Emissions.Factor","Weighted.Target.Emissions.Factor")) )
  
  PortSnapshot$Sector <- NULL
  
  OS <- merge(PortSnapshot,OSdatared,by = "ISIN")
  
  WEF <- ddply(OS,.(PortName,Sector),summarise,
               EmissionsFactor =weighted.mean(Weighted.Emissions.Factor,AUM),
               TargetEmissionsFactor =weighted.mean(Weighted.Target.Emissions.Factor,AUM))
  # Returns the WEF for each company by sector
  return(WEF)    
}

# ------------ Sector Selector
SectorSelect <- function(TechToPlot){
  SectorToPlot <- "None"
  if (TechToPlot %in% c("RenewablesCap","HydroCap", "NuclearCap","CoalCap","GasCap")){
    SectorToPlot <- "Power" 
  }
  if (TechToPlot %in% c("ICE","Hybrid", "Electric")){
    SectorToPlot <- "Automotive" 
  }
  if (TechToPlot %in% c("Coal","Oil","Gas")){
    SectorToPlot <- "Fossil Fuels"
  }
  if(TechToPlot %in% c("Fossil Fuels","Automotive","Power")){
    SectorToPlot <- TechToPlot
  }
  
  return(SectorToPlot) 
}

# ----Sector Productions
SectorProduction <- function(combin,ChartType){
  
  TechList <- c("RenewablesCap","HydroCap", "NuclearCap","CoalCap","GasCap","ICE","Hybrid", "Electric","Coal","Oil","Gas")
  
  
  if (ChartType == "CB"){
    combin <- rename(combin, c("WtProduction"="Production"),warn_missing = F)
    combinsector <- subset(combin,combin$Year == Startyear & combin$BenchmarkRegion == BenchmarkRegionchoose, select = c("Sector","Technology","Production")) 
  }else{
    combinsector <- subset(combin,combin$Year == Startyear & combin$BenchmarkRegion == BenchmarkRegionchoose & combin$CompanyDomicileRegion == CompanyDomicileRegionchoose, select = c("Sector","Technology","Production")) 
    
  }
  
  combinsector <- combinsector %>% complete(Technology = TechList, fill = list(Production = 0))
  combinsector$Sector <- t(data.frame(lapply(combinsector$Technology, function(x) SectorSelect(x))))
  prodsectors <- aggregate(Production ~ Sector, data = combinsector, function(x) sum(x, na.rm = T))
  
  return(prodsectors)
}

SectorPrint <- function(SectorToPlot,SectorProd){
  
  # TechToPlot <- "Coal"
  # SectorProd <- EQSectorProd  
  
  # SectorToPlot <- SectorSelect(TechToPlot)
  SectorProduction <- SectorProd$Production[SectorProd$Sector %in% SectorToPlot]
  
  PlotFlag <- 1
  if (SectorToPlot %in% c("Automotive","Power", "Fossil Fuels") & SectorProduction == 0){PlotFlag <- 0}
  
  return(PlotFlag)
  
}


#-------- Graph Inputs ---------- #
SetGraphInputs <- function(){
  #Saturated colours
  RenewablesColour <<- "#b3de69"
  HydroColour <<- "#428bbd"
  NuclearColour <<- "#827ab8"
  GasCapColour<<-"grey75"
  CoalCapColour <<- "#252525"
  ElectricColour<<- "#69c454"
  HybridColour<<- "#00b7be"
  ICEColour<<- "#2F4F4F"   #"#ed1c24" #"#f93620"
  GasProdColour <<- "#ffb861"
  OilProdColour <<- "#c88450"
  CoalProdColour <<- "#835834"
  
  YourportColour <<- "#265b9b"   #"#2e4b6e"  #"#17224D"
  IndexColour <<-  "grey85"
  Tar2DColourBar <<- "#b3de69"
  Tar2DColour <<- "#a1c75e"
  goodexpColour <<- "#1F891F"
  badexpColour <<- "#ed1c24" #"#fb8072"
  ReqCapColour <<- "grey55"
  CurrCapColour <<- "grey75"
  AxisColour <<- "#17375e" #"#274F80"
  
  DarkGreen <<- "#1E7B1E"
  LightGreen <<- "#C3FDB8"
  LightRed <<- "#FFFFC2"
  DarkRed <<- "#C11B17"
  
  
  
  
  ColourPalette <<- data.frame(Sector = c("Power","Power","Power","Power","Power","Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels"),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Electric","Hybrid","ICE","Gas","Oil","Coal"),Colours =c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,ElectricColour,HybridColour,ICEColour,GasProdColour,OilProdColour,CoalProdColour))
  
  textsize <<- 8
  ppi <<- 600
  
}

#-------- Green/Brown Tech ------------- #

GreenBrown <- function(Tech){
  GreenTechs <- c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap")
  
  if(Tech %in% GreenTechs){
    TechIs <- "Green"}else{
      TechIs <- "Brown"  }
  
  return(TechIs)
}
