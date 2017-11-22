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

# ------------ Exposure Data ---------------- #
exposure_data <- function(ChartType,PortSnapshot, combin, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, Startyear,PortfolioName){
  
  if (ChartType == "EQ"){
    Results <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose)
    # if (!"Automotive" %in% Results$Sector){
    #   ResultsAuto <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == "Global" &Sector== "Automotive" & CompanyDomicileRegion == CompanyDomicileRegionchoose)
    #   Results <- rbind(Results,ResultsAuto)}
  }else{
    Results <- subset(combin,Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose)
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% "IssLvlPortWeight"] <- "PortWeight"
  }
  
  if   (ChartType == "EQ"){
    Results <- subset(Results,select= c("Sector","Technology","MarketExposure","AUMExposure","Production","PortAUM")) 
  }else{
    Results <- subset(Results, select = c("Sector","Technology","MarketExposure","WtProduction")) #"AUMExposure",,"PortAUM"
    colnames(Results)[colnames(Results) %in% "WtProduction"] <- "Production"
  }
  
  
  # SectorProduction <- ddply(Results, .(Sector),summarise, Production  =sum(Production))
  
  # Results <- subset(Results, !Technology %in% c("OilCap"))
  
  piesub_tech <- unique(subset(PortSnapshot,select=c("ISIN","piesector","PortWeight")))
  piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
  piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
  pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
  
  techshares <- data.frame(piesector = c("Fossil Fuels", "Automotive","Utility Power"))
  techshares <- merge(techshares,pieshares, by = "piesector", all.x = TRUE)
  techshares$Portfolio_weight[is.na(techshares$Portfolio_weight)] <- 0
  # techshares <- subset(pieshares, piesector %in% c("Fossil Fuels", "Automotive","Utility Power")) 
  techshares$Portfolio_weight <- techshares$Portfolio_weight/sum(techshares$Portfolio_weight)
  
  Results$Production[Results$Technology == "Coal"]<- Results$Production[Results$Technology == "Coal"]*24
  Results$Production[Results$Technology == "Oil"]<- Results$Production[Results$Technology == "Oil"]*6.12
  Results$Production[Results$Technology == "Gas"]<- Results$Production[Results$Technology == "Gas"]*0.0372
  
  Results$CoverageWeight <- 1
  Results$CoverageWeight[Results$Sector == "Fossil Fuels"]<-Results$Production[Results$Sector == "Fossil Fuels"]/sum(Results$Production[Results$Sector == "Fossil Fuels"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Fossil Fuels"]
  Results$CoverageWeight[Results$Sector == "Automotive"]<-Results$Production[Results$Sector == "Automotive"]/sum(Results$Production[Results$Sector == "Automotive"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Automotive"]
  Results$CoverageWeight[Results$Sector == "Power"]<-Results$Production[Results$Sector == "Power"]/sum(Results$Production[Results$Sector == "Power"],na.rm = TRUE)*techshares$Portfolio_weight[techshares$piesector =="Utility Power"]
  
  Results$Exposure <- Results$MarketExposure*100
  Results$Exposure[Results$Sector == "Fossil Fuels"] <- Results$AUMExposure[Results$Sector == "Fossil Fuels"]*100
  
  Results <- subset(Results, select =c("Technology","Sector","CoverageWeight","Exposure"))
  
  Technology<-c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap", "Electric", "Hybrid", "ICE","Gas","Oil", "Coal")
  Sector<-c("Power","Power","Power","Power","Power","Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels")
  TechOrder<-c(1,2,3,4,5,6,7,8,9,91,92)
  Results$Sector <- NULL
  tempdb<-cbind(Technology,Sector,TechOrder)
  Results <- merge(tempdb,Results,by="Technology",all.x=TRUE,all.y=FALSE)
  Results <-arrange(Results, TechOrder)  
  Results$TechOrder <- NULL
  
  #if Exposure is >100% cut off for graphic
  Results$ExposureChart<-Results$Exposure
  Results$CoverageWeight[Results$CoverageWeight == 0] <- NA 
  Results$ExposureChart[is.na(Results$CoverageWeight) & is.na(Results$ExposureChart)] <- 0
  Results$CoverageWeight[is.na(Results$CoverageWeight) & !Results$Technology %in% c("Oil", "Gas","Coal")] <- 0.001 # add sliver for Jakob.
  Results$CoverageWeight[is.na(Results$CoverageWeight)] <- 0
  Results$ExposureChart[Results$Exposure > 100] <- 100 # Limits graphical display for really high values
  Results$ExposureChart[Results$Exposure < -100] <- -100 # Limits graphical display for really low values
  Results$CoverageWeight<-Results$CoverageWeight/sum(Results$CoverageWeight) #rescale results to accommodate sliver
  
  #calaculate the widths depending on th portfolio weights
  tempdb<-subset(Results, CoverageWeight != 0)
  tempdb$w <- cumsum(tempdb$CoverageWeight)
  tempdb$wm <- tempdb$w - tempdb$CoverageWeight
  Results <- merge(tempdb,Results,by=c("Technology","Exposure","ExposureChart","CoverageWeight","Sector"),all.x=TRUE,all.y=TRUE)
  
  #Results$w <- cumsum(Results$CoverageWeight)
  # Results$wm <- ifelse(is.na(Results$CoverageWeight),0,Results$w - Results$CoverageWeight)
  #sort dataframe
  Results$Sector <- NULL
  tempdb<-cbind(Technology,Sector,TechOrder)
  Results <- merge(tempdb,Results,by="Technology",all.x=TRUE,all.y=FALSE)
  Results <-arrange(Results, TechOrder)  
  Results$TechOrder <- NULL
  
  #round to one decimal place
  Results$Exposure <- round(Results$Exposure, digits = 1)
  Results$Exposure[which(Results$Exposure == -100 & Results$Technology %in% c("Oil", "Gas","Coal"))] <- "N/A" 
  Results$Exposure[is.na(Results$Exposure)] <- -100
  Results$ExposureChart[Results$ExposureChart == 0] <- -100
  Results$CoverageWeight <- round(Results$CoverageWeight, digits = 3) 
  
  return(Results)
}

# ------------ Average Coverage Weight Data -------- #
CoverageWeight_data <- function(ChartType,PortfolioType,BatchTest_PortSnapshots, BatchTest, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose){
  # This function returns the Coverage weight of either the average, or a specific Portfolio as specified in PortfolioName (or equal to Average)
  
  # BatchTest_PortSnapshots<-CBBatchTest_PortSnapshots
  # BatchTest<-CBBatchTest
  # ChartType<- "CB"
  # PortfolioType <- "Investor"
  # PortfolioType <- c("Portfolio","Investor","InvestorMPs")
  
  
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
    
  }
  
  
  
  # df<-ResultsDF
  # df[is.na(df)]<- 0
  # 
  # 
  # # Original Weighted mean calc
  # Results <- subset(df, select =c("PortName","Technology","Sector","SectorTotAUM","CoverageWeight"))
  # 
  # WeightedResults <- ddply(Results, .(Technology),summarise, CoverageWeight= weighted.mean(CoverageWeight,SectorTotAUM,na.rm = TRUE))
  # WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sum(WeightedResults$CoverageWeight)  
  # WeightedResults$PortName <- "WeightedResults" 
  # 
  # ResultsDF <- subset(Results, select = c("PortName","Technology","CoverageWeight"))
  # ResultsDF <- rbind(ResultsDF,WeightedResults)
  
  
  # Weighted mean with port AUM 
  
  WeightedResults <- ddply(Results, .(Technology),summarise, CoverageWeight= weighted.mean(CoverageWeight,PortAUM,na.rm = TRUE))
  WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sum(WeightedResults$CoverageWeight)  
  WeightedResults$PortName <- "WeightedResults" 
  
  ResultsDF <- subset(Results, select = c("PortName","Technology","CoverageWeight"))
  ResultsDF <- rbind(ResultsDF,WeightedResults)
  
  
  
  
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
  EQlist <- as.data.frame(unique(EQDataShort$PortName)[1:nofunds])
  colnames(EQlist) <- "PortName" 
  EQlist$Type <- "EQ"
  
  CBDataShort <- unique(subset(CBData, select=c("PortName","PortfolioAUMAnalyzed")))
  CBDataShort <- subset(CBDataShort, !CBDataShort$PortName %in% c("ListedMarket_ListedMarket","Fund"))
  CBDataShort <- CBDataShort[order(CBDataShort, decreasing = TRUE),]
  CBlist <- as.data.frame(unique(CBDataShort$PortName)[1:nofunds])
  colnames(CBlist) <- "PortName" 
  CBlist$Type <- "CB"
  
  ComparisonList <- rbind(EQlist,CBlist)
  return(ComparisonList)
}

# ----------- Port Filter Funds ------------------ #
Portfunds <- function(maxno,FundList,FundsDataAll, PortfolioName,InvestorName){
  
  if (typeof(FundList)=="list"){
  FundsListData<- FundList
  FundsListData$InvestorName<- gsub(" ","",FundsListData$InvestorName)
  FundsListData$PortfolioName<- gsub(" ","",FundsListData$PortfolioName)
  
  FundsListPort <- FundsListData[FundsListData$InvestorName == InvestorName & FundsListData$PortfolioName == PortfolioName,]
  if (nrow(FundsListPort) >0 ){
    
    # FundsDataAll$ISIN <- str_split(FundsDataAll$PortName,"[01]_AllFunds",simplify = TRUE)[,1]
    FundsDataAll$ISIN <- substring(FundsDataAll$PortName,1,12)
   
    # FundsListPort$FundISINName <-  paste0(FundsListPort$FundISIN, "0_AllFunds")
    FundsListPort <- FundsListPort[rev(order(FundsListPort$ValueUSD)),]
    
    FundsListPort <- FundsListPort[1:min(maxno, nrow(FundsListPort)),]
    FundsDataPort <- subset(FundsDataAll,FundsDataAll$ISIN %in% FundsListPort$FundISIN)  
     # FundsDataPort <- subset(FundsDataAll, FundsDataAll$PortName %in% FundsListPort$FundISINName)
    
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
  
  # ChartType <- "EQ"
  # BatchTestComparison <- EQPortfolioResultsRaw

  Results <- BatchTestComparison
  # Results <- subset(Results, Results$)
  
  if (ChartType == "EQ"){
    AUMData<-Results[Results$PortName %in% Results$PortName,]
    AUMs <- ddply(Results, .(PortName),summarise, AUM = sum(PortAUM, na.rm=FALSE))
    
    Heatmap <- subset(Results, Results$Year == Startyear+5 & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    
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
  
  if (length(unique(Heatmap$PortName)) > 1){
    CompDF <- rbind(AverageTableHori[1:11],ComparisonTable[2:12])
    SDTable <- apply(CompDF,2,function(x)x-x[1])
    SDTable<- SDTable[-1,]
    SDTable <- as.data.frame(SDTable)  
    SDTable <- cbind(ComparisonTable$PortName,SDTable)
    colnames(SDTable)<- colnames(ComparisonTable)
  }else{
    SDTable <- data.frame(rep(1,11))
  }
  
  results <- list(rankingtable,ComparisonTable, SDTable, AUMs)#, Productions)
  
  return(results)
}

# -------- Seperate Ranking chart -----------
RankPortfolios <- function(ComparisonExposures, PortfolioExposures, PortName){
  
  # ComparisonExposures<- EQComparisonExposures 
  # PortfolioExposures <- EQExposure
  ComparisonPositive <- rbind(ComparisonExposures,PortfolioExposures)
  
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
  rankingtable <- subset(rankingtable, select = colnames(ComparisonPositive))
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
  
  # OSset <- subset(OS, Sector == Sectorchoose)
  
  WEF <- ddply(OS,.(PortName,Sector),summarise,
               EmissionsFactor =weighted.mean(Weighted.Emissions.Factor,AUM),
               TargetEmissionsFactor =weighted.mean(Weighted.Target.Emissions.Factor,AUM))
  # Returns the WEF for each company by sector
  return(WEF)    
}




#-------- Top 5 companies -------
top5s <- function(ProdSnapshot,PortSnapshot){

  # ProdSnapshot <- unique(EQCompProdSnapshot)
  # PortSnapshot <- unique(EQPortSnapshot)
  #

  dfProd <- subset(ProdSnapshot,ProdSnapshot$Year == Startyear, select = c("EQY_FUND_TICKER","Technology","CompLvlProduction","Production"))
  dfPort <- subset(PortSnapshot, select = c("Name","EQY_FUND_TICKER","PortWeight"))

  df <- merge(dfProd,dfPort, by ="EQY_FUND_TICKER")

  # dfCompLvl <- df[ , names(df) != "Production"]
  dfPortProd <- df[ , names(df) != "CompLvlProduction"]
  dfPortProd <- dfPortProd[dfPortProd$PortWeight != 0, ]
  dfPortProd <- dfPortProd[dfPortProd$Production != 0, ]


}



# --------
# REPORTING FUNCTIONS
# --------

#----------- Creates the figure list for the report
figure_list <- function(figurelist){
  figurelist <- as.data.frame(list.files(RegionDirectory,pattern=c("\\.png$"), full.names = FALSE))
  colnames(figurelist)<- "FigName"
  figurelist$ordernumber <- as.numeric(gsub("[A-z _ .]","",figurelist$FigName))
  figurelist <- figurelist[order(figurelist$ordernumber),]  
  figurelist$ordernumber <- NULL
  figurelist$FigName <- gsub(".png","",figurelist$FigName)
  
  write.table(figurelist,"FigureList.txt",row.names = FALSE, col.names = FALSE)
}

# ------------- REPORT DATA ----------------- #
report_data <- function(ChartType,combin, Exposures,AUMData,Ranks,PortSnapshot,CompanyDomicileRegionchoose, BenchmarkRegionchoose,Scenariochoose, Startyear,PortfolioName){
  
  # combin <- EQCombin
  # Exposures<-EQExposures
  # AUMData<-EQAUMData
  # Ranks<-EQRanks
  # PortSnapshot<-EQPortSnapshot
  # ChartType = "EQ"
  
  
  if (nrow(combin)>0){
    
    PortSnapshot <- rename(PortSnapshot, c("IssLvlPortWeight"="PortWeight"),warn_missing = FALSE)
    # Pie Share Data
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    # Numbers to print
    PieAssessedShare <- round((1-pieshares$Portfolio_weight[pieshares$piesector %in% "Not Assessed"]),2)*100
    if(length(pieshares$Portfolio_weight[pieshares$piesector%in% "Not Assessed"])==0){PieAssessedShare<-100}
    
    # Line Chart Data
    if (ChartType == "EQ"){
      LineData <- subset(combin, BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose & Year %in% (Startyear+5))  
      LineData <- subset(LineData, select = c("Sector","Technology","Year","Production","TargetProductionAlignment","TargetProductionAUMIntensity"))
      LineData$Check <- LineData$Production-LineData$TargetProductionAlignment
      LineData$Check[LineData$Sector %in% "Fossil Fuels"] <- LineData$Production[LineData$Sector %in% "Fossil Fuels"]-LineData$TargetProductionAUMIntensity[LineData$Sector %in% "Fossil Fuels"]
      
    }else{
      LineData <- subset(combin, Year %in% (Startyear+5) & BenchmarkRegion %in% BenchmarkRegionchoose  & Scenario %in% Scenariochoose)    
      LineData <- subset(LineData, select = c("Sector","Technology","Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare","Benchmark_OGC","OGCMetrik_Portfolio")) 
      LineData$Check <- LineData$WtTechShareTechShare - LineData$Benchmark_WtTechShareTechShare
      LineData$Check[LineData$Sector %in% c("Oil&Gas","Coal")]<- LineData$OGCMetrik_Portfolio[LineData$Sector %in% c("Oil&Gas","Coal")] - LineData$Benchmark_OGC[LineData$Sector %in% c("Oil&Gas","Coal")]
      LineData$Production <- LineData$WtTechShareTechShare
      LineData$Production[LineData$Sector %in% c("Oil&Gas","Coal")] <-LineData$OGCMetrik_Portfolio[LineData$Sector %in% c("Oil&Gas","Coal")]
    } 
    
    LineData$Technology <- revalue(LineData$Technology, c("Coal"="CoalProd","Gas"="GasProd","Oil"="OilProd"))
    
    # 1 indicates it is aligned, 0 is misaligned
    # # Rating = to 1 if the Production is higher than the target for Good Techs
    goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")
    badtech <- c("ICE","OilProd","GasProd","CoalProd","GasCap","CoalCap")
    # 
    LineData$Rating <- "Check"
    # LineData$Rating[LineData$Check > 0 & LineData$Technology %in% badtech] <- 0
    # LineData$Rating[LineData$Check < 0 & LineData$Technology %in% badtech] <- 1
    # LineData$Rating[LineData$Check < 0 & LineData$Technology %in% goodtech] <- 0
    # LineData$Rating[LineData$Check > 0 & LineData$Technology %in% goodtech] <- 1
    LineData$Rating[LineData$Check < 0] <- 0
    LineData$Rating[LineData$Check > 0] <- 1
    LineData$Rating[is.na(LineData$Production)] <- NA
    LineData <- subset(LineData,!Technology %in% "OilCap", select = c("Technology","Rating"))
    LD <- setNames(data.frame(t(LineData[,-1])), LineData[,1]) 
    
    
    # Ranking Chart
    df <- Exposures
    df <- merge(df,AUMData, by= "PortName")
    df <- rename(x = df, c("PortAUM"="AUM"),warn_missing = FALSE)
    
    WM<- as.data.frame(lapply(df[ , 2:12], weighted.mean, na.rm=TRUE,  w = df$AUM))
    WM$PortName <- "WeightedMean"
    df$AUM <- NULL
    df <- df[df$PortName %in% PortfolioNameLong,]
    df <- rbind(df,WM)
    
    df <- setNames(data.frame(t(df[,-1])), df[,1]) 
    df$Check <- df$WeightedMean - df[,1]
    df$Rating <- 1
    df$Rating[df$Check > 0] <- 0
    df$Rating[is.na(df[,1])] <- NA
    
    TechsAboveAlignment <- as.integer(sum(as.numeric(LineData$Rating), na.rm = TRUE))
    TechsAboveMean <- as.integer(sum(as.numeric(df$Rating), na.rm = TRUE))
    TechsInPort <- as.integer(length(which(!is.na(LineData$Rating))))
    
    
    ReportData <- data.frame(row.names=c("PieAssessedShare","TechsAboveAlignment", "TechsAboveMean", "TechsInPort"))
    ReportData$Values <- c(PieAssessedShare,TechsAboveAlignment,TechsAboveMean,TechsInPort)
    ReportData <- data.frame(t(ReportData))
    ReportData <- cbind(ReportData,LD)
    }else{
    
    ReportData <- data.frame()
  }
  
  return(ReportData)
}

# ------------ Report Generator ------------- #
report <- function(PortfolioName,ReportName, InvestorName, template, RT,EQReportData,CBReportData, FundsInPort,OtherSectors,mini_chart_list,RenewAdds,Languagechoose){
  
  PORTFOLIONAME <- toupper(ReportName)
  
  # Copy in the template for the report
  text <- as.data.frame(template,stringsAsFactors = FALSE)  
  colnames(text) <- "text"
  
  # Add in numerics/conditionals
  # Changes the more or less for each Technology
  if (nrow(EQReportData)>0){
    EQTechList <- as.data.frame(paste0("EQCaption",colnames(EQReportData)[5:length(EQReportData)]))  
    colnames(EQTechList) <- "CaptionTitle" 
    EQTechList$Test <- t(EQReportData[5:length(EQReportData)])
    EQTechList$Caption <- RT["CaptionMore"][[1]]
    EQTechList$Caption[EQTechList$Test == 0] <- RT["CaptionLess"][[1]]
    EQTechList$Test <- NULL
    EQTechList<- setNames(data.frame(t(EQTechList[,-1])), EQTechList[,1])
    
    RT$EQCoverage <- paste(EQReportData$PieAssessedShare,as.character(" \\\\\\%"))
    
    RT$EQTechsAlign <- EQReportData$TechsAboveAlignment
    RT$EQTechsWM <- EQReportData$TechsAboveMean
    RT$EQTechsPort <- EQReportData$TechsInPort  
    RT <- cbind(RT,EQTechList)  
  }
  
  if (nrow(CBReportData)>0){
    CBTechList <- as.data.frame(paste0("CBCaption",colnames(CBReportData)[5:length(CBReportData)]))  
    colnames(CBTechList) <- "CaptionTitle" 
    CBTechList$Test <- t(CBReportData[5:length(CBReportData)])
    CBTechList$Caption <- RT["CaptionMore"][[1]]
    CBTechList$Caption[CBTechList$Test == 0] <- RT["CaptionLess"][[1]]
    CBTechList$Test <- NULL
    CBTechList<- setNames(data.frame(t(CBTechList[,-1])), CBTechList[,1])
    
    RT$CBCoverage <- paste(CBReportData$PieAssessedShare,as.character(" \\\\\\%"))
    RT$CBTechsAlign <- CBReportData$TechsAboveAlignment
    RT$CBTechsWM <- CBReportData$TechsAboveMeanreport_da
    RT$CBTechsPort <- CBReportData$TechsInPort  
    
    RT <- cbind(RT,CBTechList)
  }
  
  RT$Languagechoose <- Languagechoose
  
  # Check for each technology, 
  # mini_chart_list
  techpageremoval <- data.frame("PowerEQ"=sum(mini_chart_list[1:3]),"PowerCB"=sum(mini_chart_list[4:6]),"AutomotiveEQ"=sum(mini_chart_list[7:9]),"AutomotiveCB"=sum(mini_chart_list[10:12]),"FossilFuelsEQ"=sum(mini_chart_list[13:15]),"FossilFuelsCB"=sum(mini_chart_list[16:18]))
  removesectors <- colnames(techpageremoval[which(techpageremoval == 0)]) 
  removecaptions <- colnames(mini_chart_list[which(mini_chart_list == 0)])

  # removes mini line chart captions
  if(length(removecaptions)>0){
    for (i in 1:length(removecaptions)){
    text$text[grepl(removecaptions[i],text$text)]<- ""
  }}

  # removes the sectors  
  if(length(removesectors)>0){
    for (i in 1:length(removesectors)){
    startpage <- which(grepl(paste0(removesectors[i],"s"), text$text))
    endpage <- which(grepl(paste0(removesectors[i],"e"), text$text))
    
    removelist <- startpage:endpage
    text <- as.data.frame(text[-removelist,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    }}
  
  # removes bond pages
  if (nrow(CBReportData)==0){
    # pages <- c(9,11,13,15)
    cbpages <- which(grepl("CBPages",text$text))
    cbpageend <- which(grepl("CBPageEnd",text$text))
    
    removelist <- lapply(1:length(cbpages), function(x) c(cbpages[c(x)]:cbpageend[c(x)]))
    removelist <- melt(removelist[1:length(cbpages)])
    text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    
    piepage <- which(grepl("CBCoverage",text$text))
    text <- as.data.frame(text[-piepage,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
  }
  
  # removes equity pages
  if (nrow(EQReportData)==0){
    # pages <- c(9,11,13,15)
    cbpages <- which(grepl("EQPages",text$text))
    cbpageend <- which(grepl("EQPageEnd",text$text))
    
    removelist <- lapply(1:length(cbpages), function(x) c(cbpages[c(x)]:cbpageend[c(x)]))
    removelist <- melt(removelist[1:length(cbpages)])
    text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    
    piepage <- which(grepl("EQCoverage",text$text))
    text <- as.data.frame(text[-piepage,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    
    # Renewables page
    repages <- which(grepl("RenewaddsOut",text$text))
    repageend <- which(grepl("RenewAddsOutEnd",text$text))
    
    removelist <- lapply(1:length(repages), function(x) c(repages[c(x)]:repageend[c(x)]))
    removelist <- melt(removelist[1:length(repages)])
    text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- "\t\\vspace{-2.9cm} %renewspacingworkaround"
    
  }
  
  # removes renewable chart 
  if (RenewAdds==0 & nrow(EQReportData)>0){
    repages <- which(grepl("RenewaddsOut",text$text))
    repageend <- which(grepl("RenewAddsOutEnd",text$text))
    
    removelist <- lapply(1:length(repages), function(x) c(repages[c(x)]:repageend[c(x)]))
    removelist <- melt(removelist[1:length(repages)])
    text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
    colnames(text) <- "text"
    
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- gsub(".9cm","2.9cm",text$text[renewvspace])
  }
  
  # removes Fund Page
  if (typeof(FundsInPort)!="list"){
    fundpages <- which(grepl(paste0("FundCHECK"), text$text))
    fundpageend<-fundpages+10
    
    removelist <- lapply(1, function(x) c(fundpages[c(x)]:fundpageend[c(x)]))
    removelist <- melt(removelist[1]) 
    text <- as.data.frame(text[-removelist$value,])
    colnames(text) <- "text"
  }
  
  # removes Other Sector Pages - materials
  if ((OtherSectors$Steel+OtherSectors$Cement==0)){
    sectorpages <- which(grepl(paste0("StartOtherSectorsMaterial"), text$text))
    sectorpagesend <- which(grepl(paste0("EndOtherSectorsMaterial"), text$text))
    
    removelist <- sectorpages:sectorpagesend
    text <- as.data.frame(text[-removelist,])
    colnames(text) <- "text"
  }
  
  # removes Other Sector Pages - transportation
  if ((OtherSectors$Aviation+OtherSectors$Shipping==0)){
    sectorpages <- which(grepl(paste0("StartOtherSectorsTransport"), text$text))
    sectorpagesend <- which(grepl(paste0("EndOtherSectorsTransport"), text$text))
    
    removelist <- sectorpages:sectorpagesend
    text <- as.data.frame(text[-removelist,])
    colnames(text) <- "text"
  }  
  
  # Set Report Language
  replacelist <- colnames(RT)  
  for (i in 1:length(replacelist)){
    text$text <- gsub(replacelist[i],RT[replacelist[i]][[1]], text$text)
  }
  
  # Update the template to reflect figure names
  FigNames<-as.data.frame(readLines("FigureList.txt",skipNul = TRUE))
  colnames(FigNames) <- "Name"
  FigNames$Name <- gsub("\"","",as.character(FigNames$Name))
  FigNames$Fig <- substring(FigNames$Name,1,2)
  FigNames$Fig <- paste0("SwissFigures/Fig",FigNames$Fig)
  
  for (f in 1:nrow(FigNames)){
    text$text <- gsub(FigNames$Fig[f],FigNames$Name[f],text$text,fixed = TRUE)
  }
  
  text$text <- gsub("SamplePort",PortfolioName,text$text)
  text$text <- gsub("SAMPLEPORT",PORTFOLIONAME,text$text)
  text$text <- gsub("CO2","CO\\\\textsubscript{2}",text$text)
  text$text <- gsub("Â°","°",text$text)
  # text$text <- gsub("Ã¤","?",text$text)
  
  if (Languagechoose == "DE"){
    text$text[grepl("KLIMAVER",text$text)][1]<- "KLIMAVERTRÄGLICHKEITS-PILOTTEST"
  }
  
  if (Languagechoose == "FR"){
    text$text[grepl("TEST PILOTE DE COMPATIBILITÉ  CLIMATIQUE",text$text)] <- "TEST PILOTE\\\\ DE COMPATIBILITÉ  CLIMATIQUE"
     text$text[grepl("POSSIBILIT",text$text)][1]<- "\\SectionHeading{PARTIE 3:}{POSSIBILITÉS D'ACTION}"
    text$text[grepl("POSSIBILIT",text$text)][2]<- "\\PageHeading{POSSIBILITÉS D'ACTION - SÉLECTION DES FONDS}"
  }
  
  # Copy in the graphics folder for the report
  originalloc <- paste0(CodeLocation,"01_ReportTemplates/", "ReportGraphics/")
  graphicsloc <- paste0(LanguageDirectory ,"/","ReportGraphics/")
  flist <- list.files(originalloc, full.names = TRUE)
  
  if(!dir.exists(file.path(graphicsloc))){
    dir.create(file.path(graphicsloc), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    for (file in flist){file.copy(file, graphicsloc)}
  }  
  
  # Save the template file
  TemplateNameNew <- paste0("Template_",PortfolioName,"_",Languagechoose)
  write.table(text, paste0(TemplateNameNew,".Rnw"),col.names = FALSE,row.names = FALSE,quote=FALSE,fileEncoding = "UTF-8")  
  
  # Create the PDF
  knit2pdf(paste0(LanguageDirectory,TemplateNameNew,".Rnw"),compiler = "xelatex", encoding = 'UTF-8')
  
  # Delete remaining files and ReportGraphics Folder
  unlink("ReportGraphics",recursive = TRUE)
  excessfileendings <- c(".log",".rnw",".tex",".aux")
  file.remove(paste0(TemplateNameNew,excessfileendings))
  file.remove("FigureList.txt")

  # Rename output file
  if (InvestorName == PortfolioName){
    file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",Languagechoose,".pdf"))}else{
  file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",PortfolioName,"_",Languagechoose,".pdf"))}
  
  return()
}

# ------------ Output File ------------------ #
summaryout <- function(BenchmarkRegionchoose,CompanyDomicileRegionchoose,Startyear,FundsToTest, UserName){
  # This doc prints a short summary file
  
  bmrc <- c("Benchmark Region:", BenchmarkRegionchoose)
  cdrc <- c("Company Domicile Region:", CompanyDomicileRegionchoose)
  sy <- c("Start Year:",Startyear)
  datetoday <- c("Date:",date())
  
  if (UserName == "work"){UserName <- "Klaus"}
  tester <- c("Who ran the code:", UserName)
  
  messageinput <- c("Have a great day!", "I hope the results are to your liking","")
  message <- c(messageinput[sample(1:length(messageinput),1)],"")
  
  outputs <- rbind(datetoday,tester,bmrc,cdrc,sy, FundsToTest,message)
  
  write.csv(outputs,file = "BatchSummary.csv", row.names=FALSE)
  
  return()
}

# --------
# SWISS PLOT FUNCTIONS
# --------

# ------------ Other Sector Plots------------ #
other_sector_chart <- function(plotnumber, EQ_OS_WEM,CB_OS_WEM, OSTargets,SectorToPlot,PortfolioName){
  
  theme_linecharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_text(face="bold",colour="black",size=textsize),
          axis.title.y=element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          # legend.position=c(0.5,0),#legend.position = "none",
          legend.position = "none",
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          #legend.title=element_blank(),
          legend.title = element_text(colour = "black", size = textsize),
          legend.key = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1,1, 0, 0), "lines")
          # plot.margin = unit(c(1,1, 5, 2), "lines")
    )
  }    
  
  
  
  check = 0
  EQPlotData <- subset(EQ_OS_WEM, EQ_OS_WEM$PortName == PortfolioName & EQ_OS_WEM$Sector == SectorToPlot)
  if(nrow(EQPlotData) == 1){
    EQPlotData$ChartType <- "EQ"
    check = check+.5}
  
  CBPlotData <- subset(CB_OS_WEM, CB_OS_WEM$PortName == PortfolioName & CB_OS_WEM$Sector == SectorToPlot)
  if(nrow(CBPlotData) == 1){
    CBPlotData$ChartType <- "CB"
    check = check+1.5}
  
  if (check == 2){PlotData <- rbind(EQPlotData,CBPlotData)} else{
    if (check == 0.5){PlotData <- EQPlotData}else{
      if (check == 1.5){PlotData <-CBPlotData}}}
  
  if (check >0){
    
    PlotData<- merge(PlotData,OSTargets, by="Sector")
    PlotData <- PlotData[,!colnames(PlotData) %in% c("Sector","PortName", "TargetEmissionsFactor")]
    
    df <- melt(PlotData, id.vars = c("ChartType", "EmissionsFactor"))
    df <- df[with(df,order(ChartType)),]
    
    df$Year <- 2017:2022
    df$value <- df$EmissionsFactor*df$value
    
    year_lab <- GT["Year"][[1]]
    ylabel <- paste0(GT["OtherSectorLabel"][[1]]," (",GT[paste0(SectorToPlot,"Units")][[1]],")")
    
    df$ChartType<-factor(df$ChartType)
    # df <- df[with(df,order(Year)),]
    
    dfCB <- df[df$ChartType == "CB",]
    dfEQ <- df[df$ChartType == "EQ",]
    
    outputplot <- ggplot()
    
    if (nrow(dfCB)>0){outputplot<-outputplot+
      geom_line(data=dfCB,aes(x=Year,y=value,colour=Tar2DColour,group=1),size=1.5,linetype=1)+
      annotate(geom = "point",y=dfCB$value[dfCB$Year==2017],x=dfCB$Year[dfCB$Year==2017],size=5,colour=YourportColour,fill=YourportColour, shape=22)}
    if (nrow(dfEQ)>0){outputplot<-outputplot+geom_line(data=dfEQ,aes(x=Year,y=value,colour=Tar2DColour,group=1),size=1.5,linetype=2)+
      annotate(geom = "point",y=dfEQ$value[dfEQ$Year==2017],x=dfEQ$Year[dfEQ$Year==2017],size=5,colour=YourportColour,fill=YourportColour, shape=22)}
    
    outputplot<-outputplot+
      scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
      scale_colour_manual(name="",guide='legend',values= c(Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
      xlab(year_lab) + ylab(ylabel) + # Set axis labels
      # legend(values=legelabels)+
      scale_x_continuous(breaks=seq(Startyear,max(df$Year),1),expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(x=c(2016.5,2021.5),y= c(.95*min(df[,c(4)], na.rm=TRUE),1.05*max(df[,c(4)], na.rm=TRUE)))+
      theme_linecharts()  
    
    
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",SectorToPlot,'_OtherSectors.png', sep=""),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi)
    InPort=1
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label = GT[paste0("NoSectorOther")][[1]]
    
    
    outputplot <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,30), size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "white",colour = NA))
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",SectorToPlot,'_OtherSectors.png', sep=""),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi)
    InPort=0
  }    
  
  return(InPort)
}

# ------------ Shipping Plots------------ #
shipping_chart <- function(plotnumber, EQPortSnapshot,CBPortSnapshot,ShippingData, SectorToPlot="Shipping",PortfolioName){
  
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_text(face="bold",colour="black",size=textsize),#element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.3),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,0, 2.5, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  
  EQShipsPort<- subset(EQPortSnapshot, EQPortSnapshot$ISIN %in% ShippingData$ISIN)
  CBShipsPort<- subset(CBPortSnapshot, CBPortSnapshot$ISIN %in% ShippingData$ISIN)
  
  noships <- nrow(EQShipsPort) +nrow(CBShipsPort)
  
  if(noships >0){
  
  EQShips <- ShippingData[ShippingData$ISIN %in% EQShipsPort$ISIN,]
  CBShips <- ShippingData[ShippingData$ISIN %in% CBShipsPort$ISIN,]
  EQShipsinPort <- merge(EQShips, EQShipsPort, by="ISIN")
  CBShipsinPort <- merge(CBShips, CBShipsPort, by="ISIN")
  
  ship_summary <- function(ShipsinPort,ClassificationName){
    if(ClassificationName == "CB"){
      # WEighted Approach (CB and potentially EQY)
      ShipAUM <- sum(CBShipsinPort$AUM[ShipsinPort$Year == 2017],na.rm = TRUE)
      ShipsinPort <- CBShipsinPort
      ShipsinPort$ShipShare <- ShipsinPort$AUM/ShipAUM
      ShipsinPortlong <- melt(ShipsinPort[,c("Year","ShipShare",grep("Perc",colnames(ShipsinPort), value = TRUE))], id.var=c("Year","ShipShare"))
      ShipsinPortlong$TechShare <- ShipsinPortlong$ShipShare * ShipsinPortlong$value
      ShipsinPort <- aggregate(ShipsinPortlong["TechShare"], by = ShipsinPortlong[,c("Year","variable")], FUN = sum)
      ShipsinPort$variable <- strtrim(ShipsinPort$variable,5)
      ShipsinPort$Type <- GT["ShipsTypeListedCorporateBonds"][[1]]
      return(ShipsinPort)
    }else{
      #Ownership Approach
      EQShipsinPort <- aggregate(EQShipsinPort["Position"], by = EQShipsinPort[,c("Issuer","GHG_A", "GHG_B", "GHG_C", "GHG_D", "GHG_E", "GHG_F", "GHG_G", "Year", "TotalShares")], FUN = sum)
      ShipsinPort <- EQShipsinPort
      ShipsinPort$ShipShare <- ShipsinPort$Position / ShipsinPort$TotalShares
      ShipsinPortlong <- melt(ShipsinPort[,c("Year","ShipShare",grep("GHG_",colnames(ShipsinPort), value = TRUE))], id.var=c("Year","ShipShare"))
      ShipsinPortlong$PortfolioProduction <- ShipsinPortlong$ShipShare * as.numeric(ShipsinPortlong$value)
      ShipsinPort <- aggregate(ShipsinPortlong["PortfolioProduction"], by = ShipsinPortlong[,c("Year","variable")], FUN = sum)
      ShipsinPortRef <- ddply(ShipsinPortlong,.(Year),summarize, TotalShips = sum(PortfolioProduction, na.rm = TRUE))
      ShipsinPort <- merge(ShipsinPort,ShipsinPortRef, by = "Year", all.x = TRUE)
      ShipsinPort$TechShare <- ShipsinPort$PortfolioProduction / ShipsinPort$TotalShips
      ShipsinPort <- ShipsinPort[,c("Year", "variable", "TechShare")]
      ShipsinPort$Type <- GT["ShipsTypeListedEquity"][[1]]
      return(ShipsinPort)
    }
  }
  
  
  #Market
  ShipsListedMarket <- subset(ShippingData, Company == "ListedMarket", select = c("Year",grep("Perc",colnames(ShippingData), value = TRUE)))
  ShipsListedMarket <- melt(ShipsListedMarket, id.var=c("Year"))
  ShipsListedMarket$variable <- strtrim(ShipsListedMarket$variable,5)
  ShipsListedMarket$Type <- GT["ShipsTypeStockMarket"][[1]]
  ShipsListedMarket <- rename(ShipsListedMarket, c("value" = "TechShare"))
  
  ShipsSummary <- ShipsListedMarket
  
  #CB
  if(dim(CBShipsinPort)[1]>0){
    ShipsCB <- ship_summary(CBShipsinPort, "CB")
    ShipsSummary <- rbind(ShipsSummary, ShipsCB)
  }
  
  #EQ
  if(dim(EQShipsinPort)[1]>0){
    ShipsEQ <- ship_summary(EQShipsinPort, "EQ")
    ShipsSummary <- rbind(ShipsSummary, ShipsEQ)
  }
  
  if(length(unique(ShipsSummary$Type)) > 2){
    ShipsSummary <- subset(ShipsSummary, Year == Startyear+5)
  }
  
  wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  

  ShipsSummary$Order <- str_sub(ShipsSummary$variable,-1,-1)
  ShipsSummary$variable <- paste0("GHG ",str_sub(ShipsSummary$variable,-1,-1)," Score")
  ShipsSummary$Name <- paste0(ShipsSummary$Type," ",ShipsSummary$Year)
  ShipColourPalette <- c("#D73027", "#FC8D59", "#FEE08B", "#FFFFBF", "#D9EF8B", "#91CF60", "#1A9850")
  ShipsSummary$Order <- factor(ShipsSummary$Order, levels=rev(c("A","B","C","D","E","F","G")))
  ShipsSummary <- ShipsSummary[order(ShipsSummary$Order),]
  ShipsSummary$Name <- wrap.labels(ShipsSummary$Name,8)
  
  ylabel <- GT["ShipYLabel"][[1]]
  
  shippingchart<- ggplot(ShipsSummary, aes(Name, TechShare,fill=Order))+
    geom_bar(stat = "identity",width = .6, show.legend = TRUE)+
    scale_fill_manual(labels=unique(ShipsSummary$Order),values=ShipColourPalette)+
    scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
    expand_limits(0,0)+
    guides(fill=guide_legend(nrow = 1))+
    ylab(ylabel)+
    theme_barcharts()
  
  # print(PlotData)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_Shippingbar.png"),bg="transparent",height=3.6,width=3.6,plot=shippingchart,dpi=ppi)
  InPort=1

  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label = GT[paste0("No",SectorToPlot)][[1]]
    
    
    shippingchart <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,30), size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_Shippingbar.png"),bg="transparent",height=3.6,width=3.6,plot=shippingchart,dpi=ppi)
    
    InPort=0
    
  }
  
  return(InPort)}

# ------------- PORT DATA PIE --------------- #
port_pie <- function(plotnumber, PortData){
  
  Port <- PortData
  
  Port <- subset(PortData, select = c("Bonds","Equity","Others"))
  if(nrow(Port)>0){
  SumPort <- sum(Port[1,1:3],na.rm = TRUE)
  Port<- melt(Port)
  Port<- rename(Port,c("variable"="Classification"))
  Port$perc <- round(Port$value/SumPort,2)*100
  
  Palette <- data.frame(Classification = c("Bonds","Equity","Others"),Colour=c("dodgerblue4","dodgerblue1","grey"))
  Palette$Colour <- as.character(Palette$Colour)
  Port <- merge(Port,Palette, by="Classification")
  
  Port$Label <- lapply(Port$Classification, function(x) GT[paste0(x,"Title")][[1]])
  
  
  PieChart<- ggplot(Port, aes(x="", y=perc, fill=Classification))+
    geom_bar(stat = "identity",color=NA, width = 0.5)+
    geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
    scale_fill_manual(values= Port$Colour,labels=paste(Port$Label,": ",Port$perc,"%",sep=""))+
    guides(fill = guide_legend(override.aes = list(colour = NULL)))+
    theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
          axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_blank(), plot.margin = unit(c(0,0, 0, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.text = element_text(size=textsize,family = "Calibri",colour="black"),
          legend.key.size=unit(0.4,"cm"),legend.title=element_blank())
  
  PieChart <- PieChart + coord_polar("y", start=0, direction=-1)#+ xlab('') #+  ylab('')
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",'Portpie.png',sep=""),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
  }
  
}

# ------------- PIE CHART ------------------- #
pie_chart <- function(plotnumber,ChartType,PortSnapshot, PortfolioName, CompanyDomicileRegionchoose){
  
  if (nrow(PortSnapshot)>0){
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% "IssLvlPortWeight"] <- "PortWeight"
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    # piesub_tech$piesector[piesub_tech$piesector] <- "Not Assessed"
    
    
    #OUT OF REGION <- Anti-PortSnapshotSub if Region != Global/GLobalAgg
    piesub_tech$piesector <- revalue(piesub_tech$piesector,c("Metal-Iron" = "Iron & Steel","NonOG Production" = "Fossil Fuels","Bldg Prod-Cement/Aggreg" = "Building Materials & Fixtures", "Oil&Gas"= "Fossil Fuels","Coal"="Fossil Fuels", "Transport-Marine" = "Marine Transportation","Metal-Aluminum"="Aluminum", "Steel-Producers" = "Iron & Steel", "Transport-Air Freight"= "Airlines"),warn_missing = FALSE)
    
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    pieshares$label <- "Total Portfolio"
    
    # Create a sub dataframe for plotting
    secfull <- c("Utility Power", "Automotive", "Fossil Fuels", "Non-Utility Power", "Airlines", "Building Materials & Fixtures","Aluminum", "Iron & Steel", "Marine Transportation","Not Assessed")
    secsmiss <- setdiff(secfull,unique(pieshares$piesector))
    
    weights <- rep(0,length(secsmiss))
    label <- rep("Total Portfolio",length(secsmiss))
    missingdf <- data.frame(secsmiss,weights,label)
    names(missingdf) <- names(pieshares)
    pieshares <- rbind(pieshares,missingdf)
    pieshares <- within(pieshares,piesector <- factor(piesector, levels=secfull))
    pieshares$piesector <- revalue(pieshares$piesector,c("Building Materials & Fixtures" = "Building Materials"))
    pieshares <- pieshares[with(pieshares,order(label,piesector)),]
    pieshares <- pieshares[pieshares$label %in% "Total Portfolio",]
    pieshares$perc <- round(pieshares$Portfolio_weight*100,1)
    Palette <- c("#274f80","#30629e", "#3974bc", "#5288cA", "#934d1d","#D26E2A", "#ED7D31", "#F1A78A","#F5C7B8", "#E5E5E5", "#b7b7b7") #blue #b7b7b7
    
    pieshares$piesector <- revalue(pieshares$piesector, c("Utility Power"=GT["PS_UP"][[1]],"Automotive"=GT["PS_Aut"][[1]],"Fossil Fuels"=GT["PS_FF"][[1]],"Non-Utility Power"=GT["PS_NUP"][[1]],"Airlines"=GT["PS_Air"][[1]],"Building Materials"=GT["PS_BM"][[1]],"Iron & Steel"=GT["PS_IS"][[1]],"Marine Transportation"=GT["PS_MT"][[1]],"Not Assessed"=GT["PS_NA"][[1]]),warn_missing = FALSE) 
    
    
    
    PieChart<- ggplot(pieshares, aes(x="", y=Portfolio_weight, fill=piesector))+
      geom_bar(stat = "identity",color=NA, width = 0.5)+
      geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
      scale_fill_manual(values= Palette,labels=paste(pieshares$piesector," ",pieshares$perc,"%",sep=""))+
      guides(fill = guide_legend(override.aes = list(colour = NULL)))+
      theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
            axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_blank(), plot.margin = unit(c(0,0, 0, 0), "lines"),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA),
            legend.background = element_rect(fill = "transparent",colour = NA),
            legend.text = element_text(size=textsize,family = "Calibri",colour="black"),
            legend.key.size=unit(0.4,"cm"),legend.title=element_blank())
    
    PieChart <- PieChart + coord_polar("y", start=0, direction=-1)+ xlab('') +  ylab('')
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_pie.png',sep=""),bg="transparent",height=2,width=4,plot=PieChart,dpi=ppi)
    
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    if (ChartType == "CB"){
      Label <- GT["NoDebtPie"][[1]]
    }else{Label <- GT["NoEquityPie"][[1]]}
    

    outputplot <- 
      ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=5)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_pie.png',sep=""),bg="transparent",height=2.6,width=4,plot=outputplot,dpi=ppi)
  }
  
  return()
}

# ------------- STACKED BAR CHARTS ---------- #
stacked_bar_chart <- function(plotnumber,ChartType,combin,WeightedResults,SectorToPlot,BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,Startyear,PortfolioName, PortfolioNameLong){

  # combin <- CBCombin
  # ChartType <- "CB"
  # WeightedResults <- CBWMCoverageWeight
  # SectorToPlot <- "Fossil Fuels"
  # plotnumber=99
  
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),#element_text(face="bold",colour="black",size=textsize),
          axis.line = element_line(colour = "black",size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.3),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour="black"),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  
  wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  WeightedResults$PortName <- NULL
  
  if(SectorToPlot == "All"){cbondsgo <-nrow(combin)}
  if(SectorToPlot == "Fossil Fuels"){cbondsgo <- nrow(subset(combin, combin$Sector %in% c("Fossil Fuels","Oil&Gas","Coal")))}else{cbondsgo <- nrow(subset(combin, combin$Sector == SectorToPlot))}
  
  if(cbondsgo>0){
    
    combin <- combin[!combin$Technology %in% "OilCap",]
    
    if (ChartType=="EQ"){
      ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & Sector == SectorToPlot)
      if (SectorToPlot == "Fossil Fuels"){
        ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]*24
        ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]*6.12
        ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]*0.0372
        ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Coal"]*24
        ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Oil"]*6.12
        ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Gas"]*0.0372
        
        ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                    PortProduction= sum(Production),
                                    RefProduction = sum(RefTechProd))
      }else{
        ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                    PortProduction= sum(Production),
                                    RefProduction = sum(TargetProductionAlignment))}
      
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
      
      ProductionMix_5yrs <- melt(ProductionMix_5yrs, id = c( "Year","Technology","Scenario","Sector"))
      SectorTotals <- ddply(ProductionMix_5yrs,.(Year,Sector,variable), summarise,SectorTotal = sum(value))
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,SectorTotals)
      
      ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value/ProductionMix_5yrs$SectorTotal
      
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select= c("Sector","Technology","variable","TechShare"))
      ProductionMix_5yrs$Technology <- gsub("Cap","",ProductionMix_5yrs$Technology)
      ProductionMix_5yrs$variable <- as.character(ProductionMix_5yrs$variable)
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "PortProduction"] <- PortfolioNameLong
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "RefProduction"] <- GT["X2Target"][[1]]
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "CoverageWeight"] <- GT["AveragePort"][[1]]
      
    }else{
      
      if (SectorToPlot == "Fossil Fuels"){
        ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose & Sector %in% c("Oil&Gas","Coal"))
        
        ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$SectorWeight
        ProductionMix_5yrs$TechShare[ProductionMix_5yrs$Sector %in% "Oil&Gas"] <- ProductionMix_5yrs$SectorWeight[ProductionMix_5yrs$Sector %in% "Oil&Gas"]*ProductionMix_5yrs$PortTechShare[ProductionMix_5yrs$Sector %in% "Oil&Gas"]
        TSSUM <- sum(ProductionMix_5yrs$TechShare, na.rm = TRUE)
        ProductionMix_5yrs$TechShare <-ProductionMix_5yrs$TechShare/TSSUM 
        
        MarketTechShareOGSum <- sum(ProductionMix_5yrs$RegWtProjMarketProd[ProductionMix_5yrs$Sector %in% "Oil&Gas"],na.rm = TRUE)
        ProductionMix_5yrs$MarketTechShareOG <- ProductionMix_5yrs$RegWtProjMarketProd/MarketTechShareOGSum
        
        ProductionMix_5yrs$TechShareMarket <- ProductionMix_5yrs$SecWtMarket
        ProductionMix_5yrs$TechShareMarket[ProductionMix_5yrs$Sector %in% "Oil&Gas"]<- ProductionMix_5yrs$SecWtMarket[ProductionMix_5yrs$Sector %in% "Oil&Gas"]*ProductionMix_5yrs$MarketTechShareOG[ProductionMix_5yrs$Sector %in% "Oil&Gas"]
        TSSUMMarket  <- sum(ProductionMix_5yrs$TechShareMarket, na.rm = TRUE)
        ProductionMix_5yrs$TechShareMarket <-ProductionMix_5yrs$TechShareMarket/TSSUMMarket 
        
        ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c("Technology","TechShare","TechShareMarket"))
        
        WeightedResults <- subset(WeightedResults, Technology %in% ProductionMix_5yrs$Technology)
        sumWR <- sum(WeightedResults$CoverageWeight, na.rm = TRUE)
        WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sumWR
        WeightedResults$PortName<- NULL
        
        ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
        ProductionMix_5yrs <- rename(ProductionMix_5yrs, c("TechShareMarket"=GT["X2Target"][[1]],"TechShare"=PortfolioNameLong,"CoverageWeight"=GT["AveragePort"][[1]]),warn_missing = FALSE)
        ProductionMix_5yrs <- melt(ProductionMix_5yrs, id.vars = c("Technology"))
        ProductionMix_5yrs$Sector <- "Fossil Fuels"
        ProductionMix_5yrs <- rename(ProductionMix_5yrs, c("value"="TechShare"))
        
        ProductionMix_5yrs$TechShare[is.nan(ProductionMix_5yrs$TechShare)] <- 0
        ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c("Sector","Technology","variable","TechShare"))
        
      }else{
        ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose & Sector %in% SectorToPlot)
        ProductionMix_5yrs <- subset(ProductionMix_5yrs, select=c("Sector","Technology","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
        ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
        ProductionMix_5yrs <- rename(ProductionMix_5yrs, c("WtTechShareTechShare"=PortfolioNameLong,"Benchmark_WtTechShareTechShare"=GT["X2Target"][[1]],"CoverageWeight"=GT["AveragePort"][[1]]),warn_missing = FALSE)
        ProductionMix_5yrs <- melt(ProductionMix_5yrs, id.vars = c("Sector","Technology"))
        ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value
        ProductionMix_5yrs$value <- NULL
        
        ProductionMix_5yrs$TechShare[is.nan(ProductionMix_5yrs$TechShare)] <- 0
        ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c("Sector","Technology","variable","TechShare"))
        
        ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "CoalCap"] <- "Coal"
        ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "GasCap"] <- "Gas"
        ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "HydroCap"] <- "Hydro"
        ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "NuclearCap"] <- "Nuclear"
        ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "RenewablesCap"] <- "Renewables"
        ProductionMix_5yrs <- subset(ProductionMix_5yrs,!Technology %in% "OilCap")
        
        tsharesum <- ddply(ProductionMix_5yrs, .(Sector,variable), summarise, SectorTotal =sum(TechShare, na.rm = TRUE))
        ProductionMix_5yrs <- merge(ProductionMix_5yrs,tsharesum, by= c("Sector","variable"))
        ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$TechShare/ProductionMix_5yrs$SectorTotal 
        ProductionMix_5yrs$SectorTotal<- NULL
        
      }
    }
    
    
    if (SectorToPlot == "Automotive"){ 
      technologyorder <-c("Electric","Hybrid","ICE")
      techorder <- data.frame(order=c(1,2,3),Technology= technologyorder)
      colours <- factor(c(ICEColour,HybridColour,ElectricColour))
      ylabel <- GT["StackedBarYLabel_Automotive"][[1]]}
    
    if (SectorToPlot == "Power"){
      ylabel <- GT["StackedBarYLabel_Power"][[1]]
      technologyorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")
      techorder <- data.frame(order=c(1,2,4,3,5),Technology= technologyorder)
      colours <- factor(c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour))}
    
    if (SectorToPlot == "Fossil Fuels"){
      ylabel <- GT["StackedBarYLabel_FF"][[1]]
      technologyorder <- c("Coal","Gas","Oil")
      techorder <- data.frame(order=c(1,2,3),Technology= technologyorder)
      colours <- factor(c(CoalProdColour,GasProdColour,OilProdColour))
    }  
    colourdf <- data.frame(colours, Technology = technologyorder)
    
    PlotData <- ProductionMix_5yrs
    
    # PlotData <- subset(PlotData, !PlotData$variable == "SamplePort")
    
    PlotData <- merge(PlotData,colourdf, by="Technology")
    orderofchart <- c(GT["X2Target"][[1]],PortfolioNameLong,GT["AveragePort"][[1]])
    PlotData$variable <- factor(PlotData$variable, levels=orderofchart)
    PlotData$Technology <- factor(PlotData$Technology, levels=technologyorder)
    PlotData <- PlotData[order(PlotData$Technology,PlotData$variable),]
    PlotData$variable <- wrap.labels(PlotData$variable,20)
    
    # write.csv(PlotData, paste0("StackedBarChart_",ChartType,"_",SectorToPlot,"_",PortfolioName,".csv"),row.names = F)
    PlotData$Sector <- NULL
    
    
    # LanguageLabels <- GT[unique(paste0("T_",PlotData$Technology))]
    if (SectorToPlot == "Fossil Fuels"){PlotData$Label <- paste0(PlotData$Technology,"Prod")}else{PlotData$Label<- PlotData$Technology}
    if (SectorToPlot == "Power"){PlotData$Label <- paste0(PlotData$Label,"Cap")}
    
    PlotData$Language <- t(GT[paste0("T_",PlotData$Label)])[,1]
    
    stackedbarchart_plot<- ggplot(PlotData, aes(variable, TechShare,fill=rev(Technology)))+
      geom_bar(stat = "identity",width = .6)+
      scale_fill_manual(labels=unique(rev(PlotData$Language)),values=unique(as.character((PlotData$colours))))+
      scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
      expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 1))+
      ylab(ylabel)+
      theme_barcharts()+ 
      coord_flip()
    
    if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
    
    # print(PlotData)
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,plot=stackedbarchart_plot,dpi=ppi)
    
    
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}

      Label <- paste0("No",ChartType,gsub(" ","",SectorToPlot))
      Label <- GT[Label][[1]]
      
    outputplot <-
      ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=4)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))

    if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,plot=outputplot,dpi=ppi)
    
  }
  
  
  # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",plot=stackedbarchart_plot,dpi=ppi)
  return() 
}

# ------------- MINI LINE CHARTS ------------ #
mini_line_chart <- function(plotnumber,ChartType,combin, TechToPlot, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose,figuredirectoy, PortfolioName){
  
  # combin <- CBCombin
  # TechToPlot <- "Oil"
  # SectorToPlot <- "Fossil Fuels"
  # ChartType <- "CB"

  
  theme_linecharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.line = element_line(colour = AxisColour,size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          # legend.position=c(0.5,-.4),#legend.position = "none", 
          legend.position = "none", 
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          #legend.title=element_blank(),
          legend.title = element_text(colour = AxisColour, size = textsize),
          legend.key = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1,1, 0, 0), "lines")
    )
  }    
  
  
  production <- subset(combin, Technology %in% TechToPlot)
  
  production <- rename(production, c("WtProduction"="Production"),warn_missing = FALSE)
  
  if ((sum(production$Production, na.rm = TRUE)>0 | SectorToPlot == "Fossil Fuels") & nrow(combin)>0){
    
    
    
    if (ChartType == "EQ"){
      LineData <- subset(combin, Technology %in% TechToPlot & BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose)  
      if (SectorToPlot == "Fossil Fuels"){
        LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAUMIntensity"))
        names(LineData)[names(LineData)=="TargetProductionAUMIntensity"] <- "TargetProductionAlignment"
      } else{
        LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAlignment"))
        
      }
      
      names(LineData)[names(LineData)=="TargetProductionAlignment"] <- "Target"
      names(LineData)[names(LineData)== "Production"] <- "Portfolio"
      
      sectors <- c("Automotive", "Fossil Fuels", "Power")
      axislabels <- c(GT["Cars"][[1]], GT["FossilFuels_Unit"][[1]], GT["Power_Unit"][[1]])
      lookup <- data.frame(sectors,axislabels)
      # axislabel <- paste(TechToPlot,lookup$axislabels[grep(SectorToPlot, lookup$sectors)])
      if(SectorToPlot == "Fossil Fuels"){TechLabel <- GT[paste0("T_",TechToPlot,"Prod")][[1]]}else{TechLabel <- GT[paste0("T_",TechToPlot)][[1]] }       # Removes "Cap " from the Power labels
      axislabel <- paste(TechLabel,lookup$axislabels[grep(SectorToPlot, lookup$sectors)])
      
      if(SectorToPlot == "Automotive"){axislabel <- TechLabel}
      
      # Scaling and Labelling the Y axis
      maxval <- max(LineData[,4],LineData[,3],na.rm=TRUE)
      
      magnitude_scale <- c(1,1,1e3,1e6,1e9)
      power_units <- c("kW","MW","GW","TW","Error_powertoohigh")
      car_units <- c("","",GT["thousand"][[1]],GT["million"][[1]],GT["billion"][[1]])
      ff_units <- c("","",GT["thousand"][[1]],GT["million"][[1]],GT["billion"][[1]])
      ff_units <- paste0(ff_units," ",GT["barrels"][[1]])
      coal_units <- c("","t","kt","MT","GT")
      oil_units <- c("","",GT["thousand"][[1]],GT["million"][[1]],GT["billion"][[1]])
      oil_units <- paste0(oil_units," ",GT["barrels"][[1]])
      gas_units <- c("","",GT["thousand"][[1]],GT["million"][[1]],GT["billion"][[1]])
      gas_units <- paste0(gas_units, " m²")
      unit_lookup <- data.frame(car_units,ff_units,power_units,coal_units,oil_units,gas_units)
      # ff_sectors <- c(GT["T_Coal"][[1]],GT["T_Oil"][[1]],GT["T_Gas"][[1]])
      ff_sectors <- c("Coal","Oil","Gas")
      sectors <- cbind(sectors, ff_sectors)
      unit_lookup <- setNames(unit_lookup,sectors)
      
      # Scales the Data to the correct units based on the maximum value.
      max_magnitude <- findInterval(maxval,magnitude_scale)
      if(max_magnitude == 0){max_magnitude <- 2}
      LineData$Portfolio <- LineData$Portfolio /magnitude_scale[max_magnitude]
      LineData$Target <- LineData$Target/magnitude_scale[max_magnitude]
      
      # Looks up the units within the correct line in the unit_lookup dataframe and sets the labels
      if (SectorToPlot == "Fossil Fuels")  unit_search <- TechToPlot else
        unit_search <- SectorToPlot
      
      unitlabel <- paste("(",unit_lookup[unit_search][max_magnitude,],")",sep="")  
      if (unitlabel =="()"){unitlabel<-""}
      
    }else{  # "CB"
      LineData <- subset(combin, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot)#  & Scenario %in% Scenariochoose)    
      
      if (SectorToPlot == "Fossil Fuels"){
        LineData <- subset(LineData, select = c("Sector","Year","OGCMetrik_Portfolio","Benchmark_OGC"))
        names(LineData)[names(LineData)=="Benchmark_OGC"] <- "Target"
        names(LineData)[names(LineData)== "OGCMetrik_Portfolio"] <- "Portfolio"      
        
        LineData$Portfolio <- LineData$Portfolio/100
        LineData$Target <- LineData$Target/100  
      }else{
        
        LineData <- subset(LineData, select = c("Sector","Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
        names(LineData)[names(LineData)=="Benchmark_WtTechShareTechShare"] <- "Target"
        names(LineData)[names(LineData)== "WtTechShareTechShare"] <- "Portfolio"}
      
      max_magnitude <- 100
      if(SectorToPlot == "Fossil Fuels"){TechLabel <- GT[paste0("T_",TechToPlot,"Prod")][[1]]}else{TechLabel <- GT[paste0("T_",TechToPlot)][[1]] }       # Removes "Cap " from the Power labels
      
      if (SectorToPlot == "Fossil Fuels"){unitlabel <- paste0(TechLabel," (",Startyear, " = 100)")}else{
        unitlabel <- paste0(TechLabel," (%)")}
      axislabel <- ""
      
      LineData$Portfolio <- LineData$Portfolio*100
      LineData$Target <- LineData$Target*100  
    }
    
    LineData <- subset(LineData, LineData$Year >= Startyear)
    LineData$Portfolio[!LineData$Year %in% c(Startyear:(Startyear+5))]<- NA
    
    goodtech <- c("Renewables","Hydro","Nuclear","Hybrid","Electric")  
    badtech <- c("ICE","Oil","Gas","Coal","GasCap","CoalCap")
    
    # Image
    techicon <- readPNG(paste0(figuredirectory,TechToPlot,".png"))
    g <- rasterGrob(techicon, interpolate=TRUE)
    
    scalemax <- max(LineData$Target,LineData$Portfolio, na.rm = TRUE)
    targetline <- LineData$Target[LineData$Year == max(LineData$Year,na.rm = TRUE)]
    if(targetline>scalemax*.5 & !is.na(targetline)){
      ylocmin <- (.2/(max(LineData$Year)-Startyear))*(scalemax)
      ylocmax <- ylocmin +(1.5/(max(LineData$Year)-Startyear))*(scalemax)
    }else{
      ylocmax <- scalemax-(.2/(max(LineData$Year)-Startyear))*scalemax
      ylocmin <- ylocmax- (1.5/(max(LineData$Year)-Startyear))*(scalemax)
    } 
    
    year_lab <-  GT["Year"][[1]]
    ylabel <- paste(axislabel,unitlabel)
    # bad ones - ie coal, oil, ice
    # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
    if (TechToPlot %in% badtech){
      outputplot <- ggplot(data=LineData)+
        annotation_custom(g,xmin=max(LineData$Year)-1.5, xmax=max(LineData$Year), ymin=ylocmin, ymax=ylocmax)+
        geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=badexpColour)) +
        #geom_ribbon(aes(x=Year,ymin=0,ymax=`2°C Benchmark`,fill=ReqCapColour))+
        geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=goodexpColour)) +
        geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
        geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
        geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
        
        scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
        xlab(year_lab) + ylab(ylabel) + # Set axis labels
        scale_x_continuous(breaks=seq(Startyear,max(LineData$Year),5),expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
        theme_linecharts()
    }else{
      # good ones - ie renewables
      outputplot <- ggplot(data=LineData)+
        annotation_custom(g,xmin=max(LineData$Year)-1.5, xmax=max(LineData$Year), ymin=ylocmin, ymax=ylocmax)+
        geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=goodexpColour)) +
        # geom_ribbon(aes(x=Year,ymin=0,ymax=Target,fill=ReqCapColour))+
        geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=badexpColour)) +
        geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
        geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
        geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
        scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
        xlab(year_lab) + ylab(ylabel) + # Set axis labels
        scale_x_continuous(breaks=seq(Startyear,max(LineData$Year),5),expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
        theme_linecharts()
    }
    outputplot <- outputplot +
      guides(colour=guide_legend(keywidth = 4, keyheight = 1,order=1,override.aes = list(linetype=c(1,2),colour=c(YourportColour,Tar2DColour),size=1.5)))    
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",TechToPlot,'_MiniLinePlot.png', sep=""),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
    InPort=1
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    # techname <- paste0("T_",TechToPlot)
    
    if (SectorToPlot %in% c("Fossil Fuels","Coal","Oil&Gas")){techlabel <- GT[paste0("T_",TechToPlot,"Prod")][[1]]}else{techlabel <- GT[paste0("T_",TechToPlot)][[1]]}
    # techlabel <- GT[paste0("T_",TechToPlot)][[1]]
    replacename <- paste0("NoSector",ChartType)
    
    Label <- GT[replacename][[1]]
    Label <- gsub("techname",techlabel, Label)
    Label <- gsub("?-l", "?l",Label)
    
    outputplot <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,20), size=4)+
      geom_blank()+
      theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",TechToPlot,'_MiniLinePlot.png', sep=""),bg="transparent",height=2.2,width=2.4,plot=outputplot,dpi=ppi)
     InPort=0
     }
  return(InPort)    
}

# ------------- RANKING CHART - ALIGNMENT ----#
ranking_chart_alignment <- function(plotnumber,ChartType,Startyear,SectorToPlot, Exposures, AUMData,Ranks,figuredirectory,PortfolioNameLong){
  # 
  # ChartType <- "CB"
  # SectorToPlot <- "Power"
  # plotnumber=99
  # Exposures <- CBExposureRange
  # AUMData <- CBAUMData
  # Ranks <-CBRanks
  # PortfolioNameLong<- PortName
  
  
  if(PortfolioNameLong %in% c(Exposures$PortName, "PK","V") ){
    # Plotting Exposure
    sectors <- data.frame(Sector = c("Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels","Power","Power","Power","Power","Power"),Technology = c("Electric","Hybrid","ICE","Coal","Gas","Oil","CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"), order =1:11)
    # TechnologyNames<-c("Electric\nVehicles", "Hybrid\nVehicles", "ICE\nVehicles", "Coal\nProduction", "Gas\nProduction", "Oil\nProduction","Renewable\nCapacity","Hydro\nCapacity", "Nuclear\nCapacity",  "Gas\nCapacity", "Coal\nCapacity")
    Technology<-c("Electric", "Hybrid", "ICE","Gas","Oil", "Coal","RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap")
    badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
    goodtech <- Technology[!Technology %in% badtech]
    
    df <- Exposures
    
    df[colnames(df) %in% goodtech] <- df[colnames(df) %in% goodtech]+100
    df[colnames(df) %in% badtech] <- 100-df[colnames(df) %in% badtech]
    
    if (PortfolioNameLong %in% c("PK","V")){
      df <- merge(df,AUMData, by= "PortName")
      df <- rename(df, c("PortAUM"="AUM"),warn_missing = FALSE)
      
      
      WM<- as.data.frame(lapply(df[ , 2:12], weighted.mean, na.rm=TRUE,  w = df$AUM))
      WM$PortName <- PortfolioNameLong
      
      Rank <- Ranks[1,]
      Rank[1,]<-1
      Rank$PortName <- "Rank"
      maxrank <- 1
      
    }else{
      df <- merge(df,AUMData, by= "PortName")
      df <- rename(df, c("PortAUM"="AUM"),warn_missing = FALSE)
      
      WM<- as.data.frame(lapply(df[ , 2:12], weighted.mean, na.rm=TRUE,  w = df$AUM))
      WM$PortName <- "WeightedMean" 
      
      Rank <- Ranks[Ranks$PortName %in% PortfolioNameLong,]
      maxrank <- colMaxs(as.matrix(Ranks[2:12]),na.rm = TRUE )
      autorank <- max(maxrank[1:3],na.rm = TRUE)
      ffrank <- max(maxrank[4:6])
      powerrank <- max(maxrank[7:11])
      if(SectorToPlot== "All"){maxrank <- c(rep(autorank,3),rep(ffrank,3),rep(powerrank,5))}
      if(SectorToPlot== "Automotive"){maxrank <- c(rep(autorank,3))}
      if(SectorToPlot== "Fossil Fuels"){maxrank <- c(rep(ffrank,3))}
      if(SectorToPlot== "Power"){maxrank <- c(rep(powerrank,3))}
      
              #nrow(Ranks)
      Rank$PortName <- "Rank"
    }
    
    
    df$AUM <- NULL
    
    Mins <- colMins(as.matrix(df[2:12]),na.rm = TRUE)
    Maxs <- colMaxs(as.matrix(df[2:12]),na.rm = TRUE)
    MinMax <- as.data.frame(t(cbind(Mins,Maxs)))
    colnames(MinMax) <- colnames(df[2:12])
    MinMax$PortName <- c("Minimum","Maximum")
    df <- rbind(df,MinMax,WM,Rank)
    df <- df[df$PortName %in% c(PortfolioNameLong,"Minimum","Maximum","WeightedMean","Rank"),]
    
    PlotData <- setNames(data.frame(t(df[,-1])), df[,1]) 
    PlotData$Technology <- rownames(PlotData)
    PlotData <- merge(PlotData,sectors,by="Technology")
    
    PlotData$PortLoc <- PlotData[,PortfolioNameLong]/100
    
    # Factorise and Order by Technology  
    PlotData <- PlotData[(order(PlotData$order)),]
    PlotData$order <- factor(PlotData$order, levels = PlotData$order)
    
    # Reduce chart to values to plot 
    
    if (SectorToPlot != "All"){
      PlotData <- subset(PlotData, PlotData$Sector %in% SectorToPlot)
      if (SectorToPlot == "Power"){PlotData <- subset(PlotData, PlotData$Technology %in% c("RenewablesCap", "GasCap", "CoalCap"))}
      locations <- c(1:nrow(PlotData))
    }else{
      locations <- c(1:3,4.5:6.5,8:12)
    }
    
    # Chart variables
    barwidth <- .03
    bh <-0.6
    tbwid <- .25
    
    # Label Wrapping Functions  
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    PlotData$a <- paste0(gsub(" ","",PlotData$Sector),"_Unit")
    PlotData$b <- paste0("T_",PlotData$Technology)
    PlotData$b[PlotData$Sector %in% "Fossil Fuels"] <- paste0("T_",PlotData$Technology[PlotData$Sector %in% "Fossil Fuels"],"Prod")
    
    # Line Labels
    PlotData$TechTitle <- paste0(t(GT[PlotData$b])," ",t(GT[PlotData$a]))
    PlotData$TechTitle[PlotData$Sector %in% "Automotive"] <- paste0(t(GT[PlotData$b[PlotData$Sector %in% "Automotive"] ]))
    PlotData$TechLabel <- PlotData$TechTitle
    
    PlotData <- PlotData[order(PlotData$order),]
    PlotData$order <- factor(PlotData$order, levels = PlotData$order)
    
    PlotData$Locations <- locations
    
    PlotData$WMloc <- PlotData$WeightedMean/100
    PlotData$WMloc <- t(as.data.frame(lapply(1:nrow(PlotData),function(x) max(0, PlotData$WMloc[x]))))
    PlotData$WMloc <- t(as.data.frame(lapply(1:nrow(PlotData),function(x) min(2, PlotData$WMloc[x]))))    
    
    PlotData$UppLim <- 200 #100
    PlotData$UppLim <- rowMins(as.matrix(PlotData[,colnames(PlotData) %in% c("Maximum","UppLim")]))/100
    PlotData$LowLim <- 0#-100
    PlotData$LowLim <- rowMaxs(as.matrix(PlotData[,colnames(PlotData) %in% c("Minimum","LowLim")]))/100
    
    PlotData$xlowloc <- PlotData$LowLim
    PlotData$xupploc <- PlotData$UppLim
    PlotData$comploc <- PlotData[,PortfolioNameLong]/100
    PlotData$comploc[PlotData$comploc < 0] <- 0
    PlotData$comploc[PlotData$comploc > 2] <- 2
    
    PlotData$complabel<-PlotData[,PortfolioNameLong]
    PlotData$complabel[PlotData$complabel>200]<-200
    PlotData$complabel[PlotData$complabel<0]<-0    
    
    PlotData$complabel <- paste0(round(PlotData$complabel,0),"%")
    PlotData$minlabel<- 0 #round(PlotData$LowLim*100,0)
    PlotData$maxlabel<- 200 #round(PlotData$UppLim*100,0)        
    
    PlotData$minlabel <- paste0(PlotData$minlabel, " %")
    PlotData$maxlabel <- paste0(PlotData$maxlabel, " %")
    
    PlotData$Rank[!is.na(PlotData$Rank)]<- round(PlotData$Rank[!is.na(PlotData$Rank)],0)
    PlotData$Rank[is.na(PlotData$Rank)]<- "-"
    
    GraphTitle <- GT["Rank_Title"][[1]]
    
    repval = 200
    redgreen<- colorRampPalette(c("red","white", "darkgreen"))(repval) 
    xvals <- rep(seq(0,2,2/(repval-1)),length(locations))
    yvals <- sort(rep(locations,repval))
    plotdf <- data.frame(x=xvals,y=yvals,w=2.05/repval,h=bh, colbar=rep(redgreen,length(locations)))
    
    outputplot <-    ggplot()+
      geom_tile(data=plotdf, aes(x=x,y=y),height=plotdf$h,width=plotdf$w,fill=plotdf$colbar) +
      scale_x_continuous()+
      scale_y_discrete()+
      
      # error lines
      geom_segment(data=PlotData,aes(x=xlowloc, xend=xupploc,y=Locations,yend=Locations), linetype="dashed",colour="black")+
      geom_point(data=PlotData,aes(x=xlowloc,y=Locations), fill="black",colour="black", size=2)+
      geom_point(data=PlotData,aes(x=xupploc,y=Locations),  fill="black",colour="black",size=2)+
      
      # centre alignment line    
      annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      annotate(geom="rect",xmin =0,xmax=2,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
      
      # Weighted Mean
      annotate(xmin=PlotData$WMloc-barwidth/2,xmax=PlotData$WMloc+barwidth/2,ymin=-bh/2+locations,ymax=bh/2+locations,geom = "rect", fill="darkgrey")+
      
      # Company Circles
      geom_point(data=PlotData,aes(x=comploc,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
      annotate(geom="text",label=PlotData$complabel, x= PlotData$comploc, y= PlotData$Locations, colour="white",size=rel(3))+ 
      
      # Distribution Range 
      annotate(geom="text",x= -.1, hjust=1 , y= locations,label=PlotData$minlabel,size=rel(3),colour="black")+     # Minimum
      annotate(geom="text",x= 2.1, hjust=0 , y= locations,label=PlotData$maxlabel,size=rel(3),colour="black")+     # Maximum
      
      # Ranking box and label
      annotate("text", label = GT["RankTitle"][[1]], x= 2.35+tbwid/2,y = max(locations)+ 0.5, size=rel(3),fontface = "bold",colour="black")+ # Rank Heading
      annotate("text", label = paste0(PlotData$Rank," ",GT["RankOF"][[1]]," ",maxrank), x= 2.35+tbwid/2,hjust=0.5, y = locations,size=rel(3),fontface = "bold",colour="black")+ # Company Ranking
      
      theme(panel.background = element_rect(fill="transparent"),
            panel.grid.major.x = element_blank() ,
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x=element_text(face="bold",colour="black", size=12),
            axis.title.y=element_text(face="bold",colour="black", size=12, vjust = 1),
            plot.margin = (unit(c(0.2, 0.6, 0, 0), "lines")))
    
    
    if (SectorToPlot == "All"){
      
      leafloc <- c(11,12,2,3)
      
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL)+
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% badtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% badtech],12), size=rel(3), hjust=0, fontface = "bold",colour="black")+  # Technology Label - Black
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% goodtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% goodtech],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")+ 
        geom_hline(yintercept = c(3.75,7.25))
      
      write.csv(PlotData,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
      
      graphheight <- 7.2
    }
    
    if (SectorToPlot != "All"){
      
      if (SectorToPlot == "Power"){leafloc <- c(3,-10); ymax = 5.7; graphheight <- 2.3}
      if (SectorToPlot == "Automotive"){leafloc <- c(3,2); ymax = 3.7; graphheight <- 2.3}
      if (SectorToPlot == "Fossil Fuels"){leafloc <- c(-10,-10); ymax = 3.7; graphheight <- 2.3}
      
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL,  title= NULL)+
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% badtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% badtech],12), size=rel(3), hjust=0, fontface = "bold",colour="black")
      
      if (SectorToPlot != "Fossil Fuels"){outputplot <-outputplot+
        # Technology Label - Black
        annotate(geom="text",x=-0.8,y=PlotData$Locations[PlotData$Technology %in% goodtech],label=wrap.labels(PlotData$TechLabel[PlotData$Technology %in% goodtech],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")
      
      }
      
      # Leaf Icon
      # annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[1]-leafh,ymax = leafloc[1]+leafh)+
      # annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[2]-leafh,ymax = leafloc[2]+leafh)#+
      
      # Sector Boxes
      # annotate(geom="rect",xmin=-2.4, xmax=1.6, ymin=0.2, ymax=ymax, fill="transparent", colour="black") 
    }
    
    outputplot <- ggplot_gtable(ggplot_build(outputplot))
    outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
    grid.draw(outputplot)  
    
    if (PortfolioNameLong %in% c("PK","V")){
      PortfolioName<- PortfolioNameLong}
    
    if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_RankingChart.png', sep=""),bg="transparent",height=graphheight,width=7,plot=outputplot)
  }else{
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    # 
    # if (ChartType == "CB"){
    #   Label <- GT["NoDebt"][[1]]
    # }else{Label <- GT["NoEquity"][[1]]}
    # 
    # outputplot <- 
    #   ggplot()+
    #   annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=3)+
    #   geom_blank()+
    #   theme(
    #     axis.title.x=element_blank(),
    #     axis.title.y=element_blank(),
    #     axis.text.x=element_blank(),
    #     axis.text.y=element_blank(),
    #     axis.ticks = element_blank(),
    #     panel.grid.major = element_blank(), 
    #     panel.grid.minor = element_blank(),
    #     #panel.background = element_blank(),
    #     panel.background = element_rect(fill = "transparent",colour = NA))
    # 
    # if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    # ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_RankingChart.png', sep=""),bg="transparent",height=2.5,width=7,plot=outputplot)
    # print("nochart")
    
    
  }
  
  return()
}

# ------------- FLAT WHEEL CHARTS ----------- #
flat_wheel_chart <- function(plotnumber,companiestoprint,ChartType,PortSnapshot, combin,AlloftheCompanies, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, IndexData,Indexchoose, PortfolioName,PortGraphName){
  
  # ChartType<- "EQ"
  # SectorToPlot<-"Automotive"
  # AlloftheCompanies <- AutoCompanies
  # # AlloftheCompanies <- OGCarbonBudget
  # combin <- EQCombin
  # PortSnapshot <- EQPortSnapshot
  # companiestoprint<-20
  # AUMData<- EQAUMData
  # Ranks <- EQRanks
  # combin<-EQCombin
  # SectorToPlot <-"OG"

  # ChartType<- "CB"
  # SectorToPlot<-"OG"
  # AlloftheCompanies <- OGCarbonBudget
  # PortSnapshot <- CBPortSnapshot
  # companiestoprint<-20
  # AUMData<- CBAUMData
  # Ranks <- CBRanks
  # combin<-CBCombin
  
  
  WheelofFortune<-function(df, othercompanies = TRUE ,family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.2,techorder,PortFirmY=18,OtherFirmY=5,
                           spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1, guides = seq(100,0,by = -25),
                           alphaStart = -0.3, circleProportion = 0.8, direction = "inwards", familyLabels = FALSE, normalised = TRUE)
  {
    # 
    # df<-AllCompanies
    # family = NULL
    # columnNames = NULL
    # binSize = 1
    # spaceItem = 0.2
    # spaceFamily = 1.2 #1.2
    # innerRadius = 0.3 #0.3
    # outerRadius = 1
    # guides =seq(100,0,by = -25)
    # alphaStart = -0.3 #-0.3
    # circleProportion = .8
    # direction = "inwards"
    # familyLabels = FALSE
    # normalised = TRUE
    
    if (!is.null(columnNames)) {
      namesColumn <- names(columnNames)
      names(namesColumn) <- columnNames
      df <- rename(df, namesColumn)
    }
    
    applyLookup <- function(groups, keys, unassigned = "unassigned") {
      lookup <- rep(names(groups), sapply(groups, length, USE.NAMES = FALSE))
      names(lookup) <- unlist(groups, use.names = FALSE)
      p <- lookup[as.character(keys)]
      p[is.na(p)] <- unassigned
      p
    }
    
    df$score <- factor(df$score, levels=techorder)
    
    if (!is.null(family)) {
      df$family <- applyLookup(family, df$item)}
    df <- arrange(df, family, item, score) # original sort 
    
    
    
    if (normalised) 
    {df <- ddply(df, .(family, item), transform, value = cumsum(value/(sum(value))))
    }else {
      maxFamily <- max(plyr::ddply(df, .(family, item), summarise, 
                                   total = sum(value))$total)
      df <- ddply(df, .(family, item), transform, value = cumsum(value))
      df$value <- df$value/maxFamily
    }
    
    df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
    
    df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
    df2$indexItem <- cumsum(df2$indexItem)
    df3 <- ddply(df, .(family), summarise, indexFamily = 1)
    df3$indexFamily <- cumsum(df3$indexFamily)
    df <- merge(df, df2, by = c("family", "item"))
    df <- merge(df, df3, by = "family")
    df <- arrange(df, family, item, score)
    affine <- switch(direction, inwards = function(y) (outerRadius - innerRadius) * y + innerRadius, outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius, stop(paste("Unknown direction")))
    
    df <- within(df, {
      xmin <- (indexItem - 1) * binSize + (indexItem - 1) *
        spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
      xmax <- xmin + binSize
      ymin <- affine(1 - previous)
      ymax <- affine(1 - value)
    })
    if (normalised) {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/100, 1, each = nrow(df)))
    } else {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/maxFamily, 1, each = nrow(df)))}
    guidesDF <- within(guidesDF, {
      xend <- xmin + binSize
      y <- affine(y)
    })
    totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 0
    p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax, fill = score))
    readableAngle <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
    }
    readableJustification <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
    }
    
    dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
    
    dfItemLabels <- within(dfItemLabels, {
      x <- xmin + binSize/2
      angle <- readableAngle(xmin + binSize/2)
      hjust <- 1
    })
    # new
    
    if (othercompanies == TRUE){
      # LABELS ARE INCLUDED
      typelabel <- data.frame(labelname = c(GT["PortCompanies"][[1]],GT["Oth_Listed"][[1]]),x=c(PortFirmY,OtherFirmY),y=0.0,hjust=0.5, angle=90,labelcolours=c( AxisColour,"grey50"))
      if (PortFirmY == 0){typelabel$labelname[1]<-""}
      
      #Company Labels
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle,
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("grey50", AxisColour, "black")) #guide=FALSE,
      
      # Sector Labels
      p <- p + geom_text(aes(x = x,hjust=hjust, y=y,label = labelname, angle=angle),size = 3, colour=typelabel$labelcolours, data=typelabel)
      
    }else{
      
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle, 
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("black", AxisColour, "black")) #guide=FALSE,
    }
    
    p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y), 
                          colour = "white", data = guidesDF) #+geom_segment(aes(x = xmin, xend = .75, y = y, yend = y), colour = "grey50", data = guidesDF) #complete lines
    
    if (normalised) {
      guideLabels <- data.frame(x = 0, y = seq(0.2,1.0, by= 0.2),#affine(1 - guides/100), 
                                label = paste(guides, "% ", sep = ""))
    }else{ guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily), 
                                     label = paste(guides, "% ", sep = ""))}
    p <- p + geom_text(aes(x = x-1, y = y, label = label), data = guideLabels,
                       angle = 0, hjust = .5, size = 3)
    if (familyLabels) {
      familyLabelsDF <- aggregate(xmin ~ family, data = df, 
                                  FUN = function(s) mean(s + binSize))
      familyLabelsDF <- within(familyLabelsDF, {
        x <- xmin})
      
    }
    p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(), 
                   axis.title.y = element_blank(), panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
                   axis.text.y = element_blank(), axis.ticks = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   legend.title = element_blank(),legend.position = "bottom")
    # p <- p + ylim(0, outerRadius)
    
  }    
  
    if (SectorToPlot == "OG"){
      OG <-AlloftheCompanies # OGCarbonbudget
      CompProdSnapshot <- combin
      OG$InPort <- "AllCompanies"
  
      if (ChartType == "EQ"){AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]}
      else{
        AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
      }
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER")] <- "TICKER"
    colnames(AlloftheCompanies)[colnames(AlloftheCompanies) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER","EquityTicker","DebtTicker")] <- "TICKER"
      
    OG$InPort[OG$EQY_FUND_TICKER %in% CompProdSnapshot$EQY_FUND_TICKER] <- "PortCompanies"
  
    OGCompanies <- AllCompanyData[AllCompanyData$EQY_FUND_TICKER %in% OG$EQY_FUND_TICKER,]
    OGCompanies <- subset(OGCompanies, Year %in% (Startyear+5) & BenchmarkRegion %in% "Global" & CompanyDomicileRegion %in% CompanyDomicileRegionchoose)
    
    OGCompanies<- subset(OGCompanies, !Technology %in%  "Coal")
    OGCompanies$Production[OGCompanies$Technology == "Oil"]<- OGCompanies$Production[OGCompanies$Technology == "Oil"]*6.12
    OGCompanies$Production[OGCompanies$Technology == "Gas"]<- OGCompanies$Production[OGCompanies$Technology == "Gas"]*0.0372
    
    OGCompanies <- ddply(OGCompanies, . (EQY_FUND_TICKER),summarise, Size = sum(Production))
    
    OG <- merge(OG,OGCompanies, by = "EQY_FUND_TICKER",all.x = TRUE, all.y = FALSE)
    
    OG <- OG[!is.na(OG$Size),]
    
    OG <- subset(OG, select = c("Company","InPort","Size","TotalCarbonBudget","OutsideCarbonBudget"))
    
    # limit data
    OG <- OG[order(-OG$Size),]
    OGPort <- subset(OG, OG$InPort %in% "PortCompanies")
    OGOut <- subset(OG, OG$InPort %in% "AllCompanies")
    
    NoInPort <- nrow(OGPort)
    NoOutPort <- nrow(OGOut)
    
    # NoInPort <- 10
    # NoOutPort <-10
    
    if (NoInPort < 10){NoOutPort <- 20 -NoInPort}else
      if(NoOutPort < 10){NoInPort <- 20 -NoOutPort}else 
        if(NoOutPort>10 & NoInPort>10){NoOutPort<-NoInPort<-10}
    
    
    
    OG <- rbind(OGPort[1:NoInPort,],OGOut[1:NoOutPort,])
    OG <- subset(OG, select=-Size)
    
    PlotData <- melt(OG, id.vars = c("Company","InPort"))
    colnames(PlotData) <- c("item","family","score","value")
    techorder <- c("OutsideCarbonBudget","TotalCarbonBudget")
    
    # scale_fill_manual(values = c("ICE" = ICEColour,"Hybrid" = HybridColour, "Electric"= ElectricColour), labels = TechLabels, name = "Technology")
    Colours <- data.frame("variable"=unique(PlotData$score), "Colour"=c("firebrick","darkgrey"), labels=c(GT["OutsideCB"][[1]],GT["InCB"][[1]]))
    Colours$Colour <- as.character(Colours$Colour)
    
    circleProportion = 1
    alphaStart = 0.02
    spaceFamily = .8
    
    if(NoInPort == 0){PortFirmY <-0}else{PortFirmY <- 18}
    
    PlotData
    
    Plot<- WheelofFortune(PlotData, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=5,
                          spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                          circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
      scale_fill_manual(values = Colours$Colour, labels=Colours$labels)+
      coord_flip()
    
  }
  else{
    
    if (SectorToPlot == "Power"){techorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")} 
    if (SectorToPlot == "Automotive"){techorder <- c("ICE","Hybrid","Electric")}
    if (SectorToPlot == "Fossil Fuels"){techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
    AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "Technology"]
    AlloftheCompanies <- rename(AlloftheCompanies, c("Resource.Type" = "Technology"),warn_missing = FALSE)
    
    if (ChartType == "EQ"){AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]}
    else{
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
    }
    }
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER")] <- "TICKER"
    colnames(AlloftheCompanies)[colnames(AlloftheCompanies) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER","EquityTicker","DebtTicker")] <- "TICKER"
    
    CompaniesInPort <- subset(PortSnapshot, select = c("TICKER"), AUM>0)
    CompaniesInPort <- unique(CompaniesInPort)
    
    AllCompanies <- ddply(AlloftheCompanies, .(Technology, TICKER, Name), summarise, Production =sum(Production,na.rm = TRUE)) #Country, 
    colnames(AllCompanies)[colnames(AllCompanies)=="Production"] <- "Capacity"
    AllCompanies$Capacity[is.na(AllCompanies$Capacity)] <-0
    AllCompanies <- subset(AllCompanies, !AllCompanies$Technology %in% "OilCap")
    
    # Classify the Companies
    AllCompanies$Classification <- "AllCompanies"
    AllCompanies$Classification[AllCompanies$TICKER %in% CompaniesInPort$TICKER] <- "PortCompanies"
    
    # Portfolio Average
    Portfoliomix <- ddply(AllCompanies, .(Technology, Classification), summarize, Capacity = sum(Capacity))
    Portfoliomix <- subset(Portfoliomix, Portfoliomix$Classification == "PortCompanies")
    if(dim(Portfoliomix)[1] != 0){
      Portfoliomix$Classification <- "Portfolio"
      # Portfoliomix <- subset(Portfoliomix, !Portfoliomix$Technology %in% c("Oil","Diesel","LPGCNG","Petrol"))
      Portfoliomix$Name <- PortGraphName
      Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Portfoliomix) <- c("item", "family", "score", "value")
      Portfoliomix$value <- as.numeric(Portfoliomix$value)
      Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))*100
    }
    
    Targetmix <- subset(combin, Sector == SectorToPlot & Scenario == Scenariochoose & BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5)
    if (ChartType == "EQ"){ Targetmix <- subset(Targetmix,  CompanyDomicileRegion == CompanyDomicileRegionchoose , select = c("Technology", "TargetProductionAlignment"))}else{
      Targetmix <- subset(Targetmix, select = c("Technology","Benchmark_WtTechShare"))
      Targetmix <- rename(Targetmix, c("Benchmark_WtTechShare" = "TargetProductionAlignment"))
    }
    
    Targetmix$Classification<-"Portfolio"
    Targetmix$Name<-GT["X2Target"][[1]]
    Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
    colnames(Targetmix) <- c("item", "family", "score", "value")
    Targetmix$value <- as.numeric(as.character(Targetmix$value))
    
    
    # Add Index
    Indexmix <- ddply(IndexData, .(CompanyDomicileRegion,Technology), summarize, Capacity = sum(Production))
    Indexmix$Classification <- "Portfolio"
    Indexmix <- subset(Indexmix, select =c("CompanyDomicileRegion","Classification","Technology","Capacity"))
    colnames(Indexmix) <- c("item", "family", "score", "value")  
    Indexmix$value <- as.numeric(as.character(Indexmix$value))
    Indexmix$item <- Indexchoose
    
    # Percentage share of each technology  
    CompanyTotal <- ddply(AllCompanies, .(TICKER,Name), summarise, CompanyTotalCapacity=sum(Capacity))
    AllCompanies <- merge(AllCompanies,CompanyTotal)
    AllCompanies$TechShare <- (AllCompanies$Capacity/AllCompanies$CompanyTotalCapacity)*100
    
    TopPortCompanies <- CompanyTotal[CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopPortCompanies <- TopPortCompanies[rev(order(TopPortCompanies$CompanyTotalCapacity)),]
    TopPortCompanies <- TopPortCompanies[1:companiestoprint,]
    
    TopPortCompanies<-na.omit(TopPortCompanies)
    if(nrow(TopPortCompanies)>0){    TopPortCompanies$totorder <- seq(1,nrow(TopPortCompanies))}
    
    indexcomptoprint <- companiestoprint+ (companiestoprint - nrow(TopPortCompanies))
    TopIndexCompanies <- CompanyTotal[!CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopIndexCompanies <- TopIndexCompanies[rev(order(TopIndexCompanies$CompanyTotalCapacity)),]
    TopIndexCompanies <- TopIndexCompanies[1:indexcomptoprint,]
    TopIndexCompanies$totorder <- seq(1,indexcomptoprint)
    TopIndexCompanies <- TopIndexCompanies[1:(2*companiestoprint-nrow(TopPortCompanies)),]
    
    if (SectorToPlot != "Fossil Fuels"){
      AllTopCompanies <- rbind(TopPortCompanies, TopIndexCompanies)
    }else{
      AllTopCompanies <- TopPortCompanies
    }  
    
    AllCompanies <- subset(AllCompanies, AllCompanies$TICKER %in% AllTopCompanies$TICKER)
    AllCompanies <- subset(AllCompanies, Name != "NA")
    
    # Clean Company Names
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "LIMITED", "LTD.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "COMPANY", "CO.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "CORPORATION", "CORP.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, ",", "")
    AllCompanies$Name<-strtrim(AllCompanies$Name, 16)
    
    oldnames <- c("BAYERISCHE MOTOREN WERKE AG","FIAT CHRYSLER AUTOMOBILES NV","FUJI HEAVY INDUSTRIES LTD","HONDA MOTOR CO LTD","MITSUBISHI MOTORS CORP","BRILLIANCE CHINA AUTOMOTIVE")
    newnames <- c("BMW AG","FIAT CHRYSLER NV","FUJI HEAVY IND LTD","HONDA MOTOR CO","MITSUBISHI MOTORS","BRILLIANCE CN AUTO")
    for (i in c(1:length(oldnames))){AllCompanies$Name[AllCompanies$Name %in% oldnames[i]] <- newnames[i]}
    
    # Rearrange to be ready for WheelofFortune Function
    AllCompanies <- subset(AllCompanies, select = c("Name","Classification","Technology","TechShare"))
    colnames(AllCompanies) <- c("item", "family", "score", "value") #item = component, family = portfolio, score  = technology, value = capacity mix
    
    # Bind the remaining Lines (IEAmix comes in each section)
    
    AllCompanies[AllCompanies$item == "NA"] <- "NoName"
    
    AllCompanies <- as.data.frame(sapply(AllCompanies, function(x) gsub("Cap", "", x)))
    
    # # REMOVE COMPANY NAMES
    # nocompanies <- length(unique(AllCompanies$item))
    # names <- data.frame(item=unique(AllCompanies$item),number=paste0("Company ",LETTERS[1:nocompanies]))
    # AllCompanies <- merge(AllCompanies,names, by="item")
    # AllCompanies$item<-NULL
    # AllCompanies$item <- AllCompanies$number
    # AllCompanies$number<-NULL
    
    
    if(nrow(TopPortCompanies)>0){PortFirmY=(companiestoprint*2-3)}else{PortFirmY <-0}
    OtherFirmY=5
    
    if (SectorToPlot == "Power"){  
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$score <- gsub("Cap","",Portfoliomix$score)
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      Targetmix <- subset(Targetmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Targetmix <- as.data.frame(sapply(Targetmix, function(x) gsub("Cap", "", x)))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Indexmix <- as.data.frame(sapply(Indexmix, function(x) gsub("Cap", "", x)))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      AllCompanies <- subset(AllCompanies, AllCompanies$score != "Oil")
      
      circleProportion = 1
      alphaStart = 0.02
      spaceFamily = .8
      
      TechLabels <- c(paste0("% ", GT["T_CoalCap"][[1]]),paste0("% ", GT["T_GasCap"][[1]]),paste0("% ", GT["T_NuclearCap"][[1]]),paste0("% ", GT["T_HydroCap"][[1]]),paste0("% ", GT["T_RenewablesCap"][[1]]))
      
      labelling <- data.frame(values = c(CoalCapColour,GasCapColour,NuclearColour, HydroColour,RenewablesColour), labels = TechLabels, name = techorder)
      labelling$values <- as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        
        coord_flip()
    }
    
    if (SectorToPlot == "Automotive"){
      Targetmix <- subset(Targetmix, score  %in% c("ICE","Hybrid","Electric"))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("ICE","Hybrid","Electric"))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      TechLabels <- c(paste0("% ", GT["T_ICE"][[1]]),paste0("% ", GT["T_Hybrid"][[1]]),paste0("% ", GT["T_Electric"][[1]]))
      
      labelling <- data.frame(values = c(ICEColour,HybridColour,ElectricColour), labels = TechLabels, name = techorder)
      labelling$values <-    as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      # AllCompanies[is.na(AllCompanies$value)]<-NULL
      AllCompanies <- subset(AllCompanies,!is.na(AllCompanies$value))
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        coord_flip()
      # Plot
      
    }
    
    if (SectorToPlot == "Fossil Fuels"){
      
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      
      AllCompanies <- AllCompanies[rev(order(AllCompanies$item)),]
      AllCompanies$item <- factor(AllCompanies$item, levels=AllCompanies$item)
      
      AllCompanies <- rbind(AllCompanies, Portfoliomix)
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      oilcolours = brewer.pal(9, "YlGnBu")[5:9]
      
      TechLabels <- c(paste0("% ", GT["Conv_Oil"][[1]]),paste0("% ", GT["Heavy_Oil"][[1]]),paste0("% ", GT["Oil_Sands"][[1]]), paste0(GT["Unconv_Oil"][[1]]), paste0(GT["Other_Oil"][[1]]))
      
      Plot<- WheelofFortune(AllCompanies,othercompanies = FALSE , family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder = techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values=oilcolours,labels = TechLabels, name = "Technology")+
        coord_flip()
      
    } 
    
  }
  
  Plot <- ggplot_gtable(ggplot_build(Plot))
  Plot$layout$clip[Plot$layout$name == "panel"] <- "off"
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
  
  png(paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_',SectorToPlot,'_WheelofFortune.png'), height = 2600, width = 5500,res=ppi,bg="transparent") 
  grid.draw(Plot)
  dev.off()  
  
  # return(png(paste(PortfolioName,"_",SectorToPlot,'_WheelofFortune.png'), height = 3.300, width = 3300,res=ppi,bg="transparent") )
  return()  
}

# ------------ FUND MAP --------------------- # 
fundmap_chart <- function(plotnumber,FundsData, Startyear, Scenariochoose, PortfolioName){
  
  # FundsData <- FundsInPort
  
  
  if(typeof(FundsData) == "list"){
    if(nrow(FundsData)>0){
    BenchYear <- Startyear + 5
    AxisColour = 'Black'
    textcolour = 'Black'
    geom.text.size = 2.5
    
    BrownList <-c("CoalCap", "GasCap", "OilCap", "Gas", "Oil", "Coal", "ICE")
    GreenList <-c("NuclearCap", "HydroCap", "RenewablesCap", "Electric","Hybrid")
    
    # Read Results
    #  Results <- read.csv(paste(OutputLocation,PortfolioHoldings,"/",ResultsDate,"_",PortfolioHoldings,"_EquityAnalysisResults-450S-only.csv",sep = ""),stringsAsFactors = FALSE, strip.white = TRUE)
    Results <- FundsData #combin
    
    
    # Remove shitty name
    Results$PortName <- gsub("_.*","",Results$PortName)
    if ("ISIN" %in% colnames(Results)){Results$PortName <- Results$ISIN}
    techlist <- unique(Results$Technology)
    techlist <- techlist[techlist != "OilCap"]
    techlistshort <- c("elec","hyb","ICE","coal","gas","oil","coac","gasc","hyd","nuc","ren")
    Techlist <- as.data.frame(t(rbind(techlist,techlistshort)))
    
    ################## Heatmap ########################
    
    #Select subset of results: Year, Scenario, Where the companies are located/the investment universe, and just funds, not brands. 
    Heatmap <- subset(Results, Results$Year == BenchYear & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    # # Rename the Brand FTSE to the fund FTSE350
    # Heatmap$PortName[Heatmap$PortName == "FTSE"] <- "FTSE350"
    
    # Use AUM Exposure method for fossel fuels
    Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
    # After getting the AUM values, remove that vector from the dataframe
    Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
    Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
    # Rename the technologies to be more reader friendly
    # Heatmap$Technology <- revalue(Heatmap$Technology, c("Gas" = "Gas\nProduction","Oil" = "Oil\nProduction", "Coal" = "Coal\nProduction", "Electric" = "Electric\nVehicles", "Hybrid" = "Hybrid\nVehicles", "ICE" = "ICE\nVehicles", "RenewablesCap" = "Renewable\nCapacity", "NuclearCap" = "Nuclear\nCapacity", "HydroCap" = "Hydro\nCapacity", "GasCap" = "Gas\nCapacity", "CoalCap" = "Coal\nCapacity"))
    Heatmap$MarketExposure <- Heatmap$MarketExposure*100
    
    #fill in values if some regions or technologies are not within the portfolio
    Heatmap <- Heatmap %>% complete(PortName, Technology, BenchmarkRegion) # Keeps N/As
    
    #Create colour bands/buckets
    #alligned
    Heatmap$ExposureColour <-"grey50"#'grey95'
    # Heatmap$ExposureColour[!is.na(Heatmap$MarketExposure)] <- 'grey95'
    
    # 'good' alignment
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 0 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< 0] <- "#FFFFFF"
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 5 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -5] <- "#d2e7d2"
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 15 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -15] <- "#a5cfa5"
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 25 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -25] <- "#78b878"
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 50 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -50] <- "#4ba04b"
    Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 75 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -75] <- "#1f891f"
    
    # 'bad' alignment
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 0 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< 0 ] <- "#FFFFFF"
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 5 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -5 ] <- "#fad7d3"
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 15 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -15 ] <- "#f5afa8"
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 25 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -25 ] <- "#f0877d"
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 50 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -50 ] <- "#eb5f52"
    Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 75 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -75 ] <- "#e73827"
    
    
    # repval = 200
    # redgreen<- colorRampPalette(c("red","white", "darkgreen"))(repval) 
    # 
    # Heatmap$ExposureColour <- redgreen[(100+Heatmap$MarketExposure)]
    # Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & !is.na(Heatmap$MarketExposure)] <- redgreen[(100+Heatmap$MarketExposure[Heatmap$Technology %in% BrownList & !is.na(Heatmap$MarketExposure)] )]
    
    #add '%' text
    Heatmap$ExposureText <-Heatmap$MarketExposure# remove text for auto sector
    Heatmap$ExposureText[!is.na(Heatmap$ExposureText)]<-paste(round(Heatmap$ExposureText[!is.na(Heatmap$ExposureText)], 0), "%", sep="")
    
    # Order Technologies
    # Technology<-c("Electric\nVehicles", "Hybrid\nVehicles", "ICE\nVehicles", "Gas\nProduction", "Oil\nProduction", "Coal\nProduction","Renewable\nCapacity","Hydro\nCapacity", "Nuclear\nCapacity",  "Gas\nCapacity", "Coal\nCapacity")
    tempdb <- data.frame(Sector = c("Power","Power","Power","Power","Power","Fossil Fuels","Fossil Fuels","Fossil Fuels","Automotive","Automotive","Automotive"),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Oil","Gas","Coal","ICE","Electric","Hybrid"), XPosition =1:11)
    
    # Sector <- c("Automotive","Automotive","Automotive","FossilFuels","FossilFuels","FossilFuels","Power","Power","Power","Power","Power")
    # tempdb<-data.frame(cbind(Technology, Sector,XPosition = (seq(from = 1, to = length(Technology), by = 1))))
    Heatmap <- merge(tempdb,Heatmap,by=c("Technology"),all.x=TRUE,all.y=FALSE)
    Heatmap$XPosition <- as.numeric(as.character(Heatmap$XPosition))
    
    # Order Funds
    nofunds <-  length(unique(Heatmap$PortName))
    tempdb<-data.frame(PortName = unique(Heatmap$PortName), YPosition = 1)
    tempdb <- tempdb[order(tempdb$PortName),]
    tempdb$YPosition <- seq(1,nofunds)
    
    Heatmap <- merge(tempdb, Heatmap, by="PortName", all.x=TRUE, all.y=FALSE)
    Heatmap <- arrange(Heatmap, YPosition, XPosition)
    Heatmap$YPosition <- rev(as.numeric(as.character(Heatmap$YPosition)))
    rm(tempdb)
    
    #set region and technolgies as factor
    Heatmap<- Heatmap[order(Heatmap$YPosition,Heatmap$XPosition),]
    Heatmap$PortName <- factor(Heatmap$PortName, levels= unique(Heatmap$PortName)) #Reverse order
    Heatmap$Technology <- factor(Heatmap$Technology, levels=unique(Heatmap$Technology)) #current order
    
    #####Select individual portfolio###
    HeatmapData <- Heatmap
    # HeatMapTitle <- as.character(GT["HeatMap_Title"][[1]])
    
    # Translation Titles
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    HeatmapData$a <- paste0(gsub(" ","",HeatmapData$Sector),"_Unit")
    HeatmapData$b <- paste0("T_",HeatmapData$Technology)
    HeatmapData$b[HeatmapData$Sector %in% "Fossil Fuels"] <- paste0("T_",HeatmapData$Technology[HeatmapData$Sector %in% "Fossil Fuels"],"Prod")
    
    
    
    # HeatmapData$c <- t(GT[HeatmapData$b])
    HeatmapData$TechTitle <- wrap.labels(paste0(t(GT[HeatmapData$b])," ",t(GT[HeatmapData$a])),13)
    HeatmapData$TechTitle[HeatmapData$Sector %in% "Automotive"] <- wrap.labels(paste0(t(GT[HeatmapData$b[HeatmapData$Sector %in% "Automotive"]])),10)
    

    HeatmapData$a <-HeatmapData$b <- NULL
    
    
    TechnologyLabel<- unique(subset(HeatmapData, select = c("TechTitle", "XPosition")))
    FundLabel <- unique(subset(HeatmapData, select = c("PortName", "YPosition")))
    
    TechnologyLabel$TechTitle <- revalue(TechnologyLabel$TechTitle,c("Hybrid-Autos"="Hybrid-\nAutos","Elektro-Autos"="Elektro-\nAutos","Autos mit\nVerbrennungsmotor"="Autos mit\n Verbrennungs-\nmotor","Hydro?lectricit?" ="Hydro\n?lectricit?","Voitures\n? moteur\n?\ncombustion"="Voitures\n? moteur ?\ncombustion"),warn_missing = FALSE)
    
    # FundLabel <- FundLabel[order(FundLabel$YPosition),]
    
    AutoLabel <- as.character(GT["S_Automotive"][[1]])
    FFLabel <- as.character(GT["S_FossilFuels"][[1]])
    PowerLabel <- as.character(GT["S_Power"][[1]])
    
    technamelabelheight <- nrow(FundLabel)+2.5
    sectorlabelheight <- technamelabelheight + 2
    
    iconymin <- nrow(FundLabel)+.6
    iconymax <- iconymin+1.7
    
    ren <- readPNG(paste0(figuredirectory,"RenewablesCap",".png"))
    hyd <- readPNG(paste0(figuredirectory,"HydroCap",".png"))
    nuc <- readPNG(paste0(figuredirectory,"NuclearCap",".png"))
    coac <- readPNG(paste0(figuredirectory,"CoalCap",".png"))
    gasc <- readPNG(paste0(figuredirectory,"GasCap",".png"))
    oil <- readPNG(paste0(figuredirectory,"Oil",".png"))
    gas <- readPNG(paste0(figuredirectory,"Gas",".png"))
    coal <- readPNG(paste0(figuredirectory,"Coal",".png"))
    elec <- readPNG(paste0(figuredirectory,"Electric",".png"))
    hyb <- readPNG(paste0(figuredirectory,"Hybrid",".png"))
    ice <- readPNG(paste0(figuredirectory,"ICE",".png"))
    
    reng <- rasterGrob(ren, interpolate=TRUE)
    hydg <- rasterGrob(hyd, interpolate=TRUE)
    nucg <- rasterGrob(nuc, interpolate=TRUE)
    coacg <- rasterGrob(coac, interpolate=TRUE)
    gascg <- rasterGrob(gasc, interpolate=TRUE)
    oilg <- rasterGrob(oil, interpolate=TRUE)
    gasg <- rasterGrob(gas, interpolate=TRUE)
    coalg <- rasterGrob(coal, interpolate=TRUE)
    elecg <- rasterGrob(elec, interpolate=TRUE)
    hybg <- rasterGrob(hyb, interpolate=TRUE)
    iceg <- rasterGrob(ice, interpolate=TRUE)
    
    
    # HeatmapData$PortName <- revalue(HeatmapData$PortName, c("LänsförsäkringarFondförvaltningAB"="Länsförsäkringar\n FondförvaltningAB"))
    
    # TechnologyLabel$XPosition[1]<-0.5
  HeatmapGGPlot <- ggplot(HeatmapData, aes(x = as.factor(HeatmapData$Technology),fill = as.factor(HeatmapData$ExposureColour), y = as.factor(HeatmapData$PortName), group=HeatmapData$PortName)) +
      geom_tile(colour = "grey95") +
      # geom_text(aes(label = HeatmapData$ExposureText),colour=AxisColour, size = geom.text.size, data = data.frame()) + # text for % values
      annotation_custom(reng,xmin=.7,xmax=1.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(hydg,xmin=1.7,xmax=2.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(nucg,xmin=2.7,xmax=3.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(gascg,xmin=3.7,xmax=4.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(coacg,xmin=4.7,xmax=5.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(oilg,xmin=5.7,xmax=6.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(coacg,xmin=6.7,xmax=7.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(gascg,xmin=7.7,xmax=8.3,ymin=iconymin,ymax = iconymax)+    
      annotation_custom(iceg,xmin=8.7,xmax=9.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(hybg,xmin=9.7,xmax=10.3,ymin=iconymin,ymax = iconymax)+
      annotation_custom(elecg,xmin=10.7,xmax=11.3,ymin=iconymin,ymax = iconymax)+
      
      annotate("text", x = TechnologyLabel$XPosition, y = technamelabelheight,  label= TechnologyLabel$TechTitle, angle = 0, hjust= 0.5, vjust = 0, size = geom.text.size)+ # text for technology axis
      annotate("text", y = FundLabel$YPosition, label = wrap.labels(FundLabel$PortName,11), angle = 0, x = 0.35, hjust = 1, fontface= "plain", size = geom.text.size)+ # text for fund axis
      annotate("text", x = 2.5, y = sectorlabelheight, label = PowerLabel, angle = 0, hjust= 0, vjust = 0,fontface= "bold", size = geom.text.size)+ # label for technology axis
      annotate("text", x = 6.5, y = sectorlabelheight, label = FFLabel, angle = 0, hjust= 0, vjust = 0,fontface= "bold" ,size = geom.text.size)+ # label for technology axis
      annotate("text", x = 9.5, y = sectorlabelheight, label = AutoLabel, angle = 0, hjust= 0, vjust = 0, fontface= "bold",size = geom.text.size)+ # label for technology axis
      
      scale_fill_identity()+
      scale_x_discrete("", expand = c(0, 0)) +
      scale_y_discrete("", expand = c(0, 0)) +
      
      annotate("segment", x =5.5, xend = 5.5, y = 0.5, yend = nrow(FundLabel)+.5, colour = "black", linetype = 'solid', size = 0.3) + # vertical lines
      annotate("segment", x = 8.5, xend = 8.5, y = 0.5, yend = nrow(FundLabel)+.5, colour = "black", linetype = 'solid', size = 0.3) + # vertical lines
      theme(axis.ticks = element_line(colour= "transparent"),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_text(face="bold",colour=textcolour, size=3),
            plot.background = element_rect(fill = "transparent",colour = NA),
            plot.margin = unit(c(1., 0, 0, 3.5), "lines"),                           # (top, , , left side margin) 
            panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            legend.title=element_text(size=6),
            legend.text=element_text(size=6),
            legend.position="bottom",
            legend.key.size=unit(0.3, "cm"),
            legend.key.width=unit(1., "cm"))#+
    # labs(x=NULL, y=NULL, title=HeatMapTitle)#+
    # coord_equal()
    
    HeatmapPlot <- ggplot_gtable(ggplot_build(HeatmapGGPlot))
    HeatmapPlot$layout$clip[HeatmapPlot$layout$name == "panel"] <- "off"
    grid.draw(HeatmapPlot)
    
    chartheight <- 1.2+nrow(Heatmap)*.027
    
    
    ppi = 600
    png(paste0(plotnumber,"_",PortfolioName,"_Funds_HeatMap.png"), height = chartheight*ppi, width = 6.8*ppi,res=ppi,bg="transparent")
    grid.draw(HeatmapPlot)
    dev.off()
  }}
  
}

# ------------ Capacity Build Out ----------- #
buildout_chart <- function(plotnumber,ChartType, combin,  SectorToPlot,BenchmarkRegionchoose, Scenariochoose, CompanyDomicileRegionchoose){
  
  
  ProdData <- subset(combin,  BenchmarkRegion %in% BenchmarkRegionchoose &Scenario %in% Scenariochoose)
  
  if (ChartType == "EQ"){
    ProdData <- subset(combin,  BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose)
    if (SectorToPlot == "Fossil Fuels"){
      ProdData <- subset(ProdData, select = c("Sector","Technology","Year","Production","TargetProductionAUMIntensity"))
      names(ProdData)[names(ProdData)=="TargetProductionAUMIntensity"] <- "TargetProductionAlignment"
    } else{
      ProdData <- subset(ProdData, select = c("Sector","Technology","Year","Production","TargetProductionAlignment"))
    }
    
    
    ProdData$Production[ProdData$Technology == "Coal"]<- ProdData$Production[ProdData$Technology == "Coal"]*24
    ProdData$Production[ProdData$Technology == "Oil"]<- ProdData$Production[ProdData$Technology == "Oil"]*6.12
    ProdData$Production[ProdData$Technology == "Gas"]<- ProdData$Production[ProdData$Technology == "Gas"]*0.0372
  }else{
    
    ProdData <- subset(ProdData, select = c("Sector","Technology","Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
    names(ProdData)[names(ProdData) %in% c("WtTechShareTechShare","Benchmark_WtTechShareTechShare")] <- c("Production","TargetProductionAlignment")
  }
  
  ProdData$NextYear <- 0
  ProdData$NextYear[1:nrow(ProdData)] <- ProdData$Production[1:nrow(ProdData)+1]
  ProdData$NextYear[ProdData$Year==Startyear+10]<- 0
  
  ProdData$BuildOut <- ProdData$NextYear-ProdData$Production
  
  BuildOut <- ProdData[ProdData$Year == Startyear+5 & ProdData$Sector == SectorToPlot,]
  BuildOut <- merge(BuildOut,ColourPalette, by= c("Technology", "Sector"))
  
  if (sum(BuildOut$BuildOut, na.rm = TRUE)>0){
    BuildOutIndicator<-1
    # Select Units
    #-----------
    sectors <- c("Automotive", "Fossil Fuels", "Power")
    BuildOut$TechTrans <- t(GT[paste0("T_",BuildOut$Technology)])
    
    if (ChartType =="EQ"){
      axislabels <- c(as.character(GT["Cars"][[1]]), as.character(GT["produced"][[1]]), as.character(GT["Power_Unit"][[1]]))
      lookup <- data.frame(sectors,axislabels)
      BuildOut$Technology <- gsub("Cap","",BuildOut$Technology)               # Removes "Cap " from the Power labels
      BuildOut$axislabel <- paste(BuildOut$TechTrans,lookup$axislabels[grep(SectorToPlot, lookup$sectors)])
      
      # Scaling and Labelling the Y axis
      maxval <- max(BuildOut$BuildOut,na.rm=TRUE)
      
      magnitude_scale <- c(1e-3,1,1e3,1e6,1e9)
      Power <- c("kW","MW","GW","TW","Error_powertoohigh")
      Automotive <- c("","",as.character(GT["thousand"][[1]]),as.character(GT["million"][[1]]),as.character(GT["billion"][[1]]))
      
      unit_lookup <- data.frame(Automotive,Power)
      # unit_lookup <- setNames(unit_lookup,sectors)
      
      # Scales the Data to the correct units based on the maximum value.
      max_magnitude <- findInterval(maxval,magnitude_scale)
      if(max_magnitude == 0){max_magnitude <- 2}
      
      
      
      
      if(magnitude_scale[max_magnitude]== 1e-3 & SectorToPlot =="Automotive"){
        BuildOut$BuildOut <- BuildOut$BuildOut
        BuildOut$BuildOut <- round(BuildOut$BuildOut,3)}    else{
          BuildOut$BuildOut <- BuildOut$BuildOut/magnitude_scale[max_magnitude]
          BuildOut$BuildOut <- round(BuildOut$BuildOut,1)
          
          # Looks up the units within the correct line in the unit_lookup dataframe and sets the labels
          BuildOut$unitlabel <- unit_lookup[SectorToPlot][max_magnitude,]}
    }
    
    # CB Units
    #-----------
    if (SectorToPlot == "Power"){
      BuildOut$BuildOut[BuildOut$BuildOut < 0]<-0
      BuildOut$xaxis <- paste0(BuildOut$TechTrans,": ",round(BuildOut$BuildOut,1), " ",BuildOut$unitlabel)
      
      BOChart<- ggplot(BuildOut, aes(x="", y=BuildOut, fill=Technology))+
        geom_bar(stat = "identity",color=NA, width = 0.5)+
        geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
        
        # guides(fill = guide_legend(override.aes = list(colour = NULL)))+
        guides(fill=guide_legend(title = GT["BuildOutTitle"][[1]]))+
        scale_fill_manual(values= as.character(BuildOut$Colours),labels=BuildOut$xaxis)+
        theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
              axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_blank(), plot.margin = unit(c(0,0,25,0), "mm"),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              legend.background = element_rect(fill = "transparent",colour = NA),
              legend.text = element_text(size=textsize,family = "Calibri",colour="black"),
              legend.title = element_text(size=textsize,family = "Calibri",colour="black"),
              legend.key.size=unit(0.4,"cm"),
              legend.position = "right") +
        coord_polar("y", start=0, direction=-1)+ xlab('') +  ylab('')
    }else{
      
      BuildOut$Xaxis <- GT["Low_Carb"][[1]]
      BuildOut$Xaxis[BuildOut$Technology == "ICE"] <- GT["High_Carb"][[1]]
      
      BOChart <- ggplot(BuildOut,aes(x=Xaxis, y=BuildOut, fill = Technology))+
        geom_bar(stat = "identity",color=NA,width = .8)+
        geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = .8)+
        annotate(geom = "rect",colour="black", xmin=0.5,xmax=2.5,ymin=0,ymax=0)+
        guides(fill=guide_legend(title = GT["BuildOutTitle"][[1]]))+
        scale_fill_manual(values= as.character(BuildOut$Colours),labels=paste0(BuildOut$TechTrans,": ",BuildOut$BuildOut, " ",BuildOut$unitlabel, " Cars"))+
        theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),axis.text.x = element_text(colour="black"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_blank(), plot.margin = unit(c(0,0,15,0), "mm"),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              legend.background = element_rect(fill = "transparent",colour = NA),
              legend.text = element_text(size=textsize,family = "Calibri",colour="black"),
              legend.key.size=unit(0.4,"cm"),
              legend.position = "bottom")
      
      
    } 
    
  }else{
    wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Label <- GT["NoBuildOut"][[1]]
    
    BOChart <- ggplot()+
      annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,30), size=3)+
      geom_blank()+
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA))
    
    BuildOutIndicator<-0
    
  }
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_BuildOut.png'),bg="transparent",height=2.5,width=4.5,plot=BOChart,dpi=ppi)
  
  return(BuildOutIndicator)
}

# ------------ RENEWABLES ADDITIONS CHART --- #
renewablesadditions_chart <- function(plotnumber,ChartType,combin, PortSnapshot, Scenariochoose, MasterData, AllIEATargets,RegionCountries,PortfolioName){
  
  # combin <- EQCombin
  # PortSnapshot <- EQPortSnapshot
  # combin <-  EQCombin
  # PortSnapshot<-CBPortSnapshot
  
  if(ChartType=="EQ"){
  # Definition of Regions
  PowerRegionExclusion <- c("Global", "OECD", "NonOECD","EU", "OECDAmericas" , "LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia", "OECDAsiaOceania", "NonOECDRest","OECDAggregate","NonOECDAggregate")
  GlobalAggregate <- subset(combin, CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario == Scenariochoose & Year %in% c(Startyear,Startyear+5))# & !BenchmarkRegion %in% PowerRegionExclusion)
  GlobalAggregate <- subset(GlobalAggregate, select = c("BenchmarkRegion","Year","Direction","FairSharePerc"))
  regions <- data.frame(unique(GlobalAggregate$BenchmarkRegion[!GlobalAggregate$BenchmarkRegion %in% c("GlobalAggregate","OECDAggregate","NonOECDAggregate")]))
  
  Countries <- data.frame(BenchmarkRegion=character(),Country=character())
  if (BenchmarkRegionchoose != "GlobalAggregate"){for (j in 1:(nrow(regions))){
    countries <- data.frame(BenchmarkRegion=regions[j,],Country=unique(BenchmarkRegionList[[as.character(regions[j,])]]))
    Countries <- rbind(Countries,countries)}}else{
    Countries<- data.frame(BenchmarkRegion="Global",Country=RegionCountries$Global)
    }
  }else{
    PortSnapshot <- rename(PortSnapshot, c("COMPANY_CORP_TICKER"="EQY_FUND_TICKER"),warn_missing = FALSE)
  }
  # Companies in Port
  PortCompanies <- unique(subset(PortSnapshot, select = c("EQY_FUND_TICKER","Name","piesector"), PortSnapshot$AUM >0))
  
  # IEA Renewables Targets
  IEATargetsRenew <- subset(AllIEATargets,Year %in% c(Startyear, Startyear+5) & Scenario == Scenariochoose & Technology =="RenewablesCap", select = c("BenchmarkRegion","Year","Direction","FairSharePerc","AnnualvalIEAtech"))
  
  # Power Capacities
  PowerCapacity <- subset(MasterData,  Sector =="Power" & Year %in% c(Startyear,Startyear+5) & EQY_FUND_TICKER != "NonListedProduction" & Technology != "OilCap")
  
  # Cut down to power companies
  PowerCapacity <- merge(PortCompanies, PowerCapacity, by = "EQY_FUND_TICKER")
  renewcap <- subset(PowerCapacity, PowerCapacity$Technology %in% "RenewablesCap")
  
  if (nrow(renewcap)>0 ){
    
    # Calculate the company power total for all technology 
    PowerTotals <- ddply(PowerCapacity, .(EQY_FUND_TICKER,Year,PlantLocation), summarise,LocalCompanyCap = sum(CompLvlProduction))
    PowerTotals <- merge(PowerTotals, Countries, by.x ="PlantLocation", by.y="Country")
    PowerTotals <- merge(PowerTotals, IEATargetsRenew, by = c("Year","BenchmarkRegion"))
    PowerTotals$LocalTargetAdditions <- PowerTotals$LocalCompanyCap*(PowerTotals$FairSharePerc)
    
    CompanyPowerTotals <- ddply(PowerTotals, .(EQY_FUND_TICKER,Year),summarise, TargetAdditions =sum(LocalTargetAdditions), CompanyCap = sum(LocalCompanyCap))
    CompanyPowerTotals <- subset(CompanyPowerTotals, Year == Startyear+5, select = c("EQY_FUND_TICKER","TargetAdditions","CompanyCap"))
    
    RenewCapacity <- subset(PowerCapacity,  Technology =="RenewablesCap", select = c("EQY_FUND_TICKER","Name","Year","piesector","Technology","PlantLocation","CompLvlProduction") )
    RenewCapacity <- ddply(RenewCapacity, .(EQY_FUND_TICKER,Name,piesector,Year),summarise, RenewableCap = sum(CompLvlProduction))
    RenewAdditions <- dcast(RenewCapacity, EQY_FUND_TICKER+Name+piesector~Year, fun=sum, value.var = "RenewableCap")
    RenewAdditions$Additions <- RenewAdditions[[as.character(Startyear+5)]]-RenewAdditions[[as.character(Startyear)]]
    RenewAdditions <- subset(RenewAdditions, select = c("EQY_FUND_TICKER","Name","piesector","Additions"))
    
    Renewables <- merge(RenewAdditions,CompanyPowerTotals, by = c("EQY_FUND_TICKER"))
    
    Renewables <- subset(Renewables, piesector == "Utility Power")
    if(dim(Renewables)[1] != 0 ){
      AllUtilities <- ddply(Renewables,.(piesector),summarise, TargetAdditions=sum(TargetAdditions), CompanyCap=sum(CompanyCap), Additions =sum(Additions))  
      AllUtilities$Name <- "All Portfolio Utilities"
      AllUtilities$piesector <- "Combined"
      AllUtilities$EQY_FUND_TICKER <- GT["AllPortUtilities"][[1]]
      
      Renewables<- rbind(Renewables,AllUtilities)
      
      Renewables$AddPerc <- Renewables$Additions/Renewables$TargetAdditions
      Renewables$AddPerc[Renewables$AddPerc > 1]<- 1
      Renewables$Remaining <- 1-Renewables$AddPerc
      Renewables$StillRequired <- Renewables$TargetAdditions-Renewables$Additions
      Renewables<- subset(Renewables, Renewables$TargetAdditions != 0)
      
      Renewables <- Renewables[order(Renewables$CompanyCap),]
      
      nocompanies <- nrow(Renewables)
      companiestokeep <- 20
      if (nocompanies > companiestokeep){
        RenewablesToPlot <- Renewables[((nocompanies-companiestokeep)+1):nocompanies,]
      }else{
        RenewablesToPlot <- Renewables
      }
      
      RenewablesBar <- subset(RenewablesToPlot, select = c("Name","piesector","AddPerc","Remaining","StillRequired"))
      RenewablesBar <- melt(RenewablesBar, id.vars = c("Name","piesector"))
      RenewablesBar <- RenewablesBar[order(RenewablesBar$piesector,RenewablesBar$Name),]
      RenewablesBar$Name <- factor(RenewablesBar$Name, levels = unique(RenewablesBar$Name))
      
      RenewablesStillRequired <- subset(RenewablesBar, RenewablesBar$variable == "StillRequired")
      RenewablesStillRequired$value[RenewablesStillRequired$value < 0 ] <-0
      # RenewablesStillRequired <- RenewablesStillRequired[order(RenewablesStillRequired$piesector,RenewablesStillRequired$Name),]
      RenewablesBar <- RenewablesBar[!(RenewablesBar$variable == "StillRequired"),]
      
      # RenewablesStillRequired$value <- round(RenewablesStillRequired$StillRequired,0)
      RenewablesStillRequired$Name <- factor(RenewablesStillRequired$Name, levels = (RenewablesStillRequired$Name))
      
      Yaxislabel <- GT["REBuildout_axis"][[1]]
      stillreq <- GT["Remaining"][[1]]  # red
      remainlabel <- GT["Progress"][[1]]#GT["StillReq"][[1]]
      progresslabel <- GT["Remaining"][[1]]  #CORRECT #GT["StillReq"][[1]]#GT["X2Target"][[1]]
      Target <- GT["Progress"][[1]] # blue
      
      theme_barcharts <- function(base_size = textsize, base_family = "") {
        theme(axis.ticks=element_blank(), 
              axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
              axis.line = element_line(colour = AxisColour,size=1),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              legend.position=c(0.5,-.2),
              legend.direction="horizontal",
              legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
              legend.background = element_rect(fill = "transparent",colour = NA),
              legend.key.size=unit(0.4,"cm"),
              legend.title=element_blank(),
              legend.key = element_blank(),
              plot.margin = unit(c(.4,0, 2.2, 0), "lines"),
              plot.background = element_rect(fill = "transparent",colour = NA)
        )
      }

      
      # # REMOVE COMPANY NAMES
      # names <- data.frame(Name=unique(RenewablesBar$Name[RenewablesBar$piesector == "Utility Power"]),number=paste0("Company ",LETTERS[1:19]),order =as.numeric(as.character(seq(1:19))))
      # RenewablesBar <- merge(RenewablesBar,names, by="Name")
      # RenewablesBar$Name<-NULL
      # RenewablesBar$Name <- RenewablesBar$number
      
      
      RenAddBar <-ggplot(RenewablesBar, aes(x=rev(Name),y=rev(value), fill = variable))+
        geom_bar(stat = "identity", width=.8, colour = NA)+
        geom_segment(aes(x=0, xend = 0 , y=0, yend = 1), size=1, colour = AxisColour,  arrow = arrow(length = unit(0.4,"cm")))+
        geom_hline(yintercept = 1, colour=Tar2DColour, linetype = "longdash", size = 1)+
        scale_fill_manual(breaks=c("AddPerc","Remaining"),values = c("AddPerc"= badexpColour, "Remaining" = YourportColour), labels = c("AddPerc"=progresslabel, "Remaining" = remainlabel))+
        annotate(hjust = 1,"text", x = c(1:(length(RenewablesStillRequired$Name))) , y = .98, label = paste((round(RenewablesStillRequired$value,0)), "MW",stillreq,sep=" "), colour = "white",size = 3)+
        scale_y_continuous(breaks = c(0,.25,.5,.75,1),label = c("0%","25%","50%","75%",Target),expand=c(0,0), limits=c(0,1))+
        scale_x_discrete(expand=c(0,0))+
        theme_barcharts()+
        coord_flip()
      RenAddBar <- RenAddBar  +
        theme(  axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
                axis.title.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                # legend.position=c(0.5,-.28),
                legend.position = "bottom",
                legend.direction="horizontal",
                legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
                legend.background = element_rect(fill = "transparent",colour = NA),
                legend.key.size=unit(0.4,"cm"),
                legend.title=element_blank(),
                legend.key = element_blank(),
                plot.margin = unit(c(.4,1.5, 0, 0), "lines"))+
        ylab(Yaxislabel)
      
      
      RenAddValues <- c(4.8,8)
      
      NoCompanies <- nrow(RenewablesToPlot)
      
      plotheight <- .2*NoCompanies +.9
      plotwidth <- .1*NoCompanies +7.1
      
      
      ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_RenewableAdditions.png"),bg="transparent",height=plotheight,width=plotwidth,plot=RenAddBar,dpi=ppi)
      RenewAdds<-1
    }else{
      
      Label <- GT["NoRenewInPort"][[1]]
      
      
      RenAddBar <-ggplot()+
        annotate("text", label = Label,x = 0, y = 0, size = 4)+
        theme(  axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                # legend.position=c(0.5,-.28),
                # legend.position = "bottom",
                # legend.direction="horizontal",
                # legend.text = element_text(face="bold",size=textsize,colour="black"),
                # legend.background = element_rect(fill = "transparent",colour = NA),
                # legend.key.size=unit(0.4,"cm"),
                legend.title=element_blank(),
                legend.key = element_blank(),
                plot.margin = unit(c(.4,1.5, 0, 0), "lines"))
      
      
      # outputplot <- ggplot() + annotate("text", label = paste0("No Renewable Additions in Portfolio"),x = 0, y = 0, size = 4)
      ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_RenewableAdditions.png"),bg="transparent",height=2,width=7.5,plot=RenAddBar,dpi=ppi)
      RenewAdds <-0
    }
  }
  return(RenewAdds)
}


# --------
# OTHER PLOT FUNCTIONS
# --------
# ------------ Carbon Chart ----------------- #
OGCCarbon <- function(plotnumber,companiestoprint, OGCarbonBudget, AllCompanyData,CompProdSnapshot, CompanyDomicileRegionchoose, Startyear,BenchmarkRegionchoose, PortfolioName){
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),#element_text(face="bold",colour=AxisColour,size=textsize),
          axis.line = element_line(colour = AxisColour,size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.3),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  OG <- OGCarbonBudget
  OG$InPort <- "All Companies"
  OG$InPort[OG$EQY_FUND_TICKER %in% CompProdSnapshot$EQY_FUND_TICKER] <- "In Port"
  
  OGCompanies <- AllCompanyData[AllCompanyData$EQY_FUND_TICKER %in% OG$EQY_FUND_TICKER,]
  OGCompanies <- subset(OGCompanies, Year %in% (Startyear+5) & BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose)
  
  OGCompanies<- subset(OGCompanies, !Technology %in%  "Coal")
  OGCompanies$Production[OGCompanies$Technology == "Oil"]<- OGCompanies$Production[OGCompanies$Technology == "Oil"]*6.12
  OGCompanies$Production[OGCompanies$Technology == "Gas"]<- OGCompanies$Production[OGCompanies$Technology == "Gas"]*0.0372
  
  OGCompanies <- ddply(OGCompanies, . (EQY_FUND_TICKER),summarise, Size = sum(Production))
  
  OG <- merge(OG,OGCompanies, by = "EQY_FUND_TICKER",all.x = TRUE, all.y = FALSE)
  
  OG <- OG[!is.na(OG$Size),]
  
  
  OG <- subset(OG, select = c("Company","InPort","Size","TotalCarbonBudget","OutsideCarbonBudget"))
  OG$Out <- OG$OutsideCarbonBudget/OG$TotalCarbonBudget
  OG$Tot <- 1-OG$Out
  
  OG$OutsideCarbonBudget <- OG$TotalCarbonBudget <- NULL
  
  
  # limit data
  OG <- OG[order(-OG$Size),]
  OGPort <- subset(OG, OG$InPort %in% "In Port")
  OGOut <- subset(OG, !OG$InPort %in% "In Port")
  
  NoInPort <- nrow(OGPort)
  NoOutPort <- nrow(OGOut)
  
  if (NoOutPort < 10){NoInPort <- 20-NoOutPort}else{NoInPort <- 10}
  if (NoInPort < 10){NoOutPort <- 20-NoInPort}else{NoOutPort <- 10}
  
  OG <- rbind(OGPort[1:NoInPort,],OGOut[1:NoOutPort,])
  OG <- subset(OG, select=-Size)
  
  PlotData <- melt(OG, id.vars = c("Company","InPort"))
  
  ylabel="test"
  Colours <- data.frame("variable"=unique(PlotData$variable), "Colour"=c("firebrick","darkgrey"))
  PlotData <- merge(PlotData,Colours, by="variable")
  PlotData$Colour<- as.character(PlotData$Colour)
  
  PlotData$Company <- factor(PlotData$Company, rev(as.character(PlotData$Company)))
  
  
  ogcarbon_plot<- ggplot(PlotData, aes(Company,value,fill=variable))+
    geom_bar(stat="identity",width = .6)+
    scale_fill_manual(labels=unique(PlotData$InPort),values=unique(as.character((PlotData$Colour))))+
    scale_y_continuous(expand=c(0,0), labels=percent)+
    expand_limits(0,0)+
    guides(fill=guide_legend(nrow = 1))+
    ylab(ylabel)+
    theme_barcharts()+ 
    coord_flip()
  
  
  # Add Labels, split via in or out of port, legend
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_OGPlot.png", sep=""),bg="transparent",height=2.8,width=6.5,plot=ogcarbon_plot,dpi=ppi)
  
  return()  
}

# ------------- DONUT ----------------------- #
donut_chart <- function(plotnumber, Results, PortfolioName){
  
  plot_DonutChart <- function(x,DonutMaxSize,DonutMinSize) {
    
    DonutChart  <- ggplot(x, aes(ymin = 0))
    DonutChart  <- DonutChart + geom_rect(aes(xmin = wm, xmax = w, ymax = ExposureChart, fill = Technology), size =0.1, show.legend = TRUE)+
      coord_polar()+
      scale_fill_manual(name = "", breaks = c("RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap", "Electric", "Hybrid", "ICE","Gas","Oil", "Coal"),
                        values = c("RenewablesCap"= RenewablesColour, "HydroCap" = HydroColour, "NuclearCap" = NuclearColour, "GasCap" = GasCapColour, "CoalCap" = CoalCapColour, "Electric" = ElectricColour, "Hybrid" = HybridColour, "ICE" = ICEColour, "Gas" = GasProdColour, "Oil" = OilProdColour, "Coal" = CoalProdColour),
                        labels = c(paste("Renewable Power",x$Exposure[1], "%"), paste("Hydro Power",x$Exposure[2], "%"), paste("Nuclear Power",x$Exposure[3], "%"), paste("Gas Power",x$Exposure[4], "%"), paste("Coal Power",x$Exposure[5], "%"), paste("Electric Vehicles",x$Exposure[6], "%"), paste("Hybrid Vehicles",x$Exposure[7], "%"), paste("ICE Vehicles",x$Exposure[8], "%"), paste("Gas Supply",x$Exposure[9], "%"), paste("Oil Supply",x$Exposure[10], "%"), paste("Coal Supply",x$Exposure[11], "%")))+
      labs(x = "", y = "")+
      annotate("segment", x = 0, xend = 1 , y = 0, yend = 0 ,colour = "black", size=1,alpha=1)+ 
      geom_segment(aes(x = 0, y = DonutMinSize-40, xend = 0, yend = DonutMaxSize), colour = "transparent")+
      #geom_vline(x = 0, linetype = 1,size =1,alpha=0.25)+
      scale_x_continuous(breaks=c(),labels=c())+
      #scale_y_continuous(limits=c(yMinLimit, yMaxLimit), breaks=c())+
      theme(axis.text.y = element_blank(), axis.ticks = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = 'transparent', colour='transparent'), 
            legend.key = element_rect(colour = 'grey90', fill = 'transparent'),
            legend.position=c(0.5,0),legend.text = element_text(size=textsize+2),
            legend.key.size=unit(0.4,"cm"))+
      #guides(fill=guide_legend(ncol = 2)) #turns on legend
      guides(fill=FALSE, colour= FALSE) #turns off legend
    return(DonutChart)
  }
  
  create_table <- function(x, rownames) {
    mytheme <- gridExtra::ttheme_minimal(   
      core=list(bg_params = list(),fg_params=list(fontsize = textsize,fontface=1,col = AxisColour)),
      colhead=list(fg_params=list(fontsize = textsize,fontface=1,col = AxisColour)),
      rowhead=list(fg_params=list(fontsize = textsize,fontface=1,col = AxisColour,hjust=0, x=0.1))
    )
    
    tableGrob(x,
              theme = mytheme,
              rows = rownames)
  }
  
  Results$Technology <- as.character(Results$Technology)
  Results$Technology <- factor(Results$Technology, levels=unique(Results$Technology))
  
  # Set Tech as factor
  Results$Technology <- as.character(Results$Technology)
  Results$Technology <- factor(Results$Technology, levels=unique(Results$Technology))
  
  DonutMaxSize = as.numeric(max(Results$ExposureChart,na.rm = TRUE))
  DonutMinSize = as.numeric(min(Results$ExposureChart,na.rm = TRUE))
  
  #Table Creation
  tbleResults<-subset(Results, select=c("Technology", "Exposure","Sector"))
  tbleResults$Exposure<- ifelse(tbleResults$Exposure == "N/A", tbleResults$Exposure, paste(tbleResults$Exposure, "%", sep=""))
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  
  powertble <- data.frame(YourPortfolio = character(5))
  powertble$YourPortfolio <- subset(tbleResults$Exposure, tbleResults$Sector %in% "Power")
  powertble<-rename(powertble, c("YourPortfolio"=PortfolioName))
  powernames<-c("Renewables", "Hydro", "Nuclear", "Gas", "Coal")
  PrintPowertble<-arrangeGrob(create_table(powertble,powernames),nrow=1)
  
  autotble <- data.frame(YourPortfolio = character(3))
  autotble$YourPortfolio<-subset(tbleResults$Exposure, tbleResults$Sector %in% "Automotive")
  autotble<-rename(autotble, c("YourPortfolio"=PortfolioName))
  autonames<-c("Electric", "Hybrid", "ICE")
  PrintAutotble<-arrangeGrob(create_table(autotble,autonames),nrow=1)
  
  fossiltble <- data.frame(YourPortfolio = character(3))
  fossiltble$YourPortfolio<-subset(tbleResults$Exposure, tbleResults$Sector %in% "Fossil Fuels")
  fossiltble<-rename(fossiltble, c("YourPortfolio"=PortfolioName))
  fossilnames<-c("Gas production", "Oil production", "Coal production")
  PrintFossiltble<-arrangeGrob(create_table(fossiltble,fossilnames),nrow=1)
  
  # Donut Plot
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_Donut.png'),bg="transparent",height=5,width=5,plot=plot_DonutChart(Results,DonutMaxSize,DonutMinSize),dpi=ppi)
  
  # Tables
  ggsave(filename=paste0(plotnumber,".1_", PortfolioName, "_PowerExposureTable.png"),PrintPowertble,bg="transparent",height=1.5,width=2.5,dpi=ppi)
  ggsave(file=paste0(plotnumber,".2_", PortfolioName,"_AutoExposureTables.png"),PrintAutotble,bg="transparent",height=1,width=2.5,dpi=ppi)
  ggsave(file=paste0(plotnumber,".3_", PortfolioName,"_FossilExposureTables.png"),PrintFossiltble,bg="transparent",height=1,width=2.5,dpi=ppi)
  
  
  return()
}

# ------------- BAR CHARTS ------------------ #
bar_chart <- function(plotnumber,combin,IndexData,SectorToPlot,BenchmarkRegionchoose, CompanyDomicileRegionchoose,Scenariochoose,IndexName, PortfolioName){
  
  prodcheck <- production_check(combin, Scenariochoose,Startyear,BenchmarkRegionchoose,CompanyDomicileRegionchoose)
  
  # if (prodcheck$Production[which(prodcheck$Sector == SectorToPlot)] >0){
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.line = element_line(colour = AxisColour,size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position=c(0.5,-.2),
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          legend.key = element_blank(),
          plot.margin = unit(c(0.6,0, 2.4, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA)
    )
  }
  
  # if (SectorToPlot == "Automotive"){
  #   ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion=="Global" & CompanyDomicileRegion == CompanyDomicileRegionchoose & Sector == SectorToPlot& Scenario == Scenariochoose )
  # } else{
  ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & Sector == SectorToPlot)
  # }
  
  # if (SectorToPlot == "Automotive" |SectorToPlot == "FossilFuels" ){
  #   NumberTechs <-3  }else{NumberTechs <-5}
  
  
  # IndexName <- IndexName
  IndexMix_5yrs <- subset(IndexData,IndexData$Sector %in% SectorToPlot)
  IndexMix_5yrs <- ddply(IndexMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                         IndexProduction= sum(Production))
  IndexMix_5yrs <- melt(IndexMix_5yrs, id = c( "Year","Technology","Scenario","Sector"))
  SectorTotals <- ddply(IndexMix_5yrs,.(Year,Sector,variable), summarise,SectorTotal = sum(value))
  IndexMix_5yrs <- merge(IndexMix_5yrs,SectorTotals)  
  
  NoIndexTechs <- length(unique(IndexMix_5yrs$Technology))
  
  # ProductionMix_5yrs <- rbind(ProductionMix_5yrs,IndexMix_5yrs)
  if (SectorToPlot == "Fossil Fuels"){
    ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                PortProduction= sum(Production),
                                RefProduction = sum(TargetProductionAUMIntensity))
  }else{
    ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Year, Sector, Technology,Scenario), summarise,
                                PortProduction= sum(Production),
                                RefProduction = sum(TargetProductionAlignment))}
  ProductionMix_5yrs <- melt(ProductionMix_5yrs, id = c( "Year","Technology","Scenario","Sector"))
  SectorTotals <- ddply(ProductionMix_5yrs,.(Year,Sector,variable), summarise,SectorTotal = sum(value))
  ProductionMix_5yrs <- merge(ProductionMix_5yrs,SectorTotals)
  
  #If not all technologies are present - add a zero
  NoPortTechs <- length(unique(ProductionMix_5yrs$Technology))
  if (NoPortTechs < NoIndexTechs){
    missingtechs <- IndexMix_5yrs$Technology[!IndexMix_5yrs$Technology %in% unique(ProductionMix_5yrs$Technology)] 
    missingprod <- IndexMix_5yrs[IndexMix_5yrs$Technology %in% missingtechs,]
    missingprod$value <-0
    missingprod$SectorTotal<-0
    missingprod$variable <- "PortProduction"
    missingref <- missingprod
    missingref$variable <- "RefProduction"
    ProductionMix_5yrs <- rbind(ProductionMix_5yrs,missingref, missingprod)
  }
  
  ProductionMix_5yrs <- rbind(ProductionMix_5yrs,IndexMix_5yrs)
  
  # Normalise the FF results
  if (SectorToPlot == "Fossil Fuels"){
    indexproductions <- subset(IndexData,IndexData$Sector %in% SectorToPlot, select = c("Technology","AUMExposure"))
    refproductions <- subset(ProductionMix_5yrs, variable == "RefProduction")
    ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value/refproductions$value
    ProductionMix_5yrs$TechShare[ProductionMix_5yrs$variable == "RefProduction"] <- 1
    ProductionMix_5yrs$TechShare[ProductionMix_5yrs$variable == "IndexProduction"] <- (1+indexproductions$AUMExposure)
  } else{
    ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value/ProductionMix_5yrs$SectorTotal
  }
  
  ProductionMix_5yrs$TechShare[ProductionMix_5yrs$TechShare >10] <-10
  ProductionMix_5yrs$TechShare[is.nan(ProductionMix_5yrs$TechShare)] <- 0
  ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c("Sector","Technology","variable","TechShare"))
  
  ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "CoalCap"] <- "Coal"
  ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "GasCap"] <- "Gas"
  ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "HydroCap"] <- "Hydro"
  ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "NuclearCap"] <- "Nuclear"
  ProductionMix_5yrs$Technology[ProductionMix_5yrs$Technology %in% "RenewablesCap"] <- "Renewables"
  ProductionMix_5yrs <- subset(ProductionMix_5yrs,!Technology %in% "OilCap")
  
  ProductionMix_5yrs$variable <- as.character(ProductionMix_5yrs$variable)
  ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "PortProduction"] <- PortfolioName
  ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "RefProduction"] <- "2°C Benchmark"
  ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "IndexProduction"] <- IndexName
  
  PlotData <- subset(ProductionMix_5yrs, Sector == SectorToPlot)
  orderofchart <- c(PortfolioName,"2°C Benchmark",IndexName)
  PlotData$variable <- factor( as.character(PlotData$variable), levels=orderofchart )
  PlotData <- PlotData[order(PlotData$variable),]
  PlotData <- PlotData [order(PlotData$Technology,PlotData$variable),]
  
  if (SectorToPlot == "Fossil Fuels"){
    yscale<-(max(PlotData$TechShare, na.rm = TRUE))
    yscalemax <- ifelse(yscale>=5, 10,
                        ifelse(yscale>=2, 5,
                               ifelse(yscale>1, 2,
                                      1)))
    n<- ifelse(yscale>=5, 11,
               ifelse(yscale>=2, 6,
                      ifelse(yscale>1, 9,
                             6)))
    PlotData$TechShare <- PlotData$TechShare*100 
    
    barchart_plot<- ggplot(PlotData, aes(Technology, TechShare,fill=variable))+
      geom_bar(stat = "identity",position = position_dodge())+
      scale_fill_manual(values=c(YourportColour,Tar2DColourBar,IndexColour))+
      scale_y_continuous(expand=c(0,0), limits = c(0,max(PlotData$TechShare)*1.1))+  #labels=percent,
      #scale_y_continuous(expand=c(0,0), breaks=seq(0, yscalemax, length=n))+
      expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 2))+
      ylab("Normalized fossil fuel production")+
      theme_barcharts()
    
  }else{
    barchart_plot<- ggplot(PlotData, aes(Technology, TechShare,fill=variable, Colour = 'transparent'))+    #fill=linetypes,
      geom_bar(stat = "identity",position = position_dodge())+
      scale_fill_manual(values=c(YourportColour,Tar2DColourBar,IndexColour))+
      scale_y_continuous(labels=percent,expand=c(0,0), limits = c(0,max(PlotData$TechShare)*1.1))+
      expand_limits(y=0)+
      guides(fill=guide_legend(nrow = 2))+
      ylab(paste("Technology Share in ",Startyear+5))+
      theme_barcharts()
  }
  
  labelsize <- 2.5
  yshift <- 0.05
  notech <- length(unique(PlotData$Technology))
  xlocation <- c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3)
  if (SectorToPlot == "Power"){ 
    labelsize <- 1.5
    yshift <- 0.02
    xlocation <- c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,4.3,4.7,5,5.3)}
  if(SectorToPlot == "Fossil Fuels"){yshift <- 5 }
  
  labelcolours <- rep(c(YourportColour,"#8fb154","grey50"),notech)
  
  
  if (SectorToPlot == "Power"){
    barchart_plot <- barchart_plot + 
      annotate("text", x=xlocation, y= PlotData$TechShare+yshift, label = paste(round(100*PlotData$TechShare,0), "%",sep = ""), colour = labelcolours,size = labelsize)
  }
  if (SectorToPlot == "Automotive"){
    barchart_plot <- barchart_plot + 
      annotate("text", x=xlocation, y= PlotData$TechShare+yshift, label = paste(round(100*PlotData$TechShare,1), "%",sep = ""), colour = labelcolours,size = labelsize)
  }
  if (SectorToPlot == "Fossil Fuels"){
    barchart_plot <- barchart_plot + 
      annotate("text", x=xlocation, y= PlotData$TechShare+yshift, label = paste(round(PlotData$TechShare,0),sep = ""), colour = labelcolours,size = labelsize)
  }
  
  # barchart_plot$layout$clip[barchart_plot$layout$name == "panel"] <- "off"
  
  if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",SectorToPlot,'_bar.png', sep=""),bg="transparent",height=2.6,width=3.8,plot=barchart_plot,dpi=ppi)
  
  
  # }else{
  #   barchart_plot <- ggplot() + annotate("text", label = paste0("No ",SectorToPlot, " Production in Fund"),x = 0, y = 0, size = 4)
  #   ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",SectorToPlot,'_bar.png', sep=""),bg="transparent",height=2.6,width=3.8,plot=barchart_plot,dpi=ppi)
  # }
  
  return() 
}

# ------------- LINE CHARTS ----------------- #
line_chart <- function(plotnumber,ChartType,combin, TechToPlot, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, PortfolioName){
  theme_linecharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(), 
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.line = element_line(colour = AxisColour,size=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position=c(0.5,-.4),#legend.position = "none", 
          legend.direction="horizontal",
          legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title = element_text(colour = AxisColour, size = textsize),
          legend.key = element_blank(),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1,1, 0, 0), "lines")
    )
  }    
  
  LineData <- subset(combin, Technology %in% TechToPlot & BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose)    
  
  if (ChartType == "EQ"){
    
    if (SectorToPlot == "Fossil Fuels"){
      LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAUMIntensity"))
      names(LineData)[names(LineData)=="TargetProductionAUMIntensity"] <- "TargetProductionAlignment"
    } else{
      LineData <- subset(LineData, select = c("Sector","Year","Production","TargetProductionAlignment"))
      
    }
    
    names(LineData)[names(LineData)=="TargetProductionAlignment"] <- "Target"
    names(LineData)[names(LineData)== "Production"] <- "Portfolio"
    
    sectors <- c("Automotive", "Fossil Fuels", "Power")
    axislabels <- c("Cars Produced", "Production", "Capacity")
    lookup <- data.frame(sectors,axislabels)
    axislabel <- paste(TechToPlot,lookup$axislabels[grep(SectorToPlot, lookup$sectors)])
    axislabel <- gsub("Cap "," ",axislabel)               # Removes "Cap " from the Power labels
    
    # Scaling and Labelling the Y axis
    maxval <- max(LineData[,4],LineData[,3],na.rm=TRUE)
    
    magnitude_scale <- c(1e-3,1,1e3,1e6,1e9)
    if(SectorToPlot == "Automotive"){magnitude_scale <- c(1,1,1e3,1e6,1e9)}
    power_units <- c("kW","MW","GW","TW","Error_powertoohigh")
    car_units <- c("","","thousands","millions","billions")
    ff_units <- c("","","thousands","millions","billions")
    coal_units <- c("kg","t","kt","MT","GT")
    oil_units <- c("","barrels","thousand barrels","million barrels","billion barrels")
    gas_units <- c("","m?","thousand m?","million m?","billion m?")
    unit_lookup <- data.frame(car_units,ff_units,power_units,coal_units,oil_units,gas_units)
    ff_sectors <- c("Coal","Oil","Gas")
    sectors <- cbind(sectors, ff_sectors)
    unit_lookup <- setNames(unit_lookup,sectors)
    
    # Scales the Data to the correct units based on the maximum value.
    max_magnitude <- findInterval(maxval,magnitude_scale)
    if(max_magnitude == 0){max_magnitude <- 2}
    LineData$Portfolio <- LineData$Portfolio /magnitude_scale[max_magnitude]
    LineData$Target <- LineData$Target/magnitude_scale[max_magnitude]
    
    # Looks up the units within the correct line in the unit_lookup dataframe and sets the labels
    if (SectorToPlot == "Fossil Fuels")  {unit_search <- TechToPlot} else{
      unit_search <- SectorToPlot}
    
    unitlabel <- paste("(",unit_lookup[unit_search][max_magnitude,],")",sep="")  
    if (unitlabel =="()"){unitlabel<-""}
    
  }else{
    
    # if (SectorToPlot == "Automotive"){
    #   BenchmarkRegionchoose <- "Global"
    # } 
    LineData <- subset(combin, Technology %in% TechToPlot & BenchmarkRegion %in% BenchmarkRegionchoose & CompanyDomicileRegion %in% CompanyDomicileRegionchoose & Scenario %in% Scenariochoose)    
    LineData <- subset(LineData, select = c("Sector","Year","ScenarioShare","PorjMarketTechShare"))
    names(LineData)[names(LineData)=="PorjMarketTechShare"] <- "Target"
    names(LineData)[names(LineData)== "ScenarioShare"] <- "Portfolio"
    
    max_magnitude <- 100
    unitlabel <- paste0(TechToPlot," %")
    axislabel 
    
    LineData$Portfolio <- LineData$Portfolio*100
    LineData$Target <- LineData$Target*100  
  }
  
  LineData <- subset(LineData, LineData$Year >= Startyear)
  LineData$Portfolio[!LineData$Year %in% c(Startyear:(Startyear+5))]<- NA
  
  goodtech <- c("RenewablesCap","HydroCap","NuclearCap","Hybrid","Electric")  
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  
  if (TechToPlot %in% badtech){
    # Bad Technologies
    outputplot <- ggplot(data=LineData)+
      geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=badexpColour)) +
      geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=goodexpColour)) +
      geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
      geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
      geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
      scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
      scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
      xlab("Year") + ylab(paste(axislabel,unitlabel)) + # Set axis labels
      scale_x_continuous(breaks=seq(Startyear,Startyear+10,5),expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
      theme_linecharts()
  }else{
    # Good Technologies
    outputplot <- ggplot(data=LineData)+
      geom_ribbon(aes(x=Year,ymin=Target,ymax=pmax(Target,Portfolio),fill=goodexpColour)) +
      geom_ribbon(aes(x=Year,ymin=pmin(Target,Portfolio),ymax=Target,fill=badexpColour)) +
      geom_ribbon(aes(x=Year,ymin=0,ymax=pmin(Target,Portfolio),fill=CurrCapColour))+
      geom_line(aes(x=Year,y=Portfolio,colour=YourportColour),size=1.5,linetype=1) +
      geom_line(aes(x=Year,y=Target,colour=Tar2DColour),size=1.5,linetype=2) +
      scale_fill_identity(name = "", guide = 'legend',labels = c("Exposure gap","Current capacity + planned additions")) +
      scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2°C Benchmark"))  +
      xlab("Year") + ylab(paste(axislabel,unitlabel)) + # Set axis labels
      scale_x_continuous(breaks=seq(Startyear,Startyear+10,5),expand=c(0,0))+
      scale_y_continuous(expand=c(0,0))+
      expand_limits(y= 1.1*max(LineData[,c(3,4)], na.rm=TRUE))+
      theme_linecharts()
  }
  outputplot <- outputplot +
    guides(colour=guide_legend(keywidth = 4, keyheight = 1,order=1,override.aes = list(linetype=c(1,2),colour=c(YourportColour,Tar2DColour),size=1.5)))    
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_LinePlot.png', sep=""),bg="transparent",height=2.9,width=3.8,plot=outputplot,dpi=ppi)
  
  return()    
}

# ------------- WHEEL CHARTS ---------------- #
wheel_chart <- function(plotnumber,PortSnapshot, combin,AlloftheCompanies, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, IndexData,IndexName, PortfolioName){
  
  WheelofFortune<-function(df, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.2,
                           spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1, guides = c(0, 25, 50, 75, 100),
                           alphaStart = -0.3, circleProportion = 0.8, direction = "inwards", familyLabels = FALSE, normalised = TRUE)
  {
    
    #TEST#
    # df <- AllCompanies
    
    if (!is.null(columnNames)) {
      namesColumn <- names(columnNames)
      names(namesColumn) <- columnNames
      df <- rename(df, namesColumn)
    }
    
    
    applyLookup <- function(groups, keys, unassigned = "unassigned") {
      lookup <- rep(names(groups), sapply(groups, length, USE.NAMES = FALSE))
      names(lookup) <- unlist(groups, use.names = FALSE)
      p <- lookup[as.character(keys)]
      p[is.na(p)] <- unassigned
      p
    }
    if (!is.null(family)) {
      df$family <- applyLookup(family, df$item)}
    df <- arrange(df, family, item, score)
    if (normalised) 
    {df <- ddply(df, .(family, item), transform, value = cumsum(value/(sum(value))))
    }else {
      maxFamily <- max(plyr::ddply(df, .(family, item), summarise, 
                                   total = sum(value))$total)
      df <- ddply(df, .(family, item), transform, value = cumsum(value))
      df$value <- df$value/maxFamily
    }
    df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
    df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
    df2$indexItem <- cumsum(df2$indexItem)
    df3 <- ddply(df, .(family), summarise, indexFamily = 1)
    df3$indexFamily <- cumsum(df3$indexFamily)
    df <- merge(df, df2, by = c("family", "item"))
    df <- merge(df, df3, by = "family")
    df <- arrange(df, family, item, score)
    affine <- switch(direction, inwards = function(y) (outerRadius - 
                                                         innerRadius) * y + innerRadius, outwards = function(y) (outerRadius - 
                                                                                                                   innerRadius) * (1 - y) + innerRadius, stop(paste("Unknown direction")))
    df <- within(df, {
      xmin <- (indexItem - 1) * binSize + (indexItem - 1) * 
        spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
      xmax <- xmin + binSize
      ymin <- affine(1 - previous)
      ymax <- affine(1 - value)
    })
    if (normalised) {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)), 
                             y = rep(1 - guides/100, 1, each = nrow(df)))
    } else {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)), 
                             y = rep(1 - guides/maxFamily, 1, each = nrow(df)))}
    guidesDF <- within(guidesDF, {
      xend <- xmin + binSize
      y <- affine(y)
    })
    totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 
      0
    p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax, 
                                    ymin = ymin, ymax = ymax, fill = score))  # , guide=FALSE
    readableAngle <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 
        90
      angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * 
                                                            pi/180)) == -2, 180, 0)
    }
    readableJustification <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 
        90
      ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == 
               -2, 1, 0) 
    }
    dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
    dfItemLabels <- within(dfItemLabels, {
      x <- xmin + binSize/2
      angle <- readableAngle(xmin + binSize/2)
      hjust <- readableJustification(xmin + binSize/2)
    })
    p <- p + geom_text(aes(x = x, label = item, angle = angle, 
                           hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 1.02, size = 2.5, vjust = 0.5, data = dfItemLabels) +
      scale_colour_manual(values = c("grey50", AxisColour, "black")) #guide=FALSE,
    
    p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y), 
                          colour = "white", data = guidesDF) #+geom_segment(aes(x = xmin, xend = .75, y = y, yend = y), colour = "grey50", data = guidesDF) #complete lines
    if (normalised) {
      guideLabels <- data.frame(x = 0, y = affine(1 - guides/100), 
                                label = paste(guides, "% ", sep = ""))
    }else{ guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily), 
                                     label = paste(guides, "% ", sep = ""))}
    p <- p + geom_text(aes(x = x, y = y, label = label), data = guideLabels, 
                       angle = -alphaStart * 180/pi, hjust = 1, size = 3)
    if (familyLabels) {
      familyLabelsDF <- aggregate(xmin ~ family, data = df, 
                                  FUN = function(s) mean(s + binSize))
      familyLabelsDF <- within(familyLabelsDF, {
        x <- xmin
        angle <- xmin * (-360/totalLength) - alphaStart * 
          180/pi
      })
      p <- p + geom_text(aes(x = x, label = family, angle = angle), 
                         data = familyLabelsDF, y = 1.3)
    }
    p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(), 
                   axis.title.y = element_blank(), panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
                   axis.text.y = element_blank(), axis.ticks = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA))+
      guides(fill=FALSE, colour= FALSE)
    
    p <- p + xlim(0, tail(df$xmin + binSize + spaceFamily, 1)/circleProportion)
    p <- p + ylim(0, outerRadius + 0.2)
    p <- p + coord_polar(start = alphaStart)
    p
  }    
  
  PortSnapshot <- rename(PortSnapshot, c("EQY_FUND_TICKER"="TICKER"), warn_missing = FALSE)
  AlloftheCompanies <- rename(AlloftheCompanies, c("EQY_FUND_TICKER"="TICKER"), warn_missing = FALSE)
  
  CompaniesInPort <- subset(PortSnapshot, select = c("TICKER"), AUM>0)
  CompaniesInPort <- unique(CompaniesInPort)
  
  AllCompanies <- ddply(AlloftheCompanies, .(Technology, TICKER, Name), summarise, Production =sum(Production,na.rm = TRUE)) #Country, 
  colnames(AllCompanies)[colnames(AllCompanies)=="Production"] <- "Capacity"
  AllCompanies$Capacity[is.na(AllCompanies$Capacity)] <-0
  
  # Classify the Companies
  AllCompanies$Classification <- "AllCompanies"
  AllCompanies$Classification[AllCompanies$TICKER %in% CompaniesInPort$EQY_FUND_TICKER] <- "PortCompanies"
  
  numberportcompanies <- nrow(AllCompanies[AllCompanies$Classification %in% "PortCompanies",])
  if (numberportcompanies > 0){
    
    
    # Portfolio Average
    Portfoliomix <- ddply(AllCompanies, .(Technology, Classification), summarize, Capacity = sum(Capacity))
    Portfoliomix <- subset(Portfoliomix, Portfoliomix$Classification == "PortCompanies")
    if(dim(Portfoliomix)[1] != 0){
      Portfoliomix$Classification <- "Portfolio"
      # Portfoliomix <- subset(Portfoliomix, !Portfoliomix$Technology %in% c("Oil","Diesel","LPGCNG","Petrol"))
      Portfoliomix$Name <- PortfolioName
      Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Portfoliomix) <- c("item", "family", "score", "value")
      Portfoliomix$value <- as.numeric(Portfoliomix$value)
      Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))*100
    }
    
    # Targets take 2
    # if (SectorToPlot == "Automotive" & BenchmarkRegionchoose != "Global"){
    # Targetmix <- subset(combin, Sector == SectorToPlot & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & BenchmarkRegion == "Global" & Year == Startyear+5, select = c("Technology", "TargetProductionAlignment"))
    # }else{
    Targetmix <- subset(combin, Sector == SectorToPlot & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5, select = c("Technology", "TargetProductionAlignment"))
    # }
    Targetmix$Classification<-"Portfolio"
    Targetmix$Name<-GT["2Target"][[1]]
    Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
    colnames(Targetmix) <- c("item", "family", "score", "value")
    Targetmix$value <- as.numeric(as.character(Targetmix$value))
    
    
    # Add Index
    Indexmix <- ddply(IndexData, .(CompanyDomicileRegion,Technology), summarize, Capacity = sum(Production))
    Indexmix$Classification <- "Portfolio"
    Indexmix <- subset(Indexmix, select =c("CompanyDomicileRegion","Classification","Technology","Capacity"))
    colnames(Indexmix) <- c("item", "family", "score", "value")  
    Indexmix$value <- as.numeric(as.character(Indexmix$value))
    Indexmix$item <- IndexName
    
    # Percentage share of each technology  
    CompanyTotal <- ddply(AllCompanies, .(EQY_FUND_TICKER,Name), summarise, CompanyTotalCapacity=sum(Capacity))
    AllCompanies <- merge(AllCompanies,CompanyTotal)
    AllCompanies$TechShare <- (AllCompanies$Capacity/AllCompanies$CompanyTotalCapacity)*100
    
    # Limit number of Power Companies
    if (SectorToPlot == "Power" & BenchmarkRegionchoose %in% c("Global","GlobalAggregate")){
      AllCompanies <- subset(AllCompanies,CompanyTotalCapacity > 5000 | Classification == "PortCompanies")  # Filter by companies greater than 1GW
    }else if(SectorToPlot == "Power" & BenchmarkRegionchoose %in% c("OECD","OECDAggregate")){
      AllCompanies <- subset(AllCompanies,CompanyTotalCapacity > 1000 | Classification == "PortCompanies")  # Filter by companies greater than 1GW
    }
    
    AllCompanies <- subset(AllCompanies, Name != "NA")
    
    # Clean Company Names
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "LIMITED", "LTD.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "COMPANY", "CO.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "CORPORATION", "CORP.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, ",", "")
    AllCompanies$Name<-strtrim(AllCompanies$Name, 16)
    i=0
    for (i in 1:length(AllCompanies$Name)) {
      if(nchar(AllCompanies[i,2], type = "chars")==16){AllCompanies[i,2]<-paste(AllCompanies[i,2],"...", sep = "")}
    }
    
    oldnames <- c("NATIONAL GRID PL...","TERNA RETE ELETT...","BAYERISCHE MOTOREN WERKE AG","FIAT CHRYSLER AUTOMOBILES NV","FUJI HEAVY INDUSTRIES LTD","HONDA MOTOR CO LTD","MITSUBISHI MOTORS CORP","BRILLIANCE CHINA AUTOMOTIVE")
    newnames <- c("NATIONAL GRID PLC","TERNA RETE ELET...","BMW AG","FIAT CHRYSLER NV","FUJI HEAVY IND LTD","HONDA MOTOR CO","MITSUBISHI MOTORS","BRILLIANCE CN AUTO")
    for (i in c(1:length(oldnames))){AllCompanies$Name[AllCompanies$Name %in% oldnames[i]] <- newnames[i]}
    
    # Rearrange to be ready for WheelofFortune Function
    AllCompanies <- subset(AllCompanies, select = c("Name","Classification","Technology","TechShare"))
    colnames(AllCompanies) <- c("item", "family", "score", "value") #item = component, family = portfolio, score  = technology, value = capacity mix
    
    # Bind the remaining Lines (IEAmix comes in each section)
    
    AllCompanies[AllCompanies$item == "NA"] <- "NoName"
    
    # AllCompanies <- subset(AllCompanies,!score %in% c("OilCap","Electric","Hybrid","ICE"))
    AllCompanies <- as.data.frame(sapply(AllCompanies, function(x) gsub("Cap", "", x)))
    
    
    if (SectorToPlot == "Power"){  
      Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      Targetmix <- subset(Targetmix, score  %in% c("RenewablesCap","HydroCap", "NuclearCap", "GasCap", "CoalCap"))
      Targetmix <- as.data.frame(sapply(Targetmix, function(x) gsub("Cap", "", x)))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("RenewablesCap","HydroCap", "NuclearCap", "GasCap", "CoalCap"))
      Indexmix <- as.data.frame(sapply(Indexmix, function(x) gsub("Cap", "", x)))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      AllCompanies <- subset(AllCompanies, AllCompanies$score != "Oil")
      
      if(BenchmarkRegionchoose=="OECD"){
        circleProportion = .94
        alphaStart = -1.0
        spaceFamily = 5
      } else {circleProportion = .94
      alphaStart = -1.3
      spaceFamily = 5
      }
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.2,
                            spaceFamily = spaceFamily, innerRadius = 0.3, outerRadius = 1, guides = c(0,25, 50, 75, 100), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)
      Plot <- Plot+
        scale_fill_manual(values = c("Renewables"= RenewablesColour, "Hydro" = HydroColour, "Nuclear" = NuclearColour, "Gas" = GasCapColour, "Coal" = CoalCapColour), labels = c("Renewables", "Hydro", "Nuclear","Gas", "Coal"), name = "score")
      
      
    }
    
    if (SectorToPlot == "Automotive"){
      Targetmix <- subset(Targetmix, score  %in% c("Electric","Hybrid","ICE"))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("Electric","Hybrid","ICE"))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      
      if(BenchmarkRegionchoose=="OECD"){
        circleProportion = .905
        alphaStart = -0.3
        spaceFamily = 6
      } else {
        if(BenchmarkRegionchoose=="EU"){circleProportion = .92
        alphaStart = -0.5
        spaceFamily = 2
        } else {
          if(BenchmarkRegionchoose=="JP"){circleProportion = .94
          circleProportion = 0.94
          alphaStart = -0.5
          spaceFamily = 2
          } else {
            circleProportion = .95
            alphaStart = -1.2
            spaceFamily = 2
          }
        }
      }
      
      
      
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,
                            spaceFamily = spaceFamily, innerRadius = 0.3, outerRadius = 1, guides = c(0,25, 50, 75, 100), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)
      
      Plot<- Plot+
        scale_fill_manual(values = c("Electric"= ElectricColour, "Hybrid" = HybridColour, "ICE" = ICEColour), labels = c("Electric", "Hybrid", "ICE"), name = "Technology")
      
      
    }
    
    Plot <- ggplot_gtable(ggplot_build(Plot))
    Plot$layout$clip[Plot$layout$name == "panel"] <- "off"
    
    # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
    
    png(paste0(plotnumber,"_",PortfolioName,'_',SectorToPlot,'_WheelofFortune.png'), height = 4000, width = 4000,res=ppi,bg="transparent") 
    grid.draw(Plot)
    dev.off()  
    
  }
  
  # return(png(paste(PortfolioName,"_",SectorToPlot,'_WheelofFortune.png'), height = 3.300, width = 3300,res=ppi,bg="transparent") )
  return()  
}

# ------------ COAL RETRIREMENTS CHART ------ #
coalretire_chart <- function(plotnumber,combin, PortSnapshot, Scenariochoose, MasterData, AllIEATargets,RegionCountries,PortfolioName){
  
  # Definition of Regions
  PowerRegionExclusion <- c("Global", "OECD", "NonOECD","EU", "OECDAmericas" , "LatinAmerica", "Africa", "EEurope_Eurasia", "NonOECDAsia", "OECDAsiaOceania", "NonOECDRest")
  
  IEATargetsCoal <- subset(AllIEATargets,Year %in% c(Startyear, Startyear+5, Startyear+10)& !BenchmarkRegion %in% PowerRegionExclusion & Scenario == Scenariochoose &Technology =="CoalCap", select = c("BenchmarkRegion","Year","FairSharePerc","AnnualvalIEAtech")) 
  
  IEATargetsCoalWide <- dcast(IEATargetsCoal, BenchmarkRegion~Year, value.var = "FairSharePerc")
  IEATargetsCoalWide$Direction[IEATargetsCoalWide[as.character(Startyear+10)]>0] <- "Increasing"
  IEATargetsCoalWide$Direction[IEATargetsCoalWide[as.character(Startyear+10)]<0] <- "Decreasing"
  IEATargetsCoalWide$Direction[IEATargetsCoalWide[as.character(Startyear+10)]<0 & IEATargetsCoalWide[as.character(Startyear+5)]>0] <- "Changing"
  IEATargetsCoal <- melt(IEATargetsCoalWide,  id.vars=c("BenchmarkRegion","Direction"))
  IEATargetsCoal[IEATargetsCoal$BenchmarkRegion %in% c("GlobalAggregate","OECDAggregate","NonOECDAggregate")] <- NULL
  IEATargetsCoal <- rename(IEATargetsCoal, c("variable" = "Year", "value" = "FairSharePerc"))
  
  regions <- data.frame(unique(IEATargetsCoal$BenchmarkRegion))
  
  Countries <- data.frame(BenchmarkRegion=character(),Country=character())
  for (j in 1:(nrow(regions))){
    countries <- data.frame(BenchmarkRegion=regions[j,],Country=unique(BenchmarkRegionList[[as.character(regions[j,])]]))
    Countries <- rbind(Countries,countries)
  }
  
  # Companies in Port
  PortCompanies <- unique(subset(PortSnapshot, select = c("EQY_FUND_TICKER","Name","piesector"), PortSnapshot$AUM >0))
  
  # Power Data
  PowerCapacity <- subset(MasterData,  Sector =="Power" & Year %in% c(Startyear, Startyear+5, Startyear+10) & EQY_FUND_TICKER != "NonListedProduction" & Technology != "OilCap")
  
  # Power by region, with regional targets, inport
  PowerCapacity <- merge(PowerCapacity, Countries, by.x ="PlantLocation", by.y="Country")
  PowerCapacity <- merge(PowerCapacity, IEATargetsCoal, by = c("BenchmarkRegion","Year"))
  PowerCapacity <- merge(PortCompanies, PowerCapacity, by = "EQY_FUND_TICKER")
  PowerTotals <- ddply(PowerCapacity, .(EQY_FUND_TICKER, Year, piesector,BenchmarkRegion), summarise,TotalPowerCap = sum(CompLvlProduction))
  PowerTotals <- subset(PowerTotals, Year ==Startyear)
  PowerTotals$Year<- NULL
  nonutilitypower <- subset(PowerTotals, PowerTotals$piesector %in% "NonUtility Power")
  nonutilitypowerTotal <- sum(nonutilitypower$TotalPowerCap)
  
  # Reduce to Important Data
  PowerCapacity <- subset(PowerCapacity, select = c("EQY_FUND_TICKER","Name","PlantLocation", "BenchmarkRegion", "Year","Technology","CompLvlProduction","Direction","FairSharePerc"))
  
  # Coal Data by Plant Location
  CoalCapacity <- subset(PowerCapacity,  Technology =="CoalCap" )  
  
  # Coal Data by BenchmarkRegion  
  CoalCapacity <- ddply(CoalCapacity, .(EQY_FUND_TICKER,Name,BenchmarkRegion,Year,Direction, FairSharePerc), summarise, RegionLvlProduction = sum(CompLvlProduction))
  
  # Add Total Power
  CoalCapacity <- merge(CoalCapacity, PowerTotals, by = c("EQY_FUND_TICKER","BenchmarkRegion"))
  # CoalCapacity <- dcast(CoalCapacity, EQY_FUND_TICKER+Name+BenchmarkRegion+TotalPowerCap ~ Year, value.var=c("FairSharePerc"),fun.aggregate = sum)
  
  # Calculate Targets
  increasing <- subset(CoalCapacity, Direction == "Increasing")
  increasing$Target <- increasing$TotalPowerCap * (1+increasing$FairSharePerc)
  decreasing <- subset(CoalCapacity, Direction == "Decreasing")
  decreasing$Target<- decreasing$RegionLvlProduction * (1+decreasing$FairSharePerc)
  changing <- subset(CoalCapacity, Direction == "Changing")
  changing$Target <- changing$RegionLvlProduction * (1+changing$FairSharePerc)
  
  CoalCapacity<- rbind(increasing,decreasing,changing)
  
  CoalRetValues <- c(4.8,8,3.75)
  if (sum(CoalCapacity$RegionLvlProduction)>0){ 
    
    CoalProduction <-dcast(CoalCapacity, EQY_FUND_TICKER+Name+piesector+BenchmarkRegion+Direction+TotalPowerCap ~ Year, value.var=c("RegionLvlProduction"),fun.aggregate = sum)
    names(CoalProduction)[names(CoalProduction)==as.character(Startyear)] <- "Prod0yrs"
    names(CoalProduction)[names(CoalProduction)==as.character(Startyear+5)] <- "Prod5yrs"
    names(CoalProduction)[names(CoalProduction)==as.character(Startyear+10)] <- "Prod10yrs"   
    
    CoalProduction$Additions <- CoalProduction$Prod10yrs-CoalProduction$Prod0yrs
    
    CoalTargets <- dcast(CoalCapacity, EQY_FUND_TICKER+Name+BenchmarkRegion ~ Year, value.var=c("Target"),fun.aggregate = sum)
    names(CoalTargets)[names(CoalTargets)==as.character(Startyear+5)] <- "Target5yrs"
    names(CoalTargets)[names(CoalTargets)==as.character(Startyear+10)] <- "Target10yrs"  
    CoalTargets[as.character(Startyear)]<- NULL
    
    CoalAll <- merge(CoalProduction, CoalTargets, by = c("EQY_FUND_TICKER","Name","BenchmarkRegion"))
    
    CoalAllDecr <- subset(CoalAll, Direction =="Decreasing")
    CoalAllDecr$RequiredRetirements5yr <- CoalAllDecr$Prod0yrs-CoalAllDecr$Target5yrs
    CoalAllDecr$RequiredRetirements10yr <- CoalAllDecr$Prod0yrs-CoalAllDecr$Target10yrs - CoalAllDecr$RequiredRetirements5yr
    CoalAllDecr$ExcessiveAdditions <- CoalAllDecr$Prod10yrs-CoalAllDecr$Prod0yrs 
    
    
    CoalAllIncr <- subset(CoalAll, Direction =="Increasing")
    if (nrow(CoalAllIncr)>0){
      CoalAllIncr[,c("RequiredRetirements5yr","RequiredRetirements10yr")] <- 0
      CoalAllIncr$ExcessiveAdditions <- CoalAllIncr$Prod10yrs-CoalAllIncr$Target10yrs}
    
    CoalAllChang <- subset(CoalAll, Direction == "Changing")
    if (nrow(CoalAllChang)>0){
      CoalAllChang[,c("RequiredRetirements5yr")] <- 0
      CoalAllChang$RequiredRetirements10yr <- CoalAllChang$Prod0yrs-CoalAllChang$Target10yrs -  CoalAllChang$RequiredRetirements5yr
      CoalAllChang$ExcessiveAdditions <- CoalAllChang$Prod10yrs-CoalAllChang$Prod0yrs}
    
    CoalFinal <- rbind(CoalAllDecr,CoalAllIncr,CoalAllChang)
    CoalFinal$ExcessiveAdditions[CoalFinal$ExcessiveAdditions<0] <- 0
    CoalFinal$RequiredRetirements5yr[CoalFinal$RequiredRetirements5yr<0] <- 0
    CoalFinal$RequiredRetirements10yr[CoalFinal$RequiredRetirements10yr<0] <- 0
    
    CoalReduced <- ddply(CoalFinal, .(EQY_FUND_TICKER, Name,piesector),summarize, RequiredRetirements5yr =sum(RequiredRetirements5yr), RequiredRetirements10yr =sum(RequiredRetirements10yr), ExcessiveAdditions =sum(ExcessiveAdditions), ProdStart =sum(Prod0yrs),Prod10yrs =sum(Prod10yrs), TotalPowerCap = sum(TotalPowerCap))
    
    # Non-utility 
    nonutilitycompanies <- subset(CoalReduced, CoalReduced$piesector %in% "NonUtility Power")
    if (nrow(nonutilitycompanies)>0){
      nonutilitycompanies$Name <- "All Non-Utilities"
      nonutilitycompanies$piesector <- "Utility Power"
      nonutilitycompanies$EQY_FUND_TICKER <- "ANU"
      
      nonutilitycompany <- ddply(nonutilitycompanies, .(EQY_FUND_TICKER,Name,piesector), summarise, RequiredRetirements5yr =sum(RequiredRetirements5yr), RequiredRetirements10yr =sum(RequiredRetirements10yr), ExcessiveAdditions =sum(ExcessiveAdditions), ProdStart =sum(ProdStart),Prod10yrs =sum(Prod10yrs), TotalPowerCap = sum(TotalPowerCap))
      
      CoalRetirements <- rbind(CoalReduced,nonutilitycompany)
    }else{CoalRetirements <- CoalReduced}
    
    CoalRetirements <- subset(CoalRetirements, ProdStart > 0)
    
    CoalRetirements$RetTot <- CoalRetirements$RequiredRetirements5yr+ CoalRetirements$RequiredRetirements10yr+CoalRetirements$ExcessiveAdditions
    CoalRetirements$PctRetire <- CoalRetirements$RetTot/CoalRetirements$TotalPowerCap
    CoalRetirements <- replace(CoalRetirements, is.na(CoalRetirements), 0)  
    
    CoalRetirements <- subset(CoalRetirements, CoalRetirements$piesector %in% c("Utility Power"))
    
    CoalRetirementChart <- subset(CoalRetirements, CoalRetirements$RetTot >0,select = c("Name","piesector","RetTot","RequiredRetirements10yr","RequiredRetirements5yr","ExcessiveAdditions",   "PctRetire" ) )
    CoalRetirementChart <- melt(CoalRetirementChart, id.var = c("Name","piesector","RetTot","PctRetire"))
    
    if (nrow(CoalRetirementChart)>0){
      #CoalRetLongUnits is just scaled for GW vs MW
      # Define the maximum value here - check names
      if(max(CoalRetirementChart$RetTot) > 1000) {
        CoalRetirementChartUnits<-CoalRetirementChart
        CoalRetirementChartUnits$RetTot<-CoalRetirementChartUnits$RetTot/1000
        CoalRetirementChartUnits$value<-CoalRetirementChartUnits$value/1000
        # CoalRetirementChartUnits$Additions<-CoalRetirementChartUnits$Additions/1000
        
        CoalRetlabel = "(GW)"
        CoalRetmaxxAxis = max(ceiling(CoalRetirementChartUnits$RetTot))
        
      } else {
        CoalRetirementChartUnits <- CoalRetirementChart
        CoalRetmaxxAxis  <- 100*ceiling(max(CoalRetirementChartUnits$value/100))+200
        CoalRetlabel = "(MW)"
      }
      
      CoalRetirementChartUnits <- CoalRetirementChartUnits[order(CoalRetirementChartUnits$RetTot),]
      nocompanies <- nrow(CoalRetirementChartUnits)/3
      companiestokeep <- 20
      if (nocompanies > companiestokeep){
        nocompanies<-nocompanies*3
        companiestokeep<-companiestokeep*3
        CoalRetirementChartShort <- CoalRetirementChartUnits[((nocompanies-companiestokeep)+1):nocompanies,]
      }else{
        CoalRetirementChartShort<- CoalRetirementChartUnits
      }
      
      CoalRetLabels<- unique(subset(CoalRetirementChartShort, select = c("Name", "RetTot")))
      CoalRetirementChartShort$Name <- factor(CoalRetirementChartShort$Name, levels = unique(CoalRetirementChartShort$Name))
      CoalRetirementChartPerc <- unique(subset(CoalRetirementChartShort, select = c("Name","PctRetire")))
      
      
      
      CoalRetBarChartPlot <- ggplot(CoalRetirementChartShort, aes(x= Name ,y=value, fill = variable))+
        geom_bar(stat = "identity", width=.8, colour = NA) +
        scale_fill_manual(guide=FALSE,values = c("ExcessiveAdditions" = "#170203", "RequiredRetirements10yr" = "#ed1c24","RequiredRetirements5yr" = "#760e12"), labels = c("ExcessiveAdditions",Startyear+5,Startyear+10), name = "Year")+
        scale_y_continuous(expand=c(0,0),limits=c(0, CoalRetmaxxAxis*1.35), breaks = seq(0,CoalRetmaxxAxis,CoalRetmaxxAxis * 0.1), minor_breaks = seq(0,CoalRetmaxxAxis,CoalRetmaxxAxis * 0.05))+
        scale_x_discrete(expand=c(0,0)) +
        xlab("") +
        ylab(paste("Required coal retirements per company", CoalRetlabel, sep = " "))+ 
        annotate(hjust = 0.8,"text", x = c(1:(length(CoalRetLabels$Name))) , y = CoalRetLabels$RetTot+(CoalRetmaxxAxis/CoalRetValues[3]) , label = paste(round(CoalRetirementChartPerc$PctRetire*100,0), "% of total", Startyear, "capacity",sep = " "), colour = AxisColour,size = 3)+
        theme(  axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
                axis.title.y=element_blank(),
                axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
                axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
                axis.line = element_line(colour = AxisColour,size=1),
                panel.grid.major.x = element_line(colour = "grey90", linetype = "solid"),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_line(colour = "grey80", linetype = "dotted"),
                legend.position=c(0.5,-.28),
                legend.direction="horizontal",
                legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
                legend.background = element_rect(fill = "transparent",colour = NA),
                legend.key.size=unit(0.4,"cm"),
                legend.title=element_blank(),
                legend.key = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA),
                plot.margin = unit(c(.4,1.5, 0, 0), "lines"))+
        coord_flip()
      
      # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
      ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_CoalRetirements.png"),bg="transparent",height=CoalRetValues[1],width=CoalRetValues[2],plot=CoalRetBarChartPlot,dpi=ppi)
    }
  }else{
    CoalRetBarChartPlot <- ggplot() + annotate("text", label = "No Coal Retirements",x = 0, y = 0, size = 4)
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_CoalRetirements.png"),bg="transparent",height=CoalRetValues[1],width=CoalRetValues[2],plot=CoalRetBarChartPlot,dpi=ppi)
  }
  return()
}

# ------------ COLUMN OF FORTUNE CHART ------ #
autocolumnoffortune_chart <- function(plotnumber,PortSnapshot, combin, AutoCompanies, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, IndexData, IndexName,PortfolioName){
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  plotlimit <- 0.2    # The y max for this graph (can be increased in future when companies are greener!)
  
  AllCompanies <- AutoCompanies
  
  CompaniesInPort <- subset(PortSnapshot, select = c("EQY_FUND_TICKER"), AUM>0)
  CompaniesInPort <- unique(CompaniesInPort)
  
  # AllCompanies <- ddply(AllCompanies, .(Technology, EQY_FUND_TICKER,Country, Name), summarise, Production =sum(Production,na.rm = TRUE))
  colnames(AllCompanies)[colnames(AllCompanies)=="Production"] <- "Capacity"
  AllCompanies$Capacity[is.na(AllCompanies$Capacity)] <-0
  
  # Classify the Companies
  AllCompanies$Classification <- "AllCompanies"
  AllCompanies$Classification[AllCompanies$EQY_FUND_TICKER %in% CompaniesInPort$EQY_FUND_TICKER] <- "PortCompanies"
  
  # Portfolio Average
  Portfoliomix <- ddply(AllCompanies, .(Technology, Classification), summarize, Capacity = sum(Capacity))
  Portfoliomix <- subset(Portfoliomix, Portfoliomix$Classification == "PortCompanies")
  if(dim(Portfoliomix)[1] != 0){
    Portfoliomix$Classification <- "Portfolio"
    Portfoliomix$Name <- PortfolioName
    Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
    colnames(Portfoliomix) <- c("item", "family", "score", "value")
    Portfoliomix$value <- as.numeric(Portfoliomix$value)
    Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))
  }
  
  #Targets
  
  Targetmix <- subset(combin, Sector == "Automotive" & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5, select = c("Technology", "TargetProductionAlignment"))
  Targetmix$Classification<-"Portfolio"
  Targetmix$Name<-GT["X2Target"][[1]]
  Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
  Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
  colnames(Targetmix) <- c("item", "family", "score", "value")
  Targetmix$value <- as.numeric(as.character(Targetmix$value))
  
  # Add Index
  Indexmix <- ddply(IndexData, .(CompanyDomicileRegion,Technology), summarize, Capacity = sum(Production))
  Indexmix$Classification <- "Portfolio"
  Indexmix <- subset(Indexmix, select =c("CompanyDomicileRegion","Classification","Technology","Capacity"))
  colnames(Indexmix) <- c("item", "family", "score", "value")  
  Indexmix$value <- as.numeric(as.character(Indexmix$value))
  Indexmix$item <- IndexName
  
  # Percentage share of each technology  
  CompanyTotal <- ddply(AllCompanies, .(EQY_FUND_TICKER,Name), summarise, CompanyTotalCapacity=sum(Capacity))
  AllCompanies <- merge(AllCompanies,CompanyTotal)
  AllCompanies$TechShare <- (AllCompanies$Capacity/AllCompanies$CompanyTotalCapacity)
  
  AllCompanies <- subset(AllCompanies, Name != "NA")
  
  # Clean Company Names
  AllCompanies$Name <- str_replace_all(AllCompanies$Name, "LIMITED", "LTD.")
  AllCompanies$Name <- str_replace_all(AllCompanies$Name, "COMPANY", "CO.")
  AllCompanies$Name <- str_replace_all(AllCompanies$Name, "CORPORATION", "CORP.")
  AllCompanies$Name <- str_replace_all(AllCompanies$Name, ",", "")
  AllCompanies$Name<-strtrim(AllCompanies$Name, 16)
  i=0
  for (i in 1:length(AllCompanies$Name)) {
    if(nchar(AllCompanies[i,2], type = "chars")==16){AllCompanies[i,2]<-paste(AllCompanies[i,2],"...", sep = "")}
  }
  
  oldnames <- c("TESLA INC","VOLVO AB-B SHS","BAYERISCHE MOTOREN WERKE AG","FIAT CHRYSLER AUTOMOBILES NV","FUJI HEAVY INDUSTRIES LTD","HONDA MOTOR CO LTD","MITSUBISHI MOTORS CORP","BRILLIANCE CHINA AUTOMOTIVE")
  newnames <- c("TESLA","VOLVO","BMW AG","FIAT CHRYSLER NV","FUJI HEAVY IND LTD","HONDA MOTOR CO","MITSUBISHI MOTORS","BRILLIANCE CN AUTO")
  for (i in c(1:length(oldnames))){AllCompanies$Name[AllCompanies$Name %in% oldnames[i]] <- newnames[i]}
  
  # Rearrange to be ready for Chart Function
  AllCompanies <- subset(AllCompanies, select = c("Name","Classification","Technology","TechShare"))
  colnames(AllCompanies) <- c("item", "family", "score", "value") 
  
  AllCompanies[AllCompanies$item == "NA"] <- "NoName"
  
  AllCompanies <- as.data.frame(sapply(AllCompanies, function(x) gsub("Cap", "", x)))
  
  # padlist <- data.frame("item" = " ", "family" = "Portfolio" , "score" = "Hybrid", "value" = 0)
  # AllCompanies<-rbind(AllCompanies,padlist)
  
  Targetmix <- subset(Targetmix, score  %in% c("Electric","Hybrid","ICE"))
  Targetmix$value <- as.numeric(as.character(Targetmix$value))
  Targetmix$value <- (Targetmix$value/sum(Targetmix$value))
  
  Indexmix <- subset(Indexmix, score  %in% c("Electric","Hybrid","ICE"))
  Indexmix$value <- as.numeric(as.character(Indexmix$value))
  Indexmix$value <- (Indexmix$value/sum(Indexmix$value))
  
  # padlist <- data.frame("item" = " ", "family" = "Portfolio" , "total"=0,"score" = "Hybrid", "value" = 0)
  # AllCompanies<-rbind(AllCompanies,padlist)
  
  AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
  AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
  
  AllCompanies <- subset(AllCompanies, score != "ICE")
  
  AllCompaniesTotals <- ddply(AllCompanies, .(item, family),summarise, total =sum(value))
  # AllCompanies <- merge(AllCompanies, AllCompaniesTotals, by = c("item","family"))
  
  AllCompaniesWide <- dcast(AllCompanies, item+family~score, value.var="value")
  AllCompaniesWide <- merge(AllCompaniesWide, AllCompaniesTotals, by = c("item","family"))
  AllCompaniesWide$Hybrid[which(AllCompaniesWide$total > plotlimit & AllCompaniesWide$Electric <plotlimit & AllCompaniesWide$Hybrid >0)]<- plotlimit - AllCompaniesWide$Electric[which(AllCompaniesWide$total > plotlimit & AllCompaniesWide$Electric <plotlimit & AllCompaniesWide$Hybrid >0)]
  
  GreenCompanies <- subset(AllCompaniesWide, total > plotlimit)
  GreenCompanies$MaxTech <- "Electric"
  GreenCompanies$MaxTech[which(GreenCompanies$Electric < plotlimit | is.na(GreenCompanies$Electric))] <- "Hybrid"
  
  AllCompanies <- melt(AllCompaniesWide, id = c("item","family","total"), na.rm = TRUE)
  colnames(AllCompanies) <- c("item", "family", "total","score", "value") 
  
  padlist <- data.frame("item" = " ", "family" = "blank" , "total"=0,"score" = "Hybrid", "value" = 0)
  AllCompanies<-rbind(AllCompanies,padlist)
  
  # GreenCompanies <- AllCompanies[AllCompanies$item %in% GreenCompanies$item,]
  # GreenCompaniesWide <- dcast(GreenCompanies,item+family~score, value.var="value")
  # hybrids <- GreenCompaniesWide[(GreenCompaniesWide$Hybrid>=plotlimit & !is.na(GreenCompaniesWide$Hybrid)),]
  # GreenCompanies$MaxTech <- "Electric"
  # GreenCompanies$MaxTech[GreenCompanies$item %in% hybrids$item] <- "Hybrid" 
  # GreenCompaniesWide$MaxTech <- "Electric"
  # GreenCompaniesWide$MaxTech[GreenCompaniesWide$item %in% hybrids$item] <- "Hybrid" 
  # dcast(IEATargetsCoal, BenchmarkRegion~Year, value.var = "FairSharePerc")
  
  AllCompanies$value[which(AllCompanies$value >plotlimit)]<-plotlimit
  
  
  familylist<-c("AllCompanies","PortCompanies","blank","Portfolio")
  familyOrder<-c(1,2,3,4)
  scorelist <- c("Hybrid","Electric")
  scoreOrder<-c(1,2)
  tempdb<-data.frame(cbind(familylist,familyOrder))
  tempdb2 <-data.frame(cbind(scorelist,scoreOrder))
  names(tempdb)[1]<-"family"
  names(tempdb2)[1]<-"score"
  AutoChartResults <- merge(tempdb,AllCompanies,by="family",all.x=TRUE,all.y=FALSE)
  AutoChartResults <- merge(tempdb2,AutoChartResults,by="score",all.x=TRUE,all.y=FALSE)
  AutoChartResults <- subset(AutoChartResults,!is.na(AutoChartResults$item)) #need to remove NA's if there are no current auto components in the portfolio
  AutoChartResults <- AutoChartResults[with(AutoChartResults, order(scoreOrder, score)),]
  AutoChartResults <- AutoChartResults[with(AutoChartResults, order(familyOrder, item)),]
  
  AutoChartResults$familyOrder <- NULL
  AutoChartResults$scoreOrder <- NULL
  
  
  # AutoChartResults$item <- factor(AutoChartResults$item, levels = rev(AutoChartResults$item))  #tell ggplot that there are already in a specifc order
  Labeldf <- unique(subset(AutoChartResults,select=c("item", "family")))
  Labeldf$x<-(seq(from = 1, to = length(Labeldf$item), by = 1))
  Labeldf$x<-rev(sort(Labeldf$x))
  
  AutoChartResults <- merge(Labeldf,AutoChartResults,by=c("item","family"),all.x=TRUE,all.y=FALSE)
  AutoChartResults <- arrange(AutoChartResults, x)
  
  AutoChartResults$item <- factor(AutoChartResults$item, levels = unique(AutoChartResults$item))
  AutoChartResults$score <- factor(AutoChartResults$score, levels = unique(AutoChartResults$score))
  
  
  if (nrow(GreenCompanies) >1){
    graphwidth <- 0.211
  }else{
    graphwidth <- 0.2045
  }
  
  barwidth =0.8
  arrowstart <- 0.008#(1.4*barwidth/2e2)
  nocompanies <- length(unique(AutoChartResults$item))
  arrowwidth <- ((8-0.33)/nocompanies)*.75
  textpos <- barwidth/2 +0.3
  # (Width of Graph, Start of Arrow, Width of Arrow, Textposition)
  
  ylabel <- GT["Auto_ylabel"][[1]]
  
  if(nrow(GreenCompanies)>0){
    tempdb <- data.frame("item" = GreenCompanies$item)
    tempdb <- merge(tempdb,AutoChartResults,by="item",all.x=TRUE,all.y=FALSE)
    tempdb <- subset(tempdb, select = c("item","family","x"))
    tempdb <- unique(tempdb)
  }
  
  AutoBarChartPlot<-ggplot(AutoChartResults, aes(x=item,y=value,fill=score))+
    geom_bar(stat = "identity", width=barwidth, colour = NA)+
    scale_fill_manual(guide=FALSE,values =c("Electric" = ElectricColour, "Hybrid"= HybridColour), labels = c("Electric","Hybrid"), name = "Technology")+
    scale_y_continuous(label = percent, limits=c(-.01, graphwidth), breaks = seq(0,plotlimit,plotlimit/10), minor_breaks = seq(.01, .2, .01))+
    scale_x_discrete(expand=c(0,0))+
    xlab("")+
    ylab(ylabel)+
    geom_segment(aes(y = 0, yend = plotlimit, x = 0, xend = 0), colour = AxisColour, size =1)+
    #geom_segment(aes(y = 0, yend = plotlimit, x =length(Labeldf$item)+0.4, xend = length(Labeldf$item)+0.4), colour = AxisColour, size =1)+
    geom_hline(yintercept = 0, show.legend = FALSE, colour = AxisColour, size  =1)+
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
          axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
          panel.border = element_rect(fill = NA, colour = NA),
          panel.grid.major.x = element_blank(),#element_line(colour = "grey90", linetype = "solid"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),#element_line(colour = "grey80", linetype = "dotted"),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.background = element_rect(fill = 'transparent', colour='transparent'),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1, 1, 0, 2), "lines"),
          legend.key = element_rect(colour = 'grey90', fill = 'transparent'))+coord_flip()
  
  
  if (nrow(GreenCompanies)>0){
    for (counter in 1:nrow(GreenCompanies)){
      labelling <-  paste0(GreenCompanies[counter,"item"]," ",round(GreenCompanies[counter,"total"]*100,0),"%")
      location <- tempdb[counter,"x"]
      
      if (GreenCompanies[counter,"MaxTech"] == "Electric"){maxcolor <- ElectricColour}else{maxcolor <- HybridColour}
      
      AutoBarChartPlot<- AutoBarChartPlot+
        annotate("text", x = location, y = plotlimit, label = labelling, colour = "black", fontface="plain", size = 2.5, hjust=1)+
        geom_segment(x = location, y = plotlimit, xend = location, yend = plotlimit+(arrowwidth*1.41/100), colour = maxcolor, arrow = arrow(length = unit(arrowwidth, "cm"), type = "closed"))
    }}
  
  AutoBarChartPlot <- AutoBarChartPlot+coord_flip()
  
  AutoBarChartPlot<-AutoBarChartPlot + 
    geom_text(aes(x = x, label = item, angle = 0,colour = family, hjust= 1, fontface=ifelse(family=="PortCompanies"|family=="Portfolio","bold","plain")), y = -.0005, size = 2.5, vjust = 0.5, data = AutoChartResults) +
    scale_colour_manual(guide=FALSE,values = c("AllCompanies" = "grey50","Portfolio" = "black", "PortCompanies"= AxisColour,"blank" = "black"))
  
  gt <- ggplot_gtable(ggplot_build(AutoBarChartPlot))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  
  png(filename=paste0(plotnumber,"_",PortfolioName,'_ColumnofFortuneAuto.png'),height = 3.5*ppi,  width = 6.7*ppi,res=ppi,bg="transparent") 
  grid.draw(gt)
  dev.off()
  
}

# ------------ HEAT MAP --------------------- # 
heatmap_chart <- function(plotnumber,combin, Startyear, Scenariochoose, PortfolioName){
  BenchYear <- Startyear + 5
  AxisColour = 'Black'
  textcolour = 'Black'
  geom.text.size = 2
  
  BrownList <-c("Coal\nCapacity", "Gas\nCapacity", "Oil\nCapacity", "Gas\nProduction", "Oil\nProduction", "Coal\nProduction", "ICE\nVehicles")
  GreenList <-c("Nuclear\nCapacity", "Hydro\nCapacity", "Renewable\nCapacity", "Electric\nVehicles","Hybrid\nVehicles")
  
  # Read Results
  #  Results <- read.csv(paste(OutputLocation,PortfolioHoldings,"/",ResultsDate,"_",PortfolioHoldings,"_EquityAnalysisResults-450S-only.csv",sep = ""),stringsAsFactors = FALSE, strip.white = TRUE)
  Results <- combin
  
  
  # Remove shitty name
  Results$PortName <- gsub("_.*","",Results$PortName)
  
  ################## Heatmap ########################
  
  #Select subset of results: Year, Scenario, Where the companies are located/the investment universe, and just funds, not brands. 
  Heatmap <- subset(Results, Results$Year == BenchYear & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & (Type == "Fund" | PortName == PortfolioName),select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
  # # Rename the Brand FTSE to the fund FTSE350
  # Heatmap$PortName[Heatmap$PortName == "FTSE"] <- "FTSE350"
  
  # Use AUM Exposure method for fossel fuels
  Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
  # After getting the AUM values, remove that vector from the dataframe
  Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
  Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
  # Rename the technologies to be more reader friendly
  Heatmap$Technology <- revalue(Heatmap$Technology, c("Gas" = "Gas\nProduction","Oil" = "Oil\nProduction", "Coal" = "Coal\nProduction", "Electric" = "Electric\nVehicles", "Hybrid" = "Hybrid\nVehicles", "ICE" = "ICE\nVehicles", "RenewablesCap" = "Renewable\nCapacity", "NuclearCap" = "Nuclear\nCapacity", "HydroCap" = "Hydro\nCapacity", "GasCap" = "Gas\nCapacity", "CoalCap" = "Coal\nCapacity", "OilCap" = "Oil\nCapacity"))
  Heatmap$MarketExposure <- Heatmap$MarketExposure*100
  
  #### Select regions #####
  BenchmarkRegionList <-c("GlobalAggregate","OECD", "OECDAmericas", "OECDEurope", "OECDAsiaOceania", "NonOECD", "NonOECDAsia","LatinAmerica")
  Heatmap <- subset(Heatmap, Heatmap$BenchmarkRegion %in% BenchmarkRegionList)
  
  #fill in values if some regions or technologies are not within the portfolio
  Heatmap <- Heatmap %>% complete(PortName, Technology, BenchmarkRegion) # Keeps N/As
  
  #Create colour bands/buckets
  #alligned
  Heatmap$ExposureColour[!is.na(Heatmap$MarketExposure)] <- 'grey95'
  
  # 'good' alignment
  Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 5 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -5] <- "#d2e7d2"
  Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 15 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -15] <- "#a5cfa5"
  Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 25 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -25] <- "#78b878"
  Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 50 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -50] <- "#4ba04b"
  Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$MarketExposure> 75 | Heatmap$Technology %in% BrownList & Heatmap$MarketExposure< -75] <- "#1f891f"
  
  # 'bad' alignment
  Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 5 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -5 ] <- "#fad7d3"
  Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 15 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -15 ] <- "#f5afa8"
  Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 25 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -25 ] <- "#f0877d"
  Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 50 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -50 ] <- "#eb5f52"
  Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$MarketExposure> 75 | Heatmap$Technology %in% GreenList & Heatmap$MarketExposure< -75 ] <- "#e73827"
  
  #add '%' text
  Heatmap$ExposureText <-Heatmap$MarketExposure# remove text for auto sector
  Heatmap$ExposureText[!is.na(Heatmap$ExposureText)]<-paste(round(Heatmap$ExposureText[!is.na(Heatmap$ExposureText)], 1), "%", sep="")
  
  # Order Technologies
  Technology<-c("Electric\nVehicles", "Hybrid\nVehicles", "ICE\nVehicles", "Gas\nProduction", "Oil\nProduction", "Coal\nProduction","Renewable\nCapacity","Hydro\nCapacity", "Nuclear\nCapacity", "Oil\nCapacity", "Gas\nCapacity", "Coal\nCapacity")
  tempdb<-data.frame(cbind(Technology, XPosition = (seq(from = 1, to = length(Technology), by = 1))))
  Heatmap <- merge(tempdb,Heatmap,by=c("Technology"),all.x=TRUE,all.y=FALSE)
  Heatmap$XPosition <- as.numeric(as.character(Heatmap$XPosition))
  
  # Order Regions
  tempdb<-cbind(BenchmarkRegion = BenchmarkRegionList, YPosition = 1:length(BenchmarkRegionList))
  Heatmap <- merge(tempdb, Heatmap, by="BenchmarkRegion", all.x=TRUE, all.y=FALSE)
  Heatmap <- arrange(Heatmap, YPosition, XPosition)
  Heatmap$YPosition <- rev(as.numeric(as.character(Heatmap$YPosition)))
  rm(tempdb)
  
  #set region and technolgies as factor
  Heatmap$BenchmarkRegion <- factor(Heatmap$BenchmarkRegion, levels= rev(levels(unique(Heatmap$BenchmarkRegion)))) #Reverse order
  Heatmap$Technology <- factor(Heatmap$Technology, levels=unique(Heatmap$Technology)) #current order
  
  #####Select individual portfolio###
  # PortSelected = "BNPEquity"
  # HeatmapData <- subset(Heatmap, Heatmap$PortName == PortSelected)
  HeatmapData <- Heatmap
  
  TechnologyLabel<- unique(subset(HeatmapData, select = c("Technology", "XPosition")))
  RegionLabel <- unique(subset(HeatmapData, select = c("BenchmarkRegion", "YPosition")))
  
  HeatmapGGPlot <- ggplot(HeatmapData, aes(x = as.factor(HeatmapData$Technology),fill = as.factor(HeatmapData$ExposureColour), y = as.factor(HeatmapData$BenchmarkRegion), group=HeatmapData$BenchmarkRegion)) +
    geom_tile(colour = "grey95") +
    geom_text(aes(label = HeatmapData$ExposureText),colour=AxisColour, size = geom.text.size, data = data.frame()) + # text for % values
    annotate("text", x = 1.75, y = 4, label = "No regional benchmark\nfor automotive sector", angle = 0, hjust= 0.5, vjust = 0.5, size = geom.text.size, fontface= "bold")+ # label for missing targets
    annotate("text", x = TechnologyLabel$XPosition, y = 8.75,  label=TechnologyLabel$Technology, angle = 0, hjust= 0.5, vjust = 0.5, size = geom.text.size)+ # text for technology axis
    annotate("text", y = RegionLabel$YPosition, label = RegionLabel$BenchmarkRegion, angle = 0, x = 0.35, hjust = 1, fontface= ifelse(RegionLabel$YPosition == 8,"bold","plain"), size = geom.text.size)+ # text for regional axis
    annotate("text", x = 1.5, y = 9.15, label = "Automotive", angle = 0, hjust= 0, vjust = 0.5,fontface= "bold", size = geom.text.size)+ # label for technology axis
    annotate("text", x = 4.55, y = 9.15, label = "Fossil Fuels", angle = 0, hjust= 0, vjust = 0.5,fontface= "bold" ,size = geom.text.size)+ # label for technology axis
    annotate("text", x = 9.25, y = 9.15, label = "Power", angle = 0, hjust= 0, vjust = 0.5, fontface= "bold",size = geom.text.size)+ # label for technology axis
    
    scale_fill_identity()+
    scale_x_discrete("", expand = c(0, 0)) +
    scale_y_discrete("", expand = c(0, 0)) +
    
    annotate("segment", x = -.25, xend = 13, y = 8.5, yend = 8.5, colour = AxisColour, linetype = 'dotted', size = 0.3) + # horizontal lines
    annotate("segment", x = -.25, xend = 13, y = 7.5, yend = 7.5, colour = AxisColour, linetype = 'dotted', size = 0.3) + # horizontal lines
    annotate("segment", x =3.5, xend = 3.5, y = 0.5, yend = 9, colour = AxisColour, linetype = 'solid', size = 0.3) + # verticle lines
    annotate("segment", x = 6.5, xend = 6.5, y = 0.5, yend = 9, colour = AxisColour, linetype = 'solid', size = 0.3) + # verticle lines
    theme(axis.ticks = element_line(colour= "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_text(face="bold",colour=textcolour, size=6),
          plot.background = element_rect(fill = "transparent",colour = NA),
          plot.margin = unit(c(1.5, 0, 0, 2), "lines"),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          legend.title=element_text(size=6),
          legend.text=element_text(size=6),
          legend.position="bottom",
          legend.key.size=unit(0.2, "cm"),
          legend.key.width=unit(1, "cm"))+
    labs(x=NULL, y=NULL, title="Physical asset alignment with IEA 2°C Scenarios (by 2021)")+
    coord_equal()
  
  HeatmapPlot <- ggplot_gtable(ggplot_build(HeatmapGGPlot))
  HeatmapPlot$layout$clip[HeatmapPlot$layout$name == "panel"] <- "off"
  grid.draw(HeatmapPlot)
  
  # # Print as pdf
  # pdf(paste(PortfolioName,"HeatMap_OriginalColours.pdf", sep =""), paper="a4")
  # grid.draw(HeatmapPlot)
  # dev.off()  
  
  # Print as png
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  ppi = 600
  png(paste0(plotnumber,"_",PortfolioName,"_HeatMap.png"), height = 3.5*ppi, width = 6.7*ppi,res=ppi,bg="transparent")
  grid.draw(HeatmapPlot)
  dev.off()
  
}

# ------------- RANKING CHART - TECH WEIGHT ----------------#
ranking_chart <- function(plotnumber,Startyear,SectorToPlot, CompanyRanks, CoverageWeights,IEATargetsAll,figuredirectory,PortfolioNameLong){
  # Plotting Exposure
  sectors <- data.frame(Sector = c("Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels","Power","Power","Power","Power","Power"),Technology = c("ICE","Electric","Hybrid","Oil","Gas","Coal","CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"), order =1:11)
  TechnologyNames<-c("Electric\nVehicles", "Hybrid\nVehicles", "ICE\nVehicles", "Gas\nProduction", "Oil\nProduction", "Coal\nProduction","Renewable\nCapacity","Hydro\nCapacity", "Nuclear\nCapacity",  "Gas\nCapacity", "Coal\nCapacity")
  Technology<-c("Electric", "Hybrid", "ICE","Gas","Oil", "Coal","RenewablesCap", "HydroCap", "NuclearCap", "GasCap", "CoalCap")
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  
  #DELETE AFTERWARDS
  CoverageWeights <- EQCoverageWeights
  
  CoverageWeights <- merge(CoverageWeights,sectors, by= "Technology")
  
  sectortotals <- ddply(CoverageWeights,.(PortName,Sector), summarise, sectortot = sum(CoverageWeight))
  CoverageWeightsTot <- merge(CoverageWeights, sectortotals, by = c("Sector","PortName"))
  CoverageWeightsTot$Exposure <- CoverageWeightsTot$CoverageWeight/CoverageWeightsTot$sectortot
  
  CoverageWeightsTot[is.na(CoverageWeightsTot) ]<- 0
  
  CoverageWeightsWide <- dcast(CoverageWeightsTot, PortName~Technology, value.var = "Exposure")
  
  # CoverageWeightsWide$family <- "ComparisonCompanies"
  # CoverageWeightsWide$family[CoverageWeightsWide$PortName %in% "WeightedResults"] <- "Stats"
  # CoverageWeightsWide$family[CoverageWeightsWide$PortName %in% PortfolioNameLong] <- "Portfolio"
  
  Mins <- colMins(as.matrix(CoverageWeightsWide[2:12]))
  Maxs <- colMaxs(as.matrix(CoverageWeightsWide[2:12]))
  MinMax <- as.data.frame(t(cbind(Mins,Maxs)))
  colnames(MinMax) <- colnames(CoverageWeightsWide[2:12])
  MinMax$PortName <- c("Minimum","Maximum")
  # MinMax$family <- "Stats"
  CoverageWeightsWide <- rbind(CoverageWeightsWide,MinMax)
  CoverageWeightsWide <- CoverageWeightsWide[CoverageWeightsWide$PortName %in% c(PortfolioNameLong,"Minimum","Maximum"),]
  
  # Calculating the IEA Target Exposure
  IEATargets <- subset(IEATargetsAll, IEATargetsAll$Year %in% (Startyear+5))
  sectortotals <- ddply(IEATargets,.(Sector), summarise, sectortot = sum(AnnualvalIEAtech))
  IEATargets <- merge(IEATargets, sectortotals, by= "Sector")
  IEATargets$Exposure <- IEATargets$AnnualvalIEAtech/IEATargets$sectortot
  IEATargetsWide <- subset(IEATargets, select = c("Technology","Exposure"))
  IEATargetsWide <- setNames(data.frame(t(IEATargetsWide[,-1])), IEATargetsWide[,1])
  IEATargetsWide$PortName <- "IEATarget"
  
  # 
  PlotData <- rbind(CoverageWeightsWide,IEATargetsWide)
  PlotData <- setNames(data.frame(t(PlotData[,-1])), PlotData[,1]) 
  PlotData$Technology <- rownames(PlotData)
  PlotData <- merge(PlotData,sectors,by="Technology")
  
  
  
  
  PlotData$UppLim <- PlotData$Maximum-PlotData$IEATarget
  PlotData$LowLim <- PlotData$Minimum-PlotData$IEATarget
  
  PlotData$PortLoc <- PlotData[,PortfolioNameLong]-PlotData$IEATarget
  
  
  # PlotData <- melt(PlotData, id=c("Sector","Technology","order"),value.name="TechWeight", variable.name = "family")
  
  # Adding Colours for Company and Average Values
  redgreen<- colorRampPalette(c("red","white", "darkgreen"))(201) 
  
  
  # MedExp$colourselection <- redgreen[as.integer(MedExp$limvalue)]
  
  # Factorise and Order by Technology  
  PlotData <- PlotData[(order(PlotData$order)),]
  PlotData$order <- factor(PlotData$order, levels = PlotData$order)
  
  
  
  locations <- c(1:11)
  locations <- c(1:3,4.5:6.5,8:12)
  barwidth <- 2 
  bh <-0.6
  # nocomps <- nrow(CompanyRanks)
  # notechs <- nrow(CompExp)
  tbwid <- .2
  tbspace <- 5
  maxval <- 100#min(max(abs(AllSD$value),na.rm = TRUE),100)
  triwid <- .6
  markerwid <- 9
  markerh <- .3
  sectorbreak <-1
  
  
  wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  leaficon <- readPNG(paste0(figuredirectory,"Leaf",".png"))
  leafg <- rasterGrob(leaficon, interpolate=TRUE)
  leafxmin=-1.3
  leafxmax=-1.1
  leafloc <- c(10,11,2,3)
  leafh <- 0.3
  
  baricon <- readPNG(paste0(figuredirectory,"GradBar",".png"))
  barg <- rasterGrob(baricon, interpolate=TRUE)
  
  
  PlotData$a <- paste0(gsub(" ","",PlotData$Sector),"_Unit")
  PlotData$b <- paste0("T_",gsub("Cap","",PlotData$Technology))
  
  PlotData$TechTitle <- toupper(paste0(t(GT[PlotData$b])," ",t(GT[PlotData$a])))
  PlotData$a <-PlotData$b <-  NULL
  PlotData$TechDesc <- t(GT[paste0("Rank_",PlotData$Technology)])
  PlotData$TechLabel <- paste0(PlotData$TechTitle,"\n",wrap.labels(PlotData$TechDesc,40))
  
  PlotData <- PlotData[order(PlotData$order),]
  PlotData$order <- factor(PlotData$order, levels = PlotData$order)
  
  pointdata <- data.frame(yloc=locations, xloc=PlotData$PortLoc,label= paste0(round(PlotData[,PortfolioNameLong]*100,0)," %"))
  minpointdata <- data.frame(yloc=locations, xloc=PlotData$LowLim)
  maxpointdata <- data.frame(yloc=locations, xloc=PlotData$UppLim)
  
  BaseBars <- data.frame(tech=PlotData$Technology,ymin=locations-bh/2,ymax=locations+bh/2,xmin=-1,xmax=1)
  
  tiles <- data.frame(x=seq(-1,1,0.01), y=1, colourplot = redgreen)
  GraphTitle <- GT["Rank_Title"][[1]]
  
  outputplot <- ggplot(tiles, aes(x,y, fill=x))+
    geom_raster()+
    scale_fill_gradient2(low="green",mid="white",high="red")
  
  outputplot<-    ggplot(data=BaseBars) + #AllExp,aes(value,Technology)
    geom_rect(aes(xmin=xmin,xmax=xmax, ymin=ymin,ymax=ymax))+
    # annotation_raster(baricon,xmin=c(-1),xmax=c(1),ymin=c(1,4),ymax =c(1.8,5))+
    scale_x_continuous()+
    scale_y_discrete()+
    # geom_tile(width=barwidth,height=0.8, fill="grey")+
    labs(x=NULL,y= NULL,  title= GraphTitle)+
    
    # bars
    annotate(xmin=-1,xmax=1,ymin=locations-bh/2,ymax=locations+bh/2,geom="rect", fill="green",colour="black")+    # base bars
    
    # error lines
    annotate(xmin = PlotData$LowLim,xmax=PlotData$UppLim,ymin=locations,ymax=locations,geom="rect",fill="darkgrey",colour="black")+ # Range Line
    geom_point(data=minpointdata,aes(x=xloc,y=yloc),  fill="black",colour="black",size=2)+
    geom_point(data=maxpointdata,aes(x=xloc,y=yloc),  fill="black",colour="black",size=2)+
    # annotate(xmin = PlotData$LowLim,xmax=PlotData$LowLim,ymin=locations-bh/2,ymax=locations+bh/2,geom="rect",fill="darkgrey",colour="black")+ # Minimum Point
    # annotate(xmin = PlotData$UppLim,xmax=PlotData$UppLim,ymin=locations-bh/2,ymax=locations+bh/2,geom="rect",fill="darkgrey",colour="black")+ # Minimum Point
    
    # centre alignment line    
    annotate(geom="rect",xmin = -1,xmax=0,ymin = locations-bh/2,ymax=locations+bh/2,linetype="dashed",colour = "black",fill = "transparent")+
    # geom_vline(xintercept=0, colour = "black", linetype="dashed")+
    # annotate(xmin=AveExp$value-barwidth/2,xmax=AveExp$value+barwidth/2,ymin=-bh/2+locations,ymax=bh/2+locations,geom = "rect", fill="black")+ #Weighted Mean
    
    # Company Circles
    geom_point(data=pointdata,aes(x=xloc,y=yloc),  fill="black",colour="black",size=8)+
    annotate(geom="text",label=pointdata$label, x= pointdata$xloc, y= pointdata$yloc, colour="white",size=rel(3))+
    
    # Leaf Icon
    annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[1]-leafh,ymax = leafloc[1]+leafh)+
    annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[2]-leafh,ymax = leafloc[2]+leafh)+
    annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[3]-leafh,ymax = leafloc[3]+leafh)+
    annotation_custom(leafg,xmin=leafxmin,xmax=leafxmax,ymin=leafloc[4]-leafh,ymax = leafloc[4]+leafh)+
    
    
    # value annotations
    annotate(geom="text",x= 0 , y= locations+bh-.2,label=paste0(round(PlotData$IEATarget*100,0)," %"),size=rel(3))+     # Target
    annotate(geom="text",x= PlotData$LowLim-.05 , y= locations,label=paste0(round(PlotData$Minimum*100,0)," %"),size=rel(3))+     # Minimum
    annotate(geom="text",x= PlotData$UppLim+.1 , y= locations,label=paste0(round(PlotData$Maximum*100,0)," %"),size=rel(3))+     # Maximum
    
    annotate(xmin = 1.1,xmax =1.1+tbwid, ymin=locations-bh/2, ymax=locations+bh/2, geom="rect",fill="white",colour="black")+ #Ranking Box
    # annotate(geom="text",x=maxval+2*tbspace+tbwid*0.5,y=locations, label=CompRank$value)+ # Company Ranking
    # annotate(geom="text",x= 0, y= notechs+1., label = "Alignment")+
    # annotate(geom="text",x= 0, y= notechs+.5, label = "100%")+
    # annotate(geom="text",x= -100, y= notechs+.5, label = "0%")+
    # annotate(geom="text",x= 100, y= notechs+.5, label = "200%+")+
    
    # annotate("text", label = CompRank$value, x= CompExp$value+barwidth/2, y = locations)+ # Company Ranking
    
    # Sector Boxes
    annotate(geom="rect",xmin=-2.7, xmax=1.4, ymin=0.2, ymax=3.7, fill="transparent", colour="black")+ # Power
    annotate(geom="rect",xmin=-2.7, xmax=1.4, ymin=3.9, ymax=7.2, fill="transparent", colour="black")+ # Automotive
    annotate(geom="rect",xmin=-2.7, xmax=1.4, ymin=7.5, ymax=12.7, fill="transparent", colour="black")+ # FF
    
    
    # Labels
    annotate(geom="text",x=-2.6,y=locations,label=PlotData$TechLabel, size=rel(3), hjust=0)+
    # annotate(geom="text",x= maxval+2*tbspace+tbwid/2, y= notechs+1, label = "Portfolio \n Ranking")+
    # annotate(geom="text",x= maxval+2*tbspace+tbwid*1.5+tbspace, y= notechs+1., label = "Average \n Alignment")+
    # annotate(geom="text",x= maxval+3*tbspace+tbwid*2.5+tbspace, y= notechs+1., label = "Average \n Exposure")+
    
    theme(panel.background = element_rect(fill="transparent"),
          panel.grid.major.x = element_blank() ,
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x=element_text(face="bold",colour=AxisColour, size=12,family = "Calibri"),
          axis.title.y=element_text(face="bold",colour=AxisColour, size=12, vjust = 2.4,family = "Calibri"),
          plot.margin = (unit(c(1, .5, 0.5, 2.7), "lines")))
  
  outputplot <- ggplot_gtable(ggplot_build(outputplot))
  outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
  grid.draw(outputplot)  
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_RankingChartFinal.png', sep=""),bg="transparent",height=9,width=8,plot=outputplot,dpi=ppi)
  
  return()
}

# ------------ Port Breakdown Chart --------- #
breakdown_chart <- function(plotnumber,ChartType,WMCoverageWeight,PortCoverageWeight,PortfolioName){
  
  wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  df <- PortCoverageWeight
  colnames(df)[colnames(df) %in% "CoverageWeight"] <- "PortCoverageWeight"
  df <- merge(df,WMCoverageWeight, by = "Technology")
  colnames(df)[colnames(df) %in% "CoverageWeight"] <- "WMCoverageWeight"  
  
  techorder <- data.frame(techno =seq(1,11),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Gas","Oil","Coal","Electric","Hybrid","ICE"), Sector = c("Power","Power","Power","Power","Power","Fossil Fuels","Fossil Fuels","Fossil Fuels","Automotive","Automotive","Automotive"))
  df <- merge(df, techorder, by = "Technology")
  
  df<-df[rev(order(df$techno)),]
  
  df$Technology <- factor(df$Technology, levels = df$Technology) 
  
  colourlist <- rev(c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,GasProdColour,OilProdColour, CoalProdColour,ElectricColour,HybridColour,ICEColour) )
  labellist <- gsub("Cap","",df$Technology)
  SecList <- gsub(" ","",df$Sector)
  
  df$labellist <- t(GT[paste0("T_",df$Technology)])
  df$sectorlist <- wrap.labels(t(GT[paste0(gsub(" ","",SecList),"_Breakdown")]),13)
  
  ytitle <- GT["Axis_Breakdown"][[1]]
  
  xlabloc <- c(2,5,9)     
  
  outputplot <- ggplot(data = df, aes(x = interaction(Technology, Sector, lex.order = TRUE),y=PortCoverageWeight))+
    geom_bar(stat = 'identity', position = 'dodge', fill = colourlist)+
    scale_fill_manual( values = colourlist)+
    scale_x_discrete(labels=df$labellist)+
    scale_y_continuous(labels = scales::percent)+#, limits=c(0,max(df$CoverageWeight)))+
    ylab(label = ytitle)+
    annotate(geom="text",label = df$labellist, x=seq_len(nrow(df)),y=.15,hjust = 0)+
    annotate(geom = "text",label=unique(df$sectorlist),x = xlabloc,y = -0.01,hjust = 1)+
    annotate(geom = "rect", xmin = seq_len(nrow(df))-.45, xmax = seq_len(nrow(df)) + .45, ymin=df$WMCoverageWeight-.001,ymax=df$WMCoverageWeight+.001)+
    theme(
      plot.margin = unit(c(1,1,1,4),"lines"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    coord_flip()
  
  g2 <- ggplot_gtable(ggplot_build(outputplot))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"
  grid::grid.draw(g2)
  
  png(paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_BreakdownChart.png'), height = 3200, width = 2200,res=ppi,bg="transparent") 
  grid.draw(g2)
  dev.off() 
  
  
}

# ------------ Asset Ownership Chart -------- #
ownership_chart <- function(plotnumber,combin,exposures,IEATargetsAll, Scenariochoose,Startyear,BenchmarkRegionchoose,CompanyDomicileRegionchoose, figuredirectory){
  # Vertical bar chart showing capacity by technology
  
  
  
  df <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose,select= c("Sector","Technology","Production","TargetProductionAlignment")) 
  if (!"Automotive" %in% df$Sector){
    dfAuto <- subset(combin,Scenario %in% Scenariochoose & Year == Startyear+5 & BenchmarkRegion == BenchmarkRegionchoose & Sector== "Automotive" & CompanyDomicileRegion == CompanyDomicileRegionchoose,select= c("Sector","Technology","Production")) 
    df <- rbind(df,dfAuto)
  }
  
  df <- subset(df,!Technology %in% "OilCap")
  
  # df$Technology <- factor(df$Technology, levels = df$Technology) 
  seclist <- unique(factor(df$Sector, levels = df$Sector))
  
  colourlist <- c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,GasProdColour,OilProdColour, CoalProdColour,ElectricColour,HybridColour,ICEColour) 
  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  labellist <- gsub("Cap","",exposures$Technology)
  seclist <- unique(exposures$Sector)
  
  maglist <- nchar(as.character(round(df$Production,0)))-1
  df$ProductionMin <- df$Production/unlist(lapply(maglist[1:11],function(x){10^x}))
  
  sectors <- c("Automotive", "Fossil Fuels", "Power")
  axislabels <- c("Cars Produced", "Production", "Capacity")
  lookup <- data.frame(sectors,axislabels)
  
  magnitude_scale <- c(1e-3,1,1e3,1e6,1e9)
  power_units <- c("kW","MW","GW","TW","Error_powertoohigh")
  car_units <- c("cars","cars","thousand cars","million cars","billion cars")
  # ff_units <- c("Error_fftoolow","","thousands","millions","billions")
  coal_units <- c("kg","t","kt","MT","GT")
  oil_units <- c("Error_fftoolow","barrels","thousand barrels","million barrels","billion barrels")
  gas_units <- c("Error_fftoolow","","thousand m?","million m?","billion m?")
  
  unit_lookup <- data.frame("Hybrid"=car_units,"Electric"=car_units,"ICE"=car_units, "CoalCap"=power_units,"GasCap"=power_units,"HydroCap"=power_units,"NuclearCap"=power_units,"RenewablesCap"=power_units,"Coal"=coal_units,"Gas"=gas_units,"Oil"=oil_units)
  unit_lookup <- data.frame("Automotive"=car_units,"Power"=power_units,"Coal"=coal_units,"Gas"=gas_units,"Oil"=oil_units)
  
  df$Technology <- as.character(df$Technology)
  
  # Scales the Data to the correct units based on the maximum value.
  df$max_magnitude <- findInterval(df$Production,magnitude_scale)
  df$max_magnitude[df$max_magnitude == 0] <- 1
  
  # Portfolio Production
  df$ProductionLabel <- round(df$Production/magnitude_scale[df$max_magnitude],1)
  df$ProductionLabel[df$Sector == "Automotive" & df$max_magnitude == 1] <- round(df$Production[df$Sector == "Automotive" & df$max_magnitude == 1],1)
  
  df$ProductionUnits[df$Technology == "Coal"] <- as.character(unit_lookup["Coal"][df$max_magnitude[df$Technology == "Coal"],])
  df$ProductionUnits[df$Technology == "Gas"] <- as.character(unit_lookup["Gas"][df$max_magnitude[df$Technology == "Gas"],])
  df$ProductionUnits[df$Technology == "Oil"] <- as.character(unit_lookup["Oil"][df$max_magnitude[df$Technology == "Oil"],])
  df$ProductionUnits[df$Sector == "Power"] <- as.character(unit_lookup["Power"][df$max_magnitude[df$Sector == "Power"],])
  df$ProductionUnits[df$Sector == "Automotive"] <- as.character(unit_lookup["Automotive"][df$max_magnitude[df$Sector == "Automotive"],])
  
  df$unitlabels <- paste(df$ProductionLabel,df$ProductionUnits)  
  
  # Target Production
  df$target_max_magnitude <- findInterval(df$TargetProductionAlignment,magnitude_scale)
  df$target_max_magnitude[df$target_max_magnitude == 0] <- 1 
  
  df$TargetProductionLabel <- round(df$TargetProductionAlignment/magnitude_scale[df$target_max_magnitude],1)
  df$TargetProductionLabel[df$Sector == "Automotive" & df$target_max_magnitude == 1] <- round(df$TargetProductionAlignment[df$Sector == "Automotive" & df$target_max_magnitude == 1],1)
  
  df$TargetProductionUnits[df$Technology == "Coal"] <- as.character(unit_lookup["Coal"][df$target_max_magnitude[df$Technology == "Coal"],])
  df$TargetProductionUnits[df$Technology == "Gas"] <- as.character(unit_lookup["Gas"][df$target_max_magnitude[df$Technology == "Gas"],])
  df$TargetProductionUnits[df$Technology == "Oil"] <- as.character(unit_lookup["Oil"][df$target_max_magnitude[df$Technology == "Oil"],])
  df$TargetProductionUnits[df$Sector == "Power"] <- as.character(unit_lookup["Power"][df$target_max_magnitude[df$Sector == "Power"],])
  df$TargetProductionUnits[df$Sector == "Automotive"] <- as.character(unit_lookup["Automotive"][df$target_max_magnitude[df$Sector == "Automotive"],])
  
  df$targetunitlabels <- paste(df$TargetProductionLabel, df$TargetProductionUnits)
  
  # Difference
  df$prodifference <- df$Production - df$TargetProductionAlignment
  
  df$diff_max_magnitude <- findInterval(abs(df$prodifference),magnitude_scale)
  df$diff_max_magnitude[df$diff_max_magnitude == 0] <- 1 
  
  df$DiffProductionLabel <- round(df$prodifference/magnitude_scale[df$diff_max_magnitude],1)
  df$DiffProductionLabel[df$Sector == "Automotive" & df$diff_max_magnitude == 1] <- round(df$prodifference[df$Sector == "Automotive" & df$diff_max_magnitude == 1],1)
  
  df$DiffProductionUnits[df$Technology == "Coal"] <- as.character(unit_lookup["Coal"][df$diff_max_magnitude[df$Technology == "Coal"],])
  df$DiffProductionUnits[df$Technology == "Gas"] <- as.character(unit_lookup["Gas"][df$diff_max_magnitude[df$Technology == "Gas"],])
  df$DiffProductionUnits[df$Technology == "Oil"] <- as.character(unit_lookup["Oil"][df$diff_max_magnitude[df$Technology == "Oil"],])
  df$DiffProductionUnits[df$Sector == "Power"] <- as.character(unit_lookup["Power"][df$diff_max_magnitude[df$Sector == "Power"],])
  df$DiffProductionUnits[df$Sector == "Automotive"] <- as.character(unit_lookup["Automotive"][df$diff_max_magnitude[df$Sector == "Automotive"],])
  
  df$diffunitlabels <- paste(df$DiffProductionLabel,df$DiffProductionUnits)
  
  df$Rating <- "Aligned"
  df$Rating[df$Technology %in% badtech & df$prodifference >0]<- "Misaligned"
  df$Rating[!df$Technology %in% badtech & df$prodifference<0] <- "Misaligned"
  
  
  df$Technology <- factor(df$Technology, levels = df$Technology) 
  
  techorder <- data.frame(techno =seq(1,11),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Gas","Oil","Coal","Electric","Hybrid","ICE"))
  df <- merge(df,techorder, by = "Technology")
  df<-df[(order(df$techno)),]
  
  df$Technology <- factor(df$Technology, levels = df$Technology) 
  
  SectorLabels <- c("Installed Power Capacity", "Fossil Fuel Production", "Automotive Production")
  
  xlabloc <- c(3,7,10)  
  
  outputplot <- ggplot(data = df, aes(x = df$Technology,y=df$ProductionMin,width=0.5))+
    geom_bar(stat = 'identity', position = 'dodge', fill = colourlist)+
    scale_fill_manual( values = colourlist)+
    scale_x_discrete(labels=df$Technologylabellist)+
    annotate(geom = "text",label = df$Technology, x=seq_len(nrow(df)),y=-.2,hjust = 0.5)+
    annotate(geom = "text",label = SectorLabels,x = xlabloc,y = -0.8,hjust = 0.5)+
    annotate(geom = "text",label = df$unitlabels,x = seq_len(nrow(df)),y = df$ProductionMin + .2,hjust=0.5)+
    theme(
      plot.margin = unit(c(1,1,1,4),"lines"),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "transparent",colour = NA),
      panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  
  g2 <- ggplot_gtable(ggplot_build(outputplot))
  g2$layout$clip[g2$layout$name == "panel"] <- "off"
  grid::grid.draw(g2)
  
  png(paste0(plotnumber,"_",PortfolioName,'_AssetOwnershipChart.png'), height = 2800, width = 8000,res=ppi,bg="transparent") 
  grid.draw(g2)
  dev.off() 
  
  printtocsv <- t(subset(df, select = c("Sector","Technology","unitlabels","targetunitlabels","diffunitlabels","Rating")))
  
  write.csv(printtocsv,paste0(PortfolioName,"_PortfolioProduction.csv"))
  
}

# ------------ Fossil Fuel Capacities ------- #
ffbreakdown_chart <- function(plotnumber,ChartType,CoverageWeight,AverageCoverageWeight,IEATargetsAll,Startyear,PortfolioName){
  
  df <- CoverageWeight
  df<-  subset(df, df$Technology %in% c("Oil","Gas","Coal"))
  CWtot <- sum(df$CoverageWeight, na.rm = TRUE)
  df$PortCoverageWeight <- df$CoverageWeight/CWtot
  df$CoverageWeight<-NULL
  
  dfave<- AverageCoverageWeight
  dfave<-  subset(dfave, dfave$Technology %in% c("Oil","Gas","Coal"))
  CWavetot <- sum(dfave$CoverageWeight, na.rm = TRUE)
  dfave$AverageCoverageWeight <- dfave$CoverageWeight/CWavetot
  dfave$CoverageWeight<-NULL
  
  IEATargetsFF <- subset(IEATargetsAll, IEATargetsAll$Year %in% (Startyear+5) & IEATargetsAll$Technology %in% c("Oil","Gas","Coal")) 
  IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Coal"]<- IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Coal"]*24
  IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Oil"]<- IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Oil"]*6.12
  IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Gas"]<- IEATargetsFF$AnnualvalIEAtech[IEATargetsFF$Technology == "Gas"]*0.0372
  IEATargetsFFTot <- sum(IEATargetsFF$AnnualvalIEAtech)
  IEATargetsFF$IEACoverageWeight <- IEATargetsFF$AnnualvalIEAtech/IEATargetsFFTot
  IEATargetsFF$Year <- IEATargetsFF$AnnualvalIEAtech <- NULL
  
  df <- merge(df, IEATargetsFF, by= "Technology")
  df <- merge(df, dfave, by="Technology")
  
  dfwide <- melt(df,"Technology")
  dfwide$variable<-as.character(dfwide$variable)
  dforder <- data.frame(orderno=c(1,2,3),variable=c("PortCoverageWeight", "AverageCoverageWeight","IEACoverageWeight"))
  dfwide <- merge(dfwide,dforder, by = "variable")  
  
  dfwide$variable[dfwide$variable %in% "PortCoverageWeight"]<- PortfolioName
  dfwide$variable[dfwide$variable %in% "IEACoverageWeight"]<- GT["IEA_targ"][[1]]
  dfwide$variable[dfwide$variable %in% "AverageCoverageWeight"]<- GT["AveragePort"][[1]]
  
  dfwide <- merge(dfwide,ColourPalette, by="Technology")
  
  dfwide <- dfwide[order(dfwide$orderno),]
  dfwide$orderno <- factor(dfwide$orderno, levels = unique(dfwide$orderno))
  
  dfwide$value <- as.numeric(dfwide$value)
  
  dfwide$yloc <-1
  dfwide$yloc[dfwide$Technology %in% "Oil"] <- dfwide$value[dfwide$Technology %in% "Oil"]/2
  dfwide$yloc[dfwide$Technology %in% "Coal"] <- 1-dfwide$value[dfwide$Technology %in% "Coal"]/2
  dfwide$yloc[dfwide$Technology %in% "Gas"] <- 1-dfwide$value[dfwide$Technology %in% "Gas"]/2-dfwide$value[dfwide$Technology %in% "Coal"]
  
  dfwide$TechTrans <- t(GT[paste0("T_",dfwide$Technology)])
  
  FFChart <- ggplot(dfwide,aes(x=variable, y=value, fill = Technology))+
    geom_bar(stat = "identity",color=NA,width = .8)+
    geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = .8)+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    scale_fill_manual(values= as.character(dfwide$Colours),labels = dfwide$TechTrans)+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
    annotate(geom = "text",label=paste0(round(dfwide$value*100,0),"%"),x=dfwide$variable,y=dfwide$yloc,colour="white")+
    theme(text = element_text(size=10,colour=AxisColour),axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line.x = element_blank(), plot.margin = unit(c(0,0,10,0), "mm"),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.text = element_text(size=10,family = "Calibri",colour=AxisColour),
          legend.title=element_blank(),
          legend.position = "top")+
    coord_flip()
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_FFExposure.png'),bg="transparent",height=2.6,width=4.5,plot=FFChart,dpi=ppi)
  
}




# pnglist <- list.files(RegionDirectory,pattern=c("\\.png$"), full.names = FALSE)

#-------------------------

# ------------- FLAT WHEEL CHARTS ----------- #
flat_wheel_chartOG <- function(plotnumber,companiestoprint,ChartType,PortSnapshot, combin,AlloftheCompanies, SectorToPlot, BenchmarkRegionchoose, CompanyDomicileRegionchoose, Scenariochoose, IndexData,Indexchoose, PortfolioName,PortGraphName){
  
  # ChartType<- "EQ"
  # SectorToPlot<-"Automotive"
  # AlloftheCompanies <- AutoCompanies
  # AlloftheCompanies <- OGCarbonBudget
  # combin <- EQCombin
  # PortSnapshot <- EQPortSnapshot
  # companiestoprint<-20
  # AUMData<- EQAUMData
  # Ranks <- EQRanks
  # combin<-EQCompProdSnapshot
  # SectorToPlot <-"OG"
  
  
  
  WheelofFortune<-function(df, othercompanies = TRUE ,family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.2,techorder,PortFirmY=18,OtherFirmY=5,
                           spaceFamily = 1.2, innerRadius = 0.3, outerRadius = 1, guides = seq(100,0,by = -25),
                           alphaStart = -0.3, circleProportion = 0.8, direction = "inwards", familyLabels = FALSE, normalised = TRUE)
  {
    # 
    # df<-AllCompanies
    # family = NULL
    # columnNames = NULL
    # binSize = 1
    # spaceItem = 0.2
    # spaceFamily = 1.2 #1.2
    # innerRadius = 0.3 #0.3
    # outerRadius = 1
    # guides =seq(100,0,by = -25)
    # alphaStart = -0.3 #-0.3
    # circleProportion = .8
    # direction = "inwards"
    # familyLabels = FALSE
    # normalised = TRUE
    
    if (!is.null(columnNames)) {
      namesColumn <- names(columnNames)
      names(namesColumn) <- columnNames
      df <- rename(df, namesColumn)
    }
    
    applyLookup <- function(groups, keys, unassigned = "unassigned") {
      lookup <- rep(names(groups), sapply(groups, length, USE.NAMES = FALSE))
      names(lookup) <- unlist(groups, use.names = FALSE)
      p <- lookup[as.character(keys)]
      p[is.na(p)] <- unassigned
      p
    }
    
    df$score <- factor(df$score, levels=techorder)
    
    if (!is.null(family)) {
      df$family <- applyLookup(family, df$item)}
    df <- arrange(df, family, item, score) # original sort 
    
    
    
    if (normalised) 
    {df <- ddply(df, .(family, item), transform, value = cumsum(value/(sum(value))))
    }else {
      maxFamily <- max(plyr::ddply(df, .(family, item), summarise, 
                                   total = sum(value))$total)
      df <- ddply(df, .(family, item), transform, value = cumsum(value))
      df$value <- df$value/maxFamily
    }
    
    df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
    
    df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
    df2$indexItem <- cumsum(df2$indexItem)
    df3 <- ddply(df, .(family), summarise, indexFamily = 1)
    df3$indexFamily <- cumsum(df3$indexFamily)
    df <- merge(df, df2, by = c("family", "item"))
    df <- merge(df, df3, by = "family")
    df <- arrange(df, family, item, score)
    affine <- switch(direction, inwards = function(y) (outerRadius - innerRadius) * y + innerRadius, outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius, stop(paste("Unknown direction")))
    
    df <- within(df, {
      xmin <- (indexItem - 1) * binSize + (indexItem - 1) *
        spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
      xmax <- xmin + binSize
      ymin <- affine(1 - previous)
      ymax <- affine(1 - value)
    })
    if (normalised) {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/100, 1, each = nrow(df)))
    } else {
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/maxFamily, 1, each = nrow(df)))}
    guidesDF <- within(guidesDF, {
      xend <- xmin + binSize
      y <- affine(y)
    })
    totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 0
    p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax, fill = score))
    readableAngle <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
    }
    readableJustification <- function(x) {
      angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
      ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
    }
    
    dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
    
    dfItemLabels <- within(dfItemLabels, {
      x <- xmin + binSize/2
      angle <- readableAngle(xmin + binSize/2)
      hjust <- 1
    })
    # new
    
    if (othercompanies == TRUE){
      # LABELS ARE INCLUDED
      typelabel <- data.frame(labelname = c(GT["PortCompanies"][[1]],GT["Oth_Listed"][[1]]),x=c(PortFirmY,OtherFirmY),y=0.0,hjust=0.5, angle=90,labelcolours=c( AxisColour,"grey50"))
      if (PortFirmY == 0){typelabel$labelname[1]<-""}
      
      #Company Labels
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle,
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("grey50", AxisColour, "black")) #guide=FALSE,
      
      # Sector Labels
      p <- p + geom_text(aes(x = x,hjust=hjust, y=y,label = labelname, angle=angle),size = 3, colour=typelabel$labelcolours, data=typelabel)
      
    }else{
      
      p <- p + geom_text(aes(x = x+1.8, label = item, #angle = angle, 
                             hjust = hjust,colour = family, fontface=ifelse(family=="Portfolio","bold","plain")), y = 0.16, size = 2.5, show.legend = FALSE,vjust = 3, data = dfItemLabels) +
        scale_colour_manual(values = c("black", AxisColour, "black")) #guide=FALSE,
    }
    
    p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y), 
                          colour = "white", data = guidesDF) #+geom_segment(aes(x = xmin, xend = .75, y = y, yend = y), colour = "grey50", data = guidesDF) #complete lines
    
    if (normalised) {
      guideLabels <- data.frame(x = 0, y = seq(0.2,1.0, by= 0.2),#affine(1 - guides/100), 
                                label = paste(guides, "% ", sep = ""))
    }else{ guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily), 
                                     label = paste(guides, "% ", sep = ""))}
    p <- p + geom_text(aes(x = x-1, y = y, label = label), data = guideLabels,
                       angle = 0, hjust = .5, size = 3)
    if (familyLabels) {
      familyLabelsDF <- aggregate(xmin ~ family, data = df, 
                                  FUN = function(s) mean(s + binSize))
      familyLabelsDF <- within(familyLabelsDF, {
        x <- xmin})
      
    }
    p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(), 
                   axis.title.y = element_blank(), panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
                   axis.text.y = element_blank(), axis.ticks = element_blank(),
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   legend.title = element_blank(),legend.position = "bottom")
    # p <- p + ylim(0, outerRadius)
    
  }    
  
  if (SectorToPlot == "OG"){
    OG <-AlloftheCompanies # OGCarbonbudget
    CompProdSnapshot <- combin
    OG$InPort <- "AllCompanies"
    OG$InPort[OG$EQY_FUND_TICKER %in% CompProdSnapshot$EQY_FUND_TICKER] <- "PortCompanies"
    
    OGCompanies <- AllCompanyData[AllCompanyData$EQY_FUND_TICKER %in% OG$EQY_FUND_TICKER,]
    OGCompanies <- subset(OGCompanies, Year %in% (Startyear+5) & BenchmarkRegion %in% "Global" & CompanyDomicileRegion %in% CompanyDomicileRegionchoose)
    
    OGCompanies<- subset(OGCompanies, !Technology %in%  "Coal")
    OGCompanies$Production[OGCompanies$Technology == "Oil"]<- OGCompanies$Production[OGCompanies$Technology == "Oil"]*6.12
    OGCompanies$Production[OGCompanies$Technology == "Gas"]<- OGCompanies$Production[OGCompanies$Technology == "Gas"]*0.0372
    
    OGCompanies <- ddply(OGCompanies, . (EQY_FUND_TICKER),summarise, Size = sum(Production))
    
    OG <- merge(OG,OGCompanies, by = "EQY_FUND_TICKER",all.x = TRUE, all.y = FALSE)
    
    OG <- OG[!is.na(OG$Size),]
    
    OG <- subset(OG, select = c("Company","InPort","Size","TotalCarbonBudget","OutsideCarbonBudget"))
    
    # limit data
    OG <- OG[order(-OG$Size),]
    OGPort <- subset(OG, OG$InPort %in% "PortCompanies")
    OGOut <- subset(OG, OG$InPort %in% "AllCompanies")
    
    NoInPort <- nrow(OGPort)
    NoOutPort <- nrow(OGOut)
    
    # if (NoInPort < companiestoprint/2){ NoOutPort <- companiestoprint -NoInPort}else{
    #   NoOutPort <- companiestoprint/2
    #   NoInPort <-companiestoprint/2}
    
    # if (NoOutPort < companiestoprint/2){NoInPort <- companiestoprint-NoOutPort}else{NoOutPort <- companiestoprint/2}
    # if (NoInPort < companiestoprint/2){NoOutPort <- companiestoprint-NoInPort}#else{NoOutPort <- companiestoprint/2}
    NoInPort <- 14
    NoOutPort <-6
    
    
    OG <- rbind(OGPort[1:NoInPort,],OGOut[1:NoOutPort,])
    OG <- subset(OG, select=-Size)
    
    PlotData <- melt(OG, id.vars = c("Company","InPort"))
    colnames(PlotData) <- c("item","family","score","value")
    techorder <- c("OutsideCarbonBudget","TotalCarbonBudget")
    
    # scale_fill_manual(values = c("ICE" = ICEColour,"Hybrid" = HybridColour, "Electric"= ElectricColour), labels = TechLabels, name = "Technology")
    Colours <- data.frame("variable"=unique(PlotData$score), "Colour"=c("firebrick","darkgrey"), labels=c(GT["OutsideCB"][[1]],GT["InCB"][[1]]))
    Colours$Colour <- as.character(Colours$Colour)
    
    circleProportion = 1
    alphaStart = 0.02
    spaceFamily = .8
    
    if(NoInPort == 0){PortFirmY <-0}else{PortFirmY <- 18}
    
    Plot<- WheelofFortune(PlotData, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=5,
                          spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                          circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
      scale_fill_manual(values = Colours$Colour, labels=Colours$labels)+
      coord_flip()
    
  }
  else{
    
    if (SectorToPlot == "Power"){techorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")} 
    if (SectorToPlot == "Automotive"){techorder <- c("ICE","Hybrid","Electric")}
    if (SectorToPlot == "Fossil Fuels"){techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
    AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "Technology"]
    AlloftheCompanies <- rename(AlloftheCompanies, c("Resource.Type" = "Technology"),warn_missing = FALSE)
    
    if (ChartType == "EQ"){AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]}
    else{
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
    }
    }
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER")] <- "TICKER"
    colnames(AlloftheCompanies)[colnames(AlloftheCompanies) %in% c("COMPANY_CORP_TICKER","EQY_FUND_TICKER","EquityTicker","DebtTicker")] <- "TICKER"
    
    CompaniesInPort <- subset(PortSnapshot, select = c("TICKER"), AUM>0)
    CompaniesInPort <- unique(CompaniesInPort)
    
    AllCompanies <- ddply(AlloftheCompanies, .(Technology, TICKER, Name), summarise, Production =sum(Production,na.rm = TRUE)) #Country, 
    colnames(AllCompanies)[colnames(AllCompanies)=="Production"] <- "Capacity"
    AllCompanies$Capacity[is.na(AllCompanies$Capacity)] <-0
    AllCompanies <- subset(AllCompanies, !AllCompanies$Technology %in% "OilCap")
    
    # Classify the Companies
    AllCompanies$Classification <- "AllCompanies"
    AllCompanies$Classification[AllCompanies$TICKER %in% CompaniesInPort$TICKER] <- "PortCompanies"
    
    # Portfolio Average
    Portfoliomix <- ddply(AllCompanies, .(Technology, Classification), summarize, Capacity = sum(Capacity))
    Portfoliomix <- subset(Portfoliomix, Portfoliomix$Classification == "PortCompanies")
    if(dim(Portfoliomix)[1] != 0){
      Portfoliomix$Classification <- "Portfolio"
      # Portfoliomix <- subset(Portfoliomix, !Portfoliomix$Technology %in% c("Oil","Diesel","LPGCNG","Petrol"))
      Portfoliomix$Name <- PortGraphName
      Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Portfoliomix) <- c("item", "family", "score", "value")
      Portfoliomix$value <- as.numeric(Portfoliomix$value)
      Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))*100
    }
    
    Targetmix <- subset(combin, Sector == SectorToPlot & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose & BenchmarkRegion == BenchmarkRegionchoose & Year == Startyear+5, select = c("Technology", "TargetProductionAlignment"))
    Targetmix$Classification<-"Portfolio"
    Targetmix$Name<-GT["X2Target"][[1]]
    Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
    colnames(Targetmix) <- c("item", "family", "score", "value")
    Targetmix$value <- as.numeric(as.character(Targetmix$value))
    
    
    # Add Index
    Indexmix <- ddply(IndexData, .(CompanyDomicileRegion,Technology), summarize, Capacity = sum(Production))
    Indexmix$Classification <- "Portfolio"
    Indexmix <- subset(Indexmix, select =c("CompanyDomicileRegion","Classification","Technology","Capacity"))
    colnames(Indexmix) <- c("item", "family", "score", "value")  
    Indexmix$value <- as.numeric(as.character(Indexmix$value))
    Indexmix$item <- Indexchoose
    
    # Percentage share of each technology  
    CompanyTotal <- ddply(AllCompanies, .(TICKER,Name), summarise, CompanyTotalCapacity=sum(Capacity))
    AllCompanies <- merge(AllCompanies,CompanyTotal)
    AllCompanies$TechShare <- (AllCompanies$Capacity/AllCompanies$CompanyTotalCapacity)*100
    
    TopPortCompanies <- CompanyTotal[CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopPortCompanies <- TopPortCompanies[rev(order(TopPortCompanies$CompanyTotalCapacity)),]
    TopPortCompanies <- TopPortCompanies[1:companiestoprint,]
    
    TopPortCompanies<-na.omit(TopPortCompanies)
    if(nrow(TopPortCompanies)>0){    TopPortCompanies$totorder <- seq(1,nrow(TopPortCompanies))}
    
    indexcomptoprint <- companiestoprint+ (companiestoprint - nrow(TopPortCompanies))
    TopIndexCompanies <- CompanyTotal[!CompanyTotal$TICKER %in% CompaniesInPort$TICKER,]
    TopIndexCompanies <- TopIndexCompanies[rev(order(TopIndexCompanies$CompanyTotalCapacity)),]
    TopIndexCompanies <- TopIndexCompanies[1:indexcomptoprint,]
    TopIndexCompanies$totorder <- seq(1,indexcomptoprint)
    
    if (SectorToPlot != "Fossil Fuels"){
      AllTopCompanies <- rbind(TopPortCompanies, TopIndexCompanies)
    }else{
      AllTopCompanies <- TopPortCompanies
    }  
    
    AllCompanies <- subset(AllCompanies, AllCompanies$TICKER %in% AllTopCompanies$TICKER)
    AllCompanies <- subset(AllCompanies, Name != "NA")
    
    # Clean Company Names
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "LIMITED", "LTD.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "COMPANY", "CO.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, "CORPORATION", "CORP.")
    AllCompanies$Name <- str_replace_all(AllCompanies$Name, ",", "")
    AllCompanies$Name<-strtrim(AllCompanies$Name, 16)
    
    oldnames <- c("BAYERISCHE MOTOREN WERKE AG","FIAT CHRYSLER AUTOMOBILES NV","FUJI HEAVY INDUSTRIES LTD","HONDA MOTOR CO LTD","MITSUBISHI MOTORS CORP","BRILLIANCE CHINA AUTOMOTIVE")
    newnames <- c("BMW AG","FIAT CHRYSLER NV","FUJI HEAVY IND LTD","HONDA MOTOR CO","MITSUBISHI MOTORS","BRILLIANCE CN AUTO")
    for (i in c(1:length(oldnames))){AllCompanies$Name[AllCompanies$Name %in% oldnames[i]] <- newnames[i]}
    
    # Rearrange to be ready for WheelofFortune Function
    AllCompanies <- subset(AllCompanies, select = c("Name","Classification","Technology","TechShare"))
    colnames(AllCompanies) <- c("item", "family", "score", "value") #item = component, family = portfolio, score  = technology, value = capacity mix
    
    # Bind the remaining Lines (IEAmix comes in each section)
    
    AllCompanies[AllCompanies$item == "NA"] <- "NoName"
    
    AllCompanies <- as.data.frame(sapply(AllCompanies, function(x) gsub("Cap", "", x)))
    
    
    if(nrow(TopPortCompanies)>0){PortFirmY=(companiestoprint*2-3)}else{PortFirmY <-0}
    OtherFirmY=5
    
    if (SectorToPlot == "Power"){  
      Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      Targetmix <- subset(Targetmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Targetmix <- as.data.frame(sapply(Targetmix, function(x) gsub("Cap", "", x)))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"))
      Indexmix <- as.data.frame(sapply(Indexmix, function(x) gsub("Cap", "", x)))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      AllCompanies <- subset(AllCompanies, AllCompanies$score != "Oil")
      
      circleProportion = 1
      alphaStart = 0.02
      spaceFamily = .8
      
      TechLabels <- c(paste0("% ", GT["T_CoalCap"][[1]]),paste0("% ", GT["T_GasCap"][[1]]),paste0("% ", GT["T_NuclearCap"][[1]]),paste0("% ", GT["T_HydroCap"][[1]]),paste0("% ", GT["T_RenewablesCap"][[1]]))
      
      labelling <- data.frame(values = c(CoalCapColour,GasCapColour,NuclearColour, HydroColour,RenewablesColour), labels = TechLabels, name = techorder)
      labelling$values <- as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1, spaceItem = 0.22,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1, guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        
        coord_flip()
    }
    
    if (SectorToPlot == "Automotive"){
      Targetmix <- subset(Targetmix, score  %in% c("ICE","Hybrid","Electric"))
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
      Targetmix$value <- (Targetmix$value/sum(Targetmix$value))*100
      
      Indexmix <- subset(Indexmix, score  %in% c("ICE","Hybrid","Electric"))
      Indexmix$value <- as.numeric(as.character(Indexmix$value))
      Indexmix$value <- (Indexmix$value/sum(Indexmix$value))*100
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      AllCompanies <- rbind(AllCompanies, Portfoliomix, Targetmix, Indexmix)  
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      TechLabels <- c(paste0("% ", GT["T_ICE"][[1]]),paste0("% ", GT["T_Hybrid"][[1]]),paste0("% ", GT["T_Electric"][[1]]))
      
      labelling <- data.frame(values = c(ICEColour,HybridColour,ElectricColour), labels = TechLabels, name = techorder)
      labelling$values <-    as.character(labelling$values)
      labelling$name <- factor(labelling$name, techorder)
      
      # AllCompanies[is.na(AllCompanies$value)]<-NULL
      AllCompanies <- subset(AllCompanies,!is.na(AllCompanies$value))
      
      Plot<- WheelofFortune(AllCompanies, family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder=techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values = labelling$values, labels = labelling$labels)+
        coord_flip()
      # Plot
      
    }
    
    if (SectorToPlot == "Fossil Fuels"){
      
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      
      AllCompanies <- AllCompanies[rev(order(AllCompanies$item)),]
      AllCompanies$item <- factor(AllCompanies$item, levels=AllCompanies$item)
      
      AllCompanies <- rbind(AllCompanies, Portfoliomix)
      
      circleProportion = 1
      alphaStart = 0
      spaceFamily = 1
      
      oilcolours = brewer.pal(9, "YlGnBu")[5:9]
      
      TechLabels <- c(paste0("% ", GT["Conv_Oil"][[1]]),paste0("% ", GT["Heavy_Oil"][[1]]),paste0("% ", GT["Oil_Sands"][[1]]), paste0(GT["Unconv_Oil"][[1]]), paste0(GT["Other_Oil"][[1]]))
      
      Plot<- WheelofFortune(AllCompanies,othercompanies = FALSE , family = NULL, columnNames = NULL, binSize = 1.0, spaceItem = 0.2,techorder = techorder,PortFirmY=PortFirmY,OtherFirmY=OtherFirmY,
                            spaceFamily = spaceFamily, innerRadius = 0.18, outerRadius = 1., guides = seq(0,100,by = 25), alphaStart = alphaStart,
                            circleProportion = circleProportion, direction = "inwards", familyLabels = FALSE,normalised = TRUE)+
        scale_fill_manual(values=oilcolours,labels = TechLabels, name = "Technology")+
        coord_flip()
      
    } 
    
  }
  
  Plot <- ggplot_gtable(ggplot_build(Plot))
  Plot$layout$clip[Plot$layout$name == "panel"] <- "off"
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
  
  png(paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_',SectorToPlot,'_WheelofFortune.png'), height = 2600, width = 5500,res=ppi,bg="transparent") 
  grid.draw(Plot)
  dev.off()  
  
  # return(png(paste(PortfolioName,"_",SectorToPlot,'_WheelofFortune.png'), height = 3.300, width = 3300,res=ppi,bg="transparent") )
  return()  
}





