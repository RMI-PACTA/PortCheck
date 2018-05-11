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

CreateTestList <- function(EQBatchTest, CBBatchTest) {
  ID.COLS <- c("PortName","InvestorName","Type")
  
  Equity <- filter(EQBatchTest, Year == 2018)
  Equity <- dcast(Equity, PortName ~ Sector, sum, value.var = "WtProduction")
  Equity <- mutate(Equity, "HasCoal" = Coal > 0,
                   "HasOilGas" = `Oil&Gas` > 0,
                   "HasPower" = Power > 0,
                   "HasAuto" = Automotive > 0)
  Equity <- merge(Equity,unique(subset(EQBatchTest, select=c(ID.COLS))),by="PortName")
  Equity$HasEquity <- TRUE
  
  Debt <- filter(CBBatchTest, Year == 2018)
  Debt <- dcast(Debt, PortName ~ Sector, sum, value.var = "WtProduction")
  Debt <- mutate(Debt, "HasCoal" = Coal > 0,
                 "HasOilGas" = `Oil&Gas` > 0,
                 "HasPower" = Power > 0,
                 "HasAuto" = Automotive > 0)
  Debt <- merge(Debt,unique(subset(CBBatchTest, select=c(ID.COLS))),by="PortName")
  Debt$HasDebt <- TRUE
  
  TestList <- merge(Equity,Debt,by=ID.COLS, all=T)
  TestList[is.na(TestList)] <- FALSE
  colnames(TestList) <- gsub("\\.x",".EQ",colnames(TestList))
  colnames(TestList) <- gsub("\\.y",".CB",colnames(TestList))
  
  return(TestList)
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

# -------- Seperate Ranking chart -----------
RankPortfolios <- function( ChartType, Name){
  a<-Name
  if (ChartType == "EQ"){
    PortfolioExposures <- EQBatchTest[which(EQBatchTest$Year==Startyear),]
    
    
  }else if (ChartType == "CB"){
    PortfolioExposures <- CBBatchTest[which(CBBatchTest$Year==Startyear),]
    
  }

  # Order the table for Green vs Brown Tech

  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  goodtech <- c("Electric", "Hybrid","RenewablesCap", "HydroCap", "NuclearCap")
  PortfolioExposures$Technology<- as.factor(PortfolioExposures$Technology)
  PortfolioExposures<-PortfolioExposures[!PortfolioExposures$Technology %in% "OilCap",]
  PortfolioExposures$forrank <- NA
  PortfolioExposures[PortfolioExposures$Technology %in% goodtech,]$forrank<- PortfolioExposures[PortfolioExposures$Technology %in% goodtech,]$CarstenMetric_Port
  PortfolioExposures[PortfolioExposures$Technology %in% badtech,]$forrank<- 1- PortfolioExposures[PortfolioExposures$Technology %in% badtech,]$CarstenMetric_Port
  
  PortfolioExposures$forrank <- as.numeric(PortfolioExposures$forrank)
  
  # ranking
  # smallest number is number 1
  PortfolioExposures<-PortfolioExposures %>%
    group_by(Technology) %>%
    mutate(my_ranks = order(order(forrank,decreasing = TRUE)),
           mx = max(my_ranks))
  #order(forrank,decreasing=TRUE),

  # colnames(rankingtable)[1] <- "PortName"
  rankingtable <- subset(PortfolioExposures, select = c(PortName ,Technology, my_ranks,mx))
  # rankportfolio <- rankingtable[rankingtable$PortName == PortName,]
  
  return(rankingtable)
}

#------------ Clean O&G Data ---------------- #
cleanOGData <- function(OGData,Startyear){
  techlist <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
  OilData <- subset(OGData, OGData$Technology %in% "Oil" & OGData$Year %in% (Startyear+5))  
  OilData$Resource.Type[!OilData$Resource.Type %in% techlist] <- "Other & Unknown"
  OilData$Resource.Type[OilData$Resource.Type %in% "Other"] <- "Other & Unknown"
  return(OilData)
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
#matchOS <- function(OSdata, PortSnapshot){
  
#  OSdatared <- unique(subset(OSdata, select = c("Issuer","Sector","ISIN","Weighted.Emissions.Factor","Weighted.Target.Emissions.Factor")) )
  
#  PortSnapshot$Sector <- NULL
  
#  OS <- merge(PortSnapshot,OSdatared,by = "ISIN")
  
#  WEF <- ddply(OS,.(PortName,Sector),summarise,
#               EmissionsFactor =weighted.mean(Weighted.Emissions.Factor,AUM),
#               TargetEmissionsFactor =weighted.mean(Weighted.Target.Emissions.Factor,AUM))
  # Returns the WEF for each company by sector
#  return(WEF)    
#}

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
  #tech
  # RenewablesColour <<- "#8cd98c"
  # HydroColour <<- "#6abaff"
  # NuclearColour <<- "#ae89c5"
  # GasCapColour <<- "#a6cad8"
  # CoalCapColour <<- "#1a3577"
  # ElectricColour <<- "#847577"
  # HybridColour <<- "#a6a2a2"
  # ICEColour <<-"#e5e6e4"
  # OilProdColour <<-"#00677f"
  # GasProdColour <<-"#a7c5d1"
  # CoalProdColour <<- "#004335"
  # 
  # #sector
  # energy<<-"#0090b2"
  # pow <<- "#2348a1"
  # trans<<- "#cfd2cd"
  # othr<<- "#9793c6"

  ## orangey/brown
  RenewablesColour <<- "#feedde"
  HydroColour <<- "#fdbe85"
  NuclearColour <<- "#fd8d3c"
  GasCapColour <<- "#e6550d"
  CoalCapColour <<- "#a63603"
  
  # purpleish
  ElectricColour <<- "#efedf5"
  HybridColour <<- "#bcbddc"
  ICEColour <<-"#756bb1"

  #goldish
  GasProdColour <<- "#D9DDD4" #"#F5F5F5" #D9DDD4
  OilProdColour <<- "#BEBCAE"       #"#BEA07B" #BEBCAE
  CoalProdColour <<-  "#8B7E66" # "#8C510A" #8B7E66
  
  #sector
  energy<<-"#8B7E66"
  pow <<- "#a63603"
  trans<<- "#756bb1"
  
  othr<<- "#9793c6"
  
    
  #trajectory
  area_6 <<- "#e07b73"
  area_4_6 <<- "#fde291"
  area_2_4 <<- "#FFFFCC"
  area_2 <<- "#c3d69b"
  eq_port <<- "#1056ff"
  
 
  #stock_market<<- "black"
  peer_group <<- "black"

  eq_line <<- "#a63603" # same as CoalCapColor
  cb_line <<- "#756bb1" # same as ICEColour

#   eq_line <<- "#897D54"
#   cb_line <<- "#59629A"

  #moodys
  HighRisk <<- area_6
  MedRisk <<- area_4_6
  LowRisk <<- area_2_4

  #text size
  textsize <<- 8.5
  textcolor <<- "#3D3D3C"
  AxisColour <<- "#3D3D3C"
  
  YourportColour <<- "#265b9b"   #"#2e4b6e"  #"#17224D"
  IndexColour <<-  "grey85"
  Tar2DColourBar <<- "#b3de69"
  Tar2DColour <<- "#a1c75e"
  goodexpColour <<- "#1F891F"
  badexpColour <<- "#ed1c24" #"#fb8072"
  ReqCapColour <<- "grey55"
  CurrCapColour <<- "grey75"
  # AxisColour <<- "#17375e" #"#274F80"
  
  technology_order <<- c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap",
                         "Electric","Hybrid","ICE","Gas","Oil","Coal")
  
  ColourPalette <<- data.frame(Sector = c("Power","Power","Power","Power","Power",
                                          "Automotive","Automotive","Automotive",
                                          "Fossil Fuels","Fossil Fuels","Fossil Fuels"),
                               Technology = technology_order,
                               Colours =c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,
                                          ElectricColour,HybridColour,ICEColour,GasProdColour,OilProdColour,CoalProdColour))
  ColourPalette$Technology <- factor(ColourPalette$Technology, levels=technology_order)
  ColourPalette <- arrange(ColourPalette, Technology)
  
  linewidth_in <<- 390 / 72.27
  linesize <<- 0.8
  ppi <<- 600
  
}

#-------- Green/Brown Tech ------------- #
GreenBrown <- function(Tech){
  GreenTechs <- c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap")
  
  if(Tech %in% GreenTechs){
    TechIs <- "green"}else{
      TechIs <- "brown"  }
  
  return(TechIs)
}
