# --------
# REPORTING FUNCTIONS
# --------

#----------- Creates the figure list for the report
figure_list <- function(figurelist){
  figurelist <- as.data.frame(list.files(LANGUAGE.PATH,pattern=c("\\.png$"), full.names = FALSE))
  colnames(figurelist)<- "FigName"
  figurelist$ordernumber <- as.numeric(gsub("[A-z _ .]","",figurelist$FigName))
  figurelist <- figurelist[order(figurelist$ordernumber),]  
  figurelist$ordernumber <- NULL
  figurelist$FigName <- gsub(".png","",figurelist$FigName)
  
  write.table(figurelist,"FigureList.txt",row.names = FALSE, col.names = FALSE)
}

# ------------- REPORT DATA ----------------- #
report_data <- function(ChartType){
  
  if (ChartType =="EQ"){
    combin <- EQCombin
    Exposures<-EQExposures
    AUMData<-EQAUMData
    Ranks<-EQRanks
    PortSnapshot<-EQPortSnapshot
  }else if (ChartType == "CB"){
    combin <- CBCombin
    Exposures <- CBExposureRange
    AUMData <-CBAUMDatarange
    Ranks<-CBRanks
    PortSnapshot<-CBPortSnapshot
  }
  
  
  if (nrow(combin)>0){
    
    PortSnapshot <- rename(PortSnapshot, c("IssLvlPortWeight"="PortWeight"),warn_missing = FALSE)
    # Pie Share Data
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    # Numbers to print
    PieAssessedShare <- round((sum(pieshares$Portfolio_weight)-pieshares$Portfolio_weight[pieshares$piesector %in% "Not Assessed"]),2)*100
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
    TechList <- c("Electric","Hybrid","ICE","Coal","Oil","Gas","RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap")
    
    AUMData <- subset(AUMData, select = c("PortName","PortAUM"))
    Exposures <- Exposures[, -which(colnames(Exposures) %in% c("ComparisonType","Type"))]
    
    df <- Exposures
    df <- merge(df,AUMData, by= "PortName")
    df <- rename(x = df, c("PortAUM"="AUM"),warn_missing = FALSE)
    
    WM<- as.data.frame(lapply(df[colnames(df) %in% TechList], weighted.mean, na.rm=TRUE,  w = df$AUM))
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
report <- function(){
  
  PORTFOLIONAME <- toupper(ReportName)
  
  # Copy in the template for the report
  text <- as.data.frame(template,stringsAsFactors = FALSE)  
  colnames(text) <- "text"
  
  
  removetextlines <- function(handlename){
    startpage <- which(grepl(paste0(handlename,"S"),text$text))
    endpage <- which(grepl(paste0(handlename,"E"),text$text))
    
    if (length(startpage) >0 ){
      
      removelist <- lapply(1:length(startpage), function(x) c(startpage[c(x)]:endpage[c(x)]))
      removelist <- melt(removelist[1:length(startpage)])
      text <- as.data.frame(text[-removelist$value,],stringsAsFactors =FALSE)
      colnames(text) <- "text"
    }else{
      removeline <- which(grepl(handlename,text$text))
      text <- as.data.frame(text[-removeline,],stringsAsFactors =FALSE)
      colnames(text) <- "text"
    }
    return(text)
  }
  
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
  
  # Update the template to reflect figure names
  FigNames<-as.data.frame(readLines("FigureList.txt",skipNul = TRUE))
  colnames(FigNames) <- "Name"
  FigNames$Name <- gsub("\"","",as.character(FigNames$Name))
  FigNames$Fig <- substring(FigNames$Name,1,2)
  FigNames$Fig <- paste0("SwissFigures/Fig",FigNames$Fig)
  
  for (f in 1:nrow(FigNames)){
    text$text <- gsub(FigNames$Fig[f],FigNames$Name[f],text$text,fixed = TRUE)
  }
  
  RenewAdds<-0
  if (length(grep("Fig51", FigNames$Fig))>0){RenewAdds <- 1}
  
  # Check for each technology, 
  techpageremoval <- data.frame("PowerEQ"=EQSectorProd$Production[EQSectorProd$Sector == "Power"],
                                "PowerCB"=CBSectorProd$Production[CBSectorProd$Sector == "Power"],
                                "AutomotiveEQ"=EQSectorProd$Production[EQSectorProd$Sector == "Automotive"],
                                "AutomotiveCB"=CBSectorProd$Production[CBSectorProd$Sector == "Automotive"],
                                "FossilFuelsEQ"=1,
                                "FossilFuelsCB"=CBSectorProd$Production[CBSectorProd$Sector == "Fossil Fuels"])
  removesectors <- colnames(techpageremoval[which(techpageremoval == 0)]) 
  
  # removes the sectors  
  if(length(removesectors)>0){
    for (i in 1:length(removesectors)){
      text <- removetextlines(removesectors[i])
    }}
  
  # removes bond pages
  if (nrow(CBReportData)==0){
    text <- removetextlines("CBPage")
    text <- removetextlines("CBPie")
  }
  
  # removes equity pages
  if (nrow(EQReportData)==0){
    # pages <- c(9,11,13,15)
    text <- removetextlines("EQPage")
    text <- removetextlines("EQPie")
    text <- removetextlines("RenewAddsOut")
    
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- "\t\\vspace{-2.9cm} %renewspacingworkaround"
  }
  
  # removes renewable chart 
  if (RenewAdds==0 & nrow(EQReportData)>0){
    text <- removetextlines("RenewAddsOut")
    
    renewvspace<- which(grepl("renewspacingworkaround",text$text))
    text$text[renewvspace] <- gsub(".9cm","2.9cm",text$text[renewvspace])
  }
  
  # removes Fund Page
  if (typeof(FundsInPort)!="list"){
    text <- removetextlines("FundCheck")
  }
  
  # removes Other Sector Pages - materials
  if ((OtherSectors$Steel+OtherSectors$Cement==0)){
    text <- removetextlines("OtherSectorsMaterial")
  }
  
  # removes Other Sector Pages - transportation
  if ((OtherSectors$Aviation+OtherSectors$Shipping==0)){
    text <- removetextlines("OtherSectorsTransport")
  }  
  
  # Set Report Language
  replacelist <- colnames(RT)  
  for (i in 1:length(replacelist)){
    text$text <- gsub(replacelist[i],RT[replacelist[i]][[1]], text$text)
  }
  
  
  
  text$text <- gsub("SamplePort",PortfolioName,text$text)
  text$text <- gsub("SAMPLEPORT",PORTFOLIONAME,text$text)
  text$text <- gsub("CO2","CO\\\\textsubscript{2}",text$text)
  text$text <- gsub("Â°","°",text$text)
  
  if (Languagechoose == "DE"){
    text$text[grepl("KLIMAVER",text$text)][1]<- "KLIMAVERTRÄGLICHKEITS-PILOTTEST"
  }
  
  if (Languagechoose == "FR"){
    text$text[grepl("TEST PILOTE DE COMPATIBILITÉ  CLIMATIQUE",text$text)] <- "TEST PILOTE\\\\ DE COMPATIBILITÉ  CLIMATIQUE"
    text$text[grepl("POSSIBILIT",text$text)][1]<- "\\SectionHeading{PARTIE 3:}{POSSIBILITÉS D'ACTION}"
    text$text[grepl("POSSIBILIT",text$text)][2]<- "\\PageHeading{POSSIBILITÉS D'ACTION - SÉLECTION DES FONDS}"
  }
  
  # Copy in the graphics folder for the report
  originalloc <- paste0(TEMPLATE.PATH,"ReportGraphics/")
  graphicsloc <- paste0(LANGUAGE.PATH ,"/","ReportGraphics/")
  flist <- list.files(originalloc, full.names = TRUE)
  
  if(!dir.exists(file.path(graphicsloc))){
    dir.create(file.path(graphicsloc), showWarnings = TRUE, recursive = FALSE, mode = "0777")
    for (file in flist){file.copy(file, graphicsloc)}
  }  
  
  # Save the template file
  TemplateNameNew <- paste0("Template_",PortfolioName,"_",Languagechoose)
  write.table(text, paste0(TemplateNameNew,".Rnw"),col.names = FALSE,row.names = FALSE,quote=FALSE,fileEncoding = "UTF-8")  
  
  # Create the PDF
  knit2pdf(paste0(LANGUAGE.PATH,TemplateNameNew,".Rnw"),compiler = "xelatex", encoding = 'UTF-8')
  
  # knit(input = paste0(LANGUAGE.PATH,TemplateNameNew,".Rnw"), output = "test.tex")
  # tinytex::xelatex("test.tex")
  # 
  
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



# --------
# PLOT FUNCTIONS
# -------- 

# ------------ Other Sector Plots------------ #
other_sector_chart <- function(plotnumber, SectorToPlot){
  
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
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
shipping_chart <- function(plotnumber, SectorToPlot="Shipping"){
  
  
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
    
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    # 
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
pie_chart <- function(plotnumber,ChartType){
  
  if (ChartType == "EQ"){
    PortSnapshot <- EQPortSnapshot
  }else if(ChartType == "CB"){
    PortSnapshot <- CBPortSnapshot
  }
  
  
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
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

#------------- SECTOR BAR CHARTS ------------ #
sector_bar_chart <- function(plotnumber){
  
  
  
  # Bar chart of the Sector Weights in the portfolio for both CB and EQ
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(),
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),#element_text(face="bold",colour="black",size=textsize),
          axis.line.x = element_line(colour = "black",size=1),
          axis.line.y = element_blank(),
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
  
  PSSProcessing <- function(ChartType){
    if (ChartType == "EQ"){
      PortSnapshot <- EQPortSnapshot
    }else if(ChartType == "CB"){PortSnapshot <- CBPortSnapshot}
    
    colnames(PortSnapshot)[colnames(PortSnapshot) %in% "IssLvlPortWeight"] <- "PortWeight"
    PortSnapshotSub <- subset(PortSnapshot, CNTRY_OF_DOMICILE %in% IndexUniverses[,names(IndexUniverses) == eval(paste0(CompanyDomicileRegionchoose,"_ISO"))])
    piesub_tech <- unique(subset(PortSnapshotSub,select=c("ISIN","piesector","PortWeight")))
    
    piesub_tech$piesector<-gsub("NonUtility Power", "Non-Utility Power", piesub_tech$piesector)
    piesub_tech$piesector[is.na(piesub_tech$piesector)] <- "Not Assessed"
    
    piesub_tech$piesector <- revalue(piesub_tech$piesector,c("Metal-Iron" = "Iron & Steel","NonOG Production" = "Fossil Fuels","Bldg Prod-Cement/Aggreg" = "Building Materials & Fixtures", "Oil&Gas"= "Fossil Fuels","Coal"="Fossil Fuels", "Transport-Marine" = "Marine Transportation","Metal-Aluminum"="Aluminum", "Steel-Producers" = "Iron & Steel", "Transport-Air Freight"= "Airlines"),warn_missing = FALSE)
    
    ### New Classifications ###
    piesub_tech$piesector <- revalue(piesub_tech$piesector,c("Metal-Iron" = "Other High Carbon Sectors","NonOG Production" = "Fossil Fuels","Bldg Prod-Cement/Aggreg" = "Other High Carbon Sectors", "Oil&Gas"= "Fossil Fuels","Coal"="Fossil Fuels", "Transport-Marine" = "Other High Carbon Sectors","Metal-Aluminum"="Other High Carbon Sectors", "Steel-Producers" = "Other High Carbon Sectors", "Transport-Air Freight"= "Other High Carbon Sectors"),warn_missing = FALSE)
    piesub_tech$piesector <- revalue(piesub_tech$piesector, c("Building Materials & Fixtures"= "Other High Carbon Sectors", "Iron & Steel" = "Other High Carbon Sectors", "Aluminum" = "Other High Carbon Sectors", "Airlines" = "Other High Carbon Sectors", "Marine Transportation" = "Other High Carbon Sectors"),warn_missing = F)
    
    
    piesub_tech <- piesub_tech[!(piesub_tech$piesector=="Not Assessed"),]
    
    pieshares <- ddply(piesub_tech, .(piesector),summarize,Portfolio_weight=sum(PortWeight, na.rm=TRUE))
    
    if (ChartType == "EQ"){pieshares$label <- "Equity Portfolio"
    }else if(ChartType == "CB"){pieshares$label <- "Corporate Bond Portfolio"}
    
    return(pieshares)
  }
  
  piesharesEQ <- PSSProcessing("EQ") 
  
  if(!exists("piesharesEQ")){pieshares <- PSSProcessing("EQ") 
  }  else{
    piesharesCB <- PSSProcessing("CB") 
    pieshares <- rbind(piesharesEQ,piesharesCB)}  
  
  ### Need to be changed!
  Palette <- c("blue","red","orange","brown","cyan","green","black","pink","grey","magenta")
  
  colourdf <- data.frame(piesector=unique(pieshares$piesector),colour=Palette[1:nrow(pieshares)])
  pieshares <- merge(pieshares, colourdf, by= "piesector")  
    
  ggplot(pieshares, aes(x=label, y=Portfolio_weight,fill=piesector),show.guide = TRUE)+
    geom_bar(stat = "identity",width = .6)+
    theme_minimal()+
    scale_fill_manual(labels=unique(as.character(pieshares$piesector)),values=unique(as.character(pieshares$colour)))+
    scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
    expand_limits(0,0)+
    guides(fill=guide_legend(nrow = 1))+
    ylab(ylabel)+
    theme_barcharts()+
    theme(legend.position = "bottom")

}

# -------------STACKED BAR CHARTS ---------- #
stacked_bar_chart_data <- function(ChartType){
  
  if (ChartType == "EQ"){
    combin <- EQCombin
    WeightedResults <- EQWMCoverageWeight
    
  }else if (ChartType == "CB"){
    combin <- CBCombin
    WeightedResults <- CBWMCoverageWeight
  }
  
  WeightedResults$PortName <- NULL
  
  # Test to check for results; If == 0, no chart is printed. 
  PlotChart <-nrow(combin)
  
  if(PlotChart>0){
    
    combin <- combin[!combin$Technology %in% "OilCap",]                  # While there are Oil power production results, we do not use them. 
    # combin <- combin[, -which(colnames(combin) %in% c("ComparisonType","Type"))]
    
    # WeightedResults <- WeightedResults[, -which(colnames(WeightedResults) %in% c("ComparisonType","Type","PortName"))]
    WeightedResults <- subset(WeightedResults, select = c("Technology","CoverageWeight"))
    
    
    if (ChartType=="EQ"){
      
      ### Equity Results Processing
      
      ProductionMix_5yrs <- subset(combin, Year==(Startyear+5) & BenchmarkRegion==BenchmarkRegionchoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & Scenario == Scenariochoose)
      
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c("InvestorName","PortName","Sector","Technology","Production", "TargetProductionAlignment","TargetProductionAUMIntensity"))  
      
      
      ### For Power and Automotive, the Reference Production (Benchmark) is the TargetProductionAlignment; for FF it's based of the TargetProductionAUMIntensity
      ProductionMix_5yrs$RefTechProd <- ProductionMix_5yrs$TargetProductionAlignment
      
      ### Convert the Production values to Energy (rather than units of coal, gas or oil (kt, m3, barrels))
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Coal"]*24
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Oil"]*6.12
      ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$Production[ProductionMix_5yrs$Technology == "Gas"]*0.0372
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Coal"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Coal"]*24
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Oil"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Oil"]*6.12
      ProductionMix_5yrs$RefTechProd[ProductionMix_5yrs$Technology == "Gas"]<- ProductionMix_5yrs$TargetProductionAUMIntensity[ProductionMix_5yrs$Technology == "Gas"]*0.0372
      
      ProductionMix_5yrs$TargetProductionAlignment <-ProductionMix_5yrs$TargetProductionAUMIntensity <- NULL
      
      ProductionMix_5yrs <- ddply(ProductionMix_5yrs, .(Sector, Technology), summarise,
                                  PortProduction= sum(Production),
                                  RefProduction = sum(RefTechProd))
      
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c( "Sector","Technology","PortProduction","RefProduction","CoverageWeight"))
      ProductionMix_5yrs <- melt(ProductionMix_5yrs, id = c( "Technology","Sector"))
      SectorTotals <- ddply(ProductionMix_5yrs,.(Sector,variable), summarise,SectorTotal = sum(value))
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,SectorTotals)
      
      ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$value/ProductionMix_5yrs$SectorTotal
      
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select= c("Sector","Technology","variable","TechShare"))
      ProductionMix_5yrs$Technology <- gsub("Cap","",ProductionMix_5yrs$Technology)
      ProductionMix_5yrs$variable <- as.character(ProductionMix_5yrs$variable)
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "PortProduction"] <- PortfolioNameLong
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "RefProduction"] <- GT["X2Target"][[1]]
      ProductionMix_5yrs$variable[ProductionMix_5yrs$variable %in% "CoverageWeight"] <- GT["AveragePort"][[1]]
      
      ProductionMix <- ProductionMix_5yrs
      
    }else{
      
      ### Corporate Bonds Results Processing ### 
      
      ### FOSSIL FUELS
      FFMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose & Sector %in% c("Oil&Gas","Coal"))
      
      FFMix_5yrs$TechShare <- FFMix_5yrs$SectorWeight
      FFMix_5yrs$TechShare[FFMix_5yrs$Sector %in% "Oil&Gas"] <- FFMix_5yrs$SectorWeight[FFMix_5yrs$Sector %in% "Oil&Gas"]*FFMix_5yrs$PortTechShare[FFMix_5yrs$Sector %in% "Oil&Gas"]
      TSSUM <- sum(FFMix_5yrs$TechShare, na.rm = TRUE)
      FFMix_5yrs$TechShare <- FFMix_5yrs$TechShare/TSSUM 
      
      MarketTechShareOGSum <- sum(FFMix_5yrs$RegWtProjMarketProd[FFMix_5yrs$Sector %in% "Oil&Gas"],na.rm = TRUE)
      FFMix_5yrs$MarketTechShareOG <- FFMix_5yrs$RegWtProjMarketProd/MarketTechShareOGSum
      
      FFMix_5yrs$TechShareMarket <- FFMix_5yrs$SecWtMarket
      FFMix_5yrs$TechShareMarket[FFMix_5yrs$Sector %in% "Oil&Gas"]<- FFMix_5yrs$SecWtMarket[FFMix_5yrs$Sector %in% "Oil&Gas"]*FFMix_5yrs$MarketTechShareOG[FFMix_5yrs$Sector %in% "Oil&Gas"]
      TSSUMMarket  <- sum(FFMix_5yrs$TechShareMarket, na.rm = TRUE)
      FFMix_5yrs$TechShareMarket <-FFMix_5yrs$TechShareMarket/TSSUMMarket 
      
      FFMix_5yrs <- unique(subset(FFMix_5yrs, select = c("Technology","TechShare","TechShareMarket")))
      
      FFWeightedResults <- subset(WeightedResults, Technology %in% FFMix_5yrs$Technology)
      sumWR <- sum(FFWeightedResults$CoverageWeight, na.rm = TRUE)
      FFWeightedResults$CoverageWeight <- FFWeightedResults$CoverageWeight/sumWR
      
      FFMix_5yrs <- merge(FFMix_5yrs,FFWeightedResults, by="Technology")
      FFMix_5yrs <- rename(FFMix_5yrs, c("TechShareMarket"=GT["X2Target"][[1]],
                                         "TechShare"=PortfolioNameLong,
                                         "CoverageWeight"=GT["AveragePort"][[1]]),warn_missing = FALSE)
      
      
      FFMix_5yrs <- melt(FFMix_5yrs, id.vars = c("Technology"))
      FFMix_5yrs$Sector <- "Fossil Fuels"
      FFMix_5yrs <- rename(FFMix_5yrs, c("value"="TechShare"))
      
      FFMix_5yrs$TechShare[is.nan(FFMix_5yrs$TechShare)] <- 0
      FFMix_5yrs <- subset(FFMix_5yrs, select = c("Sector","Technology","variable","TechShare"))
      
      ### NON FOSSIL FUELS
      ProductionMix_5yrs <- subset(combin, Year==Startyear+5 & BenchmarkRegion==BenchmarkRegionchoose &  Scenario == Scenariochoose & Sector %in% c("Automotive","Power"))
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select=c("Sector","Technology","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by=c("Technology"))
      ProductionMix_5yrs <- rename(ProductionMix_5yrs, 
                                   c("WtTechShareTechShare"=PortfolioNameLong,
                                     "Benchmark_WtTechShareTechShare"=GT["X2Target"][[1]],
                                     "CoverageWeight"=GT["AveragePort"][[1]]),warn_missing = FALSE)
      
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
      
      tsharesum <- ddply(ProductionMix_5yrs, .(Sector,variable), summarise, SectorTotal =sum(TechShare, na.rm = TRUE))
      ProductionMix_5yrs <- merge(ProductionMix_5yrs,tsharesum, by= c("Sector","variable"))
      ProductionMix_5yrs$TechShare <- ProductionMix_5yrs$TechShare/ProductionMix_5yrs$SectorTotal 
      ProductionMix_5yrs$SectorTotal<- NULL
      
      ### Merge the FF Results back in
      
      ProductionMix <- rbind(ProductionMix_5yrs, FFMix_5yrs)
      
    }
  } else{
    ProductionMix <- 0
  }
  
  
  return(ProductionMix)
  
  
  
}

stacked_bar_chart_vertical <- function(plotnumber,ChartType,SectorToPlot,Production){
   wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
   wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  theme_barcharts <- function(base_size = textsize, base_family = "") {
    theme(axis.ticks=element_blank(),
          axis.text.x=element_text(face="bold",colour="black",size=textsize),
          axis.text.y=element_text(face="bold",colour="black",size=textsize),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),#element_text(face="bold",colour="black",size=textsize),
          axis.line.x = element_line(colour = "black",size=1),
          axis.line.y = element_blank(),
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
  
  
  if(nrow(Production)>0){
    ylabel <- GT["StackedBarYLabel_FF"][[1]]
    technologyorder <- c("Coal","Gas","Nuclear","Hydro","Renewables","Electric","Hybrid","ICE","Coal","Gas","Oil")
    colours <- c(CoalCapColour,"#afabab",NuclearColour,HydroColour,RenewablesColour,ElectricColour,HybridColour,ICEColour,CoalProdColour,GasProdColour,OilProdColour)
    eng <- c("Power","Automotive","Fossil Fuels")
    sectororder<-rep(eng, times=c(5,3,3))
    colourdf <- data.frame(colours, Technology = technologyorder,Sector= sectororder)
    colourdf$Technology<-as.factor(colourdf$Technology)
    colourdf$Sector<-as.factor(colourdf$Sector)
    Production$Technology<-as.factor(Production$Technology)
    Production$Sector<-as.factor(Production$Sector)
    
    # Production <- right_join(mutate(Production, Technology=factor(Technology, levels=combined),Sector=factor(Sector, levels=combined1)),
    # mutate(colourdf, Technology=factor(Technology, levels=combined),Sector=factor(Sector, levels=combined1)),by=c("Technology","Sector"))
    
    combined <- sort(union(levels(Production$Technology), levels(colourdf$Technology)))
    combined1 <- sort(union(levels(Production$Sector), levels(colourdf$Sector)))
    
    library(dplyr,warn.conflicts = F)
    Production <- right_join(mutate(Production, Technology=factor(Technology, levels=combined),Sector=factor(Sector, levels=combined1)),
                                     mutate(colourdf, Technology=factor(Technology, levels=combined),Sector=factor(Sector, levels=combined1)),by=c("Technology","Sector"))
    detach("package:dplyr", unload=TRUE)
    
    
    
    orderofchart <- c(PortfolioNameLong,GT["X2Target"][[1]],GT["AveragePort"][[1]])
    Production$variable <- factor(Production$variable, levels=orderofchart)
    #Production$Technology <- factor(Production$Technology, levels=technologyorder)
    Production <- Production[order(Production$Technology,Production$variable),]
    Production$variable <- wrap.labels(Production$variable,20)
    
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Production, Sector == SectorToPlot)
      chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
      dat$variable <- factor(dat$variable, levels=chartorder)
      p1<- ggplot(data=dat, aes(x=variable, y=TechShare,fill=Technology),show.guide = TRUE)+
            geom_bar(stat = "identity",width = .6)+
            theme_minimal()+
            scale_fill_manual(labels=unique(as.character(dat$Technology)),values=unique(as.character(dat$colours)))+
            scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
            expand_limits(0,0)+
            guides(fill=guide_legend(nrow = 1))+
            ylab(ylabel)+
            theme_barcharts()+
            ggtitle(paste0(unique(as.character(dat$Sector)),"Production"))+
            theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                  axis.line.y = element_blank(),axis.text.y = element_blank(),axis.line.x = element_blank())
      print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){SectorToPlot == "FossilFuels"}
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
    }else if (SectorToPlot == "TechToPlot"){
      dat<- subset(Production,Sector=="Automotive")
      chartorder1 <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
      dat$variable <- factor(dat$variable, levels=chartorder1)
        p1<- ggplot(data=dat, aes(x=variable, y=TechShare,fill=Technology),show.guide = TRUE)+
          geom_bar(stat = "identity",width = .6)+
          theme_minimal()+
          scale_fill_manual(labels=unique(as.character(dat$Technology)),values=unique(as.character(dat$colours)))+
          scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
          expand_limits(0,0)+
          guides(fill=guide_legend(nrow = 1))+
          ylab(ylabel)+
          theme_barcharts()+
          ggtitle("Automotive Production")+
          theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                axis.line.y = element_blank(),axis.text.y = element_blank(),axis.line.x = element_blank())

        dat1<- subset(Production,Sector=="Fossil Fuels")
        chartorder2 <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
        dat$variable <- factor(dat$variable, levels=chartorder2)
        p2 <- ggplot(dat1, aes(x=variable, y=TechShare,fill=Technology),show.guide = TRUE)+
          geom_bar(stat = "identity",width = .6)+
          theme_minimal()+
          scale_fill_manual(labels=unique(as.character(dat1$Technology)),values=unique(as.character(dat1$colours)))+
          scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
          expand_limits(0,0)+
          guides(fill=guide_legend(nrow = 1))+
          ylab(ylabel)+
          theme_barcharts()+
          ggtitle("Fossil Fuels Production")+
          theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                axis.line.y = element_blank(),axis.text.y = element_blank(),axis.line.x = element_blank())

        dat2<- subset(Production,Sector=="Power")
        chartorder3 <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
        dat$variable <- factor(dat$variable, levels=chartorder3)
        p3 <- ggplot(dat2, aes(x=variable, y=TechShare,fill=Technology),show.guide = TRUE)+
          geom_bar(stat = "identity",width = .6)+
          theme_minimal()+
          scale_fill_manual(labels=unique(as.character(dat2$Technology)),
                            values=unique(as.character(dat2$colours)))+
          scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
          expand_limits(0,0)+
          guides(fill=guide_legend(nrow = 1))+
          ylab(ylabel)+
          theme_barcharts()+
          ggtitle("Power Capacity")+
          theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom",
                axis.line.y = element_blank(),axis.text.y = element_blank(),axis.line.x = element_blank())
        # print(grid.arrange(p2,p3,p1,nrow=1))
        ggsave(grid.arrange(p2,p3,p1,nrow=1),filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=7.8,dpi=ppi)
      
    }
  }else{
    Label <- paste0("No",ChartType,gsub(" ","",SectorToPlot))
  #  Label <- GT[Label][[1]]
    
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
    print(outputplot)
    if(SectorToPlot == "Fossil Fuels"){SectorToPlot<- "FossilFuels"}
    ggsave(outputplot,filename=paste0(plotnumber,"_","PortfolioName","_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,dpi=ppi)
  }
}

stacked_bar_chart_horizontal <- function(plotnumber,ChartType,SectorToPlot,inc_average=T){
  

  if (ChartType == "EQ"){
    combin <- EQCombin
    WeightedResults <- EQWMCoverageWeight
    
  }else if (ChartType == "CB"){
    combin <- CBCombin
    WeightedResults <- CBWMCoverageWeight
  }
  
  
  
  
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
    combin <- combin[, -which(colnames(combin) %in% c("ComparisonType","Type"))]
    WeightedResults <- WeightedResults[, -which(colnames(WeightedResults) %in% c("ComparisonType","Type","PortName"))]
    
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
      ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c( "Year","Technology","Scenario","Sector","PortProduction","RefProduction","CoverageWeight"))
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
        
        ProductionMix_5yrs <- unique(subset(ProductionMix_5yrs, select = c("Technology","TechShare","TechShareMarket")))
        
        WeightedResults <- subset(WeightedResults, Technology %in% ProductionMix_5yrs$Technology)
        sumWR <- sum(WeightedResults$CoverageWeight, na.rm = TRUE)
        WeightedResults$CoverageWeight <- WeightedResults$CoverageWeight/sumWR
        
        ProductionMix_5yrs <- merge(ProductionMix_5yrs,WeightedResults, by="Technology")
        ProductionMix_5yrs <- rename(ProductionMix_5yrs, c("TechShareMarket"=GT["X2Target"][[1]],"TechShare"=PortfolioNameLong,"CoverageWeight"=GT["AveragePort"][[1]]),warn_missing = FALSE)
        # ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c( "Year","Technology","Scenario","Sector","PortProduction","RefProduction","CoverageWeight"))
        
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
        # ProductionMix_5yrs <- subset(ProductionMix_5yrs, select = c( "Year","Technology","Scenario","Sector","PortProduction","RefProduction","CoverageWeight"))
        
        
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
    
    
    
    ### Add or Remove Average Portfolio Results ####
    if (inc_average == F){
      PlotData <- subset(PlotData, !PlotData$variable == GT["AveragePort"][[1]])
    }
    ################################################
    
    
    PlotData <- merge(PlotData,colourdf, by="Technology")
    orderofchart <- c(GT["X2Target"][[1]],PortfolioNameLong,GT["AveragePort"][[1]])
    PlotData$variable <- factor(PlotData$variable, levels=orderofchart)
    PlotData$Technology <- factor(PlotData$Technology, levels=technologyorder)
    PlotData <- PlotData[order(PlotData$Technology,PlotData$variable),]
    PlotData$variable <- wrap.labels(PlotData$variable,20)
    
    # PlotData$variable <- revalue(PlotData$variable,c("AggregiertesPortfolio" = GT["AggregatedPortName"][[1]]))
    
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
mini_line_chart <- function(plotnumber,ChartType,TechToPlot, SectorToPlot){
  
  # combin <- EQCombin
  # TechToPlot <- "CoalCap"
  # SectorToPlot <- "Power"
  # ChartType <- "EQ"
  
  
  if (ChartType == "EQ"){
    combin <- EQCombin
    
  }else if (ChartType == "CB"){
    combin <- CBCombin
  }
  
  
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
  
  # if ((sum(production$Production, na.rm = TRUE)>0 | SectorToPlot == "Fossil Fuels") & nrow(combin)>0){
  
  if (nrow(combin)>0){
    
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
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
ranking_chart_alignment <- function(plotnumber,ChartType,SectorToPlot){

  
  if (ChartType == "EQ"){
    Exposures <- EQExposureRange
    AUMData <- EQAUMDatarange
    Ranks <-EQRanks
    
  }else if (ChartType == "CB"){
    Exposures <- CBExposureRange
    AUMData <- CBAUMData
    Ranks <-CBRanks
  }
  
  
  TechList <- c("Electric","Hybrid","ICE","Coal","Oil","Gas","RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap")
  
  
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
      
      WM<- as.data.frame(lapply(df[colnames(df) %in% TechList], weighted.mean, na.rm=TRUE,  w = df$AUM))
      WM$PortName <- PortfolioNameLong
      
      Rank <- Ranks[1,]
      Rank[1,]<-1
      Rank$PortName <- "Rank"
      maxrank <- 1
      
    }else{
      # AUMData <- 
      AUMData <- AUMData[,colnames(AUMData) %in% c("PortName","PortAUM") ]
      
      df <- merge(df,AUMData, by= "PortName")
      df <- rename(df, c("PortAUM"="AUM"),warn_missing = FALSE)
      
      WM<- as.data.frame(lapply(df[ colnames(df) %in% TechList], weighted.mean, na.rm=TRUE,  w = df$AUM))
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
    
    Mins <- colMins(as.matrix(df[colnames(df) %in% TechList]),na.rm = TRUE)
    Maxs <- colMaxs(as.matrix(df[colnames(df) %in% TechList]),na.rm = TRUE)
    MinMax <- as.data.frame(t(cbind(Mins,Maxs)))
    colnames(MinMax) <- colnames(df[colnames(df) %in% TechList])
    MinMax$PortName <- c("Minimum","Maximum")
    
    # row.names(WM) <- "WeightedMean"
    df$ComparisonType <- df$Type <- NULL
    
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
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
      # annotate(xmin=PlotData$WMloc-barwidth/2,xmax=PlotData$WMloc+barwidth/2,ymin=-bh/2+locations,ymax=bh/2+locations,geom = "rect", fill="darkgrey")+
      
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
flat_wheel_chart <- function(plotnumber,companiestoprint,ChartType, SectorToPlot){
  
  # ChartType<- "EQ"
  # SectorToPlot<-"Automotive"
  # AlloftheCompanies <- UtilityCompanies
  # AlloftheCompanies <- OGCarbonBudget
  # combin <- EQCombin
  # PortSnapshot <- EQPortSnapshot
  # companiestoprint<-20
  # combin<-EQCombin
  # SectorToPlot <-"OG"
  
  # ChartType<- "CB"
  # SectorToPlot<-"Power"
  # AlloftheCompanies <- OGCarbonBudget
  # PortSnapshot <- CBPortSnapshot
  # companiestoprint<-20
  # combin<-CBCombin
  
  
  if (ChartType == "EQ"){
    PortSnapshot <- EQPortSnapshot
    combin <- EQCompProdSnapshot
  } else if(ChartType == "CB"){
    PortSnapshot <- CBPortSnapshot
    combin <- CBCompProdSnapshot
  }
  
  
  if (SectorToPlot == "Power"){AlloftheCompanies <- UtilityCompanies}
  if (SectorToPlot == "Automotive"){AlloftheCompanies <- AutoCompanies}
  if (SectorToPlot == "OG"){AlloftheCompanies <- OGCarbonBudget}
  
  
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
      
      # labelling$item <- revalue(labelling$item,c(Pensionskassen = GT["Pensionfunds"][[1]]))
      
      
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

#------------- Data for HeatMap (Portfolio) --- #
heatmap_data <- function(EQDataInput, CBDataInput,FundsOrPort,PortName){
  
  # EQDataInput <- EQCombin
  # CBDataInput <- CBCombin
  
  BenchYear <- Startyear + 5
  # FundsData <- data.frame( "PortName"=character(),
  # "Technology"=character(),
  # "BenchmarkRegion"=character(),
  # "Exposure"=numeric()  )
  
  # if(nrow(EQDataInput) > 1){ 
  if (typeof(EQDataInput) == "list"){ if (nrow(EQDataInput) > 0){
    EQData <- subset(EQDataInput,Year == BenchYear & Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose & BenchmarkRegion == BenchmarkRegionchoose,
                     select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
    
    EQData$MarketExposure[EQData$Technology %in% c("Oil", "Gas", "Coal")] <- EQData$AUMExposure[EQData$Technology %in% c("Oil", "Gas", "Coal")]
    EQData$Exposure <- EQData$MarketExposure
    
    EQData <- EQData[,!names(EQData) %in%   c("MarketExposure",'AUMExposure')]
    EQData$Exposure <- as.numeric(EQData$Exposure)
    
    if (FundsOrPort == "Port"){ EQData$PortName <- "Equity"}
    
    FundsDataTemp <- EQData
  }}
  
  # if (nrow(CBDataInput) > 1 & nrow(CBDataInput) != NULL){
  if (typeof(CBDataInput) == "list"){ if (nrow(CBDataInput) > 0){
    CBData <- subset(CBDataInput, Year == BenchYear & Scenario == Scenariochoose ,
                     select = c("PortName","Technology","BenchmarkRegion","Exposure_WtTechShareTechShare", "Exposure_OGCMetrik"))
    
    CBData$Exposure_WtTechShareTechShare[CBData$Technology %in% c("Oil", "Gas", "Coal")] <- CBData$Exposure_OGCMetrik[CBData$Technology %in% c("Oil", "Gas", "Coal")]
    CBData$Exposure <- CBData$Exposure_WtTechShareTechShare
    
    CBData <- CBData[,!names(CBData) %in%   c("Exposure_WtTechShareTechShare",'Exposure_OGCMetrik')]
    CBData$Exposure <- as.numeric(CBData$Exposure)
    
    if (FundsOrPort == "Port"){CBData$PortName <- "Corporate Bonds"}
    
    if(exists("FundsDataTemp")){FundsDataTemp <- CBData}
    else{FundsDataTemp <- rbind(FundsData,CBData)}
  }}
  
  if(exists("FundsDataTemp")){return(FundsDataTemp)}
  
  
  
}

# ------------ FUND MAP --------------------- # 
fundmap_chart <- function(plotnumber,FundsData){
  
  # FundsData <- FundsInPort
  # FundsData <- EQBatchTest
  
  # EQDataInput <- EQCombin
  # CBDataInput <- CBCombin
  
  BenchYear <- Startyear + 5
  
  
  
  ### FundsData is the BatchTest ie. EquityAnalysisHeatmap-450S-only.csv
  ### Improve flexibility to include Corporate Bonds as well. 
  
  if(typeof(FundsData) == "list"){
    if(nrow(FundsData)>0){
      
      AxisColour = 'Black'
      textcolour = 'Black'
      geom.text.size = 2.5
      
      BrownList <-c("CoalCap", "GasCap", "OilCap", "Gas", "Oil", "Coal", "ICE")
      GreenList <-c("NuclearCap", "HydroCap", "RenewablesCap", "Electric","Hybrid")
      
      # Read Heatmap
      #  Heatmap <- read.csv(paste(OutputLocation,PortfolioHoldings,"/",HeatmapDate,"_",PortfolioHoldings,"_EquityAnalysisHeatmap-450S-only.csv",sep = ""),stringsAsFactors = FALSE, strip.white = TRUE)
      Heatmap <- FundsData #combin
      
      
      # Remove shitty name
      Heatmap$PortName <- gsub("[01]_.*","",Heatmap$PortName)
      if ("ISIN" %in% colnames(Heatmap)){Heatmap$PortName <- Heatmap$ISIN}
      techlist <- unique(Heatmap$Technology)
      techlist <- techlist[techlist != "OilCap"]
      techlistshort <- c("elec","hyb","ICE","coal","gas","oil","coac","gasc","hyd","nuc","ren")
      Techlist <- as.data.frame(t(rbind(techlist,techlistshort)))
      
      ################## Heatmap ########################
      
      #Select subset of results: Year, Scenario, Where the companies are located/the investment universe, and just funds, not brands. 
      # Heatmap <- subset(Results, Results$Year == BenchYear & Results$Scenario == Scenariochoose & CompanyDomicileRegion == CompanyDomicileRegionchoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
      # # No Companydomregion for Stoxx600
      # Heatmap <- subset(Results, Results$Year == BenchYear & Results$Scenario == Scenariochoose ,select = c("PortName","Technology","BenchmarkRegion","MarketExposure", "AUMExposure"))
      
      
      # # Rename the Brand FTSE to the fund FTSE350
      # Heatmap$PortName[Heatmap$PortName == "FTSE"] <- "FTSE350"
      
      # Use AUM Exposure method for fossel fuels
      # Heatmap$MarketExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")] <- Heatmap$AUMExposure[Heatmap$Technology %in% c("Oil", "Gas", "Coal")]
      # # After getting the AUM values, remove that vector from the dataframe
      # Heatmap <- Heatmap[,names(Heatmap) != 'AUMExposure']
      # Heatmap$MarketExposure <- as.numeric(Heatmap$MarketExposure)
      # Rename the technologies to be more reader friendly
      # Heatmap$Technology <- revalue(Heatmap$Technology, c("Gas" = "Gas\nProduction","Oil" = "Oil\nProduction", "Coal" = "Coal\nProduction", "Electric" = "Electric\nVehicles", "Hybrid" = "Hybrid\nVehicles", "ICE" = "ICE\nVehicles", "RenewablesCap" = "Renewable\nCapacity", "NuclearCap" = "Nuclear\nCapacity", "HydroCap" = "Hydro\nCapacity", "GasCap" = "Gas\nCapacity", "CoalCap" = "Coal\nCapacity"))
      Heatmap$Exposure <- Heatmap$Exposure*100
      
      #fill in values if some regions or technologies are not within the portfolio
      Heatmap <- Heatmap %>% complete(PortName, Technology, BenchmarkRegion) # Keeps N/As
      
      #Create colour bands/buckets
      #alligned
      Heatmap$ExposureColour <-"grey50"#'grey95'
      # Heatmap$ExposureColour[!is.na(Heatmap$MarketExposure)] <- 'grey95'
      
      # 'good' alignment
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 0 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< 0] <- "#FFFFFF"
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 5 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< -5] <- "#d2e7d2"
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 15 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< -15] <- "#a5cfa5"
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 25 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< -25] <- "#78b878"
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 50 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< -50] <- "#4ba04b"
      Heatmap$ExposureColour[Heatmap$Technology %in% GreenList & Heatmap$Exposure> 75 | Heatmap$Technology %in% BrownList & Heatmap$Exposure< -75] <- "#1f891f"
      
      # 'bad' alignment
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 0 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< 0 ] <- "#FFFFFF"
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 5 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< -5 ] <- "#fad7d3"
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 15 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< -15 ] <- "#f5afa8"
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 25 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< -25 ] <- "#f0877d"
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 50 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< -50 ] <- "#eb5f52"
      Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & Heatmap$Exposure> 75 | Heatmap$Technology %in% GreenList & Heatmap$Exposure< -75 ] <- "#e73827"
      
      
      # repval = 200
      # redgreen<- colorRampPalette(c("red","white", "darkgreen"))(repval) 
      # 
      # Heatmap$ExposureColour <- redgreen[(100+Heatmap$Exposure)]
      # Heatmap$ExposureColour[Heatmap$Technology %in% BrownList & !is.na(Heatmap$Exposure)] <- redgreen[(100+Heatmap$Exposure[Heatmap$Technology %in% BrownList & !is.na(Heatmap$Exposure)] )]
      
      #add '%' text
      Heatmap$ExposureText <-Heatmap$Exposure# remove text for auto sector
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
      # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
      # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
      
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
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
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
renewablesadditions_chart <- function(plotnumber,ChartType){
  
  # combin <- EQCombin
  # PortSnapshot <- EQPortSnapshot
  # combin <-  EQCombin
  # PortSnapshot<-CBPortSnapshot
  
  if (ChartType == "EQ"){
    PortSnapshot <- EQPortSnapshot
    combin <- EQCombin
  } 
  
  
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
      RenewablesBar <- RenewablesBar[!(RenewablesBar$variable == "StillRequired"),]
      
      RenewablesStillRequired$Name <- factor(RenewablesStillRequired$Name, levels = (RenewablesStillRequired$Name))
      
      Yaxislabel <- GT["REBuildout_axis"][[1]]
      stillreq <- GT["Remaining"][[1]]  # red
      remainlabel <- GT["Progress"][[1]]#GT["StillReq"][[1]]
      progresslabel <- GT["Remaining"][[1]]  #CORRECT #GT["StillReq"][[1]]#GT["X2Target"][[1]]
      Target <- GT["Progress"][[1]] # blue
      
      # theme_barcharts <- function(base_size = textsize, base_family = "") {
      #   theme(axis.ticks=element_blank(), 
      #         axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
      #         axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
      #         axis.title.x=element_blank(),
      #         axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
      #         axis.line = element_line(colour = AxisColour,size=1),
      #         panel.grid.major = element_blank(), 
      #         panel.grid.minor = element_blank(),
      #         panel.background = element_blank(), 
      #         legend.position=c(0.5,-.2),
      #         legend.direction="horizontal",
      #         legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
      #         legend.background = element_rect(fill = "transparent",colour = NA),
      #         legend.key.size=unit(0.4,"cm"),
      #         legend.title=element_blank(),
      #         legend.key = element_blank(),
      #         plot.margin = unit(c(.4,0, 2.2, 0), "lines"),
      #         plot.background = element_rect(fill = "transparent",colour = NA)
      #   )
      # }
      
      RenAddBar <- ggplot(RenewablesBar, aes(x=rev(Name),y=rev(value), fill = variable))+
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
    }
  }
  
  return()
}

# ------------ Data Inputs to 246 Chart ----- #
Inputs246 <- function(ChartType, Combin, BatchTest,echToPlot){
  # Combin <- EQCombin
  # ChartType <- "EQ"
  # BatchTest <- EQBatchTest
  
  # Combin <- CBCombin
  # ChartType <- "CB"
  # BatchTest <- CBBatchTest
  
  ### Function to calculate the % Build Out over 5 years
  ### data frame needs Year, Prod and TargetProd and a label for the Chart
  BuildOutCalc <- function(df, Label){
    df$Value <- (df$Prod - df$Prod[df$Year == Startyear])/(df$TargetProd[df$Year == (Startyear+5)]-df$TargetProd[df$Year == Startyear])
    df$Label <- Label
    df$Prod <- df$TargetProd <- NULL
    return(df)
  }
  
  ### Production Inputs - normalised to the start year
  Production <- subset(Combin, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
  
  
  if (ChartType == "EQ"){  
    ### Equity Portfolio Build Out
    Production <- subset(Production, select = c("Year","Production", "TargetProductionAlignment"))
    Production <- rename(Production, c("Production"="Prod","TargetProductionAlignment"="TargetProd"))
    Production <- BuildOutCalc(Production, "Portfolio")
    
    ### Stock Market Build Out
    MarketBuildOut <- subset(BatchTest, InvestorName == "ListedMarket" & ComparisonType == "BatchResults")
    MarketBuildOut <- subset(MarketBuildOut, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5) , select = c("Year","Production", "TargetProductionAlignment"))
    MarketBuildOut <- rename(MarketBuildOut, c("Production"="Prod","TargetProductionAlignment"="TargetProd"))
    MarketBuildOut <- BuildOutCalc(MarketBuildOut, "Stock Market")
    
  }else{
    ### Debt Portfolio Build Out
    Production <- subset(Production, select = c("Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
    Production <- rename(Production,c("WtTechShareTechShare"="Prod","Benchmark_WtTechShareTechShare"="TargetProd"))
    Production <- BuildOutCalc(Production, "Portfolio")
    
    ### Debt Market Build Out
    MarketBuildOut <- subset(BatchTest, InvestorName == "GlobalBondUniverse" & ComparisonType == "BatchResults")
    MarketBuildOut <- subset(MarketBuildOut, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5) , select = c("Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
    MarketBuildOut <- rename(MarketBuildOut,c("WtTechShareTechShare"="Prod","Benchmark_WtTechShareTechShare"="TargetProd"))
    MarketBuildOut <- BuildOutCalc(MarketBuildOut, "Debt Market")
    
  }
  
  ### Global Economy Data
  ### To include or not to include...
  
  
  
  ### Inputs to the 246 chart. 
  IEATargets <- subset(IEATargets246, Technology %in% TechToPlot)  
  
  IEATargetsRef <- IEATargets[IEATargets$Year == Startyear,]
  IEATargetsRef <- subset(IEATargetsRef, select = c("Scenario","AnnualvalIEAtech"))
  IEATargetsRef <- rename(IEATargetsRef, c("AnnualvalIEAtech"="StartValue"))
  
  
  IEATargets <- merge(IEATargets, IEATargetsRef, by = "Scenario")
  
  IEATargets$Denominator <- IEATargets$AnnualvalIEAtech[IEATargets$Scenario == "450S" & IEATargets$Year==Startyear+5]-IEATargets$StartValue[IEATargets$Scenario == "450S" & IEATargets$Year==Startyear+5]
  
  
  IEATargets$Value <- (IEATargets$AnnualvalIEAtech-IEATargets$StartValue) / IEATargets$Denominator
  
  IEATargets <- subset(IEATargets, select = c("Scenario","Year","Value"))
  IEATargets <- rename(IEATargets, c("Scenario"="Label"))
  
  
  df <- rbind(Production,MarketBuildOut,IEATargets)
  
  
  return(df)
}

# ------------ 246 Chart -------------------- #
Graph246 <- function(ChartType, TechToPlot){
  
  # ChartType <- "EQ"
  # Combin <- EQCombin
  # BatchTest <- EQBatchTest
  # TechToPlot <- "RenewablesCap"
  
  if (ChartType == "EQ"){
    BatchTest <- EQBatchTest
    Combin <- EQCombin
  } else if(ChartType == "CB"){
    BatchTest <- CBBatchTest
    Combin <- CBCombin
  }
  
  # Check whether the tech is a green or brown technology
  GoodBad <- GreenBrown(TechToPlot)
  
  df <- Inputs246(ChartType, Combin, BatchTest, IEATargets246,TechToPlot)
  
  IEATargetMax <- data.frame(Year = Startyear:(Startyear+5))
  IEATargetMax$Value <- max(df$Value)+.1
  IEATargetMax$Label <- "MaxValue"
  
  df <- rbind(df,IEATargetMax)
  
  dfwide <- dcast(df,Year~Label, value.var="Value")
  
  
  Colourvals <- c( "< 2°C"=DarkGreen,"2-4°C"=LightGreen,"4-6°C"=LightRed,"> 6°C" =DarkRed )
  
  ### Calculates the y axis scales and labels      
  ymin <- min(df$Value,na.rm = T)-0.1
  ymax <- max(df$Value,na.rm = T)
  year_lab <- GT["Year"][[1]]
  ylabel <- "Build Out"
  alphaval <- 1
  linesize <- 1.5
  
  ### Identifies the Lines to plot in the Chart
  ### Selects colours up until the max number of inputs
  LinesToPlot <- unique(df$Label)
  LinesToPlot <- LinesToPlot[!LinesToPlot %in% c("450S","CPS","NPS","MaxValue")]
  LinesToPlot <- factor(LinesToPlot, levels = LinesToPlot)
  LineColours <- c("black", "grey","blue","pink")
  LineColours <- LineColours[1: length(LinesToPlot)]
  
  LineVector <- setNames(LineColours,LinesToPlot)
  # LineColours <- factor(LineColours, levels = LineColours)
  
  if(GoodBad == "Brown"){
    ### Brown Tech  
    outputplot <- ggplot(dfwide, aes(x=Year))+
      geom_area(aes(y= MaxValue,fill= "> 6°C"))+
      geom_area(aes(y= CPS, fill= "4-6°C"))+
      geom_area(aes(y= NPS, fill= "2-4°C"))+
      geom_area(aes(y= `450S`, fill= "< 2°C"))+ 
      
      geom_line(aes(x=Year,y=dfwide[LinesToPlot[1]], colour =  LinesToPlot[1]), size = linesize)+
      geom_line(aes(x=Year,y=dfwide[LinesToPlot[2]], colour =  LinesToPlot[2]), size = linesize)+
      
      ### I can't find a way to automaticall check whether 3 exists... 
      # geom_line(aes(x=Year,y=dfwide[LinesToPlot[3]], colour =  LinesToPlot[3]))+
      
      xlab(year_lab) + 
      ylab(ylabel) +
      scale_size(guide=FALSE)+
      coord_cartesian(ylim = c(ymin,ymax), xlim= c(Startyear, Startyear+5), expand = FALSE )+
      scale_fill_manual(values = Colourvals, 
                        guide = guide_legend(nrow = 2))+
      scale_color_manual(values = LineVector,
                         guide = guide_legend(nrow = 2))+
      
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.ticks=element_blank(), 
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"))
  }
  
  if(GoodBad == "Green"){
    ### Green Tech  
    # Works, need to edit legend order
    outputplot <- ggplot(dfwide, aes(x=Year))+
      geom_area(aes(y= MaxValue,fill= "< 2°C"),alpha=alphaval)+
      geom_area(aes(y= `450S`, fill= "2-4°C"),alpha=alphaval)+
      geom_area(aes(y= NPS, fill= "4-6°C"),alpha=alphaval)+
      geom_area(aes(y= CPS, fill= "> 6°C"),alpha=alphaval)+
      
      geom_line(aes(x=Year,y=dfwide[as.character(LinesToPlot[[1]])], colour =  as.character(LinesToPlot[1])), size = linesize)+
      geom_line(aes(x=Year,y=dfwide[as.character(LinesToPlot[[2]])], colour =  as.character(LinesToPlot[2])), size = linesize)+
      
      ### I can't find a way to automaticall check whether 3 exists... 
      # geom_line(aes(x=Year,y=dfwide[LinesToPlot[3]], colour =  LinesToPlot[3]))+
      
      xlab(year_lab) + 
      ylab(ylabel) +
      scale_size(guide=FALSE)+
      coord_cartesian(ylim = c(ymin,ymax), xlim= c(Startyear, Startyear+5), expand = FALSE )+
      scale_fill_manual(values = Colourvals, 
                        guide = guide_legend(nrow = 2))+
      scale_color_manual(values = LineVector,
                         guide = guide_legend(nrow = 2))+
      
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.ticks=element_blank(), 
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"))
    
    
  }
  
  
  
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),bg="transparent",height=3.6,width=3.6,plot=outputplot,dpi=ppi*2)
  
  # library(grid)
  # 
  # a <- gtable(unit(c(1,3,1,3,1,3,1,3),c("cm")),unit(c(1,.5,1),c("cm")))
  # # b <- gtable(unit(6,"cm"),unit(6,"cm"))
  # # a <- combine(a,b)
  # gtable_show_layout(a)
  # 
  # rectDG <- rectGrob(gp = gpar(fill = DarkGreen))
  # rectLG <- rectGrob(gp = gpar(fill = LightGreen))
  # rectDR <- rectGrob(gp = gpar(fill = DarkRed))
  # rectLR <- rectGrob(gp = gpar(fill = LightRed))
  # 
  # textDG <- textGrob("< 2°C")
  # textLG <- textGrob("2-4°C")
  # textLR <- textGrob("4-6°C")
  # textDR <- textGrob("> 6°C")
  # 
  # textPP <- textGrob("Portfolio")
  # 
  # plotitem <- ggplotGrob(outputplot)
  # 
  # linePP <- linesGrob(x=unit(c(0,1),"cm"), y=unit(c(0.5,.5),"cm"))
  # 
  # a <- gtable_add_grob(a,rectDG,1,1)
  # a <- gtable_add_grob(a,rectLG,1,3)
  # a <- gtable_add_grob(a,rectLR,1,5)
  # a <- gtable_add_grob(a,rectDR,1,7)
  # 
  # a <- gtable_add_grob(a,textDG,1,2)
  # a <- gtable_add_grob(a,textLG,1,4)
  # a <- gtable_add_grob(a,textLR,1,6)
  # a <- gtable_add_grob(a,textDR,1,8)
  # 
  # a <- gtable_add_grob(a,linePP,3,1)
  # 
  # a <- gtable_add_grob(a,textPP,3,2)
  # 
  # plot(a)
  
  # Out246Plot <- ggplot_gtable(ggplot_build(outputplot))
  # Out246Plot$layout$clip[Out246Plot$layout$name == "panel"] <- "off"
  # grid.draw(Out246Plot)
  
  
}


#----------- Distribution Chart ------------- #
distribution_chart <- function(Title, MetricName, MetricCol, Combin, BatchTest,BarHighl, BarLabels, BarColors, LineHighl, LineLabels, LineColors){
  
  #Title - Title of plot. MetricName - Name of metric (used for y-axis label)
  #MetricCol - name of column inwhich to find the metric (assumes it is precalculated)
  #Combin - This portfolio's data. BatchTest - All other portfolios in this batch.
  #BarHighl - Portfoilio names for which we want highlighting
  #BarLabels - Labels for those portfolios
  #BarColors - Colors in which to highlight
  #LineHighl - "Portfolio" names for which to create reference lines
  #LineLabels - Labels for those reference lines
  #LineColors - Colors for those reference lines

  BatchTest <- CBComparisonBatchTest
  Combin <- CBCombin
  MetricCol <- "CarstensMetric"
  Title <- "% of Portfolio in Climate Relevent Sectors"
  MetricName <- "Carsten's Metric"
  
  BarHighl <- c(InvestorName)
  #Will add comparison to the passed in highlights, for factoring
  BarHighl <- c(BarHighl,"Comparison")
  
  BarLabels <- c(InvestorNameLong)
  #Will add Other Portfolios to the passed in labels, for factoring
  BarLabels <- c(BarLabels, "Other Portfolios")
  names(BarLabels) <- BarHighl
  
  BarColors <- c("Orange")
  #Will add black for the other nonhighlighted bars
  BarColors <- c(BarColors,"Black")
  names(BarColors) <- BarLabels
  
  LineHighl <- c("GlobalBondUniverse", "Market_Benchmark")
  LineLabels <- c("Global Bond Universe", "Market Benchmark")
  names(LineLabels) <- LineHighl
  
  LineColors <- c("Blue","Green")
  names(LineColors) <- LineLabels
  
  
  Combin <- rbind(Combin, BatchTest)
  
  df <- unique(subset(Combin, BenchmarkRegion %in% BenchmarkRegionchoose  & 
                        Scenario %in% Scenariochoose & Year %in% (Startyear+5) &
                        !(PortName %in% c("MetaPortfolio", "MetaPortfolio_MetaPortfolio")), 
                      select = c("PortName","Year","Sector","Technology",MetricCol,"ComparisonType")))
  
  dfagg <- aggregate(df[MetricCol],by=df[c("PortName","ComparisonType")],FUN=sum)
  dfagg <- dfagg %>% dplyr::arrange_(.dots=MetricCol)
  dfagg <- dfagg<-dfagg[dim(dfagg)[1]:1,]
  dfagg$ComparisonType <- dfagg$PortName
  dfagg[!(dfagg$ComparisonType %in% c(BarHighl,LineHighl)),"ComparisonType"] <- "Comparison"
  dfagg$PortName <- factor(dfagg$PortName, levels=dfagg$PortName)
  dfagg$ComparisonType <- factor(dfagg$ComparisonType,
                                 levels=c(BarHighl,LineHighl),
                                 labels=c(BarLabels,LineLabels))
  dfagg$CarstensMetric <- as.numeric(dfagg$CarstensMetric)
  x_coord <- as.numeric(row.names(subset(dfagg, dfagg$ComparisonType %in% BarLabels[BarLabels != "Other Portfolios"])))
  
  distribution_plot<- ggplot(dfagg)+
    geom_bar(data=subset(dfagg, dfagg$ComparisonType %in% BarLabels),
             aes_string("PortName", MetricCol, fill="ComparisonType"),
             stat = "identity",width = .6)+
    scale_fill_manual(breaks=BarLabels[BarLabels != "Other Portfolios"],
                      values=c(BarColors))+
    geom_hline(data=subset(dfagg, dfagg$ComparisonType %in% LineLabels),
               aes_string(yintercept=MetricCol,color="ComparisonType"),linetype=2)+
    geom_label(data=subset(dfagg, dfagg$ComparisonType %in% BarLabels[BarLabels != "Other Portfolios"]),
               aes_string(label="ComparisonType",x=x_coord,y=MetricCol),
               size=4, color="white", fill="orange",vjust=-.3,hjust=-.1)+
    scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
    scale_x_discrete(labels=NULL)+
    expand_limits(0,0)+
    guides(fill=FALSE)+
    ggtitle(Title)+
    xlab(paste0(ReportName," Portfolios"))+
    ylab(MetricName)+
    theme_distribution()
  
  print(distribution_plot)
}


#----------- CHART THEMES --------------------
# Can these be extracted and run from here somehow?


# 
# # Stacked Bar
# theme_barcharts <- function(base_size = textsize, base_family = "") {
#   theme(axis.ticks=element_blank(), 
#         axis.text.x=element_text(face="bold",colour="black",size=textsize),
#         axis.text.y=element_text(face="bold",colour="black",size=textsize),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),#element_text(face="bold",colour="black",size=textsize),
#         axis.line = element_line(colour = "black",size=1),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         legend.position=c(0.5,-.3),
#         legend.direction="horizontal",
#         legend.text = element_text(face="bold",size=textsize,colour="black"),
#         legend.background = element_rect(fill = "transparent",colour = NA),
#         legend.key.size=unit(0.4,"cm"),
#         legend.title=element_blank(),
#         legend.key = element_blank(),
#         plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
#         plot.background = element_rect(fill = "transparent",colour = NA)
#   )
# }
# 
# # Renewable additions
# theme_barcharts <- function(base_size = textsize, base_family = "") {
#   theme(axis.ticks=element_blank(), 
#         axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.line = element_line(colour = AxisColour,size=1),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         legend.position=c(0.5,-.2),
#         legend.direction="horizontal",
#         legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
#         legend.background = element_rect(fill = "transparent",colour = NA),
#         legend.key.size=unit(0.4,"cm"),
#         legend.title=element_blank(),
#         legend.key = element_blank(),
#         plot.margin = unit(c(.4,0, 2.2, 0), "lines"),
#         plot.background = element_rect(fill = "transparent",colour = NA)
#   )
# }
# 
# # Distribution Chart
# theme_distribution <- function(base_size = textsize, base_family = "") {
#   theme(axis.ticks=element_blank(), 
#         axis.text.x=element_text(face="bold",colour="black",size=textsize),
#         axis.text.y=element_text(face="bold",colour="black",size=textsize),
#         axis.line = element_line(colour = "black",size=1),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "skyblue",color = NA),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         plot.title = element_text(hjust = 0.5)
#   )
# }
# 
# # shipping
# theme_barcharts <- function(base_size = textsize, base_family = "") {
#   theme(axis.ticks=element_blank(), 
#         axis.text.x=element_text(face="bold",colour="black",size=textsize),
#         axis.text.y=element_text(face="bold",colour="black",size=textsize),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(face="bold",colour="black",size=textsize),#element_text(face="bold",colour="black",size=textsize),
#         axis.line = element_line(colour = "black",size=1),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         legend.position=c(0.5,-.3),
#         legend.direction="horizontal",
#         legend.text = element_text(face="bold",size=textsize,colour="black"),
#         legend.background = element_rect(fill = "transparent",colour = NA),
#         legend.key.size=unit(0.4,"cm"),
#         legend.title=element_blank(),
#         legend.key = element_blank(),
#         plot.margin = unit(c(0.6,0, 2.5, 0), "lines"),
#         plot.background = element_rect(fill = "transparent",colour = NA)
#   )
# }
# 
# # mini line charts & other sector charts
# theme_linecharts <- function(base_size = textsize, base_family = "") {
#   theme(axis.ticks=element_blank(), 
#         axis.text.x=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.text.y=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.title.x=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.title.y=element_text(face="bold",colour=AxisColour,size=textsize),
#         axis.line = element_line(colour = AxisColour,size=1),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         #panel.background = element_blank(),
#         panel.background = element_rect(fill = "transparent",colour = NA),
#         # legend.position=c(0.5,-.4),#legend.position = "none", 
#         legend.position = "none", 
#         legend.direction="horizontal",
#         legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
#         legend.background = element_rect(fill = "transparent",colour = NA),
#         legend.key.size=unit(0.4,"cm"),
#         #legend.title=element_blank(),
#         legend.title = element_text(colour = AxisColour, size = textsize),
#         legend.key = element_blank(),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         plot.margin = unit(c(1,1, 0, 0), "lines")
#   )
# }


