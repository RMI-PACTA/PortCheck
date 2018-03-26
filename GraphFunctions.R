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
  text$text <- gsub("ÃÂ°","Â°",text$text)
  
  if (Languagechoose == "DE"){
    text$text[grepl("KLIMAVER",text$text)][1]<- "KLIMAVERTRÃGLICHKEITS-PILOTTEST"
  }
  
  if (Languagechoose == "FR"){
    text$text[grepl("TEST PILOTE DE COMPATIBILITÃ  CLIMATIQUE",text$text)] <- "TEST PILOTE\\\\ DE COMPATIBILITÃ  CLIMATIQUE"
    text$text[grepl("POSSIBILIT",text$text)][1]<- "\\SectionHeading{PARTIE 3:}{POSSIBILITÃS D'ACTION}"
    text$text[grepl("POSSIBILIT",text$text)][2]<- "\\PageHeading{POSSIBILITÃS D'ACTION - SÃLECTION DES FONDS}"
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
# GENERAL PLOT FUNCTIONS
# ------------ Theme -------------------------#
themecolor <- function() {
  #tech
  RenewablesColour <<- "#8cd98c"
  HydroColour <<- "#6abaff"
  NuclearColour <<- "#ae89c5"
  GasCapColour <<- "#a6cad8"
  CoalCapColour <<- "#1a3577"
  ElectricColour <<- "#847577"
  HybridColour <<- "#a6a2a2"
  ICEColour <<-"#e5e6e4"
  OilProdColour <<-"#00677f"
  GasProdColour <<-"#a7c5d1"
  CoalProdColour <<- "#004335"
  
  #sector
  energy<<-"#0090b2"
  pow <<- "#2348a1"
  trans<<- "#cfd2cd"
  othr<<- "#9793c6"
  
  #trajectory
  area_6 <<- "#e80942"
  area_4_6 <<- "#fa8086"
  area_2_4 <<- "#9bbe9d"
  area_2 <<- "#6da06f"
  eq_port <<- "#1056ff"
  stock_market<<- "black"
  peer_group <<- "black"
  
  #text size
  textsize <<- 8
  
  YourportColour <<- "#265b9b"   #"#2e4b6e"  #"#17224D"
  IndexColour <<-  "grey85"
  Tar2DColourBar <<- "#b3de69"
  Tar2DColour <<- "#a1c75e"
  goodexpColour <<- "#1F891F"
  badexpColour <<- "#ed1c24" #"#fb8072"
  ReqCapColour <<- "grey55"
  CurrCapColour <<- "grey75"
  AxisColour <<- "#17375e" #"#274F80"
  
  ColourPalette <<- data.frame(Sector = c("Power","Power","Power","Power","Power","Automotive","Automotive","Automotive","Fossil Fuels","Fossil Fuels","Fossil Fuels"),Technology = c("RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap","Electric","Hybrid","ICE","Gas","Oil","Coal"),Colours =c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,ElectricColour,HybridColour,ICEColour,GasProdColour,OilProdColour,CoalProdColour))
  
  textsize <<- 8
  linesize <<- 2
  ppi <<- 600
  
}  

theme_barcharts <-function(base_size = textsize, base_family = "") {
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
        #plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}    

theme_distribution <- function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(face="bold",colour="black",size=textsize),
        axis.text.y=element_text(face="bold",colour="black",size=textsize),
        axis.line = element_line(colour = "black",size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "skyblue",color = NA),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.title = element_text(hjust = 0.5)
  )
}
                           
# ------------ Other Sector Plots------------ #
other_sector_chart <- function(plotnumber, SectorToPlot){


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
      scale_colour_manual(name="",guide='legend',values= c(Tar2DColour),labels=c(PortfolioName,"2Â°C Benchmark"))  +
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
      gas_units <- paste0(gas_units, " mÂ²")
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
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2Â°C Benchmark"))  +
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
        scale_colour_manual(name="",guide='legend',values= c(YourportColour,Tar2DColour),labels=c(PortfolioName,"2Â°C Benchmark"))  +
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
      
      
      # HeatmapData$PortName <- revalue(HeatmapData$PortName, c("LÃ¤nsfÃ¶rsÃ¤kringarFondfÃ¶rvaltningAB"="LÃ¤nsfÃ¶rsÃ¤kringar\n FondfÃ¶rvaltningAB"))
      
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

      RenAddBar <- ggplot(RenewablesBar, aes(x=rev(Name),y=rev(value), fill = variable))+
        geom_bar(stat = "identity", width=.8, colour = NA)+
        geom_segment(aes(x=0, xend = 0 , y=0, yend = 1), size=1, colour = AxisColour,  arrow = arrow(length = unit(0.4,"cm")))+
        geom_hline(yintercept = 1, colour=Tar2DColour, linetype = "longdash", size = 1)+
        scale_fill_manual(breaks=c("AddPerc","Remaining"),values = c("AddPerc"= badexpColour, "Remaining" = YourportColour), labels = c("AddPerc"=progresslabel, "Remaining" = remainlabel))+
        annotate(hjust = 1,"text", x = c(1:(length(RenewablesStillRequired$Name))) , y = .98, label = paste((round(RenewablesStillRequired$value,0)), "MW",stillreq,sep=" "), colour = "white",size = 3)+
        scale_y_continuous(breaks = c(0,.25,.5,.75,1),label = c("0%","25%","50%","75%",Target),expand=c(0,0), limits=c(0,1))+
        scale_x_discrete(expand=c(0,0))+
        theme_barcharts()+
        theme(axis.title.x=element_blank(),
              axis.line = element_line(colour = AxisColour,size=1),
              panel.background = element_blank(),
              legend.position=c(0.5,-.2),
              legend.text = element_text(face="bold",size=textsize,colour=AxisColour),
              legend.title=element_blank(),
              plot.margin = unit(c(.4,0, 2.2, 0), "lines"))+
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


# --------
# CAINS PLOT FUNCTIONS
# -------- 
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
  # ChartType = "EQ"
  # plotnumber = 99
  # companiestoprint = 20
  # SectorToPlot = "Automotive"
   if (ChartType == "EQ"){
    PortSnapshot <- EQPortSnapshot
    combin <- EQCombin
  } else if(ChartType == "CB"){
    PortSnapshot <- CBPortSnapshot
    combin <- CBCombin
  }
  
  
  if (SectorToPlot == "Power"){AlloftheCompanies <- UtilityCompanies}
  if (SectorToPlot == "Automotive"){AlloftheCompanies <- AutoCompanies}
  if (SectorToPlot == "OG"){AlloftheCompanies <- OGCarbonBudget}
  if (SectorToPlot == "Oil"){AlloftheCompanies <- OilData}
  
  
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
    
    if (ChartType == "EQ"){
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]
    }else{
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
    
  }else{
    
    if (SectorToPlot == "Power"){techorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")}
    
    if (SectorToPlot == "Automotive"){techorder <- c("ICE","Hybrid","Electric")}
    
    if (SectorToPlot == "Oil"){
      techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "Technology"]
      AlloftheCompanies <- rename(AlloftheCompanies, c("Resource.Type" = "Technology"),warn_missing = FALSE)
    }
    
    if (ChartType == "EQ"){
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "DebtTicker"]
    }else{
      AlloftheCompanies <- AlloftheCompanies[!colnames(AlloftheCompanies) %in% "EquityTicker"]
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
      Portfoliomix$Name <- PortfolioNameLong
      Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Portfoliomix) <- c("item", "family", "score", "value")
      Portfoliomix$value <- as.numeric(Portfoliomix$value)
      Portfoliomix$value <- (Portfoliomix$value/sum(Portfoliomix$value))*100
    }
    
    if (SectorToPlot %in% c("Automotive","Power")){
      Targetmix <- subset(combin, Sector == SectorToPlot & Scenario == Scenariochoose  & Year == Startyear+5)
      
      if (ChartType %in% c("EQ","CB")){ 
        Targetmix <- subset(Targetmix,  BenchmarkRegion == BenchmarkRegionchoose, 
                            select = c("Technology", "ProjMarketProd","PortWt","MarketTechShare"))
        Targetmix$TargetProductionAlignment <- Targetmix$ProjMarketProd*Targetmix$PortWt*Targetmix$MarketTechShare
        Targetmix <- subset(Targetmix, select=c("Technology","TargetProductionAlignment"))
      # }else{
      #   Targetmix <- subset(Targetmix, select = c("Technology","Benchmark_WtTechShare"))
      #   Targetmix <- rename(Targetmix, c("Benchmark_WtTechShare" = "TargetProductionAlignment"))
      }
      
      Targetmix$Classification<-"Portfolio"
      Targetmix$Name<-GT["X2Target"][[1]]
      Targetmix<-rename(Targetmix, c("TargetProductionAlignment"="Capacity"))
      Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Capacity"))
      colnames(Targetmix) <- c("item", "family", "score", "value")
      Targetmix$value <- as.numeric(as.character(Targetmix$value))
    }
    
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
    
    # if (SectorToPlot != "Oil"){
      AllTopCompanies <- rbind(TopPortCompanies, TopIndexCompanies)
    # }else{
    #   AllTopCompanies <- TopPortCompanies
    # }  
    
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
    
    if (SectorToPlot == "Oil"){
      
      # Portfoliomix <- as.data.frame(sapply(Portfoliomix, function(x) gsub("Cap", "", x)))
      Portfoliomix$value <- as.numeric(as.character(Portfoliomix$value))
      
      AllCompanies$value <- as.numeric(as.character(AllCompanies$value))
      
      AllCompanies <- AllCompanies[rev(order(AllCompanies$item)),]
      # AllCompanies$item <- factor(AllCompanies$item, levels=AllCompanies$item)
      
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
  print(Plot)
  Plot <- ggplot_gtable(ggplot_build(Plot))
  Plot$layout$clip[Plot$layout$name == "panel"] <- "off"
  
  # PortfolioName <- gsub("_.*", "\\1", PortfolioName)
  if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
  
  png(paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_',SectorToPlot,'_WheelofFortune.png'), height = 2600, width = 5500,res=ppi,bg="transparent") 
  grid.draw(Plot)
  dev.off()  

}

#------------- SECTOR BAR CHARTS ------------ #
# Bar chart of the Sector Weights in the portfolio for both CB and EQ

sector_processing <- function(){
  
    EQBatchTest <- EQCombin
    EQBatchTest$Type <- "Equity Portfolio"
    CBBatchTest <- CBCombin
    CBBatchTest$Type <- "Corporate Bond Portfolio"
    
    ID.COLS = c("PortName","Year","Sector","Technology","CarstenMetric_Port","Type")
    
    CB <- unique(subset(CBBatchTest, BenchmarkRegion %in% BenchmarkRegionchoose  & 
                          Scenario %in% Scenariochoose & Year %in% (Startyear+5), 
                        select = c(ID.COLS)))
    EQ <- unique(subset(EQBatchTest, BenchmarkRegion %in% BenchmarkRegionchoose  & 
                          Scenario %in% Scenariochoose & Year %in% (Startyear+5), 
                        select = c(ID.COLS)))

    df <- rbind(CB,EQ)
    df <- df %>% gather(key=Type, value=Value, -c(ID.COLS))
    df$Sector<-as.factor(df$Sector)
    levels(df$Sector)[levels(df$Sector)=="Oil&Gas"] <- "Fossil Fuels"
    levels(df$Sector)[levels(df$Sector)=="Power"] <- "Utility Power"
    dfagg <- aggregate(df["CarstenMetric_Port"],by=df[c("Sector","Type","PortName")],FUN=sum)
    
    return(dfagg)
    }                                       
   
sector_bar_chart <- function(plotnumber, dfagg){

  ### Need to be changed!
  
  sectorpalette <- c(energy,pow,trans)
  sectororder <-c("Fossil Fuels","Utility Power","Automotive")
  colourdf <- data.frame(colour=sectorpalette, Sector =sectororder)
  dfagg$Sector<-as.factor(dfagg$Sector)
  combined <- sort(union(levels(dfagg$Sector), levels(colourdf$sectororder)))
  dfagg <- merge(dfagg, colourdf, by= "Sector") 
  orderofchart <- c("Equity Portfolio","Corporate Bond Portfolio")
  dfagg$Type <- factor(dfagg$Type,levels=orderofchart)
  dfagg$Sector<- factor(dfagg$Sector,levels = sectororder)
  dfagg <- dfagg[order(dfagg$Sector,dfagg$Type),]
  temp<-max(dfagg$CarstenMetric_Port)
  ylabel = ""

  a<-ggplot(dfagg, aes(x=Type, y=CarstenMetric_Port,fill=Sector),show.guide = TRUE)+
    geom_bar(stat = "identity",width = .6)+
    theme_minimal()+
    scale_fill_manual(labels=unique(as.character(dfagg$Sector)),values=unique(as.character(dfagg$colour)))+
    scale_y_continuous(expand=c(0,0), limits = c(0,temp+0.3), labels=percent)+
    expand_limits(0,0)+
    ylab(ylabel)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()+
    theme(legend.position = "bottom",
        axis.title=element_blank(),
        axis.line.x = element_line(colour = "black",size=1),
        axis.line.y = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face="bold",size=textsize,colour="black"),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key.size=unit(0.4,"cm"),
        legend.title=element_blank(),
        plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines")
  )
  print(a)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_SectorBarChart.png',sep=""),bg="transparent",height=4,width=4,plot=a,dpi=ppi)
}

# -------------STACKED BAR CHARTS ---------- #

stacked_bar_chart_vertical <- function(plotnumber,ChartType,SectorToPlot){
  library(dplyr)
  if (ChartType == "EQ"){
    Portfolio <- EQCombin
    Batch <- EQBatchTest[EQBatchTest$PortName != PortName,]
  }else if (ChartType == "CB"){
    Portfolio <- CBCombin
    Batch <- CBBatchTest[CBBatchTest$PortName != PortName,]
  }else if (ChartType == "Summary") {
    Portfolio <- rbind(subset(EQCombin,select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology","CarstenMetric_Port","ComparisonType")),
                       subset(CBCombin,select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology","CarstenMetric_Port","ComparisonType")))
    Batch <- rbind(subset(EQBatchTest,
                          select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology","CarstenMetric_Port","ComparisonType")),
                   subset(CBBatchTest,
                          select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology","CarstenMetric_Port","ComparisonType")))
    Batch <- Batch[Batch$PortName != PortName,]
  }
  #Tag Target portfolio, benchmark
  Portfolio$ComparisonType = "Portfolio"
  Batch$ComparisonType = "Peers"
  Combin <- rbind(Portfolio,Batch)
  Production <- subset(Combin, Year == Startyear & 
                         BenchmarkRegion %in% BenchmarkRegionchoose & 
                         Scenario %in% Scenariochoose &
                         Technology != "OilCap",
                       select=c("PortName","Sector","Technology","CarstenMetric_Port","ComparisonType"))
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","ComparisonType")
  Production <- Production %>% gather(key=Metric, value=Value, "CarstenMetric_Port")
  Production <- aggregate(Production["Value"],by=Production[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  if(nrow(Production)>0){
    Production$TechName <- Production$Technology
    Production[Production$Sector=="Oil&Gas","TechName"] <- revalue(Production[Production$Sector=="Oil&Gas","TechName"],
                                                                        c("Coal" = "CoalProd",
                                                                          "Gas" = "GasProd",
                                                                          "Oil" = "OilProd"))
    ylabel <- GT["StackedBarYLabel_FF"][[1]]
    technologyorder <- c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap","Electric","Hybrid","ICE","CoalProd","GasProd","OilProd")
    colours <- c(CoalCapColour,GasCapColour,NuclearColour,HydroColour,RenewablesColour,ElectricColour,HybridColour,ICEColour,CoalProdColour,GasProdColour,OilProdColour)
    names(colours) <- technologyorder
    labels <- c("Coal","Gas","Nuclear","Hydro","Renewables","Electric","Hybrid","ICE","Coal","Gas","Oil")
    names(labels) <- technologyorder
    
    
    Production$Technology<-as.factor(Production$TechName)
    Production$Sector<-as.factor(Production$Sector)
    
    Production$ComparisonType <- wrap.labels(Production$ComparisonType,20)
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    Production$ComparisonType <- factor(Production$ComparisonType)
    dat <- Production
    detach("package:dplyr",unload=TRUE)
    
    templete <- ggplot(data=dat, aes(x=ComparisonType, y=Value,fill=TechName),show.guide = TRUE)+
      geom_bar(stat = "identity", position = "fill", width = .6)+
      theme_minimal()+
      scale_fill_manual(labels=labels,values=colours)+
      scale_y_continuous(expand=c(0,0), labels=percent)+
      # expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 1))+
      ylab(ylabel)+
      theme_barcharts()+
      ggtitle("Templete")+
      theme(plot.title = element_text(hjust = 0.5,face="bold",colour="black",size=textsize),
            legend.position = "bottom",legend.title = element_blank(),
            axis.line = element_blank())
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Production, Sector == SectorToPlot)
      p1 <- templete %+% dat +
        ggtitle(paste0(unique(as.character(dat$Sector)),"Production"))
      
      print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
      
    }
    else if (SectorToPlot == "All"){
      dat<- subset(Production,Sector=="Automotive")
      p1 <- templete %+% dat +
        ggtitle("Automotive Production")
      
      dat<- subset(Production,Sector=="Oil&Gas")
      p2 <- templete %+% dat +
        ggtitle("Fossil Fuels Production")
      
      dat<- subset(Production,Sector=="Power")
      p3 <- templete %+% dat +
        ggtitle("Power Capacity")
      
      cmd<-grid.arrange(p2,p3+theme(axis.text.y = element_blank()),p1+theme(axis.text.y = element_blank()),nrow=1)
      
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
    }
    else{
      Label <- paste0("No",ChartType,gsub(" ","",SectorToPlot))
      #  Label <- GT[Label][[1]]
      
      outputplot <-
        ggplot()+
        annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=4)+
        geom_blank()+
        theme(
          axis.title=element_blank(),
          axis.text.=element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA))
      print(outputplot)
      if(SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
      ggsave(outputplot,filename=paste0(plotnumber,"_","PortfolioName","_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,dpi=ppi)
      
    }
  }
}

stacked_bar_chart_horizontal <- function(plotnumber,ChartType,SectorToPlot,Production){
  library(dplyr)
  if (ChartType == "EQ"){
    Portfolio <- EQCombin
    Batch <- EQBatchTest[EQBatchTest$PortName != PortName,]
  }else if (ChartType == "CB"){
    Portfolio <- CBCombin
    Batch <- CBBatchTest[CBBatchTest$PortName != PortName,]
  }
  #Tag Target portfolio, benchmark
  Portfolio$ComparisonType = "Portfolio"
  Batch$ComparisonType = "Peers"
  Combin <- rbind(Portfolio,Batch)
  Production <- subset(Combin, Year == Startyear & 
                         BenchmarkRegion %in% BenchmarkRegionchoose & 
                         Scenario %in% Scenariochoose &
                         Technology != "OilCap",
                       select=c("PortName","Sector","Technology","CarstenMetric_Port","ComparisonType"))
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","ComparisonType")
  Production <- Production %>% gather(key=Metric, value=Value, "CarstenMetric_Port")
  Production <- aggregate(Production["Value"],by=Production[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  if(nrow(Production)>0){
    Production$TechName <- Production$Technology
    Production[Production$Sector=="Oil&Gas","TechName"] <- revalue(Production[Production$Sector=="Oil&Gas","TechName"],
                                                                   c("Coal" = "CoalProd",
                                                                     "Gas" = "GasProd",
                                                                     "Oil" = "OilProd"))
    ylabel <- GT["StackedBarYLabel_FF"][[1]]
    technologyorder <- c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap","Electric","Hybrid","ICE","CoalProd","GasProd","OilProd")
    colours <- c(CoalCapColour,GasCapColour,NuclearColour,HydroColour,RenewablesColour,ElectricColour,HybridColour,ICEColour,CoalProdColour,GasProdColour,OilProdColour)
    names(colours) <- technologyorder
    labels <- c("Coal","Gas","Nuclear","Hydro","Renewables","Electric","Hybrid","ICE","Coal","Gas","Oil")
    names(labels) <- technologyorder
    
    
    Production$Technology<-as.factor(Production$TechName)
    Production$Sector<-as.factor(Production$Sector)
    
    Production$ComparisonType <- wrap.labels(Production$ComparisonType,20)
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    Production$ComparisonType <- factor(Production$ComparisonType)
    dat <- Production
    detach("package:dplyr",unload=TRUE)
    
    templete <- ggplot(data=dat, aes(x=ComparisonType, y=Value,fill=TechName),show.guide = TRUE)+
      geom_bar(stat = "identity", position = "fill", width = .6)+
      theme_minimal()+
      scale_fill_manual(labels=labels,values=colours)+
      scale_y_continuous(expand=c(0,0), labels=percent)+
      # expand_limits(0,0)+
      guides(fill=guide_legend(nrow = 1))+
      ylab(ylabel)+
      theme_barcharts()+
      ggtitle("Templete")+
      coord_flip()+
      theme(plot.title = element_text(hjust = 0.5,face="bold",colour="black",size=textsize),
            legend.position = "bottom",legend.title = element_blank(),
            axis.line = element_blank())
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Production, Sector == SectorToPlot)
      p1 <- templete %+% dat +
        ggtitle(paste0(unique(as.character(dat$Sector)),"Production"))
      
      print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
      
    }
    else if (SectorToPlot == "All"){
      dat<- subset(Production,Sector=="Automotive")
      p1 <- templete %+% dat +
        ggtitle("Automotive Production")
      
      dat<- subset(Production,Sector=="Oil&Gas")
      p2 <- templete %+% dat +
        ggtitle("Fossil Fuels Production")
      
      dat<- subset(Production,Sector=="Power")
      p3 <- templete %+% dat +
        ggtitle("Power Capacity")
      
      cmd<-grid.arrange(p2+theme(axis.text.x = element_blank()),p3+theme(axis.text.x = element_blank()),p1,ncol=1)
      
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
    }
    else{
      Label <- paste0("No",ChartType,gsub(" ","",SectorToPlot))
      #  Label <- GT[Label][[1]]
      
      outputplot <-
        ggplot()+
        annotate(geom = "text", x=0,y=0, label=wrap.labels(Label,15), size=4)+
        geom_blank()+
        theme(
          axis.title=element_blank(),
          axis.text.=element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA))
      print(outputplot)
      if(SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
      ggsave(outputplot,filename=paste0(plotnumber,"_","PortfolioName","_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=1.8,width=7.5,dpi=ppi)
      
    }
  }
}

# ------------ 246 Chart -------------------- #
Inputs246 <- function(ChartType, TechToPlot){
  
  if (ChartType == "EQ"){  
    Combin <- EQCombin
    BatchTest <- EQBatchTest
  }else if(ChartType == "CB"){
    Combin <- CBCombin
    BatchTest <- CBBatchTest
  }
  
  ### Function to calculate the % Build Out over 5 years
  ### data frame needs Year, Prod and TargetProd and a label for the Chart
  BuildOutCalc <- function(df, Label){
    GoodBad <- GreenBrown(TechToPlot)
    
    df <- rename(df, c("Production"="Prod","TargetProductionAlignment"="TargetProd",
                       "WtTechShareTechShare"="Prod","Benchmark_WtTechShareTechShare"="TargetProd",
                       "AnnualvalIEAtech"="Prod"),warn_missing = F)
    
    if (GoodBad == "Green") {
      df$Value <- 1+(df$Prod - df$Prod[df$Year == Startyear])/(df$TargetProd[df$Year == (Startyear+5)]-df$TargetProd[df$Year == Startyear])
    } else if(GoodBad == "Brown"){
      df$Value <- (df$Prod/df$Prod[df$Year == Startyear])
    }
    
    df$Label <- Label
    df$Prod <- df$TargetProd <- NULL
    return(df)
  }
  
  ### Production Inputs - normalised to the start year
  Production <- subset(Combin, BenchmarkRegion %in% BenchmarkRegionchoose&Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
  
  
  if (ChartType == "EQ"){  
    ### Equity Portfolio Build Out
    Production <- subset(Production, select = c("Year","Production", "TargetProductionAlignment"))
    Production <- BuildOutCalc(Production, "Portfolio")
    
    ### Stock Market Build Out
    MarketBuildOut <- subset(BatchTest, InvestorName == "ListedMarket" & ComparisonType == "BatchResults")
    MarketBuildOut <- subset(MarketBuildOut, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5) , select = c("Year","Production", "TargetProductionAlignment"))
    MarketBuildOut <- BuildOutCalc(MarketBuildOut, "Stock Market")
    
  }else{
    ### Debt Portfolio Build Out
    Production <- subset(Production, select = c("Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
    Production <- BuildOutCalc(Production, "Portfolio")
    
    ### Debt Market Build Out
    MarketBuildOut <- subset(BatchTest, InvestorName == "GlobalBondUniverse" & ComparisonType == "BatchResults")
    MarketBuildOut <- subset(MarketBuildOut, BenchmarkRegion %in% BenchmarkRegionchoose & Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5) , select = c("Year","WtTechShareTechShare","Benchmark_WtTechShareTechShare"))
    MarketBuildOut <- BuildOutCalc(MarketBuildOut, "Debt Market")
    
  }
  
  ### Global Economy Data
  ### To include or not to include...
  
  
  
  ### Inputs to the 246 chart. 
  IEATargets <- subset(IEATargets246, Technology %in% TechToPlot)  
  IEATargetsRef <- subset(IEATargets, Scenario == "450S", select=c("Year","AnnualvalIEAtech"))
  IEATargetsRef <- rename(IEATargetsRef, c("AnnualvalIEAtech"="TargetProd"))
  IEATargets <- merge(IEATargets,IEATargetsRef, by="Year")
  
  
  IEATargets <- lapply(unique(IEATargets$Scenario), function(x) BuildOutCalc(IEATargets[IEATargets$Scenario == x,],x))
  IEATargets <- do.call("rbind", IEATargets)
  
  IEATargets <- subset(IEATargets, select = c("Label","Year","Value"))
  
  df <- rbind(Production,MarketBuildOut,IEATargets)
  
  
  return(df)
}

Graph246 <- function(plotnumber, ChartType, TechToPlot){
  
  
  if (ChartType == "EQ"){
    BatchTest <- EQBatchTest
    Combin <- EQCombin
    LinesToPlot <- c("Portfolio","Stock Market")
  } else if(ChartType == "CB"){
    BatchTest <- CBBatchTest
    Combin <- CBCombin
    LinesToPlot <- c("Portfolio", "Debt Market")
  }
  
  # Check whether the tech is a green or brown technology
  GoodBad <- GreenBrown(TechToPlot)
  
  df <- Inputs246(ChartType, TechToPlot)
  
  IEATargetMax <- data.frame(Year = Startyear:(Startyear+5))
  IEATargetMax$Value <- max(df$Value)+.1
  IEATargetMax$Label <- "MaxValue"
  
  df <- rbind(df,IEATargetMax)
  
  dfwide <- dcast(df,Year~Label, value.var="Value")
  
  
  if (GoodBad == "Green"){
    dfwide$Line1 <- dfwide$CPS
    dfwide$Line2 <- dfwide$NPS-dfwide$CPS
    dfwide$Line3 <- dfwide$`450S`-dfwide$NPS
    dfwide$Line4 <- dfwide$MaxValue-dfwide$`450S`
    # dfwide$Line1 <- dfwide$`450S`
    # dfwide$Line2 <- dfwide$NPS - dfwide$`450S`
    # dfwide$Line3 <- dfwide$CPS - dfwide$NPS
    # dfwide$Line4 <- dfwide$MaxValue - dfwide$CPS
    # lineorder <-c("Line1","Line2","Line3","Line4")
    Palette <- c(area_6,area_4_6,area_2_4,area_2)
    AreaNames <-  c( "> 6°C","4-6°C","2-4°C","< 2°C") 
  }else if (GoodBad == "Brown"){
    dfwide$Line1 <- dfwide$`450S`
    dfwide$Line2 <- dfwide$NPS - dfwide$`450S`
    dfwide$Line3 <- dfwide$CPS - dfwide$NPS
    dfwide$Line4 <- dfwide$MaxValue - dfwide$CPS   
    Palette <- c(area_2,area_2_4,area_4_6,area_6)
    AreaNames <-  c( "< 2°C","2-4°C","4-6°C","> 6°C") 
    # lineorder <-c("Line4","Line3","Line2","Line1")
  }
  
  
  dftargets <- subset(dfwide, select = c("Year","Line1","Line2","Line3","Line4"))
  dftargets <- melt(dftargets, id.vars =  "Year", variable.name = "Target")
  # dftargets <- rev(dftargets)
  
  # AreaNames <-  c( "< 2°C","2-4°C","4-6°C","> 6°C") 
  # Palette <- c(DarkGreen,LightGreen,LightRed,DarkRed)
  lineorder <-c("Line4","Line3","Line2","Line1")
  colourdf <- data.frame(colour=Palette, Target =lineorder, Labels = AreaNames)
  
  dftargets$Target<-as.factor(dftargets$Target)
  combined <- sort(union(levels(dftargets$Target), levels(colourdf$Target)))
  dftargets <- merge(dftargets, colourdf, by= "Target") 
  dftargets$Target<- factor(dftargets$Target,levels = lineorder)
  
  
  maxval <- ceiling(max(df$Value)*10)/10-0.1
  minval <- max(floor(min(df$Value)*10)/10,0)
  
  LineColours <- c(eq_port, stock_market,peer_group,"pink")
  LineColours <- LineColours[1: length(LinesToPlot)]
 
  year_lab = 2017 #2018
  LineVector <- setNames(LineColours,LinesToPlot)
  
  ylabel <- "Normalized Built Out"
 outputplot <-  ggplot()+
    geom_area(aes(x=Year,y=value, fill=Target),data=dftargets)+
    geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[1])],colour =  "Equity Portfolio"), data=dfwide, size = linesize,linetype="solid")+  # Portfolio
    geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[2])],colour =  "Stock Market"), data=dfwide, size = linesize,linetype="dashed")+   # Market
    #geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[3])],colour =  "Peer Group"), data=dfwide, size = linesize,linetype="longdash")+   # peer
    
    scale_fill_manual(labels=unique(as.character(dftargets$Labels)),
                      values=unique(as.character(dftargets$colour)))+
    
    scale_color_manual(name="",values = c("Equity Portfolio"=eq_port,"Stock Market"=stock_market))+
    
    xlab(year_lab) +
    ylab(ylabel)+
    coord_cartesian(ylim=c(0,maxval))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks=element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(.5,1,0.5,.5), "cm"))
    
    print(outputplot)
  
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),bg="transparent",height=3.6,width=4.6,plot=outputplot,dpi=ppi*2)
  
  
  
  
}

#----------- Distribution Chart ------------- #
distribution_chart <- function(plotnumber, MetricName, ChartType){
  library(dplyr)
  # MetricName = "Risk Exposure"
  # plotnumber = 99
  # ChartType ="CB"
  if (MetricName == "Carsten's Metric") {
    Title <- "Exposure of Portfolios to Climate Relevent Sectors"
    MetricCol <- "CarstenMetric_Port"
    if(ChartType == "CB") {
      BatchTest <- CBBatchTest
    } else if (ChartType == "EQ") {
      BatchTest <- EQBatchTest
    }
    ID.COLS = c("PortName","Year","Sector","Technology")
    BarColors <- c("Orange")
    df <- unique(subset(BatchTest, BenchmarkRegion %in% BenchmarkRegionchoose  & 
                          Scenario %in% Scenariochoose & Year %in% (Startyear+5), 
                        select = c(ID.COLS,MetricCol)))
    
  } else if (MetricName == "Risk Exposure") {
    Title <- "Risk Exposure of Portfolios"
    MetricCol <- c("Risk 2", "Risk 1")
    if(ChartType == "CB") {
      PortSS <- CBBatchTest_PortSnapshots
    } else if (ChartType == "EQ") {
      PortSS <- EQBatchTest_PortSnapshots
    }
    df <- PortSS %>% 
      mutate("AUM" = AUM/PortfolioAUMAnalyzed) %>%
      spread("MoodysRiskLvl", "AUM", fill = 0) %>%
      rename("Risk 1" = "1", "Risk 2" = "2")
    
    ID.COLS = c("PortName")
    BarColors <- c("Orange","Red")
    df <- subset(df, select = c(ID.COLS,MetricCol))
    
  }
  
  BarColors <- c(BarColors,"Black","skyblue")
  names(BarColors) <- c(MetricCol,"Comparison","Unexposed")
  
  LineHighl <- c("Market_Benchmark")
  LineLabels <- c("Market Benchmark")
  names(LineLabels) <- LineHighl
  LineColors <- c("Green")
  names(LineColors) <- LineLabels
  

  df <- df %>% gather(key=Metric, value=Value, -c(ID.COLS))
  
  dfagg <- aggregate(df["Value"],by=df[c("PortName","Metric")],FUN=sum)
  dfagg[dfagg$PortName %in% LineHighl,"Metric"] <- "Reference"
  dfagg[dfagg$PortName == PortName,"Metric"] <- "Comparison"
  dfagg$Value <- as.numeric(dfagg$Value)

  dfagg <- dfagg %>%
    filter(Metric != "Reference") %>%
    group_by(PortName) %>%
    summarise("Value" = 1-sum(Value)) %>%
    mutate("Metric" = "Unexposed") %>%
    select(PortName,Metric,Value) %>%
    rbind(dfagg)
  
  values = subset(dfagg, Metric=="Comparison" & PortName==PortName)[["Value"]]
  if (MetricName == "Carsten's Metric") {
    portfolio_label = paste0("Your Portfolio\n",
                             "Carsten's Metric: ",percent(values[1]))
  } else if (MetricName == "Risk Exposure") {
    portfolio_label = paste0("Your Portfolio\n",
                             "Risk 1: ",percent(values[1]),"\n",
                             "Risk 2: ",percent(values[2]))
  }
  order <- dfagg %>% filter(Metric == "Unexposed") %>% arrange(Value)
  dfagg$PortName <- factor(dfagg$PortName, levels=unique(order$PortName))
  dfagg$Metric <- factor(dfagg$Metric, levels=c("Unexposed",MetricCol,"Comparison","Reference"))
  
  x_coord <- length(unique(order$PortName))
  detach("package:dplyr",unload=TRUE)
  
  distribution_plot<- ggplot(dfagg)+
    geom_bar(data=subset(dfagg, dfagg$Metric != "Reference"),
             aes(x=PortName, y=Value, fill=Metric),
             stat = "identity", position = "fill", width=1)+
    scale_fill_manual(values=BarColors)+
    geom_hline(data=subset(dfagg, dfagg$Metric == "Reference"),
               aes(yintercept=Value),color=LineColors,linetype=2)+
    geom_text(data=subset(dfagg, dfagg$Metric == "Reference"),
              aes(y=Value),x=x_coord,label=LineLabels,
              color="white",vjust=-.2,hjust=1)+
    annotate("label", x = x_coord, y = 1, 
             label = portfolio_label,
             hjust=1.05,vjust=1.05)+ 
    scale_y_continuous(expand=c(0,0), limits = c(0,1.0001), labels=percent)+
    scale_x_discrete(labels=NULL)+
    expand_limits(0,0)+
    # guides(fill=FALSE)+
    ggtitle(Title)+
    xlab(paste0(BatchName," Portfolios"))+
    ylab(MetricName)+
    theme_distribution()
  
  print(distribution_plot)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_Distribution.png', sep=""),height=3.6,width=3.6,plot=distribution_plot,dpi=ppi*2)
  
}


#----------- CA Report Data------------------ #
CAReportData <- function(){
  
  # Output is a dataframe with the values for the text

  ### Exec Summary Data ###
  ReportInsuranceName <- PortfolioNameLong
  SizeofPortfolio <- PortfolioBreakdown$comma.PortfolioSize.[PortfolioBreakdown$PortName == PortName]
  NoPeers <- nrow(TestList)
  

  ### Port Weights ###
  SectorData$ChartType<- "CB"
  SectorData$ChartType[SectorData$label == "Equity Portfolio"] <- "EQ"
  
  FFSectorPortEQ <- SectorData$Portfolio_weight[SectorData$piesector == "Fossil Fuels" & SectorData$label == "Equity Portfolio"]
  PowerSectorPortEQ <- SectorData$Portfolio_weight[SectorData$piesector == "Utility Power" & SectorData$label == "Equity Portfolio"]
  AutoSectorPortEQ <- SectorData$Portfolio_weight[SectorData$piesector == "Automotive" & SectorData$label == "Equity Portfolio"]

  FFSectorPortCB <- SectorData$Portfolio_weight[SectorData$piesector == "Fossil Fuels" & SectorData$label == "Corporate Bond Portfolio"]
  PowerSectorPortCB <- SectorData$Portfolio_weight[SectorData$piesector == "Utility Power" & SectorData$label == "Corporate Bond Portfolio"]
  AutoSectorPortCB <- SectorData$Portfolio_weight[SectorData$piesector == "Automotive" & SectorData$label == "Corporate Bond Portfolio"]

  # Need to add the averages in
  
  FFSectorPeerEQ <- 1
  PowerSectorPeerEQ<- 1
  AutoSectorPeerEQ<- 1
  FFSectorPeerCB<- 1
  PowerSectorPeerCB<- 1
  AutoSectorPeerCB<- 1
  
  ### RANKINGS ###
  
  EQPortRanks <- EQRanks[EQRanks$PortName == PortName,] 
  EQPeerRanks <- data.frame(t(colSums(!is.na(EQRanks))))
  EQPeerRanks$PortName <- "PeerTotal"
  techlist <- unlist(colnames(EQPortRanks)[2:12])
  EQReportRanks <- as.data.frame(lapply(techlist,function(x) paste0(EQPortRanks[[as.character(x)]], " of ", EQPeerRanks[[as.character(x)]])))
  colnames(EQReportRanks) <- techlist
  
  CBPortRanks <- CBRanks[CBRanks$PortName == PortName,] 
  CBPeerRanks <- data.frame(t(colSums(!is.na(CBRanks))))
  CBPeerRanks$PortName <- "PeerTotal"
  techlist <- unlist(colnames(CBPortRanks)[2:12])
  CBReportRanks <- lapply(techlist,function(x) paste0(CBPortRanks[[as.character(x)]], " of ", CBPeerRanks[[as.character(x)]]))
  colnames(CBReportRanks) <- paste0("CB",techlist)
  
  
  ### MERGE ALL RESULTS ###
  # reportdata <- data.frame(
  #          c("ReportInsuranceName",ReportInsuranceName), 
  #          c("SizeofPortfolio",SizeofPortfolio),
  #          c("NoPeers",NoPeers),
  #          c("FFSectorPortEQ",FFSectorPortEQ),
  #          c("PowerSectorPortEQ",PowerSectorPortEQ),
  #          c("AutoSectorPortEQ",AutoSectorPortEQ),
  #          c("FFSectorPortCB",FFSectorPortCB),
  #          c("PowerSectorPortCB",PowerSectorPortCB),
  #          c("AutoSectorPortCB",AutoSectorPortCB)
  #          )
  # 
  # colnames(reportdata) <- as.character(unlist(reportdata[1,]))
  # reportdata = reportdata[-1, ]
  # 
  # reportdata <- cbind(reportdata,EQReportRanks)
  # reportdata <- cbind(reportdata,CBReportRanks)
  # 
  # return(reportdata)
  
}

CAReport <- function(){
  
  CAReportData()
  
  # Copy in the template for the report
  text <- as.data.frame(template,stringsAsFactors = FALSE)  
  colnames(text) <- "text"
  
  # Function removes text lines between 
  # "handlenameS" and "handlenameE" ie CBPowerS, CBPowerE
  # Need to add these handles into the final doc when ready. Also determine what will be kicked out/what will remain. 
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
  
  
  # Ranks
  techranks <- data.frame( "CoalCap", "NuclearCap", "RenewablesCap", "GasCap", "ICE","Electric", "Oil","Gas")
  for (j in techranks){
    text$text <- gsub(as.character(paste0("EQ",techranks[[j]],"Rank")),EQReportRanks[,as.character(techranks[[j]])],text$text)
  }
  for (j in techranks){
    text$text <- gsub(as.character(paste0("CB",techranks[[j]],"Rank")),CBReportRanks[,as.character(techranks[[j]])],text$text)
  }
  
  # Exec Summary Values
  execsummarylist <- data.frame("ReportInsuranceName","SizeofPortfolio","NoPeers")
  for (j in execsummarylist){
    text$text <- gsub(j,eval(as.symbol(as.character(j))),text$text)
  }
  
  # Replace Sector Weight Values
  a<-data.frame("SectorList"=paste0(rep(c("FF","Power","Auto"),1,each=4),"Sector",rep(c("Port","Peer"),3,each=2),rep(c("EQ","CB"),6)))
  for (j in 1:nrow(a)){
    text$text <- gsub(as.character(a$SectorList[j]),round(eval(as.symbol(as.character(a$SectorList[j])))*100,1),text$text)
  }  
  
  # Replace Insurer Name
  text$text <- gsub("InsurerSampleReport",PortfolioNameLong,text$text)
  
  # Figures
  # Replace CAFigures
  
  # Update the template to reflect figure names
  
  # FigNames<-as.data.frame(readLines("FigureList.txt",skipNul = TRUE))
  # colnames(FigNames) <- "Name"
  # FigNames$Name <- gsub("\"","",as.character(FigNames$Name))
  # FigNames$Fig <- substring(FigNames$Name,1,2)
  # FigNames$Fig <- paste0("CAFigures/Fig",FigNames$Fig)
  # 
  # for (f in 1:nrow(FigNames)){
  #   text$text <- gsub(FigNames$Fig[f],FigNames$Name[f],text$text,fixed = TRUE)
  # }
  
  
  ##### PRINT REPORT ######
  
  # Copy in the graphics folder for the report
  originalloc <- paste0(GIT.PATH,"Templates/ReportGraphics/")
  graphicsloc <- paste0(LANGUAGE.PATH ,"ReportGraphics/")
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
  
  # Delete remaining files and ReportGraphics Folder
  unlink("ReportGraphics",recursive = TRUE)
  excessfileendings <- c(".log",".rnw",".tex",".aux")
  file.remove(paste0(TemplateNameNew,excessfileendings))
  file.remove("FigureList.txt")
  
  # Rename output file
  if (InvestorName == PortfolioName){
    file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",Languagechoose,".pdf"))}else{
      file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",PortfolioName,"_",Languagechoose,".pdf"))}
  
  
  
}

