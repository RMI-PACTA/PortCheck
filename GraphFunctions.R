# --------REPORT FUNCTIONS--------------------

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

# --------GENERAL PLOT FUNCTIONS---------
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
   area_6 <<- "#e07b73"
  area_4_6 <<- "#fde291"
  area_2_4 <<- "#FFFFCC"
  area_2 <<- "#c3d69b"
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

                           
# -------- Seperate Ranking chart -----------

RankPortfolios <- function( ChartType, Name){
  a<-Name
  if (ChartType == "EQ"){
    PortfolioExposures <- EQBatchTest[which(EQBatchTest$Year==Startyear+5),]
    
    
  }else if (ChartType == "CB"){
    PortfolioExposures <- CBBatchTest[which(CBBatchTest$Year==Startyear+5),]
    
  }

  # Order the table for Green vs Brown Tech

  badtech <- c("CoalCap","GasCap","ICE","Oil","Gas","Coal")
  goodtech <- c("Electric", "Hybrid","RenewablesCap", "HydroCap", "NuclearCap")
  PortfolioExposures$Technology<- as.factor(PortfolioExposures$Technology)
  PortfolioExposures<-PortfolioExposures[!PortfolioExposures$Technology %in% "OilCap",]
  #PortfolioExposures$forrank <- NA
  #PortfolioExposures[PortfolioExposures$Technology %in% goodtech,]$forrank<- PortfolioExposures[PortfolioExposures$Technology %in% goodtech,]$CarstenMetric_Port
  #PortfolioExposures[PortfolioExposures$Technology %in% badtech,]$forrank<- 1- PortfolioExposures[PortfolioExposures$Technology %in% badtech,]$CarstenMetric_Port
  
  #PortfolioExposures$forrank <- as.numeric(PortfolioExposures$forrank)
  if (PortfolioExposures $Technology %in%  badtech){
    PortfolioExposures $Exp.Carsten.Market <-PortfolioExposures $Exp.Carsten.Market *-1
  }
  # ranking
  # smallest number is number 1
  PortfolioExposures<-PortfolioExposures %>%
    group_by(Technology) %>%
    mutate(my_ranks = order(order(Exp.Carsten.Market,decreasing = TRUE)),
           mx = max(my_ranks))
  #order(forrank,decreasing=TRUE),

  # colnames(rankingtable)[1] <- "PortName"
  rankingtable <- subset(PortfolioExposures, select = c(PortName ,Technology, my_ranks,mx))
  # rankportfolio <- rankingtable[rankingtable$PortName == PortName,]
  
  return(rankingtable)
}                           
                           
# ------------- RANKING CHART - ALIGNMENT ----#

ranking_chart_alignment <- function(plotnumber,ChartType,SectorToPlot){
  if (ChartType == "EQ"){
    Exposures <- EQCombin[which(EQCombin$Year==Startyear+5),]
    Ranks<- RankPortfolios("EQ",PortName)
    BatchTest<-EQBatchTest[!EQBatchTest$Technology %in% "OilCap",]
  }else if (ChartType == "CB"){
    Exposures <- CBCombin[which(CBCombin$Year==Startyear+5),]
    Ranks<- RankPortfolios("CB",PortName)
    BatchTest<-CBBatchTest[!CBBatchTest$Technology %in% "OilCap",]

  }
  
  Exposures <- merge(Exposures,Ranks, by =c("PortName","Technology"))
  Exposures$Exp.Carsten.Market <- Exposures$Exp.Carsten.Scen.Port.Scen.Market/Exposures$CarstenMetric_Port.Market  #will be delete once file is updated
  BatchTest$Exp.Carsten.Market <- BatchTest$Exp.Carsten.Scen.Port.Scen.Market/BatchTest$CarstenMetric_Port.Market  #will be delete once file is updated
  
  if (Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")){
    Exposures$Exp.Carsten.Market <-Exposures$Exp.Carsten.Market *-1
  }
  if (BatchTest$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")){
    BatchTest$Exp.Carsten.Market <-BatchTest$Exp.Carsten.Market *-1
  }
  
  Mins<- aggregate(Exp.Carsten.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], min)  #
  Maxs <- aggregate(Exp.Carsten.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], max) # need define variable
  MinMax <- merge(Mins,Maxs, by="Technology")
  colnames(MinMax)[2] <- "Minimum"
  colnames(MinMax)[3] <- "Maximum"
   
   Exposures <- merge(Exposures,MinMax,by="Technology")
   Exposures$Technology<- as.factor(Exposures$Technology)
   Exposures$Sector<- as.factor(Exposures$Sector)
   levels(Exposures$Sector)[levels(Exposures$Sector)=="Oil&Gas"] <- "FossilFuels"
   levels(Exposures$Sector)[levels(Exposures$Sector)=="Coal"] <- "FossilFuels"
   levels(Exposures$Technology)[levels(Exposures$Technology)=="HydroCap"] <- "Hydro"
   levels(Exposures$Technology)[levels(Exposures$Technology)=="NuclearCap"] <- "Nuclear"
   #Exposures$rank <- rank(Exposures$CarstenMetric_Port)
   # Factorise and Order by Technology  
   #PlotData <- PlotData[(order(PlotData$order)),]
   #PlotData$order <- factor(PlotData$order, levels = PlotData$order)
  
     
    # ranking
    # smallest number is number 1
   
    # Reduce chart to values to plot 
   
   ordrsec<- c("Power","FossilFuels","Automotive")
   ordrsec<-as.factor(ordrsec)
   Exposures$Sector<-factor(Exposures$Sector, ordrsec)
   
   ordrtech<-c("Electric","Hybrid","ICE","Coal","Gas","Oil","CoalCap","GasCap","Nuclear","Hydro","RenewablesCap")
   ordrtech<-as.factor(ordrtech)
   Exposures$Technology<- factor(Exposures$Technology, levels = ordrtech)
   Exposures<- Exposures[order(Exposures$Technology),]

   n1<-n_distinct(Exposures[which(Exposures$Sector=="Automotive"),]$Technology)
   if(n1 >0){a<-c(1:n1)}else{a<-c(n1:1)}
   n2<-n_distinct(Exposures[which(Exposures$Sector=="FossilFuels"),]$Technology)
   if((4.5+n2-1) >4.5){b<-c(4.5:(4.5+n2-1))}else{b<-c((4.5+n2-1):4.5)}
   n3<-n_distinct(Exposures[which(Exposures$Sector=="Power"),]$Technology)
   if((4.5+n2+0.5) >((4.5+n2+0.5)+n3-1)){d<-c(((4.5+n2+0.5)+n3-1):(4.5+n2+0.5))}else{d<-c((4.5+n2+0.5):((4.5+n2+0.5)+n3-1))}
  
   locations<-c(a,b,d)

   #  if (SectorToPlot != "All"){
   #     Exposures <- subset(Exposures, Exposures$Sector %in% SectorToPlot)
   #     if (SectorToPlot == "Power"){
   #       Exposures <- subset(Exposures, Exposures$Technology %in% c("RenewablesCap", "GasCap", "CoalCap"))
   #     }
   #     locations <- c(1:nrow(Exposures))
   #  }else{
   #    locations <- c(1:3,4.5:6.5,8:12)
   #  }
    # 
    # Chart variables
    barwidth <- .03
    bh <-0.6
    tbwid <- .25
    # Label Wrapping Functions  
    # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
    # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
    
    Exposures$a <- paste0(gsub(" ","",Exposures$Sector),"_Unit")
    Exposures$b <- paste0("T_",Exposures$Technology)
    Exposures$b[Exposures$Sector %in% "FossilFuels"] <- paste0("T_",Exposures$Technology[Exposures$Sector %in% "FossilFuels"],"Prod")
    
    # Line Labels
    Exposures$TechTitle <-paste0(t(GT[Exposures$b])," ",t(GT[Exposures$a]))
    Exposures$TechTitle[Exposures$Sector %in% "Automotive"] <- paste0(t(GT[Exposures$b[Exposures$Sector %in% "Automotive"] ]))
    
    Exposures$TechLabel <- Exposures$TechTitle
   
    Exposures$Locations <- locations
    
    Exposures$LowLim <- rowMins(as.matrix(Exposures[,colnames(Exposures) %in% c("Minimum","LowLim")]))
    Exposures$UppLim <- rowMaxs(as.matrix(Exposures[,colnames(Exposures) %in% c("Maximum","UppLim")]))
    
    Exposures$xlowloc <- Exposures$LowLim/100
    Exposures$xupploc <- Exposures$UppLim/100
    # PlotData$comploc <- PlotData[,PortName]/100
    # PlotData$comploc[PlotData$comploc < 0] <- 0
    # PlotData$comploc[PlotData$comploc > 2] <- 2
    
    Exposures$comploc<-Exposures$Exp.Carsten.Market/100
    # PlotData$complabel[PlotData$complabel>200]<-200
    # PlotData$complabel[PlotData$complabel<0]<-0    
    
    Exposures$complabel <-paste0(round(Exposures$comploc,1),"%")
    Exposures$minlabel<- -100 #round(PlotData$LowLim*100,0)
    Exposures$maxlabel<- 100 #round(PlotData$UppLim*100,0)        
    
    Exposures$minlabel <- paste0(Exposures$minlabel, " %")
    Exposures$maxlabel <- paste0(Exposures$maxlabel, " %")
    
    #Exposures$my_ranks[!is.na(Exposures$my_ranks)]<- round(Exposures$Rank[!is.na(Exposures$my_ranks)],0)
    Exposures$my_ranks[is.na(Exposures$my_ranks)]<- "-"
    #Exposures$mx[is.na(Exposures$mx)]<- "-"
    GraphTitle <- GT["Rank_Title"][[1]]
    
    repval = 200
    redgreen<- colorRampPalette(c("red","white", "darkgreen"))(repval) 
    xvals <- rep(seq(-1,1,2/(repval-1)),length(locations))
    #xvals <- xvals[which(xvals<1)]
    yvals <- sort(rep(locations,repval))
    plotdf <- data.frame(x=xvals,y=yvals,w=2.05/repval,h=bh, colbar=rep(redgreen,length(locations)))
    #plotdf <- plotdf[which(plotdf$x<1),]
    
    
    #xmx<- as.numeric(aggregate(CarstenMetric_Port.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], min)[2][,1])
    
    outputplot <-    ggplot()+
      geom_tile(data=plotdf, aes(x=x,y=y),height=plotdf$h,width=plotdf$w,fill=plotdf$colbar) +
   
      scale_x_continuous()+
      scale_y_discrete()+
      
      # error lines
      geom_segment(data=Exposures,aes(x=xlowloc, xend=xupploc,y=Locations,yend=Locations), linetype="dashed",colour="black")+
      geom_point(data=Exposures,aes(x=xlowloc,y=Locations), fill="black",colour="black", size=2)+
      geom_point(data=Exposures,aes(x=xupploc,y=Locations),  fill="black",colour="black",size=2)+
    
      # centre alignment line    # xmax
      annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour=Tar2DColour ,fill = "transparent")+
      annotate(geom="rect",xmin =-1,xmax=0,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour=Tar2DColour)+ # Box around the bars
      # annotate(geom="rect",xmin = 0,xmax=0.001,ymin = 0.7,ymax=1.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.002,ymin = 1.7,ymax=2.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.01,ymin = 2.7,ymax=3.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.019,ymin = 4.2,ymax=4.8,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.021,ymin = 5.2,ymax=5.8,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.004,ymin = 6.2,ymax=6.8,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.007,ymin = 7.7,ymax=8.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.002,ymin = 8.7,ymax=9.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.003,ymin = 9.7,ymax=10.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # annotate(geom="rect",xmin = 0,xmax=0.005,ymin = 10.7,ymax=11.3,colour=Tar2DColour ,fill = "transparent")+ #linetype="dashed",
      # 
      # Company Circles
      geom_point(data=Exposures,aes(x=comploc,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
      annotate(geom="text",label=Exposures$complabel, x= Exposures$comploc, y= Exposures$Locations, colour="white",size=rel(3))+ 
      
      # Distribution Range 
      annotate(geom="text",x= -1.1, hjust=1 , y= locations,label=Exposures$minlabel,size=rel(3),colour="black")+     # Minimum
      annotate(geom="text",x= 1.1, hjust=0 , y= locations,label=Exposures$maxlabel,size=rel(3),colour="black")+     # Maximum
      
      # Ranking box and label
      annotate("text", label = GT["RankTitle"][[1]], x= 1.5,y = max(locations)+ 0.5, size=rel(3),fontface = "bold",colour="black")+ # Rank Heading
      annotate("text", label = paste0(Exposures$my_ranks," ",GT["RankOF"][[1]]," ",Exposures$mx), x= 1.5,hjust=0.5, y = locations,size=rel(3),fontface = "bold",colour="black")+ # Company Ranking
      
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
    
    
   
      
      leafloc <- c(11,12,2,3)
      
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL)+
        annotate(geom="text",x=-0.2,y=Exposures$Locations[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],12), size=rel(3), hjust=0, fontface = "bold",colour="black")+  # Technology Label - Black
        annotate(geom="text",x=-0.2,y=Exposures$Locations[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")+ 
        geom_hline(yintercept = c(3.75,7.25))
    
      
      #write.csv(Exposures,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
      
      graphheight <- 7.2
   
     

    
   #outputplot <- ggplot_gtable(ggplot_build(outputplot))
   # outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
   # grid.draw(outputplot)  
  
    print(outputplot)

  #return()
}

# ------------- TECH SHARE CHARTS ----------- #

company_techshare <- function(plotnumber, companiestoprint, ChartType, SectorToPlot){
  # ChartType = "CB"
  # # plotnumber = 99
  # companiestoprint = 20
  # SectorToPlot = "Power"
  if (ChartType == "EQ"){
    CompProdSS <- EQCompProdSnapshot
    combin <- EQCombin
    market <- EQBatchTest[EQBatchTest$Type == "Market",]

  } else if(ChartType == "CB"){
    CompProdSS <- CBCompProdSnapshot
    combin <- CBCombin
    market <- CBBatchTest[CBBatchTest$Type == "Market",]
  }

  if (SectorToPlot == "Power"){
    techorder <- c("Coal","Gas","Nuclear","Hydro","Renewables")
    CompProdSS <- subset(CompProdSS, Sector == "Power")
    combin <- subset(combin, Sector == "Power")
    market <- subset(market, Sector == "Power")
  }
  
  if (SectorToPlot == "Automotive"){
    techorder <- c("ICE","Hybrid","Electric")
    CompProdSS <- subset(CompProdSS, Sector == "Automotive")
    combin <- subset(combin, Sector == "Automotive")
    market <- subset(combin, Sector == "Automotive")
  }
  
  if (SectorToPlot == "Fossil Fuels") {
    techorder <- c("Coal", "Gas", "Oil")
    CompProdSS <- subset(CompProdSS, Sector %in% c("Coal","Oil&Gas"))
    combin <- subset(combin, Sector %in% c("Coal","Oil&Gas"))
    market <- subset(market, Sector %in% c("Coal","Oil&Gas"))
    CompProdSS$Sector <- recode(CompProdSS$Sector, `Coal` = "Fossil Fuels", `Oil&Gas` = "Fossil Fuels", .default = CompProdSS$Sector)
    combin$Sector <- recode(combin$Sector, `Coal` = "Fossil Fuels", `Oil&Gas` = "Fossil Fuels", .default = combin$Sector)
    market$Sector <- recode(market$Sector, `Coal` = "Fossil Fuels", `Oil&Gas` = "Fossil Fuels", .default = market$Sector)
  }
  
  # if (SectorToPlot == "Oil"){
  #   techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
  #   CompProdSS <- subset(CompProdSS, Technology == "Oil")
  #   combin <- subset(combin, Technology == "Oil")
  #   market <- subset(market, Technology == "Oil")
  # }
  
  if (nrow(combin) > 0) {
    CompProdSS <- subset(CompProdSS, Year == (Startyear+5))
    combin <- subset(combin, Year == (Startyear+5))
    market <- subset(market, Year == (Startyear+5))
    
  
    # Portfolio (Weighted by the AUM)
    Portfoliomix <- subset(combin, select=c("Technology","WtProduction"))
    Portfoliomix$Classification <- "Portfolio"
    Portfoliomix$Name <- PortfolioNameLong
    Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","WtProduction"))
    Portfoliomix$WtProduction <- Portfoliomix$WtProduction / sum(Portfoliomix$WtProduction) * 100
    colnames(Portfoliomix) <- c("Name","Classification","Technology","TechShare")
    
    # Add 2D Target (Global Market under 2D Scenario)
    Targetmix <- subset(combin, select = c("Technology","Scen.WtProduction"))
    Targetmix$Classification <- "Portfolio"
    Targetmix$Name<-GT["X2Target"][[1]]
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Scen.WtProduction"))
    Targetmix$Scen.WtProduction <- Targetmix$Scen.WtProduction / sum(Targetmix$Scen.WtProduction) * 100
    colnames(Targetmix) <- c("Name","Classification","Technology","TechShare")
    
    # Add Benchmark / Global Market
    Marketmix <- subset(market, select=c("Technology","WtProduction"))
    Marketmix$Classification <- "Portfolio"
    Marketmix$Name <- "Market"
    Marketmix <- subset(Marketmix, select=c("Name","Classification","Technology","WtProduction"))
    Marketmix$WtProduction <- Marketmix$WtProduction / sum(Marketmix$WtProduction) * 100
    colnames(Marketmix) <- c("Name","Classification","Technology","TechShare")
    
    # Percentage share of each technology for each company in the portfolio
    colnames(CompProdSS)[colnames(CompProdSS) %in% c("EQY_FUND_TICKER","COMPANY_CORP_TICKER")] <- "Ticker"
    Companies <- subset(CompProdSS, select=c("Ticker","Technology","CompanyLvlProd","CompanyLvlSecProd"))
    Companies$TechShare <- (Companies$CompanyLvlProd/Companies$CompanyLvlSecProd)*100
    Companies$Classification <- "Companies"
    Companies <- Companies[rev(order(Companies$CompanyLvlProd)),]
    TopPortCompanies <- unique(Companies$Ticker)[1:companiestoprint]
    Companies <- subset(Companies, Ticker %in% TopPortCompanies, select = c("Ticker","Classification","Technology","TechShare"))
    colnames(Companies) <- c("Name","Classification","Technology","TechShare")
    Companies[Companies$item == "NA"] <- "NoName"
    Companies$Name <- factor(Companies$Name, levels=TopPortCompanies)
    Companies <- Companies[order(Companies$Name),]
    AllData <- rbind(Marketmix, Targetmix, Portfoliomix, Companies)
    AllData$Name <- factor(AllData$Name, levels=rev(unique(AllData$Name)))
  
    
    if (SectorToPlot == "Power"){  
      
      AllData$Technology <- gsub("Cap","",AllData$Technology)
      AllData <- subset(AllData, AllData$Technology != "Oil")
      AllData$Technology <- factor(AllData$Technology, levels=techorder)
      
      tech_labels <- c(paste0("% ", GT["T_CoalCap"][[1]]),paste0("% ", GT["T_GasCap"][[1]]),
                      paste0("% ", GT["T_NuclearCap"][[1]]),paste0("% ", GT["T_HydroCap"][[1]]),
                      paste0("% ", GT["T_RenewablesCap"][[1]]))
      names(tech_labels) <- techorder
      tech_labels <- factor(tech_labels, levels=tech_labels)
      colors <- c(CoalCapColour,GasCapColour,NuclearColour, HydroColour,RenewablesColour)
      names(colors) <- techorder
    }
    
    if (SectorToPlot == "Automotive"){
      
      AllData$Technology <- factor(AllData$Technology, levels=techorder)
      
      tech_labels <- c(paste0("% ", GT["T_ICE"][[1]]),paste0("% ", GT["T_Hybrid"][[1]]),paste0("% ", GT["T_Electric"][[1]]))
      names(tech_labels) <- techorder
      tech_labels <- factor(tech_labels, levels=tech_labels)
      colors <- c(ICEColour,HybridColour,ElectricColour)
      names(colors) <- techorder
    }
    
    if (SectorToPlot == "Fossil Fuels") {
      
      AllData$Technology <- factor(AllData$Technology, levels=techorder)
      
      labels <- c(paste0("% ", GT["T_CoalProd"][[1]]),paste0("% ", GT["T_GasProd"][[1]]),paste0("% ", GT["T_CoalProd"][[1]]))
      names(labels) <- techorder
      # labels <- factor(labels, levels=labels)
      colors <- c(CoalProdColour,GasProdColour,OilProdColour)
      names(colors) <- techorder
    }
  
    # if (SectorToPlot == "Oil"){
    #   
    # }
  
    PortfolioData <- subset(AllData, Classification == "Portfolio")
    
    CompanyData <- subset(AllData, Classification == "Companies")
    
    PortPlot <- stacked_bar_chart(PortfolioData)+
      scale_fill_manual(values=colors)+
      ggtitle("Templete")+
      xlab("Portfolio")+
      coord_flip()+
      theme(plot.title = element_text(hjust = 0.5,face="bold",colour="black",size=textsize),
            axis.line = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank())
    
    CompPlot <- stacked_bar_chart(CompanyData)+
      scale_fill_manual(values=colors,labels = labels, name = "Technology")+
      xlab("Companies")+
      ylab("TechShare")+
      coord_flip()+
      theme(legend.position = "bottom",legend.title = element_blank(),
            axis.line = element_blank(), plot.title = element_blank())
  
    cmd<-grid.arrange(PortPlot,CompPlot,ncol=1,nrow=2,heights=c(1/4,3/4))
  
    if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_CompanyTechShare.png', sep=""),
           bg="transparent",height=3.2,width=9.7,dpi=ppi)
  } else {
    print(paste0("No ", SectorToPlot, " data to plot."))
  }
}

sector_processing <- function(){
  
  ID.COLS = c("PortName","Year","Sector","Technology","CarstenMetric_Port","Type")
  
  #Filter to our region, scenario, and year
  if(nrow(EQCombin) > 0) {
    EQ <- EQCombin
    EQ$Type <- "Equity Portfolio"
    EQ <- unique(subset(EQ, Year == Startyear, 
                        select = c(ID.COLS)))
  } else {
    EQ <- data.frame("NoResults",2018,"Power","RenewablesCap",0,"Equity Portfolio")
    colnames(EQ) <- ID.COLS
  }
  if(nrow(CBCombin) > 0) {
    CB <- CBCombin
    CB$Type <- "Corporate Bond Portfolio"
    CB <- unique(subset(CB, Year == Startyear, 
                        select = c(ID.COLS)))
  } else {
    CB <- data.frame("NoResults",2018,"Power","RenewablesCap",0,"Corporate Bond Portfolio")
    colnames(CB) <- ID.COLS
  }
  
  #Aggregate by sector, breaking down by the type (equity vs debt)
  df <- rbind(CB,EQ)
  df <- df %>% gather(key=Type, value=Value, -c(ID.COLS))
  df$Sector<-as.factor(df$Sector)
  levels(df$Sector)[levels(df$Sector)=="Coal"] <- "Fossil Fuels"
  levels(df$Sector)[levels(df$Sector)=="Oil&Gas"] <- "Fossil Fuels"
  levels(df$Sector)[levels(df$Sector)=="Power"] <- "Utility Power"
  dfagg <- aggregate(df["CarstenMetric_Port"],by=df[c("Sector","Type","PortName")],FUN=sum)
  
  return(dfagg)
}                                       
   
portfolio_sectorshare <- function(plotnumber){
  
  dfagg <- sector_processing()
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

sector_techshare <- function(plotnumber,ChartType,SectorToPlot){
  # plotnuber = 99
  # ChartType = "EQ"
  # SectorToPlot = "All"
  if (ChartType == "EQ"){
    Combin <- EQCombin
    Batch <- EQBatchTest
  }else if (ChartType == "CB"){
    Combin <- CBCombin
    Batch <- CBBatchTest
  }else if (ChartType == "Summary") {
    Combin <- rbind(subset(EQCombin,select=c("Year","BenchmarkRegion","Scenario","PortName","Sector",
                                                "Technology","WtProduction","Type")),
                       subset(CBCombin,select=c("Year","BenchmarkRegion","Scenario","PortName","Sector",
                                                "Technology","WtProduction","Type")))
    Batch <- rbind(subset(EQBatchTest,
                          select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology",
                                   "WtProduction","Type")),
                   subset(CBBatchTest,
                          select=c("Year","BenchmarkRegion","Scenario","PortName","Sector","Technology",
                                   "WtProduction","Type")))
  }
  
  #Tag Target portfolio, benchmark
  Batch <- subset(Batch, Type != "Portfolio")
  Portfolios <- rbind(Combin,Batch)
  Production <- subset(Portfolios, Year == Startyear &
                         Technology != "OilCap",
                       select=c("PortName","Sector","Technology","WtProduction","Type"))
  Production$Sector <- as.factor(Production$Sector)
  levels(Production$Sector)[levels(Production$Sector)=="Coal"] <-"Fossil Fuels"
  levels(Production$Sector)[levels(Production$Sector)=="Oil&Gas"] <-"Fossil Fuels"
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","Type")
  Production <- Production %>% gather(key=Metric, value=Value, "WtProduction")
  Production <- aggregate(Production["Value"],by=Production[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  
  
  if(nrow(Production)>0){
    Production[Production$Sector=="Fossil Fuels",]$Technology <- paste0(Production$Technology[Production$Sector %in%"Fossil Fuels"],"Prod")

    ylabel <- GT["StackedBarYLabel_FF"][[1]]
    technologyorder <- c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap","Electric","Hybrid","ICE","CoalProd","GasProd","OilProd")
    colours <- c(CoalCapColour,GasCapColour,NuclearColour,HydroColour,RenewablesColour,ElectricColour,HybridColour,ICEColour,CoalProdColour,GasProdColour,OilProdColour)
    names(colours) <- technologyorder
    labels <- c("Coal","Gas","Nuclear","Hydro","Renewables","Electric","Hybrid","ICE","Coal","Gas","Oil")
    names(labels) <- technologyorder
    
    
    Production$Technology<-as.factor(Production$Technology)
    Production$Sector<-as.factor(Production$Sector)
    
    Production$Type <- wrap.labels(Production$Type,20)
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    Production$Type <- factor(Production$Type)

    Production <- subset(Production, select = c("Type", "Sector", "Technology", "Value"))
    colnames(Production) <- c("item", "family", "score", "value")
    
    template <- stacked_bar_chart(Production)+
      ggtitle("Template")+
      ylab("TechShare")+
      scale_fill_manual(labels=labels,values=colours)+
      theme(plot.title = element_text(hjust = 0.5,face="bold",colour="black",size=textsize),
            legend.position = "bottom",legend.title = element_blank(),
            axis.line = element_blank())
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Production, Sector == SectorToPlot)
      p1 <- template %+% dat +
        ggtitle(paste0(unique(as.character(dat$Sector)),"Production"))
      
      print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){
        SectorToPlot <- "FossilFuels"
      }
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
      
    } else if (SectorToPlot == "All"){
      dat <- subset(Production,family=="Automotive")
      if (nrow(subset(dat, item=="Portfolio")) > 0) {  
        p1 <- template %+% dat +
          ggtitle("Automotive Production")
      } else {
        dat <- rbind(dat, c("item" = "Portfolio",
                            "family" = "Automotive",
                            "score" = "ICE",
                            "value" = 0))
        dat$value <- as.numeric(dat$value)
        p1 <- template %+% dat +
          ggtitle("Automotive Production") +
          geom_text(data = subset(dat,item=="Portfolio"),
                 aes(item, y = .5, angle = 90, label = "No Automotive Data Available"))
      }
      
      dat <- subset(Production,family=="Fossil Fuels")
      if (nrow(subset(dat, item=="Portfolio")) > 0) {  
        p2 <- template %+% dat +
          ggtitle("Fossil Fuels Production")
        
      } else {
        dat <- rbind(dat, c("item" = "Portfolio",
                            "family" = "Fossil Fuels",
                            "score" = "OilProd",
                            "value" = 0))
        dat$value <- as.numeric(dat$value)
        p2 <- template %+% dat +
          ggtitle("Fossil Fuels Production") +
          geom_text(data = subset(dat,item=="Portfolio"),
                 aes(item, y = .5, angle = 90, label = "No Fossil Fuel Data Available"))
      }
      
      dat <- subset(Production,family=="Power")
      if (nrow(subset(dat, item=="Portfolio")) > 0) {  
        p3 <- template %+% dat +
          ggtitle("Power Capacity")
      } else {
        dat <- rbind(dat, c("item" = "Portfolio",
                            "family" = "Power",
                            "score" = "CoalCap",
                            "value" = 0))
        dat$value <- as.numeric(dat$value)
        p3 <- template %+% dat +
          ggtitle("Power Capacity") +
          geom_text(data = subset(dat,item=="Portfolio"),
                 aes(item, y = .5, angle = 90, label = "No Power Data Available"))
      }
      
      cmd<-grid.arrange(p2,
                        p3+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
                        p1+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),nrow=1)
      
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
    }
  }
}

# ------------ 246 Chart -------------------- #
Inputs246 <- function(ChartType, TechToPlot){

  if (ChartType == "EQ"){
    Combin <- EQCombin
    Aldprod <- EQALDAggProd
  }else if(ChartType == "CB"){
    Combin <- CBCombin
    Aldprod <- CBALDAggProd
  }


  Aldprod$PortName <- gsub(" ", "", Aldprod$PortName, fixed=TRUE)
  Combin<- merge(Combin,Aldprod, by =c("PortName","Technology","Year"))
  Combin <- subset(Combin,select=c("PortName","Technology","Year","Sector.x","Plan.Pct.Build.Out","Plan.Build.Out","InvestorName.x","Scenario.y"))
  Combin <- subset(Combin,Scenario.y %in% Scenariochoose)
  ### Function to calculate the % Build Out over 5 years
  ### data frame needs Year, Prod and TargetProd and a label for the Chart
  BuildOutCalc <- function(df, TechToPlot){
    Tech <- c("RenewablesCap","HydroCap","NuclearCap",
                  "Coal","CoalCap","Gas","GasCap","Oil","OilCap")
    
    if (TechToPlot %in% Tech){
      names(df)[names(df)== "AnnualvalIEAtech"]<-"Prod"
      df$Scenario <- as.factor(df$Scenario)
      df<- df %>%
        group_by(Scenario) %>%
        arrange(Year) %>%
        mutate(Diff=(df[which(df$Year == (Startyear+5)&df$Scenario=="450S"),]$Prod-df[which(df$Year == Startyear&df$Scenario=="450S"),]$Prod),
               value=Prod- first(Prod))
      
      # library(data.table)
      # df<-data.table(df)
      # setkey(df,Scenario)
      # df[, value:= c(NA, diff(Prod)), by = Scenario]
      # setDF(df)
      #df[is.na(df$value),]$value <- 0
      #df$Diff=(df[which(df$Year == (Startyear+5)&df$Scenario=="450S"),]$Prod-df[which(df$Year == Startyear&df$Scenario=="450S"),]$Prod)
      if (df$Diff <0){df$Diff<- df$Diff*-1}
      df$Plan.Pct.Build.Out<-df$value/df$Diff
      names(df)[names(df)=="Scenario"]<-"Label"
      df$Prod <- df$TargetProd <- NULL
      
    } else if (TechToPlot =="Electric"){
      Year <- rep(2018:2023,3)
      Plan.Pct.Build.Out <- c(0,0.2,0.4,0.6,0.8,1,0,0.1,0.2,0.3,0.4,0.5,0,0.15,0.3,0.45,0.6,0.75)
      Label <- c(rep(c("450S"),6),rep(c("CPS"),6),rep(c("NPS"),6))
      df <- data.frame(Year, Plan.Pct.Build.Out, Label)
      
    }else if (TechToPlot =="ICE"){
      Year <- rep(2018:2023,3)
      Plan.Pct.Build.Out <- c(1,0.983889583,0.967779166,0.951668749,0.935558333,0.919447916,
                              1,1,1,1,1,1,
                              1,0.992183138,0.983249581,0.97319933,0.967615857,0.96091569)
      Label <- c(rep(c("450S"),6),rep(c("CPS"),6),rep(c("NPS"),6))
      df <- data.frame(Year, Plan.Pct.Build.Out, Label)
    }
    return(df)
  }
  #
  ### Production Inputs - normalised to the start year
  Production <- subset(Combin, Technology %in% TechToPlot & Year %in% Startyear:(Startyear+5))
  Production <- subset (Production, select=c("Year","Plan.Pct.Build.Out"))
   if (nrow(Production)>0){
    Production$Label <- "Portfolio"
  }



  if (ChartType == "EQ"){

    ### Stock Market Build Out
    MarketBuildOut <- subset( Aldprod, InvestorName == "Market"& Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
    MarketBuildOut <- subset(MarketBuildOut, select =  c("Year","Plan.Pct.Build.Out"))
    MarketBuildOut$Label <- "Stock Market"

  }else{
    ### Debt Market Build Out
    MarketBuildOut <- subset(Aldprod, InvestorName == "Market"& Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
    MarketBuildOut <- subset(MarketBuildOut, select =  c("Year","Plan.Pct.Build.Out"))
    MarketBuildOut$Label <- "Debt Market"

  }

  ### Global Economy Data
  ### To include or not to include...



  ### Inputs to the 246 chart.

  IEATargets246 <- subset(AllIEATargets, BenchmarkRegion == "Global" & Year %in% Startyear:(Startyear+5)  &
                            Scenario %in% c("450S","NPS","CPS"), select = c("Sector","Technology","Scenario","Year","AnnualvalIEAtech"))

  IEATargets <- subset(IEATargets246, Technology %in% TechToPlot)
  IEATargetsRef <- subset(IEATargets, Scenario == "450S", select=c("Year","AnnualvalIEAtech"))
  names(IEATargetsRef)[names(IEATargetsRef)=="AnnualvalIEAtech"] <- "TargetProd"
  IEATargets <- merge(IEATargets,IEATargetsRef, by="Year")


  IEATargets <- BuildOutCalc(IEATargets,TechToPlot)
  #IEATargets <- do.call("rbind", IEATargets)

  IEATargets <- subset(IEATargets, select = c("Label","Year","Plan.Pct.Build.Out"))


  df <- rbind(Production,MarketBuildOut,IEATargets)


  return(df)
}

#----------- Graph   246 ------------- #

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
    IEATargetMax$Plan.Pct.Build.Out <- 4
    IEATargetMax$Label<- "MaxValue"


  df <- rbind(df,IEATargetMax)

  dfwide <- dcast(df,Year~Label, value.var="Plan.Pct.Build.Out")


  if (GoodBad == "Green"){
    dfwide$Line1 <- dfwide$CPS
    dfwide$Line2 <- dfwide$NPS#-dfwide$CPS
    dfwide$Line3 <- dfwide$`450S`#-dfwide$NPS
    dfwide$Line4 <- dfwide$MaxValue#-dfwide$`450S`#(dfwide$`450S`+dfwide$NPS+dfwide$CPS)
    lineorder<- c("Line1","Line2","Line3","Line4")
    Palette <- c(area_6,area_4_6,area_2_4,area_2)
    AreaNames <-  c( "> 6°C","4-6°C","2-4°C","< 2°C")
  }else if (GoodBad == "Brown"){
    dfwide$Line1 <- dfwide$`450S`
    dfwide$Line2 <- dfwide$NPS #- dfwide$`450S`
    dfwide$Line3 <- dfwide$CPS #- dfwide$NPS
    dfwide$Line4 <- dfwide$MaxValue #- dfwide$`450S`
    Palette <- c(area_2,area_2_4,area_4_6,area_6)
    AreaNames <-  c( "< 2°C","2-4°C","4-6°C","> 6°C")
    lineorder <-c("Line4","Line3","Line2","Line1")
  } 

   dftargets <- subset(dfwide, select = c("Year","Line1","Line2","Line3","Line4"))
   dftargets <- melt(dftargets, id.vars =  "Year", variable.name = "Target")

  colourdf <- data.frame(colour=Palette, Target =lineorder, Labels = AreaNames)
  # 
   combined <- sort(union(levels(dftargets$Target), levels(colourdf$Target)))
   dftargets <- merge(dftargets, colourdf, by= "Target")
   dftargets$Target<- factor(dftargets$Target,levels = lineorder, ordered=TRUE)
  # 

  # 
  LineColours <- c(eq_port, stock_market,peer_group,"pink")
  LineColours <- LineColours[1: length(LinesToPlot)]

  year_lab = Startyear
  LineVector <- setNames(LineColours,LinesToPlot)



  ylabel <- "Normalized Built Out"
  
  if(('Portfolio' %in% colnames(dfwide)) == TRUE)  {
  if (GoodBad == "Brown"){
    dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line1)
    outputplot <- ggplot(data = dftargets)+
      geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target))+
      geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[1])],colour =  "Portfolio"), data=dfwide, show.legend=F,size = linesize,linetype="solid")+  # Portfolio
      geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[2])],colour =  "Stock Market"), data=dfwide, size = linesize,linetype="solid")+   # Market
      scale_fill_manual(labels=unique(dftargets$Labels),
                        values=rep(unique(as.character(dftargets$colour)),1))+
      
      scale_color_manual(name="",values = c("Portfolio"=eq_port,"Stock Market"=stock_market))+
      xlab("") +
      ylab(ylabel)+
      coord_cartesian(ylim=c(-2,2))+
      theme_minimal()+
      theme(panel.grid.major = element_line(color="black", size=1),
            panel.grid.minor = element_blank(),
            axis.ticks=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"))
  } else if (GoodBad =="Green"){
    dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line3)
    outputplot <- ggplot(data = dftargets)+
      geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target))+
      geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[1])],colour =  "Portfolio"), data=dfwide, show.legend=F,size = linesize,linetype="solid")+  # Portfolio
      geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[2])],colour =  "Stock Market"), data=dfwide, size = linesize,linetype="solid")+   # Market
      scale_fill_manual(labels=rev(unique(dftargets$Labels)),
                                             values=rev(unique(as.character(dftargets$colour))))+
      
      scale_color_manual(name="",values = c("Portfolio"=eq_port,"Stock Market"=stock_market))+
     
      xlab("") +
      ylab(ylabel)+
      coord_cartesian(ylim=c(0,1))+
      theme_minimal()+
      theme(panel.grid.major = element_line(color="black", size=1),
            panel.grid.minor = element_blank(),
            axis.ticks=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"))
  }
    }else{
    if (GoodBad == "Brown"){
      dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line1)
      outputplot <- ggplot(data = dftargets)+
        geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target))+
        geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[2])],colour =  "Market"), data=dfwide, size = linesize,linetype="solid")+   # Market
        scale_fill_manual(labels=unique(dftargets$Labels),
                          values=rep(unique(as.character(dftargets$colour)),1))+
        
        scale_color_manual(name="",values = c("Market"=stock_market))+
        #scale_y_continuous(minor_breaks = seq(2018 ,2023 , 4), breaks = seq(-2, 2, 1))
        #labels=unique(dftargets$Labels)
        xlab("") +
        ylab(ylabel)+
        coord_cartesian(ylim=c(-2,2))+
        theme_minimal()+
        theme(panel.grid.major = element_line(color="black", size=1),
              panel.grid.minor = element_blank(),
              axis.ticks=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.margin = unit(c(.5,1,0.5,.5), "cm"))
    } else if (GoodBad =="Green"){
      dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line3)
      outputplot <- ggplot(data = dftargets)+
        geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target))+
        geom_line(aes(x=dfwide$Year,y=dfwide[as.character(LinesToPlot[2])],colour =  "Market"), data=dfwide, size = linesize,linetype="solid")+   # Market
        scale_fill_manual(labels=(unique(dftargets$Labels)),
                          values=(unique(as.character(dftargets$colour))))+
        
        scale_color_manual(name="",values = c("Market"=stock_market))+
        #scale_y_continuous(minor_breaks = seq(2018 ,2023 , 4), breaks = seq(-2, 2, 1))
        #labels=unique(dftargets$Labels)
        xlab("") +
        ylab(ylabel)+
        coord_cartesian(ylim=c(0,1))+
        theme_minimal()+
        theme(panel.grid.major = element_line(color="black", size=1),
              panel.grid.minor = element_blank(),
              axis.ticks=element_blank(),
              panel.border = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              plot.margin = unit(c(.5,1,0.5,.5), "cm"))
  }}
  
 
  print(outputplot)


  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),bg="transparent",height=3.6,width=4.6,plot=outputplot,dpi=ppi*2)



}

#----------- Distribution Chart ------------- #

distribution_chart <- function(plotnumber, MetricName, ChartType){
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
    ID.COLS = c("PortName","Year","Sector","Technology", "Type")
    BarColors <- c("Orange")
    Labels <- c("Unexposed", "Exposed", PortName)
    df <- unique(subset(BatchTest, Year == Startyear, 
                        select = c(ID.COLS,MetricCol)))
    
  } else if (MetricName == "Risk Exposure") {
    Title <- "Risk Exposure of Portfolios"
    MetricCol <- c("Risk2", "Risk1")
    if(ChartType == "CB") {
      PortSS <- CBBatchTest_PortSnapshots
    } else if (ChartType == "EQ") {
      PortSS <- EQBatchTest_PortSnapshots
    }
    PortSS$MoodysRiskLvl[is.na(PortSS$MoodysRiskLvl)] <- "5"
    
    metaport <- PortSS %>%
      group_by(MoodysRiskLvl) %>%
      summarise("PortName" = "MetaPort",
                "ValueUSD" = sum(ValueUSD))
    
    cols <- setdiff(colnames(PortSS),names(metaport))
    metaport[cols] <- 0
    metaport <- metaport[colnames(PortSS)]
    
    PortSS <- rbind(PortSS,metaport)
    
    df <- PortSS %>% 
      group_by(PortName) %>%
      summarise("TotalPortValue" = sum(ValueUSD)) %>%
      ungroup() %>%
      merge(PortSS, by="PortName") %>%
      mutate("PortWeight" = ValueUSD / TotalPortValue) %>%
      spread("MoodysRiskLvl", "PortWeight", fill = 0) %>%
      rename("Risk1" = "1", "Risk2" = "2", "Risk3" = "3", "Risk4" = "4", "Risk5" = "5")

    ID.COLS = c("PortName", "Type")
    df <- unique(subset(df, select = c(ID.COLS,MetricCol)))
        
    BarColors <- c("Orange","Red")
    Labels <- c("Insubstantial Risk","Elevated Risk", "Substantial Risk", PortName)
    
  }
  
  BarColors <- c(BarColors,"Black","skyblue")
  names(BarColors) <- c(MetricCol,"Comparison","Unexposed")
  
  LineHighl <- c("Market")
  LineLabels <- c("Market")
  names(LineLabels) <- LineHighl
  LineColors <- c("Green")
  names(LineColors) <- LineLabels
  

  df <- df %>% gather(key=Metric, value=Value, -c(ID.COLS))
  
  dfagg <- aggregate(df["Value"],by=df[c("PortName","Metric", "Type")],FUN=sum)
  dfagg[dfagg$Type == LineHighl,"Metric"] <- "Reference"
  dfagg[dfagg$PortName == PortName,"Metric"] <- "Comparison"
  dfagg$Value <- as.numeric(dfagg$Value)

  dfagg <- dfagg %>%
    filter(Metric != "Reference") %>%
    group_by(PortName,Type) %>%
    summarise("Value" = 1-sum(Value), "Metric" = "Unexposed") %>%
    ungroup() %>%
    mutate("Metric" = "Unexposed") %>%
    select(PortName,Metric,Type,Value) %>%
    rbind(dfagg)
  
  values = subset(dfagg, Metric=="Comparison" & PortName==PortName)[["Value"]]
  if (MetricName == "Carsten's Metric") {
    portfolio_label = paste0("Your Portfolio\n",
                             "Carsten's Metric: ",percent(values[1]))
  } else if (MetricName == "Risk Exposure") {
    portfolio_label = paste0("Your Portfolio\n",
                             "Substantial Risk: ",percent(values[1]),"\n",
                             "Elevated Risk: ",percent(values[2]))
  }
  order <- dfagg %>% filter(Metric == "Unexposed") %>% arrange(Value)
  dfagg$PortName <- factor(dfagg$PortName, levels=unique(order$PortName))
  dfagg$Metric <- factor(dfagg$Metric, levels=c("Unexposed",MetricCol,"Comparison","Reference"))
  
  x_coord <- length(unique(order$PortName))

  distribution_plot<- ggplot(dfagg)+
    geom_bar(data=subset(dfagg, dfagg$Metric != "Reference"),
             aes(x=PortName, y=Value, fill=Metric),
             stat = "identity", position = "fill", width=1)+
    scale_fill_manual(values=BarColors,labels=Labels)+
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
    xlab(paste0("California Insurers"))+
    ylab(MetricName)+
    theme_distribution()
  
  print(distribution_plot)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_Distribution.png', sep=""),height=3.6,width=3.6,plot=distribution_plot,dpi=ppi*2)
  
}

# -------------STACKED BAR CHARTS ---------- #

stacked_bar_chart <- function(dat){
  
  colnames(dat) <- c("item", "family", "score", "value")
  
  template <- ggplot(data=dat, aes(x=item, y=value,fill=score),show.guide = TRUE)+
    geom_bar(stat = "identity", position = "fill", width = .6)+
    geom_hline(yintercept = c(.25,.50,.75), color="white")+
    theme_minimal()+
    scale_y_continuous(expand=c(0,0), labels=percent)+
    # expand_limits(0,0)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()
  
  return(template)
}
