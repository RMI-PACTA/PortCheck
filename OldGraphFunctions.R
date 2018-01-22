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
    Portfoliomix$Name <- revalue(Portfoliomix$Name, c(PortGraphName = GT["Pensionfunds"][[1]]))
    
    
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