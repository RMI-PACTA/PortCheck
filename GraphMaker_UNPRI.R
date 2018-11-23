#This project was funded by the European Commission through LIFE program under grant: LIFE16 GIC/FR/000061 - PACTA)
### Graph Functions designed for UNPRI ###

# Required Inputs
# In a data frame format 
# ChartType, ResearchApproach, Scenario, BenchmarkRegion, DomicileRegion, 
# Benchmark, AdditionalInputs (SectorToPlot, TechToPlot), PeerGroup, ToPlot (not always required)

# Provided from the website
ChartInputs <- data.frame(
    ChartType = "EQ",                 # EQ, CB
    ResearchApproach = "Ownership",   # Ownership, PortfolioWeight
    Scenario = "450S",                # 450s, B2DS etc
    BenchmarkRegion = "Global",       # Global, OECD, NonOECD
    DomicileRegion = "Global",        # Global, OECD, NonOECD
    Benchmark = "Market",             # Market or Portfolio
    PeerGroup = "TopFunds",           # Optional/ToBeDefined
    ToPlot = "Power"                  # Sector/Technology/NotRequired
)

# To easily access the input variable
CI <- function(variable){
  output <- as.character(ChartInputs[variable][[1]])
  return(output)
}



# -------------TECHNOLOGY MIX CHARTS ---------- #

sector_techshare <- function(plotnumber,ChartInputs){
  
  if (CI("ChartType") == "EQ"){
    Combin <- EQCombin
    Batch <- EQBatchTest
  }else if (CI("ChartType") == "CB"){
    Combin <- CBCombin
    Batch <- CBBatchTest
  }
  
  #Remove all portfolios other than Market, Average
  Batch <- subset(Batch, Type != "Portfolio")
  
  #Add our target portfolio back
  Portfolios <- rbind(Combin,Batch)
  
  #Filter and select
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
    
    technologyorder <- technology_order
    colours <- as.vector(ColourPalette[["Colours"]])
    names(colours) <- technologyorder
    labels <- c("Renewables","Hydro","Nuclear","Gas","Coal","Electric","Hybrid","ICE","Gas","Oil","Coal")
    names(labels) <- technologyorder
    
    
    Production$Technology <- factor(Production$Technology, levels = technologyorder)
    Production$Sector <- factor(Production$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
    
    Production$Type <- wrap.labels(Production$Type,20)
    Production$Type <- factor(Production$Type, levels=c("Portfolio","MetaPortfolio","Market"))
    xlabels = c("Your Portfolio", "All Insurers", ifelse(ChartType=="EQ","Listed Market","Bond Universe"))
    
    titles = c("Fossil Fuel Production", "Power Capacity", "Vehicle Production")
    names(titles) <- c("Fossil Fuels", "Power", "Automotive")
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    #Production$Type <- factor(Production$Type)
    
    Production <- subset(Production, select = c("Type", "Sector", "Technology", "Value"))
    
    plottheme<- stacked_bar_chart(Production, colours, labels)+
      ylab("Share of Sector Production")+
      theme(plot.title = element_text(hjust =0.5,colour=textcolor,size=textsize,margin = unit(c(0,0,1,0),"lines")),
            legend.position = "bottom",
            legend.title = element_blank())+
      scale_x_discrete(labels = xlabels)
    
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Production, Sector == SectorToPlot)
      p1 <- template %+% dat +
        ggtitle(titles[names(titles) == SectorToPlot])
      
      # print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){
        SectorToPlot <- "FossilFuels"
      }
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
      
    } else if (SectorToPlot == "All"){
      dat <- subset(Production,Sector=="Automotive")
      if (nrow(subset(dat, Type=="Portfolio")) > 0) {  
        p1 <- plottheme %+% dat +
          ggtitle("Vehicle Production")
      } else {
        dat <- rbind(dat, c("Type" = "Portfolio",
                            "Sector" = "Automotive",
                            "Technology" = "ICE",
                            "Value" = 0))
        dat$Value <- as.numeric(dat$Value)
        p1 <- plottheme %+% dat +
          ggtitle("Automotive Production") +
          geom_text(data = subset(dat,Type=="Portfolio"),
                    aes(Type, y = .5, angle = 90, label = "No Automotive Production"))
      }
      
      
      dat <- subset(Production,Sector=="Fossil Fuels")
      if (nrow(subset(dat, Type=="Portfolio")) > 0) {  
        p2 <- plottheme %+% dat +
          ggtitle("Fossil Fuel Production")
        
      } else {
        dat <- rbind(dat, c("Type" = "Portfolio",
                            "Sector" = "Fossil Fuels",
                            "Technology" = "OilProd",
                            "Value" = 0))
        dat$Value <- as.numeric(dat$Value)
        p2 <- plottheme %+% dat +
          ggtitle("Fossil Fuel Production") +
          geom_text(data = subset(dat,Type=="Portfolio"),
                    aes(Type, y = 0.5, angle = 90, label = "No Fossil Fuel Production"))
      }
      
      dat <- subset(Production,Sector=="Power")
      if (nrow(subset(dat, Type=="Portfolio")) > 0) {  
        p3 <- plottheme %+% dat +
          ggtitle("Power Capacity")
      } else {
        dat <- rbind(dat, c("Type" = "Portfolio",
                            "Sector" = "Power",
                            "Technology" = "CoalCap",
                            "Value" = 0))
        dat$Value <- as.numeric(dat$Value)
        p3 <- plottheme %+% dat +
          ggtitle("Power Capacity") +
          geom_text(data = subset(dat,Type=="Portfolio"),
                    aes(Type, y = .5, angle = 90, label = "No Power Capacity"))
      }
      
      cmd<-grid.arrange(p2,
                        p3+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),
                        p1+theme(axis.text.y = element_blank(), axis.title.y = element_blank()),nrow=1)
      dev.off()
      if(PrintPlot){print(cmd)}
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
    }
  }
}