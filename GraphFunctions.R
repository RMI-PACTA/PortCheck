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
  InsuranceCompanyName <- PortfolioNameLong
  if(PortfolioName == "MetaPort"){InsuranceCompanyName <- "INSURANCE COMPANIES OPERATING IN CALIFORNIA"}
  
  InsuranceCompanyName <- gsub("&","\\\\\\\\&",InsuranceCompanyName)
  
  
  # SizeofPortfolio <- PortfolioBreakdown$comma.PortfolioSize.[PortfolioBreakdown$PortName == PortName]
  if(PortfolioName == "MetaPort"){SizeofPortfolio <- 4020919115682
  } else{
    SizePortfolio <-  Subgroup.Overview %>%
      filter(Portfolio.Name == PortName) %>%
      distinct(Port.ValueUSD)
    SizeofPortfolio<- SizePortfolio[[1]]
  }
  
  comprss_long <- function(tx) { 
    tx[is.na(tx)] <- 0
    div <- findInterval(tx, c(1, 1e3, 1e6, 1e9, 1e12))
    div[div==0] <- 1
    labels <- paste(round(tx/10^(3*(div-1)), 3),
                    c("","Thousand","Million","Billion","Trillion")[div])
    return(labels)
  }
  
  SizeofPortfolio <- comprss_long(SizeofPortfolio)
  TodaysDate <- format(Sys.Date(),format = "%m.%d.%Y")
  
  
  NoPeers <- nrow(TestList)-1
  
  if(HasEquity & HasDebt){AssetClass <- "Corporate Bonds plus Bonds of Largest Government/Municipal Power Producers and Listed Equity"
  }else if(HasEquity & !HasDebt){AssetClass <- "Listed Equity"
  }else if(!HasEquity & HasDebt){AssetClass <- "Corporate Bonds plus Bonds of Largest Government/Municipal Power Producers "}
  
  ### Sector Check
  SectorCheck <- TestList[TestList$PortName == PortName,]
  
  HasPower <- SectorCheck$HasPower.CB| SectorCheck$HasPower.EQ
  HasAuto <- SectorCheck$HasAuto.CB | SectorCheck$HasAuto.EQ
  HasOG <- SectorCheck$HasOilGas.CB | SectorCheck$HasOilGas.EQ
  HasCoal <- SectorCheck$HasCoal.CB | SectorCheck$HasCoal.EQ
  HasPowerCB <- SectorCheck$HasPower.CB
  HasAutoCB <- SectorCheck$HasAuto.CB
  HasOGCB <- SectorCheck$HasOilGas.CB
  HasCoalCB <- SectorCheck$HasCoal.CB
  HasPowerEQ <- SectorCheck$HasPower.EQ
  HasAutoEQ <- SectorCheck$HasAuto.EQ
  HasOGEQ <- SectorCheck$HasOilGas.EQ
  HasCoalEQ <- SectorCheck$HasCoal.EQ
  
  
  
  ### MERGE ALL RESULTS ###
  reportdata <<- data.frame(
    c("InsuranceCompanyName",InsuranceCompanyName),
    c("SizeofPortfolio",SizeofPortfolio),
    c("TodaysDate",TodaysDate),
    c("ClimateRelevant",ClimateRelevant),
    c("AnalysisCoverage",AnalysisCoverage),
    c("HasPower",HasPower),
    c("HasAuto",HasAuto),
    c("HasOG",HasOG),
    c("HasCoal",HasCoal),
    c("HasCarbonBudget",HasCarbonBudget),
    c("HasPowerCB",HasPowerCB),
    c("HasAutoCB",HasAutoCB),
    c("HasOGCB",HasOGCB),
    c("HasCoalCB",HasCoalCB),
    c("HasPowerEQ",HasPowerEQ),
    c("HasAutoEQ",HasAutoEQ),
    c("HasOGEQ",HasOGEQ),
    c("HasCoalEQ",HasCoalEQ),
    c("NoPeers",NoPeers),
    c("AssetClass", AssetClass)#,
    # c("FFSectorPortEQ",FFSectorPortEQ),
    # c("PowerSectorPortEQ",PowerSectorPortEQ),
    # c("AutoSectorPortEQ",AutoSectorPortEQ),
    # c("FFSectorPortCB",FFSectorPortCB),
    # c("PowerSectorPortCB",PowerSectorPortCB),
    # c("AutoSectorPortCB",AutoSectorPortCB)
  )
  
  colnames(reportdata) <- as.character(unlist(reportdata[1,]))
  reportdata = reportdata[-1, ]
  
  
  return(reportdata)
  
}

CAReport <- function(){
  
  reportdata <-CAReportData()
  HasAuto <- as.logical(reportdata$HasAuto[[1]])
  HasPower <- as.logical(reportdata$HasPower[[1]])
  HasOG <- as.logical(reportdata$HasOG[[1]])
  HasAutoCB <- as.logical(reportdata$HasAutoCB[[1]])
  HasPowerCB <- as.logical(reportdata$HasPowerCB[[1]])
  HasOGCB <- as.logical(reportdata$HasOGCB[[1]])
  HasAutoEQ <- as.logical(reportdata$HasAutoEQ[[1]])
  HasPowerEQ <- as.logical(reportdata$HasPowerEQ[[1]])
  HasOGEQ <- as.logical(reportdata$HasOGEQ[[1]])
  
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
  
  if (!HasEquity){
    text <- removetextlines("EQSpecific")
  }
  
  if (!HasDebt){
    text <- removetextlines("CBSpecific")
  }
  
  if(!HasAuto){
    text <- removetextlines("AutoSector")
  } else {
    if(!HasAutoCB){
      text <- removetextlines("AutoSector_CB")
    }
    if(!HasAutoEQ){
      text <- removetextlines("AutoSector_EQ")
    }
  }
  if(!HasPower){
    text <- removetextlines("PowerSector")
  } else {
    if(!HasPowerCB){
      text <- removetextlines("PowerSector_CB")
    }
    if(!HasPowerEQ){
      text <- removetextlines("PowerSector_EQ")
    }
  }
  if(!HasOG){
    text <- removetextlines("FossilFuelSector")
  } else {
    if(!HasOGCB){
      text <- removetextlines("FossilFuelSector_CB")
    }
    if(!HasOGEQ){
      text <- removetextlines("FossilFuelSector_EQ")
    }
    if(!HasCarbonBudget){
      text <- removetextlines("CarbonBudget")
    }
  }
  
  # Replace Sector Weight Values
  # a<-data.frame("SectorList"=paste0(rep(c("FF","Power","Auto"),1,each=2),"Sector","Port",rep(c("EQ","CB"),3)))
  # for (j in 1:nrow(a)){
  #   text$text <- gsub(as.character(a$SectorList[j]),reportdata[as.character(a$SectorList[j])][[1]],text$text)
  # }  
  
  # Replace Insurer Name
  # reportdata$InsuranceCompanyName <- gsub("&","\\\\&",reportdata$InsuranceCompanyName)
  text$text <- gsub("InsuranceCompanyName",reportdata$InsuranceCompanyName,text$text)
  text$text <- gsub("SizeofPortfolio",paste0("\\\\$",reportdata$SizeofPortfolio),text$text)
  text$text <- gsub("TodaysDate",reportdata$TodaysDate,text$text)
  text$text <- gsub("NoPeers",reportdata$NoPeers,text$text)
  text$text <- gsub("AssetClass",reportdata$AssetClass,text$text)
  text$text <- gsub("AnalysisCoverage",reportdata$AnalysisCoverage,text$text)
  text$text <- gsub("ClimateRelevant",reportdata$ClimateRelevant,text$text)
  
  
  # Figures
  FigNames<-as.data.frame(readLines("FigureList.txt",skipNul = TRUE))
  colnames(FigNames) <- "Name"
  FigNames$Name <- gsub("\"","",as.character(FigNames$Name))
  FigNames$Fig <- substring(FigNames$Name,1,2)
  FigNames$Fig <- paste0("CAFigures/Fig",FigNames$Fig)
  
  for (f in 1:nrow(FigNames)){
    text$text <- gsub(FigNames$Fig[f],FigNames$Name[f],text$text,fixed = TRUE)
  }
  
  
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
  TemplateNameNew <- paste0("Template_",PortfolioName)
  write.table(text, paste0(TemplateNameNew,".Rnw"),col.names = FALSE,row.names = FALSE,quote=FALSE,fileEncoding = "UTF-8")  
  
  # Create the PDF
  # knit2pdf(paste0(LANGUAGE.PATH,TemplateNameNew,".Rnw"),compiler = "xelatex", encoding = 'UTF-8', engine_args = "-synctex=1", emulation=TRUE)
  knit2pdf(paste0(LANGUAGE.PATH,TemplateNameNew,".Rnw"),compiler = "xelatex", encoding = 'UTF-8')
  
  # Delete remaining files and ReportGraphics Folder
  unlink("ReportGraphics",recursive = TRUE)
  # excessfileendings <- c(".log",".rnw",".tex",".aux")
  excessfileendings <- c(".rnw",".tex")
  file.remove(paste0(TemplateNameNew,excessfileendings))
  file.remove("FigureList.txt")
  
  # Rename output file
  if (InvestorName == PortfolioName){
    file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,".pdf"))}else{
      file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",PortfolioName,".pdf"))}
  
  
  
}

# --------GENERAL PLOT FUNCTIONS---------
# ------------ Theme -------------------------#

theme_barcharts <-function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(colour=textcolor,size=textsize),
        axis.text.y=element_text(colour=textcolor,size=textsize),
        axis.title.x=element_blank(),
        axis.title.y=element_text(colour=textcolor,size=textsize),
        axis.line.x = element_line(colour = textcolor,size=1),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # legend.position=c(0.5,0),#legend.position = "none",
        legend.position = "none",
        legend.direction="horizontal",
        legend.text = element_text(size=textsize,colour=textcolor),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key.size=unit(0.4,"cm"),
        #legend.title=element_blank(),
        legend.title = element_text(colour = textcolor, size = textsize),
        legend.key = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(1,1, 0, 0), "lines"),
        plot.title = element_blank(),
        text=element_text(family="Arial",size = textsize)
        # plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}

theme_linecharts <- function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(), 
        axis.text.x=element_text(colour=textcolor,size=textsize),
        axis.text.y=element_text(colour=textcolor,size=textsize),
        axis.title.x=element_text(colour=textcolor,size=textsize),
        axis.title.y=element_text(colour=textcolor,size=textsize),
        axis.line.x = element_line(colour = textcolor, size=1),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        # legend.position=c(0.5,0),#legend.position = "none",
        legend.position = "none",
        legend.direction="horizontal",
        legend.text = element_text(size=textsize,colour=textcolor),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key.size=unit(0.4,"cm"),
        #legend.title=element_blank(),
        legend.title = element_text(colour = textcolor, size = textsize),
        legend.key = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(1,1, 0, 0), "lines"),
        plot.title = element_blank(),
        text=element_text(family="Arial")
        #plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}    

theme_distribution <- function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(colour=textcolor,size=textsize),
        axis.text.y=element_text(colour=textcolor,size=textsize),
        axis.title.x=element_text(colour=textcolor,size=textsize),
        axis.title.y=element_text(colour=textcolor,size=textsize),
        axis.line.x = element_line(colour = textcolor,size=1),
        axis.line.y = element_line(colour = textcolor,size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(color = textcolor, size = textsize),
        legend.title = element_blank(),
        plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
        plot.background = element_blank(),
        plot.title = element_blank(),
        text=element_text(family="Arial")
  )
}

theme_cdi <- function() {
  theme_minimal(base_size = 11, base_family = "Calibri") +
    theme(
      panel.background = element_rect(fill = "white", color = "white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_line(),
      plot.title = element_text(
        family="Arial",
        face = "bold",
        size = 12,
        hjust = 0
      )
    )
}

theme_246 <- function() {
  theme_minimal(base_size=11, base_family="Calibri") + 
    theme(
      panel.background  = element_rect(fill="white", color="white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(.5,.5,.5,.5), "cm"),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text = element_text(size=11.5, color="#3D3D3C"),
      axis.title = element_text(size=11, color="#3D3D3C"),
      plot.title = element_text(size = 12, 
                                hjust = 0))
}                             

comprss <- function(tx) { 
  tx[is.na(tx)] <- 0
  div <- findInterval(tx, c(1, 1e3, 1e6, 1e9, 1e12))
  div[div==0] <- 1
  labels <- paste("$",round(tx/10^(3*(div-1)), 2),
                  c("","K","Mn","Bn","Tn")[div])
  return(labels)
}

#----------- Distribution Chart ------------- #

distribution_chart <- function(plotnumber, ChartType, df, ID.COLS, MetricCol, ylim,
                               portfolio_label, Title, Labels, BarColors){
  
  df <- df %>% gather(key=Metric, value=Value, -c(ID.COLS))
  
  dfagg <- aggregate(df["Value"],by=df[c(ID.COLS,"Metric")],FUN=sum)
  dfagg$Value <- as.numeric(dfagg$Value)
  
  dfagg <- dfagg %>%
    group_by_(ID.COLS) %>%
    summarise("Value" = 1-sum(Value), "Metric" = "Unexposed") %>%
    ungroup() %>%
    select_(ID.COLS,"Metric","Value") %>%
    rbind(dfagg)
  
  dfagg <- rename(dfagg, "Name" = PortName)
  order <- dfagg %>% filter(Metric == "Unexposed") %>% arrange(Value)
  dfagg$Name <- factor(dfagg$Name, levels=unique(order$Name))
  dfagg <- filter(dfagg, Metric != "Unexposed")
  dfagg$Metric <- factor(dfagg$Metric, levels=c(MetricCol))
  
  x_length <- length(unique(order$Name))
  
  #arrow <- sum(filter(dfagg, Name == PortName)$Value)
  
  ylimval <- ylim
  
  distribution_plot <- ggplot(dfagg)+
    geom_bar(aes(x=Name, y=Value, fill=Metric),
             stat = "identity", width=1)+
    scale_fill_manual(values=BarColors,labels=Labels, breaks=c(MetricCol))+
    scale_y_continuous(expand=c(0,0), limits = c(0,ylimval+0.001), labels=percent)+
    scale_x_discrete(labels=NULL)+
    theme_distribution()+
    expand_limits(0,0)+
    ylab(Title)+
    xlab(paste0("All CA Insurer ",
                ifelse(ChartType == "EQ", "Equity", "Fixed Income"),
                " Portfolios"))
  #arrowlength <- ylimval/5
  
  if (PortName %in% dfagg$Name) {
    x_coord = which(order$Name == PortName)
    is_left = x_coord/x_length < .50
    
    distribution_plot <- distribution_plot +
      geom_vline(xintercept = x_coord, linetype = 2)+
      annotate("text", x = PortName, y = ylim,
               label = portfolio_label,
               hjust = ifelse(is_left, -.05, 1.05),
               vjust = 1,
               size = textsize*(5/14))
    
  }
  
  return(distribution_plot)
  
}

# -------------STACKED BAR CHARTS ---------- #

stacked_bar_chart <- function(dat, colors, bar_labels, legend_labels){
  # "item", "family", "score", "value"
  colnames <- colnames(dat)
  
  plottheme <- ggplot(data=dat, show.guide = TRUE)+
    geom_bar(aes_string(x=colnames[1], y=colnames[4], fill=colnames[3]),
             stat = "identity", position = "fill", width = .6)+
    #geom_hline(yintercept = c(.25,.50,.75), color="white")+
    scale_fill_manual(values=colors,labels = legend_labels, breaks = names(legend_labels))+
    scale_y_continuous(expand=c(0,0), labels=percent)+
    scale_x_discrete(labels=bar_labels)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()
  
  return(plottheme)
}

# ------------- RANKING CHART - ALIGNMENT ----#

ranking_chart_alignment <- function(plotnumber,ChartType){
  
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
  
  
  Exposures[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"),]$Exp.Carsten.Plan.Port.Scen.Market <- -1*Exposures[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"),]$Exp.Carsten.Plan.Port.Scen.Market
  BatchTest[BatchTest$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"),]$Exp.Carsten.Plan.Port.Scen.Market <- -1*BatchTest[BatchTest$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"),]$Exp.Carsten.Plan.Port.Scen.Market
  
  
  Test <- BatchTest %>%
    group_by(Technology) %>%
    do(data.frame(t(quantile(.$Exp.Carsten.Plan.Port.Scen.Market, probs = c(0.25, 0.5,0.75)))))
  
  Test<- as.data.frame(Test) 
  Test$Technology <- as.factor(Test$Technology)
  
  
  Exposures <- merge(Exposures,Test,by="Technology")
  Exposures$Technology<- as.factor(Exposures$Technology)
  Exposures$Sector<- as.factor(Exposures$Sector)
  levels(Exposures$Sector)[levels(Exposures$Sector)=="Oil&Gas"] <- "FossilFuels"
  levels(Exposures$Sector)[levels(Exposures$Sector)=="Coal"] <- "FossilFuels"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="HydroCap"] <- "Hydro"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="NuclearCap"] <- "Nuclear"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="RenewablesCap"] <- "Renewables"
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
  
  ordrtech<-c("Electric","Hybrid","ICE","Coal","Gas","Oil","CoalCap","GasCap","Nuclear","Hydro","Renewables")
  ordrtech<-as.factor(ordrtech)
  Exposures$Technology<- factor(Exposures$Technology, levels = ordrtech)
  Exposures<- Exposures[order(Exposures$Technology),]
  
  
  
  n1<-n_distinct(Exposures[which(Exposures$Sector=="Automotive"),]$Technology)
  if(n1 >0){a<-c(1:n1)}else{a<-c()}
  n2<-n_distinct(Exposures[which(Exposures$Sector=="FossilFuels"),]$Technology)
  if(n2==0){b<-c()}else if(n2>0){
    if (((4.5+n2-1) >4.5)){b<-c(4.5:(4.5+n2-1))}else{b<-c((4.5+n2-1):4.5)}
  }
  n3<-n_distinct(Exposures[which(Exposures$Sector=="Power"),]$Technology)
  if(n3==0){d<-c()}else if(n3>0){
    if ((4.5+n2+0.5) >((4.5+n2+0.5)+n3-1)){d<-c(((4.5+n2+0.5)+n3-1):(4.5+n2+0.5))}else{d<-c((4.5+n2+0.5):((4.5+n2+0.5)+n3-1))}
  }
  locations<-c(a,b,d)
  
  
  # Chart variables
  barwidth <- .03
  bh <-0.6
  tbwid <- .25
  # Label Wrapping Functions  
  # wrap.it <- function(x, len){sapply(x, function(y) paste(strwrap(y, len),collapse = "\n"), USE.NAMES = FALSE)}
  # wrap.labels <- function(x, len){if (is.list(x)){lapply(x, wrap.it, len)} else {wrap.it(x, len)}}
  
  # Exposures$a <- paste0(gsub(" ","",Exposures$Sector),"_Unit")
  # Exposures$b <- Exposures$Technology
  
  Exposures$a <- paste0(gsub(" ","",Exposures$Sector),"_Unit")
  Exposures$b <- paste0("T_",Exposures$Technology)
  Exposures$b[Exposures$Sector %in% "FossilFuels"] <- paste0("T_",Exposures$Technology[Exposures$Sector %in% "FossilFuels"],"Prod")
  
  # Line Labels
  Exposures$TechTitle <-paste0(t(GT[Exposures$b])," ",t(GT[Exposures$a]))
  Exposures$TechTitle[Exposures$Sector %in% "Automotive"] <- paste0(t(GT[Exposures$b[Exposures$Sector %in% "Automotive"] ]))
  
  Exposures$TechLabel <- Exposures$TechTitle
  
  Exposures$Locations <- locations
  
  #Exposures$LowLim <- rowMins(as.matrix(Exposures[,colnames(Exposures) %in% c("Minimum","LowLim")]))
  #Exposures$UppLim <- rowMaxs(as.matrix(Exposures[,colnames(Exposures) %in% c("Maximum","UppLim")]))
  # Exposures[which(Exposures$X25. >1),]$X25. <-1
  # Exposures[which(Exposures$X75. >1),]$X75. <-1
  # Exposures[which(Exposures$X25. <  -1),]$X25. <-1
  # Exposures[which(Exposures$X75. <  -1),]$X75. <-1
  Exposures$X25.<-ifelse(Exposures$X25. >1,1,Exposures$X25.)
  Exposures$X75.<-ifelse(Exposures$X75. >1,1,Exposures$X75.)
  Exposures$X50.<-ifelse(Exposures$X50. >1,1,Exposures$X50.)
  Exposures$X25.<-ifelse(Exposures$X25. < -1,-1,Exposures$X25.)
  Exposures$X75.<-ifelse(Exposures$X75. < -1,-1,Exposures$X75.)
  Exposures$X50.<-ifelse(Exposures$X50. < -1,-1,Exposures$X50.)
  
  
  Exposures$comploc<-Exposures$Exp.Carsten.Plan.Port.Scen.Market*100
  Exposures$complabel <- paste0(round(Exposures$comploc,0),"%")
  Exposures$comploc<-ifelse(Exposures$comploc >100,100,Exposures$comploc)
  Exposures$comploc<-ifelse(Exposures$comploc < -100,-100,Exposures$comploc)
  
  
  Exposures$minlabel<- -100 #round(PlotData$LowLim*100,0)
  Exposures$maxlabel<- 100 #round(PlotData$UppLim*100,0) 
  Exposures$minlabel <- paste0(Exposures$minlabel, " %")
  Exposures$maxlabel <- paste0(Exposures$maxlabel, " %")
  
  #Exposures$my_ranks[!is.na(Exposures$my_ranks)]<- round(Exposures$Rank[!is.na(Exposures$my_ranks)],0)
  Exposures$my_ranks[is.na(Exposures$my_ranks)]<- "-"
  #Exposures$mx[is.na(Exposures$mx)]<- "-"
  GraphTitle <- GT["Rank_Title"][[1]]
  
  repval = 200
  redgreen<- colorRampPalette(c(area_6,area_2_4, area_2))(repval) 
  xvals <- rep(seq(-1,1,2/(repval-1)),length(locations))
  #xvals <- xvals[which(xvals<1)]
  yvals <- sort(rep(locations,repval))
  plotdf <- data.frame(x=xvals,y=yvals,w=2.05/repval,h=bh, colbar=rep(redgreen,length(locations)))
  #plotdf <- plotdf[which(plotdf$x<1),]
  
  
  #xmx<- as.numeric(aggregate(CarstenMetric_Port.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], min)[2][,1])
  #xmx should be 
  outputplot <-    ggplot()+
    geom_tile(data=plotdf, aes(x=x,y=y),height=plotdf$h,width=plotdf$w,fill=plotdf$colbar) +
    
    scale_x_continuous()+
    scale_y_discrete()+
    
    # error lines
    geom_segment(data=Exposures,aes(x=X25., xend=X75.,y=Locations,yend=Locations), linetype="dashed",colour="black")+
    geom_point(data=Exposures,aes(x=X25.,y=Locations), fill="black",colour="black", size=2)+
    geom_point(data=Exposures,aes(x=X75.,y=Locations),  fill="black",colour="black",size=2)+
    geom_point(data=Exposures,aes(x=X50.,y=Locations), fill="white",colour="black", size=2,shape=21)+
    
    # centre alignment line    # xmax
    #annotate(geom="rect",xmin = 0,xmax=Exposures$X50.,ymin = locations-bh/2,ymax=locations+bh/2,colour="green",fill = "transparent")+
    annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour="black",fill = "transparent")+
    annotate(geom="rect",xmin =-1,xmax=1,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
    
    # Company Circles
    geom_point(data=Exposures,aes(x=comploc/100,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
    annotate(geom="text",label=Exposures$complabel, x= Exposures$comploc/100, y= Exposures$Locations, colour="white",size=rel(3))+ 
    
    # Distribution Range 
    annotate(geom="text",x= -1.03, hjust=1 , y= locations,label=Exposures$minlabel,size=rel(3),colour=textcolor)+     # Minimum
    annotate(geom="text",x= 1.03, hjust=0 , y= locations,label=Exposures$maxlabel,size=rel(3),colour=textcolor)+     # Maximum
    
    # Ranking box and label
    
    annotate("text", label = GT["RankTitle"][[1]], x= 1.3,y = max(locations)+ 0.5, size=rel(4),colour=textcolor,fontface = "bold")+ # Rank Heading
    
    annotate("text", label = paste0(Exposures$my_ranks," ",GT["RankOF"][[1]]," ",Exposures$mx), x= 1.3,hjust=0.5, y = locations,size=rel(4),colour=textcolor)+ # Company Ranking
    
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
          plot.margin = (unit(c(0.2, 0.6, 0, 0), "lines")),
          text=element_text(family="Arial"))
  
  
  labelloc <- -1.4
  leafloc <- c(11,12,2,3)
  
  #if (any(Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal",
  #                                     "Electric", "Hybrid","Renewables", "Hydro", "Nuclear"))) {
  outputplot<-    outputplot+
    labs(x=NULL,y= NULL)+
    annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal","Electric", "Hybrid","Renewables", "Hydro", "Nuclear")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal","Electric", "Hybrid","Renewables", "Hydro", "Nuclear")],12), size=rel(4), hjust=0,colour=textcolor)
  
  if ((is.null(a)==FALSE) & (is.null(b)==FALSE) & (is.null(d)==FALSE)){     
    outputplot <- outputplot +
      geom_hline(yintercept = c((tail(a,1)+0.75),(d[1]-0.75)))#+   Technology Label - Black
  } else if ((is.null(a)==FALSE) & (is.null(b)==FALSE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = (tail(a,1)+0.75))
  }else if ((is.null(a)==FALSE) & (is.null(b)==TRUE) & (is.null(d)==FALSE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = (tail(a,1)+1))
  }else if ((is.null(a)==FALSE) & (is.null(b)==TRUE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot
  }else if((is.null(a)==TRUE) & (is.null(b)==FALSE) & (is.null(d)==FALSE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = c((d[1]-0.75)))
  } else if((is.null(a)==TRUE) & (is.null(b)==FALSE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot
  }  else if((is.null(a)==TRUE) & (is.null(b)==TRUE) & (is.null(d)==FALSE)){
    outputplot<-outputplot
  }
  
  #write.csv(Exposures,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
  # if (all(Exposures$Technology %in%  c("Oil","Gas","Coal")))  {
  #   graphheight <-2.3
  # } else if (all(Exposures$Technology %in%  c("Electric", "Hybrid","ICE")))  {
  #     graphheight <-2.3
  # } else if (all(Exposures$Technology %in%  c("CoalCap", "GasCap","Renewables","Hydro","Nuclear"))) {
  #       graphheight <- 5.7
  #       }else {graphheight <- 7.2}
  #   
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_',ChartType,'_rankingchart.png', sep=""),bg="transparent",height=7.2,width=9.7,dpi=ppi)
  
  
  # outputplot <- ggplot_gtable(ggplot_build(outputplot))
  # outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
  # grid.draw(outputplot)  
  if (PrintPlot) {print(outputplot)}
  #return()
}

# ------------- RANKING CHART - ALIGNMENT USING CarstenMetric_Port ----#

ranking_chart_alignment_Carstenmetric <- function(plotnumber,ChartType){
  
  if (ChartType == "EQ"){
    Exposures <- EQCombin[which(EQCombin$Year==Startyear),]
    Ranks<- RankPortfolios("EQ",PortName)
    BatchTest<-EQBatchTest[!EQBatchTest$Technology %in% "OilCap",]
    Market<- subset(BatchTest, Year==Startyear & InvestorName =="Market")
  }else if (ChartType == "CB"){
    Exposures <- CBCombin[which(CBCombin$Year==Startyear),]
    Ranks<- RankPortfolios("CB",PortName)
    BatchTest<-CBBatchTest[!CBBatchTest$Technology %in% "OilCap",]
    Market<- subset(BatchTest, Year==Startyear & InvestorName =="Market")
    
  }
  
  Exposures <- merge(Exposures,Ranks, by =c("PortName","Technology"))
  
  Test <- BatchTest %>%
    group_by(Technology) %>%
    do(data.frame(t(quantile(.$CarstenMetric_Port, probs = c(0.5)))))
  
  Test<- as.data.frame(Test) 
  Test$Technology <- as.factor(Test$Technology)
  
  
  Exposures <- merge(Exposures,Test,by="Technology")
  Exposures$Technology<- as.factor(Exposures$Technology)
  Exposures$Sector<- as.factor(Exposures$Sector)
  levels(Exposures$Sector)[levels(Exposures$Sector)=="Oil&Gas"] <- "FossilFuels"
  levels(Exposures$Sector)[levels(Exposures$Sector)=="Coal"] <- "FossilFuels"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="HydroCap"] <- "Hydro"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="NuclearCap"] <- "Nuclear"
  levels(Exposures$Technology)[levels(Exposures$Technology)=="RenewablesCap"] <- "Renewables"
  
  Market$Technology<- as.factor(Market$Technology)
  levels(Market$Technology)[levels(Market$Technology)=="HydroCap"] <- "Hydro"
  levels(Market$Technology)[levels(Market$Technology)=="NuclearCap"] <- "Nuclear"
  levels(Market$Technology)[levels(Market$Technology)=="RenewablesCap"] <- "Renewables"
  
  ordrsec<- c("Power","FossilFuels","Automotive")
  ordrsec<-as.factor(ordrsec)
  Exposures$Sector<-factor(Exposures$Sector, ordrsec)
  
  ordrtech<-c("Electric","Hybrid","ICE","Coal","Gas","Oil","CoalCap","GasCap","Nuclear","Hydro","Renewables")
  ordrtech<-as.factor(ordrtech)
  Exposures$Technology<- factor(Exposures$Technology, levels = ordrtech)
  Exposures<- Exposures[order(Exposures$Technology),]
  Market$Technology<- factor(Market$Technology, levels = rev(ordrtech))
  Market<- Market[order(Market$Technology),]
  
  
  n1<-n_distinct(Exposures[which(Exposures$Sector=="Automotive"),]$Technology)
  if(n1 >0){a<-c(1:n1)}else{a<-c()}
  n2<-n_distinct(Exposures[which(Exposures$Sector=="FossilFuels"),]$Technology)
  if(n2==0){b<-c()}else if(n2>0){
    if (((4.5+n2-1) >4.5)){b<-c(4.5:(4.5+n2-1))}else{b<-c((4.5+n2-1):4.5)}
  }
  n3<-n_distinct(Exposures[which(Exposures$Sector=="Power"),]$Technology)
  if(n3==0){d<-c()}else if(n3>0){
    if ((4.5+n2+0.5) >((4.5+n2+0.5)+n3-1)){d<-c(((4.5+n2+0.5)+n3-1):(4.5+n2+0.5))}else{d<-c((4.5+n2+0.5):((4.5+n2+0.5)+n3-1))}
  }
  locations<-c(a,b,d)
  
  
  # Chart variables
  barwidth <- .03
  bh <-0.6
  tbwid <- .25
  
  Exposures$a <- paste0(gsub(" ","",Exposures$Sector),"_Unit")
  Exposures$b <- paste0("T_",Exposures$Technology)
  Exposures$b[Exposures$Sector %in% "FossilFuels"] <- paste0("T_",Exposures$Technology[Exposures$Sector %in% "FossilFuels"],"Prod")
  
  # Line Labels
  Exposures$TechTitle <-paste0(t(GT[Exposures$b])," ",t(GT[Exposures$a]))
  Exposures$TechTitle[Exposures$Sector %in% "Automotive"] <- paste0(t(GT[Exposures$b[Exposures$Sector %in% "Automotive"] ]))
  
  Exposures$TechLabel <- Exposures$TechTitle
  
  Exposures$Locations <- locations
  
  Exposures$X50.<-ifelse(Exposures$X50. >0.05,0.05,Exposures$X50.)
  
  
  Exposures$comploc<-Exposures$CarstenMetric_Port*100
  Exposures$complabel <- paste0(round(Exposures$comploc,0),"%")
  Exposures$comploc<-ifelse(Exposures$comploc >5,5,Exposures$comploc)
  
  
  
  Exposures$minlabel<- 0 #round(PlotData$LowLim*100,0)
  Exposures$maxlabel<- 5 #round(PlotData$UppLim*100,0) 
  Exposures$minlabel <- paste0(Exposures$minlabel, " %")
  Exposures$maxlabel <- paste0(Exposures$maxlabel, " %")
  
  #Exposures$my_ranks[!is.na(Exposures$my_ranks)]<- round(Exposures$Rank[!is.na(Exposures$my_ranks)],0)
  Exposures$my_ranks[is.na(Exposures$my_ranks)]<- "-"
  #Exposures$mx[is.na(Exposures$mx)]<- "-"
  GraphTitle <- GT["Rank_Title"][[1]]
  
  plotdf<-data.frame(cb=c(0,0.0050893760,0,0.0067338614,0,0.0049852914,
                          0,0.0119512334,0,0.0106194293,0,0.0160616956,0,0.0122631742,0,0.0055197990,
                          0,0.0133068061,0,0.0004955862,0,0.0001357059), 
                     cb1=c(0.0050893760,0.05,0.0067338614,0.05,0.0049852914,0.05,
                           0.0119512334,0.05,0.0106194293,0.05,0.0160616956 ,0.05,0.0122631742,0.05,0.0055197990,0.05,
                           0.0133068061,0.05,0.0004955862,0.05, 0.0001357059,0.05), 
                     y1=c(0.7,0.7,1.7,1.7,2.7,2.7,4.2,4.2,5.2,5.2,6.2,6.2,7.7,7.7,8.7,8.7,9.7,9.7,10.7,10.7,11.7,11.7), 
                     y2=c(1.3,1.3,2.3,2.3,3.3,3.3, 4.8,4.8,5.8,5.8,6.8,6.8,8.3,8.3,9.3,9.3,10.3,10.3,11.3,11.3,12.3,12.3), 
                     c= c(rep(c(area_6,area_2),3),rep(c(area_2,area_6),5),rep(c(area_6,area_2),3)),
                     eq=c(0,0.0027512839,0,0.0028710250,0,0.0020101071,
                          0,0.0076688983,0,0.0055495046,0,0.0218600200,0,0.0178460760,0,0.0050812161,
                          0,0.0121317846,0,0.0006165246,0,0.0006584942),
                     eq1=c(0.0027512839,0.05,0.0028710250,0.05,0.0020101071,0.05,
                           0.0076688983,0.05, 0.0055495046,0.05, 0.0218600200,0.05, 0.0178460760,0.05,0.0050812161,0.05,
                           0.0121317846,0.05, 0.0006165246,0.05,0.0006584942,0.05)
  )
  
  if (ChartType =="EQ"){
    plotdf<- subset(plotdf,select = c(eq,eq1,y1,y2,c))
    colnames(plotdf)[1] <-"x1"
    colnames(plotdf)[2] <-"x2"
  }else{
    plotdf<- subset(plotdf,select = c(cb,cb1,y1,y2,c))
    colnames(plotdf)[1] <-"x1"
    colnames(plotdf)[2] <-"x2"
  }
  
  
  #xmx<- as.numeric(aggregate(CarstenMetric_Port.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], min)[2][,1])
  #xmx should be
  
  outputplot <-    ggplot()+
    geom_rect(data=plotdf,mapping=aes(xmin=x1,xmax=x2, ymin=y1, ymax=y2,fill=c)) +
    scale_fill_manual(name="",labels=c("area_6","area_2"),values = c(area_2,area_6))+
    
    scale_x_continuous()+
    scale_y_discrete()+
    
    # error lines
    # geom_segment(data=Exposures,aes(x=X25., xend=X75.,y=Locations,yend=Locations), linetype="dashed",colour="black")+
    # geom_point(data=Exposures,aes(x=X25.,y=Locations), fill="black",colour="black", size=2)+
    # geom_point(data=Exposures,aes(x=X75.,y=Locations),  fill="black",colour="black",size=2)+
    geom_point(data=Exposures,aes(x=X50.,y=Locations), fill="white",colour="black", size=2,shape=21)+
    
    # centre alignment line    # xmax
    annotate(geom="rect",xmin = 0,xmax=Market$CarstenMetric_Port,ymin = locations-bh/2,ymax=locations+bh/2,colour="black",fill = "transparent")+
    annotate(geom="rect",xmin = 0,xmax=0.05,ymin = locations-bh/2,ymax=locations+bh/2,colour="black",fill = "transparent")+
    # annotate(geom="rect",xmin =0,xmax=0.1,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
    # 
    # Company Circles
    geom_point(data=Exposures,aes(x=comploc/100,y=Locations),  fill=YourportColour,colour=YourportColour,size=8)+
    annotate(geom="text",label=Exposures$complabel, x= Exposures$comploc/100, y= Exposures$Locations, colour="white",size=rel(3))+ 
    
    # Distribution Range 
    annotate(geom="text",x= -0.0015, hjust=1 , y= locations,label=Exposures$minlabel,size=rel(3),colour=textcolor)+     # Minimum
    annotate(geom="text",x= 0.051, hjust=0 , y= locations,label=Exposures$maxlabel,size=rel(3),colour=textcolor)+     # Maximum
    
    # Ranking box and label
    
    annotate("text", label = GT["RankTitle"][[1]], x= 0.06,y = max(locations)+ 0.5, size=rel(3),colour=textcolor,fontface = "bold")+ # Rank Heading
    
    annotate("text", label = paste0(Exposures$my_ranks," ",GT["RankOF"][[1]]," ",Exposures$mx), x= 0.06,hjust=0.5, y = locations,size=rel(3),colour=textcolor)+ # Company Ranking
    
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
          plot.margin = (unit(c(0.2, 0.6, 0, 0), "lines")),
          text=element_text(family="Arial"),
          legend.position = "none")
  
  
  labelloc <- -0.01
  leafloc <- c(11,12,2,3)
  
  #if (any(Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal",
  #                                     "Electric", "Hybrid","Renewables", "Hydro", "Nuclear"))) {
  outputplot<-    outputplot+
    labs(x=NULL,y= NULL)+
    annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal","Electric", "Hybrid","Renewables", "Hydro", "Nuclear")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal","Electric", "Hybrid","Renewables", "Hydro", "Nuclear")],12), size=rel(3), hjust=0,colour=textcolor)
  
  if ((is.null(a)==FALSE) & (is.null(b)==FALSE) & (is.null(d)==FALSE)){     
    outputplot <- outputplot +
      geom_hline(yintercept = c((tail(a,1)+0.75),(d[1]-0.75)))#+   Technology Label - Black
  } else if ((is.null(a)==FALSE) & (is.null(b)==FALSE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = (tail(a,1)+0.75))
  }else if ((is.null(a)==FALSE) & (is.null(b)==TRUE) & (is.null(d)==FALSE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = (tail(a,1)+1))
  }else if ((is.null(a)==FALSE) & (is.null(b)==TRUE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot
  }else if((is.null(a)==TRUE) & (is.null(b)==FALSE) & (is.null(d)==FALSE)) {
    outputplot <- outputplot +
      geom_hline(yintercept = c((d[1]-0.75)))
  } else if((is.null(a)==TRUE) & (is.null(b)==FALSE) & (is.null(d)==TRUE)) {
    outputplot <- outputplot
  }  else if((is.null(a)==TRUE) & (is.null(b)==TRUE) & (is.null(d)==FALSE)){
    outputplot<-outputplot
  }
  
  #write.csv(Exposures,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
  # if (all(Exposures$Technology %in%  c("Oil","Gas","Coal")))  {
  #   graphheight <-2.3
  # } else if (all(Exposures$Technology %in%  c("Electric", "Hybrid","ICE")))  {
  #     graphheight <-2.3
  # } else if (all(Exposures$Technology %in%  c("CoalCap", "GasCap","Renewables","Hydro","Nuclear"))) {
  #       graphheight <- 5.7
  #       }else {graphheight <- 7.2}
  #   
  
  ggsave(outputplot,
         filename=paste0(plotnumber,"_",PortfolioName,'_',ChartType,'_carstenrankingchart.png', sep=""),
         bg="transparent",height=7.2,width=9.7,dpi=ppi)
  
  
  # outputplot <- ggplot_gtable(ggplot_build(outputplot))
  # outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
  # grid.draw(outputplot)  
  if (PrintPlot) {print(outputplot)}
  #return()
}


# ------------- Sector Classification ------ #
SectorDataAnalysis <- function(){
  over <- Subgroup.Overview
  over$Asset.Type <- ifelse(over$Asset.Type=="Other Holdings", "Other", over$Asset.Type)
  
  
  ###### sector categories #####
  Powr <- c("Alternative Electricity","Conventional Electricity","Multiutilities",
            "Electric-Generation", "Electric-Integrated", "Independ Power Producer","Energy-Alternate Sources", "Utilities","Power.Generation")
  OilGasCoal <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production","Coal", "General Mining",
                  "Oil Comp-Explor&Prodtn", "Oil Comp-Integrated","Oil&Gas Drilling" ,"Exploration...Production","Integrated.Oils","Coal","Metal-Diversified",  "Coal.Operations", "Metals...Mining", "Diversified Minerals")
  Futuresecs <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation",
                  "Bldg Prod-Cement/Aggreg","Steel-Producers", "Metal-Iron","Metal-Aluminum","Transport-Air Freight", "Transport-Marine")
  Auto <- c("Automobiles","Commercial Vehicles & Trucks",
            "Auto-Cars/Light Trucks", "Automobiles.Manufacturing")
  
  # round(sum(filter(over,!Sector %in% c("Other Sectors", "Excluded") &Valid==1)$ValueUSD)/sum(over[which(over$Valid==1),]$ValueUSD)*100,1),"%")
  
  over$Sector <-ifelse (over$Subgroup %in% Powr,"Power","Other Sectors")
  over$Sector <-ifelse (over$Subgroup %in% OilGasCoal,"Fossil Fuels",over$Sector)
  over$Sector <-ifelse (over$Subgroup %in% Auto,"Automotive",over$Sector)
  over$Sector <-ifelse (over$Subgroup %in% Futuresecs,"Other Sectors",over$Sector)
  
  # over$Sector.All <- ifelse(over$Valid == 0, "Excluded", "Climate Relevant w/ 2Ãƒ‚° Scenario")
  # over$Sector.All <- ifelse(over$Sector == "Climate Relevant No 2Ãƒ‚° Scenario" & over$Valid == 1 , "Climate Relevant No 2Ãƒ‚° Scenario",over$Sector.All)
  # over$Sector.All <- ifelse(over$Sector == "Other Sectors" & over$Valid==1, "Other Sectors", over$Sector.All)
  
  
  AnalysisCoverage <<-  round(sum(filter(over,Valid==1)$ValueUSD)/sum(over$ValueUSD)*100,1)
  ClimateRelevant <<- round(AnalysisCoverage/100 * sum(filter(over,!Sector %in% c("Other Sectors", "Excluded") &Valid==1)$ValueUSD)/sum(over[which(over$Valid==1),]$ValueUSD)*100,1)
  
  
  return(over)
}

# ------------- Portfolio Exposure ----#
Overview_portfolio_sector_stack <- function(plotnumber){
  
  
  over <- SecAnalysis
  
  if (PortName == "MetaPort"){
    over$Portfolio.Name <- "MetaPort"
  }
 
  # over$Sector.All <- ifelse(over$Valid == 0, "Excluded", "Climate Relevant w/ 2Ãƒ‚° Scenario")
  # over$Sector.All <- ifelse(over$Sector == "Climate Relevant No 2Ãƒ‚° Scenario" & over$Valid == 1 , "Climate Relevant No 2Ãƒ‚° Scenario",over$Sector.All)
  # over$Sector.All <- ifelse(over$Sector == "Other Sectors" & over$Valid==1, "Other Sectors", over$Sector.All)
  over$Sector[is.na(over$Sector) & over$Sector== "<NA>"]<- "Other Sectors"
  over$Sector <- factor(over$Sector, levels = c("Other Sectors","Fossil Fuels","Power", "Automotive"), ordered=TRUE)
 
  over<- as.data.frame(over)
  
  ## "steelblue" color below should be changed to whatever our Portfolio color is
  over1<- subset(over, Valid==1 & Portfolio.Name ==PortName & Asset.Type %in% c("Equity","Debt"))
  over1$Asset.Type <- gsub("Debt", "Fixed Income",over1$Asset.Type)
  over1$Asset.Type <- factor(over1$Asset.Type,levels=c("Fixed Income","Equity")) 
  over1$Sector <- factor(over1$Sector, levels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), ordered=TRUE) #"Climate Relevant No 2Ãƒ‚° Scenario",
  # over1$Sector.All <- factor(over1$Sector.All, levels=c("Excluded","Other Sectors","Climate Relevant No 2Ãƒ‚° Scenario","Climate Relevant w/ 2Ãƒ‚° Scenario"), ordered=TRUE)
  
  if (PortName!="MetaPort"){
    plot <- ggplot(data=over1, aes(x=Asset.Type, y=ValueUSD, fill=Sector)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(name="", labels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), values=c("#deebf7",energy, trans, pow),drop = FALSE) +
      scale_x_discrete(name="Asset Type",drop=F) +
      scale_y_continuous(name="Market Value (USD)", labels=comprss, expand=c(0,0)) +
      guides(fill=guide_legend(nrow=2))+
      theme_barcharts() +
      theme(legend.position = "bottom",
            legend.text=element_text(size=textsize),
            axis.text.x=element_text(colour=textcolor,size=11),
            axis.text.y=element_text(colour=textcolor,size=11)) 
    
    # portfolio_label = paste0("Climate Relevant: ", round(sum(filter(over1,!Sector %in% c("Other Sectors", "Excluded"))$ValueUSD)/sum(over1$ValueUSD)*100,1),"%")
    ymax<-max(aggregate(over1["ValueUSD"],by=over1["Asset.Type"],FUN=sum)$ValueUSD)
    
  }else {
    plot <- ggplot(data=subset(over1,Valid==1), aes(x=Asset.Type, y=ValueUSD, fill=Sector)) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(name="", labels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), values=c("#deebf7",energy, trans, pow),drop = FALSE) +
      scale_x_discrete(name="Asset Type") +
      scale_y_continuous(name="Market Value (USD)", labels=comprss, expand=c(0,0)) +
      # geom_bar(data=subset(over, Valid==0 & Asset.Type=="Other"), aes(x=Asset.Type, y=ValueUSD), fill="white", stat="identity") +
      guides(fill=guide_legend(nrow=2))+
      theme_barcharts() +
      theme(legend.position = "bottom",
            legend.text=element_text(size=textsize),
            axis.text.x=element_text(colour=textcolor,size=11),
            axis.text.y=element_text(colour=textcolor,size=11)) 
    
    # portfolio_label = paste0("Climate Relevant: ", round(sum(filter(over,!Sector %in% c("Other Sectors", "Excluded") &Valid==1)$ValueUSD)/sum(over[which(over$Valid==1),]$ValueUSD)*100,1),"%")
    ymax<- max(aggregate(over[which(over$Valid==1),]["ValueUSD"],by=over[which(over$Valid==1),]["Asset.Type"],FUN=sum)$ValueUSD)
  }
  
  
  # plot <- plot+
  #   annotate("text", x = "Equity", y = ymax,
  #            label = portfolio_label, color = YourportColour, vjust = 1, hjust = .25, size = textsize*(7/14))
  
  if(PrintPlot){print(plot)}
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_SectorBarChart.png',sep=""),
         bg="transparent",height=3,width=4,dpi=ppi)   #linewidth_in*.9
}

portfolio_sector_stack <- function(plotnumber){

  if (PortName != "MetaPort") {
    over <- Subgroup.Overview %>%
      filter(Portfolio.Name == PortName)
  } else {
    over <- Subgroup.Overview
  }
  # over <- Subgroup.Overview
  over$Asset.Type <- ifelse(over$Asset.Type=="Other Holdings", "Other", over$Asset.Type)
  over$Asset.Type <- gsub("Debt", "Fixed Income",over$Asset.Type)
  
  ###### sector categories #####
  Powr <- c("Alternative Electricity","Conventional Electricity","Multiutilities",
            "Electric-Generation", "Electric-Integrated", "Independ Power Producer","Energy-Alternate Sources", "Utilities","Power.Generation")
  OilGasCoal <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production","Coal", "General Mining",
                  "Oil Comp-Explor&Prodtn", "Oil Comp-Integrated","Oil&Gas Drilling" ,"Exploration...Production","Integrated.Oils","Coal","Metal-Diversified",  "Coal.Operations", "Metals...Mining", "Diversified Minerals")
  Futuresecs <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation",
                  "Bldg Prod-Cement/Aggreg","Steel-Producers", "Metal-Iron","Metal-Aluminum","Transport-Air Freight", "Transport-Marine")
  Auto <- c("Automobiles","Commercial Vehicles & Trucks",
            "Auto-Cars/Light Trucks", "Automobiles.Manufacturing")
  
  
  over$Sector <-ifelse (over$Subgroup %in% Powr,"Power","Other Sectors")
  over$Sector <-ifelse (over$Subgroup %in% OilGasCoal,"Fossil Fuels",over$Sector)
  over$Sector <-ifelse (over$Subgroup %in% Auto,"Automotive",over$Sector)
  # over$Sector <-ifelse (over$Subgroup %in% Futuresecs,"Climate Relevant No 2Ãƒ‚° Scenario",over$Sector)
  
  over$Sector.All <- ifelse(over$Valid==0, "Excluded", "Climate Relevant w/ 2Ãƒ‚° Scenario")
  over$Sector.All <- ifelse(over$Sector== "Climate Relevant No 2Ãƒ‚° Scenario" & over$Valid==1 , "Climate Relevant No 2Ãƒ‚° Scenario",over$Sector.All)
  over$Sector.All <- ifelse(over$Sector =="Other Sectors" & over$Valid==1, "Other Sectors",over$Sector.All)
  
  over$Sector <- factor(over$Sector, levels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), ordered=TRUE) #,"Climate Relevant No 2Ãƒ‚° Scenario",
  over$Sector.All <- factor(over$Sector.All, levels=c("Excluded","Other Sectors","Climate Relevant No 2Ãƒ‚° Scenario","Climate Relevant w/ 2Ãƒ‚° Scenario"), ordered=TRUE)
  
  portfolio_label = paste0(round(sum(filter(over,Valid==1)$ValueUSD)/sum(over$ValueUSD)*100,1),"%")
  
  
  over<-over%>%
    select(Sector,Asset.Type,Valid,ValueUSD,Portfolio.Name)%>%
    group_by(Sector,Asset.Type,Valid,Portfolio.Name) %>%
    summarise(ValueUSD=sum(ValueUSD))%>%
    ungroup() %>%
    group_by(Valid,Asset.Type) %>%
    mutate(per=ValueUSD/sum(ValueUSD))
  over<- over %>%
    complete(Asset.Type=c("Fixed Income","Equity"),
             Sector = c("Other Sectors","Fossil Fuels", "Automotive","Power"), #"Climate Relevant No 2Ãƒ‚° Scenario",
             fill=list(ValueUSD = 0, Valid=1,Portfolio.Name=PortName)) %>%
    unique()
  over<-as.data.frame(over)
  orderofchart <- c("Debt","Equity","Other")
  over$Asset.Type <- factor(over$Asset.Type,levels=orderofchart)
  over$Sector <- factor(over$Sector, levels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), ordered=TRUE) #"Climate Relevant No 2Ãƒ‚° Scenario",
  
  temp <-max(sum(filter(over,Portfolio.Name==PortName&Valid==1)$per))
  
  if (PortName!="MetaPort"){
    plot <- ggplot(data=subset(over, Portfolio.Name==PortName&Valid==1), aes(x=Asset.Type, y=per, fill=Sector)) +
      geom_bar(position="stack", stat="identity",width =0.6) +
      scale_fill_manual(name="", labels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), values=c("#deebf7",energy, trans, pow),drop = FALSE) +   #"Climate Relevant No 2Ãƒ‚° Scenario", "#90b6e4",
      scale_x_discrete(name="Asset Type") +
      scale_y_continuous(name="", labels = scales::percent, expand=c(0,0),limits = c(0,temp+0.005)) +
      guides(fill=guide_legend(nrow=2))+
      theme_barcharts() +
      theme(legend.position = "bottom",
            legend.text=element_text(size=11),
            axis.text.x=element_text(colour=textcolor,size=11),
            axis.text.y=element_text(colour=textcolor,size=11)) 
  }else {
    plot <- ggplot(data=subset(over, Valid==1), aes(x=Asset.Type, y=per, fill=Sector)) +
      geom_bar(position="stack", stat="identity",width =0.6) +
      scale_fill_manual(name="", labels=c("Other Sectors","Fossil Fuels", "Automotive","Power"), values=c("#deebf7",energy, trans, pow),drop = FALSE) + #"Climate Relevant No 2Ãƒ‚° Scenario", "#90b6e4",
      scale_x_discrete(name="Asset Type") +
      scale_y_continuous(name="", labels=scales::percent, expand=c(0,0),limits = c(0,1)) +
      guides(fill=guide_legend(nrow=2))+
      geom_bar(data=subset(over, Valid==0 & Asset.Type=="Other"), aes(x=Asset.Type, y=per), fill="white", stat="identity") +
      theme_barcharts() +
      theme(legend.position = "bottom",
            legend.text=element_text(size=11),
            axis.text.x=element_text(colour=textcolor,size=11),
            axis.text.y=element_text(colour=textcolor,size=11)) 
  }
  # 
  if(PrintPlot){print(plot)}
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_SectorBarChart.png',sep=""),
         bg="transparent",height=3,width=4,dpi=ppi)   #linewidth_in*.9
}

exposure_summary <- function(plotnumber,ChartType){
  
  if(ChartType == "CB") {
    Portfolio <- CBCombin
  } else if (ChartType == "EQ") {
    Portfolio <- EQCombin
  }
  
  BrownTech = c("ICE","Coal","Gas","Oil","CoalCap","GasCap", "OilCap")
  
  technologyorder <- technology_order
  
  Portfolio <- Portfolio %>%
    filter(Year == Startyear+5, Technology != "OilCap") %>%   #!= "OilCap"
    select(PortName, Sector, Technology, Exp.Carsten.Plan.Port.Scen.Market) %>%
    rename("Exposure" = Exp.Carsten.Plan.Port.Scen.Market)
  
  Portfolio <- Portfolio %>%
    #group_by(PortName) %>%
    complete(Technology=technologyorder, fill=list(Exposure = 0,PortName = PortName))
  Portfolio[Portfolio$Technology %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"),"Sector"] <- "Power"
  Portfolio[Portfolio$Technology %in% c("Coal","Gas","Oil"),"Sector"] <- "Fossil Fuels"
  Portfolio[Portfolio$Technology %in% c("Electric","Hybrid","ICE"),"Sector"] <- "Automotive"
  
  Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"] <- -1*Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"]
  
  Portfolio$Sector <- factor(Portfolio$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
  Portfolio$Technology <- factor(Portfolio$Technology, levels = technologyorder)
  
  Portfolio <- arrange(Portfolio, desc(Technology))
  
  TechLabels <- gsub("Cap","",technologyorder)
  names(TechLabels) <- technologyorder
  
  
  Portfolio$Show<-ifelse(Portfolio$Exposure>1,1,Portfolio$Exposure)
  Portfolio$Show<- ifelse(Portfolio$Show< -1,-1,Portfolio$Show)
  
  plot <- ggplot(Portfolio) +
    geom_bar(aes(x = Technology, y = Show, fill = Show >= 0), stat = "identity")+
    scale_fill_manual(values=c("FALSE" = area_6, "TRUE" = area_2))+
    geom_text(size=textsize*(5/14),aes(x = Technology, y = Show,label = paste0(round(100*Exposure),"%"),vjust = ifelse(Exposure >= 0, -.3, 1)))+
    facet_grid(. ~ Sector, scales = "free", space = "free")+
    geom_hline(yintercept = 0, size = 1, color = textcolor)+
    scale_y_continuous(labels=percent, limits = c(-1,1),expand = c(0.08,0.08))+
    scale_x_discrete(labels=TechLabels,expand=c(0,0))+
    ylab("Alignment of Portfolio with 2Ãƒ‚° Market Benchmark")+
    theme_barcharts()+
    theme(panel.spacing.x = unit(.5,"cm"),
          strip.text = element_text(size=textsize,colour=textcolor),
          strip.background = element_blank(),
          plot.margin = (unit(c(0, 0, 0.1, 0), "lines")))
  
  if(PrintPlot){print(plot)}
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_ExposureSummary.png', sep=""),
         bg="transparent",height=3,width=7.5, units="in",dpi=ppi)
}

exposure_summary_carstens <- function(plotnumber,ChartType){
  
  if(ChartType == "CB") {
    Portfolio <- CBCombin
  } else if (ChartType == "EQ") {
    Portfolio <- EQCombin
  }
  
  BrownTech = c("ICE","Coal","Gas","Oil","CoalCap","GasCap", "OilCap")
  
  technologyorder <- technology_order
  
  Portfolio <- Portfolio %>%
    filter(Year == Startyear+5, Technology != "OilCap") %>%   #!= "OilCap"
    select(PortName, Sector, Technology, CarstenMetric_Port, Scen.CarstenMetric_Port.Market) %>%
    mutate("Exposure" = CarstenMetric_Port - Scen.CarstenMetric_Port.Market)
  
  Portfolio <- Portfolio %>%
    #group_by(PortName) %>%
    complete(Technology=technologyorder, fill=list(Exposure = 0,PortName = PortName))
  Portfolio[Portfolio$Technology %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"),"Sector"] <- "Power"
  Portfolio[Portfolio$Technology %in% c("Coal","Gas","Oil"),"Sector"] <- "Fossil Fuels"
  Portfolio[Portfolio$Technology %in% c("Electric","Hybrid","ICE"),"Sector"] <- "Automotive"
  
  Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"] <- -1*Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"]
  
  Portfolio$Sector <- factor(Portfolio$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
  Portfolio$Technology <- factor(Portfolio$Technology, levels = technologyorder)
  
  Portfolio <- arrange(Portfolio, desc(Technology))
  
  TechLabels <- gsub("Cap","",technologyorder)
  names(TechLabels) <- technologyorder
  
  # 
  # Portfolio$Show<-ifelse(Portfolio$Exposure>1,1,Portfolio$Exposure)
  # Portfolio$Show<- ifelse(Portfolio$Show< -1,-1,Portfolio$Show)
  # 
  plot <- ggplot(Portfolio) +
    geom_bar(aes(x = Technology, y = Exposure, fill = Exposure >= 0), stat = "identity")+
    scale_fill_manual(values=c("FALSE" = area_6, "TRUE" = area_2))+
    geom_text(size=textsize*(5/14),aes(x = Technology, y = Exposure,label = paste0(round(100*Exposure,2),"%"),vjust = ifelse(Exposure >= 0, -.3, 1)))+
    facet_grid(. ~ Sector, scales = "free", space = "free")+
    geom_hline(yintercept = 0, size = 1, color = textcolor)+
    scale_y_continuous(labels=percent, limits = c(-.02,.02),expand = c(0,0))+
    scale_x_discrete(labels=TechLabels,expand=c(0,0))+
    ylab("Alignment of Portfolio with 2Ãƒ‚° Market Benchmark")+
    theme_barcharts()+
    theme(panel.spacing.x = unit(.5,"cm"),
          strip.text = element_text(size=textsize,colour=textcolor),
          strip.background = element_blank(),
          plot.margin = (unit(c(0, 0, 0.1, 0), "lines")))
  
  if(PrintPlot){print(plot)}
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_CarstenExposureSummary.png', sep=""),
         bg="transparent",height=3,width=7.5, units="in",dpi=ppi)
}

analysed_summary <- function(plotnumber){
  
  over <- SecAnalysis
  
  if (PortName != "MetaPort") {
    over <- over %>%
      filter(Portfolio.Name == PortName)
  } else {
    over <- over
    # group_by(Asset.Type, Valid) %>%
    # summarise("ValueUSD" = sum(ValueUSD)) %>%
    # ungroup() %>%
    # mutate("Portfolio.Name" = "MetaPort")
  }
  
  over$Asset.Type <- gsub("Debt", "Fixed Income",over$Asset.Type)
  
  #names(over) <- gsub("TwoD\\.","",names(over))
  #over$Asset.Type <- ifelse(over$Asset.Type=="Debt", "Bonds", over$Asset.Type)
  
  #over$Asset.Type <- plyr::revalue(over$Asset.Type, 
  #c("Bonds"="Bond Portfolio", "Equity" = "Equity Portfolio", "Other"="Other Holdings"), warn_missing = F)
  ###### sector categories #####
  # Powr <- c("Alternative Electricity","Conventional Electricity","Multiutilities",
  #           "Electric-Generation", "Electric-Integrated", "Independ Power Producer","Energy-Alternate Sources", "Utilities","Power.Generation")
  # OilGasCoal <- c("Integrated Oil & Gas","Oil Equipment & Services","Coal", "General Mining", "Exploration & Production","Coal", "General Mining",
  #                 "Oil Comp-Explor&Prodtn", "Oil Comp-Integrated","Oil&Gas Drilling" ,"Exploration...Production","Integrated.Oils","Coal","Metal-Diversified",  "Coal.Operations", "Metals...Mining", "Diversified Minerals")
  # Futuresecs <- c("Building Materials & Fixtures","Iron & Steel","Aluminum","Airlines","Marine Transportation",
  #                    "Bldg Prod-Cement/Aggreg","Steel-Producers", "Metal-Iron","Metal-Aluminum","Transport-Air Freight", "Transport-Marine")
  # Auto <- c("Automobiles","Commercial Vehicles & Trucks",
  #              "Auto-Cars/Light Trucks", "Automobiles.Manufacturing")
  # 
  # 
  # over$Sector <-ifelse (over$Subgroup %in% Powr,"Power","Other Sectors")
  # over$Sector <-ifelse (over$Subgroup %in% OilGasCoal,"Fossil Fuels",over$Sector)
  # over$Sector <-ifelse (over$Subgroup %in% Auto,"Automotive",over$Sector)
  # over$Sector <-ifelse (over$Subgroup %in% Futuresecs,"Climate Relevant No 2Ãƒ‚° Scenario",over$Sector)
  # 
  over$Sector.All <- ifelse(over$Valid==0, "Excluded", "Scope of the Analysis")
  # #over$Sector.All <- ifelse(over$Sector== "Climate Relevant No 2Ãƒ‚° Scenario" & over$Valid==1 , "Climate Relevant No 2Ãƒ‚° Scenario",over$Sector.All)
  over$Sector.All <- ifelse(over$Sector =="Other Sectors" & over$Valid==1, "Other Sectors",over$Sector.All)
  # 
  over$Sector <- factor(over$Sector, levels=c("Other Sectors","Climate Relevant No 2Ãƒ‚° Scenario","Fossil Fuels", "Automotive","Power"), ordered=TRUE)
  over$Sector.All <- factor(over$Sector.All, levels=c("Excluded","Other Sectors","Scope of the Analysis"), ordered=TRUE)
  
  # portfolio_label = paste0("Analysed: ", round(sum(filter(over,Valid==1)$ValueUSD)/sum(over$ValueUSD)*100,1),"%")
  
  over <- over %>%
    group_by(Sector) %>%
    complete(Asset.Type=c("Fixed Income","Equity","Other"), fill=list(ValueUSD = 0, Sector.All ="Excluded"))
  over<- as.data.frame(over)
  orderofchart <- c("Fixed Income","Equity","Other")
  over$Asset.Type <- factor(over$Asset.Type,levels=orderofchart)
  
  ## "steelblue" color below should be changed to whatever our Portfolio color is
  plot <- ggplot(over, aes(x=Asset.Type, y=ValueUSD, fill=Sector.All)) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(name="", labels=c("Excluded", "Other Sectors","Scope of the Analysis"), values=c("grey80", "#deebf7","#265b9b"),drop = FALSE) +
    scale_x_discrete(name="Asset Type") +
    scale_y_continuous(name="Market Value (USD)", labels=comprss, expand=c(0,0)) +
    guides(fill=guide_legend(nrow=2))+
    theme_barcharts() + 
    theme(legend.position = "bottom",
          legend.text=element_text(size=11),
          axis.text.x=element_text(colour=textcolor,size=11),
          axis.text.y=element_text(colour=textcolor,size=11))
  
  # plot <- plot+
  #   annotate("text", x = "Equity", y = max(aggregate(over["ValueUSD"],by=over["Asset.Type"],FUN=sum)$ValueUSD),
  #            label = portfolio_label, color = YourportColour, vjust = 1, hjust = .25, size = textsize*(7/14))
  
  if(PrintPlot){print(plot)}
  
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,'_AnalysedSummary.png', sep=""),
         bg="transparent",height=3,width=4,dpi=ppi)   #linewidth_in*.9
}

carsten_metric_chart <- function(plotnumber, ChartType){
  
  # ### TAJ Commented Out My Testing - Will remove shortly !!
  # EQBatchTest <- read.csv(paste0(PROJ.RESULTS.PATH,"CA-INS", "_Equity-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
  # EQBBatchTest <- subset(EQBatchTest, Type == "Portfolio" & BenchmarkRegion == "GlobalAggregate")
  # ChartType <- "EQ"
  # 
  # 
  # CBBatchTest <- read.csv(paste0(PROJ.RESULTS.PATH,"CA-INS", "_Debt-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
  # CBBatchTest <- subset(CBBatchTest, Type == "Portfolio" & BenchmarkRegion == "GlobalAggregate")
  # ChartType <- "CB"
  # 
  # PortName <- "AETNA LIFE INSURANCE COMPANY"
  # 
  # 
  # #test <- subset(CBBatchTest, PortName=="STATE COMPENSATION INSURANCE FUND")
  # #test %>% filter(Sector=="Power") %>% group_by(PortName, Sector, Year, CarstenMetric_PortSec, Scen.CarstenMetric_PortSec) %>% summarise(sum(CarstenMetric_Port), sum(Scen.CarstenMetric_Port))
  # 
  # RenewablesColour <<- "#feedde"
  # HydroColour <<- "#fdbe85"
  # NuclearColour <<- "#fd8d3c"
  # GasCapColour <<- "#e6550d"
  # CoalCapColour <<- "#a63603"
  # 
  # # purpleish
  # ElectricColour <<- "#efedf5"
  # HybridColour <<- "#bcbddc"
  # ICEColour <<-"#756bb1"
  # 
  # #goldish
  # GasProdColour <<- "#D9DDD4" #"#F5F5F5" #D9DDD4
  # OilProdColour <<- "#BEBCAE"       #"#BEA07B" #BEBCAE
  # CoalProdColour <<-  "#8B7E66" # "#8C510A" #8B7E66
  # textcolor <<- "#3D3D3C"
  # 
  # Startyear <- 2018  
  
  ### END TAJ COMMENTS TO DELETE
  
  ### This needs to havea  variable name different from "portName" else the subset below does not work
  PortName_IN <- PortName
  
  if (ChartType == "CB"){
    port <- CBBatchTest
    #port <- subset(port, Scenario == "450S" & Year==2018)
    
     if (PortName == "MetaPort"){
       port <- subset(port, PortName %in% c("MetaPort","Bond Universe"))
       port$PortName <- plyr::mapvalues(port$PortName, c("MetaPort","Bond Universe"), c("Portfolio","Fixed Income Market"))
       port$PortName <- factor(port$PortName, levels=c("Portfolio", "Fixed Income Market"), ordered=TRUE)
     }else{
       port <- subset(port, PortName %in% c(PortName_IN,"Bond Universe"))
       port$PortName <- plyr::mapvalues(port$PortName, c(PortName_IN, "Bond Universe"), c("Portfolio", "Fixed Income Market"))
       port$PortName <- factor(port$PortName, levels=c("Portfolio","Fixed Income Market"), ordered=TRUE)
     }
  
  }else{
    port <- EQBatchTest
    #port <- subset(port, Scenario == "450S" & Year==2018)
    if (PortName == "MetaPort"){
      port <- subset(port, port$PortName %in% c("MetaPort","Listed Market"))
      port$PortName <- plyr::mapvalues(port$PortName, c("MetaPort","Listed Market"), c("Portfolio","Listed Equity Market"))
      port$PortName <- factor(port$PortName, levels=c("Portfolio", "Listed Equity Market"), ordered=TRUE)
    }else{
      port <- subset(port, PortName %in% c(PortName_IN,"Listed Market"))
      port$PortName <- plyr::mapvalues(port$PortName, c(PortName_IN,"Listed Market"), c("Portfolio","Listed Equity Market"))
      port$PortName <- factor(port$PortName, levels=c("Portfolio", "Listed Equity Market"), ordered=TRUE)
    }
  }
  
  current.port <- subset(port, Year==Startyear & PortName=="Portfolio") %>% 
    mutate(Metric=CarstenMetric_Port) #, PortName2="Portfolio Today") 
  current.market <- subset(port, Year==Startyear & InvestorName=="Market") %>% 
    mutate(Metric=CarstenMetric_Port) #, PortName2="Market Today")
  # future.port <- subset(port, Year==(START.YEAR+5) & PortName=="Portfolio") %>% 
  #   mutate(Metric=Scen.CarstenMetric_Port, PortName2="Portfolio in 2023\nunder 2° Scenario")

  port <- bind_rows(current.port, current.market)

  port$Sector <- factor(port$Sector, levels = c("Coal","Oil&Gas", "Power","Automotive"))
  port <- subset(port, Technology != "OilCap")
  tech.levels <- c("Coal","Oil","Gas",
    "CoalCap", "GasCap","NuclearCap","HydroCap", "RenewablesCap",
    "ICE","Hybrid","Electric")
  tech.labels <- gsub("Cap","Capacity", tech.levels)
  port$Technology <- factor(port$Technology, levels = tech.levels, ordered=TRUE)

  tech.colors <- c(CoalProdColour, OilProdColour, GasProdColour, CoalCapColour, GasCapColour, NuclearColour, HydroColour, RenewablesColour, ICEColour, HybridColour, ElectricColour)
  tots <- port %>% group_by(PortName, Sector, CarstenMetric_PortSec, Scen.CarstenMetric_PortSec) %>% summarise(Metric=sum(Metric))
  
  outputplot <- ggplot(port, aes(x=PortName, y=Metric, group=Technology, fill=Technology)) +   
    geom_bar(stat="identity", position="stack") +
    #geom_text(data=tots, aes(x=PortName, y=Metric, label=percent(Metric))) +
    scale_x_discrete(name="") + 
    scale_y_continuous(name="Weight of issuers exposed to the\ntechnologies in the portfolio", labels=percent, expand=c(0,0),
                       limits=c(0, max(tots$Metric) + .01)) +
    scale_fill_manual(name="", labels=tech.labels, values=tech.colors) + 
    theme_cdi() +
    facet_wrap(~ Sector, nrow=1) +
    theme(axis.text.x = element_text(angle = 0,colour=textcolor)) +
    theme(axis.ticks.y = element_line(colour=textcolor)) + 
    theme(axis.line.x = element_line()) 
    
  
  ggsave(outputplot, filename = paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_CMChart.png',sep=""),
         bg="transparent",height=3,width=10,dpi=ppi*0.8)
}


# ------------- DISTRIBUTIONS --------------- #

Risk_Distribution <- function(plotnumber, ChartType){
  Title <- "Percent of Fixed Income Portfolio Value"
  MetricCol <- c("Risk1", "Risk2", "Risk3")
  
  if(ChartType == "CB") {
    RiskData <- Moodys
  } else {
    print("No Data.")
    return()
  }
  
  RiskData$MoodysRiskLvl[is.na(RiskData$MoodysRiskLvl)] <- "5"
  
  df <- RiskData %>% 
    mutate("PortWeight" = Position / PortTotalAUM) %>%
    spread("MoodysRiskLvl", "PortWeight", fill = 0) %>%
    rename("Risk1" = "1", "Risk2" = "2", "Risk3" = "3", "Risk4" = "4", "Risk5" = "5") %>%
    rename("PortName" = "Portfolio.Name")
  
  metaport <- RiskData %>%
    group_by(MoodysRiskLvl) %>%
    summarise("PortWeight" = sum(Position) / sum(RiskData$Position)) %>%
    spread("MoodysRiskLvl", "PortWeight", fill = 0) %>%
    rename("Risk1" = "1", "Risk2" = "2", "Risk3" = "3", "Risk4" = "4", "Risk5" = "5") %>%
    mutate("PortName" = "MetaPort")
  
  ID.COLS = c("PortName")
  df <- unique(subset(df, select = c(ID.COLS,MetricCol)))
  metaport <- subset(metaport, select = c(ID.COLS,MetricCol))
  df <- rbind(df,metaport)
  name = PortName
  
  BarColors <- c(HighRisk, MedRisk, LowRisk)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Immediate Elevated", "Emerging Elevated", "Emerging Moderate")
  ylim = .5
  
  portfolio_label = paste0("Immediate Elevated: ", round(100*sum(filter(df, PortName == name)$Risk1),1), "%\n",
                           "Emerging Elevated: ", round(100*sum(filter(df, PortName == name)$Risk2),1), "%\n",
                           "Emerging Moderate: ", round(100*sum(filter(df, PortName == name)$Risk3),1), "%")
  
  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol, ylim, 
                             portfolio_label, Title, Labels, BarColors)
  
  if(PrintPlot){print(plot)}
  
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_Risk_Distribution.png"),
         height=3,width=7,dpi=ppi, bg="transparent")
}

Fossil_Distribution <- function(plotnumber, ChartType){
  Title <- paste0("Percent of ", ifelse(ChartType=="CB","Debt","Equity")," Portfolio Value")
  if (ChartType == "EQ"){
    Batch <- EQBatchTest
    ylim = 1
  }else if (ChartType == "CB"){
    Batch <- CBBatchTest
    ylim = .25
  }
  
  #Tag Target portfolio, benchmark
  Batch <- subset(Batch, Year == Startyear & Sector %in% c("Coal","Oil&Gas") & Type != "Market",
                  select=c("PortName","Technology","CarstenMetric_Port"))
  ID.COLS = c("PortName")
  MetricCol <- "CarstenMetric_Port"
  name = PortName
  
  BarColors <- c(energy)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Fossil Fuels")
  df <- unique(subset(Batch, select = c(ID.COLS,MetricCol)))
  portfolio_label = paste0("Fossil Fuel Share: ", round(100*sum(filter(df,PortName==name)$CarstenMetric_Port),1), "%")
  
  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol, ylim,
                             portfolio_label, Title, Labels, BarColors) +
    theme(legend.position = "none")
  
  
  if(PrintPlot){print(plot)}
  ggsave(plot=plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",'Fossil_Distribution.png', sep=""),
         height=3,width=7,dpi=ppi, bg="transparent")
  
}


# ------------- COMPANY EXPOSURE CHART ----------- #


company_og_buildout <- function(plotnumber, companiestoprint, ChartType, SectorToPlot){

  ### !! TAJ REMOVE THESE
  ### LOADED ELSEWHERE

  # BATCH.RES.PATH <- paste0(RESULTS.PATH,"01_BatchResults/CA-INS/2016Q4/")
  # BatchName <- "CA-INS"
  # BenchmarkRegionchoose <- "GlobalAggregate"
  # Scenariochoose <- "450S"
  # 
  # CBBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
  # CBBatchTest <- subset(CBBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
  # CBCombin <- CBBatchTest[CBBatchTest$PortName == PortName,]
  # 
  # EQBatchTest <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-ALD-Results-450S.csv"),stringsAsFactors=FALSE,strip.white = T)
  # EQBatchTest <- subset(EQBatchTest, Type == "Portfolio" & BenchmarkRegion == BenchmarkRegionchoose)
  # EQCombin <- EQBatchTest[EQBatchTest$PortName == PortName,]
  # 
  # EQCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Company-ALD.csv"),stringsAsFactors = FALSE,strip.white = T)
  # EQCompALD <- subset(EQCompALD, Scenario == Scenariochoose & Aggregation=="GlobalAggregate")
  # eqnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Equity-Port-Names.csv"),stringsAsFactors = FALSE,strip.white = T)
  # 
  # CBCompALD <- read.csv(paste0(BATCH.RES.PATH,BatchName,"-Debt-Port-Company-ALD-Short-ALL.csv"),stringsAsFactors = FALSE,strip.white = T)
  # CBCompALD <- subset(CBCompALD, Scenario == Scenariochoose & Aggregation=="GlobalAggregate")
  # cbnames <- read.csv(paste0(BATCH.RES.PATH,BatchName,"_Debt-Port-Names.csv"),stringsAsFactors = FALSE,strip.white = T)
  # 
  # GasProdColour <<- "#D9DDD4" #"#F5F5F5" #D9DDD4
  # OilProdColour <<- "#BEBCAE"       #"#BEA07B" #BEBCAE
  
  
  ### NEW HERE
  
  GLOBAL.OIL.2D <- -.02
  GLOBAL.GAS.2D <- .05
  global.targets <- data.frame(Technology=c("Oil","Gas"), Target=c(GLOBAL.OIL.2D, GLOBAL.GAS.2D))
  
  PortName <- "STATE COMPENSATION INSURANCE FUND" #STATE COMPENSATION INSURANCE FUND BOSTON MUTUAL LIFE INSURANCE COMPANY
  ChartType <- "EQ"
  PortName_IN <- PortName
  
  if (ChartType == "CB"){
    comp <- subset(CBCompALD, Portfolio.Name == PortName_IN & Sector=="Oil&Gas")
    comp <- left_join(comp, cbnames, by=("COMPANY_CORP_TICKER"))
    comp$Company <- comp$COMPANY_CORP_TICKER
    port.targets <- CBCombin %>% filter(Sector=="Oil&Gas") %>% 
      select(Scenario, BenchmarkRegion, Sector, Technology, Scen.WtProduction, Year) %>%
      arrange(Year) %>% 
      group_by(Scenario, BenchmarkRegion, Sector, Technology) %>% 
      summarise(Port.Scen.Diff=last(Scen.WtProduction) - first(Scen.WtProduction),
                Port.Scen.Pct=Port.Scen.Diff/first(Scen.WtProduction)) %>%
      ungroup() %>% select(-Scenario, -BenchmarkRegion, -Sector)
    
    
  } else if(ChartType == "EQ"){
    comp <- subset(EQCompALD, PortName == PortName_IN & Sector=="Oil&Gas")
    comp <- left_join(comp, eqnames, by=("EQY_FUND_TICKER"))
    comp$Company <- comp$EQY_FUND_TICKER
    comp$Plan.WtTechProd <- comp$WtProduction
    comp$Scen.WtTechProd <- comp$Scen.WtProduction
    comp$Port.Sec.ClimateWt <- comp$PortWeightEQYlvl
  
    port.targets <- EQCombin %>% filter(Sector=="Oil&Gas") %>% 
      select(Scenario, BenchmarkRegion, Sector, Technology, Scen.WtProduction, Year) %>%
      arrange(Year) %>% 
      group_by(Scenario, BenchmarkRegion, Sector, Technology) %>% 
      summarise(Port.Scen.Diff=last(Scen.WtProduction) - first(Scen.WtProduction),
                Port.Scen.Pct=Port.Scen.Diff/first(Scen.WtProduction)) %>%
      ungroup() %>% select(-Scenario, -BenchmarkRegion, -Sector)
  }
  
  comp <- comp %>% filter(Sector=="Oil&Gas") %>%
    select(Scenario, Company, Final.Name, Sector, Port.Sec.ClimateWt, Technology, 
                          Plan.WtTechProd, Scen.WtTechProd, Year) %>% 
    arrange(Scenario, Company, Final.Name, Sector, Port.Sec.ClimateWt, Technology, Year) %>%
    group_by(Scenario, Company, Final.Name, Sector, Port.Sec.ClimateWt, Technology) %>% 
    summarise(first(Plan.WtTechProd), last(Plan.WtTechProd),
              Plan.Diff=last(Plan.WtTechProd) - first(Plan.WtTechProd),
              Scen.Diff=last(Scen.WtTechProd) - first(Scen.WtTechProd),
              Plan.Pct=(last(Plan.WtTechProd) - first(Plan.WtTechProd))/first(Plan.WtTechProd))
  
  comp <- left_join(comp, port.targets %>% ungroup() %>% as.data.frame() , by="Technology")
  comp$Global.Scen.Pct <- ifelse(comp$Technology=="Gas", GLOBAL.GAS.2D, GLOBAL.OIL.2D)
  comp <- comp %>% ungroup() %>% arrange(desc(`first(Plan.WtTechProd)`))
  comp$Final.Name <- factor(comp$Final.Name, levels=rev(unique(comp$Final.Name)), ordered=TRUE)
  comp$Technology <- factor(comp$Technology, levels=c("Oil","Gas"), ordered=TRUE)
  comp <- comp %>% group_by(Scenario, Technology) %>% top_n(wt=Port.Sec.ClimateWt, n=10)
  
  ggplot(comp, aes(x=Final.Name, y=Plan.Pct, fill=Technology)) + 
    geom_bar(stat="identity") + 
    geom_hline(data=port.targets, aes(yintercept=Port.Scen.Pct, linetype="Pct. Change in Portfolio Production\nSpecified by 2° Scenario (2018-2023)")) + 
    scale_x_discrete(name = "") + 
    scale_y_continuous(name = "Pct. Change in Planned Portfolio Production (2018-2023)", labels=percent) + 
    scale_linetype_manual(name="", values=c("dashed")) +
    scale_fill_manual(name="", values=c(OilProdColour, GasProdColour), guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~Technology, ncol=1) +
    theme_cdi() + 
    theme(legend.position = "bottom") +
    theme(axis.line=element_line()) +
    theme(axis.ticks.x=element_line()) + 
    coord_flip()
    
}

# ------------- TECH SHARE CHARTS ----------- #


company_techshare <- function(plotnumber, companiestoprint, ChartType, SectorToPlot){
  
  if (ChartType == "EQ"){
    CompProdSS <- EQCompProdSnapshot
    combin <- EQCombin
    market <- EQBatchTest[EQBatchTest$Type == "Market",]
  } else if(ChartType == "CB"){
    CompProdSS <- CBCompProdSnapshot
    combin <- CBCombin
    market <- CBBatchTest[CBBatchTest$Type == "Market",]
  }
  
  if (nrow(combin) > 0) {
    
    CompProdSS <- subset(CompProdSS, Year == (Startyear+5))
    combin <- subset(combin, Year == (Startyear+5))
    market <- subset(market, Year == (Startyear+5))
    
    # Portfolio (Weighted by the AUM)
    Portfoliomix <- subset(combin, select=c("Technology","WtProduction"))
    Portfoliomix$Classification <- "Portfolio"
    Portfoliomix$Name <- "Portfolio"
    Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","WtProduction"))
    Portfoliomix$WtProduction <- Portfoliomix$WtProduction
    colnames(Portfoliomix) <- c("Name","Classification","Technology","TechShare")
    
    # Add 2D Target (Global Market under 2D Scenario)
    Targetmix <- subset(combin, select = c("Technology","Scen.WtProduction"))
    Targetmix$Classification <- "Portfolio"
    Targetmix$Name<-GT["X2Target"][[1]]
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Scen.WtProduction"))
    Targetmix$Scen.WtProduction <- Targetmix$Scen.WtProduction
    colnames(Targetmix) <- c("Name","Classification","Technology","TechShare")
    
    # Add Benchmark / Global Market
    Marketmix <- subset(market, select=c("Technology","WtProduction"))
    Marketmix$Classification <- "Portfolio"
    Marketmix$Name <- "Market Benchmark"
    Marketmix <- subset(Marketmix, select=c("Name","Classification","Technology","WtProduction"))
    Marketmix$WtProduction <- Marketmix$WtProduction
    colnames(Marketmix) <- c("Name","Classification","Technology","TechShare")
    
    PortfolioData <- rbind(Marketmix, Targetmix, Portfoliomix)
    
    # Percentage share of each technology for each company in the portfolio
    Companies <- subset(CompProdSS, select=c("Name","Technology","CompanyLvlProd","PortWeightEQYlvl"))
    Companies$TechShare <- Companies$CompanyLvlProd
    Companies$Classification <- "Companies"
    #Companies$Name <- paste0(substr(Companies$Name, 1, 15),"...")
    Companies <- subset(Companies, select = c("Name","Classification","Technology","TechShare","PortWeightEQYlvl"))
    colnames(Companies) <- c("Name","Classification","Technology","TechShare","PortWeight")
    Companies[is.na(Companies$Name),"Name"] <- "No Name"
    
    if (SectorToPlot == "Power"){  
      techorder <- technology_order[1:5]
      
      tech_labels <- c(GT["T_RenewablesCap"][[1]],GT["T_HydroCap"][[1]],
                       GT["T_NuclearCap"][[1]],GT["T_GasCap"][[1]],
                       GT["T_CoalCap"][[1]])
      colors <- as.vector(ColourPalette$Colours[1:5])
    }
    
    if (SectorToPlot == "Automotive"){
      techorder <- technology_order[6:8]
      tech_labels <- c(GT["T_Electric"][[1]],GT["T_Hybrid"][[1]],GT["T_ICE"][[1]])
      colors <- as.vector(ColourPalette$Colours[6:8])
    }
    
    if (SectorToPlot == "Fossil Fuels") {
      techorder <- technology_order[9:11]
      tech_labels <- c(GT["T_GasProd"][[1]],GT["T_OilProd"][[1]],GT["T_CoalProd"][[1]])
      colors <- as.vector(ColourPalette$Colours[9:11])
    }
    
    PortfolioData <- filter(PortfolioData, Technology %in% techorder)
    Companies <- Companies %>%
      filter(Technology %in% techorder) %>%
      arrange(-PortWeight)
    
    Companies <- Companies %>%
      filter(Name %in% unique(Companies$Name)[1:min(companiestoprint,length(unique(Companies$Name)))])
    
    dummy <- data.frame(c("Name", ""),
                        c("Classification", NA),
                        c("Technology", NA),
                        c("TechShare", 0))
    colnames(dummy) <- as.character(unlist(dummy[1,]))
    dummy = dummy[-1, ]
    dummy$TechShare <- as.numeric(dummy$TechShare)
    
    AllData <- rbind(PortfolioData,
                     dummy,
                     select(Companies,-PortWeight))
    AllData$Name <- factor(AllData$Name, levels=rev(unique(c(PortfolioData$Name,"",Companies$Name))))
    AllData$Technology <- factor(AllData$Technology, levels=rev(techorder))
    
    names(colors) <- techorder
    names(tech_labels) <- techorder
    Companies <- unique(select(Companies, Name, PortWeight))
    PortfolioData <- unique(select(PortfolioData, Name))
    
    bar_labels = c(PortfolioData$Name,"",paste0(substr(Companies$Name, 1, 15),"..."))
    
    scaleFUN <- function(x) {
      x <- sprintf("%.1f", x)
      x[x<10] <- paste0("  ",x[x<10])
      return(x)
    }
    
    
    PortPlot <- stacked_bar_chart(AllData, colors, rev(bar_labels), tech_labels)+
      geom_text(data = Companies,
                aes(x = Name, y = 1),
                label = paste0(scaleFUN(100*Companies$PortWeight),"%"),
                hjust = -1, color = textcolor, size=12*(5/14))+
      geom_text(data = Companies,
                aes(x = "", y = 1),
                label = "Weight",
                hjust=-0.9,color = textcolor, size=12*(5/14))+
      xlab("")+
      coord_flip()+
      theme(legend.position = "bottom",legend.title = element_blank(),
            plot.margin = unit(c(1, 6, 0, 0), "lines"))
    
    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    if (PrintPlot){
      dev.off()
      grid.draw(gt)
    }
    if(SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    
    
    ggsave(gt,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_CompanyTechShare.png', sep=""),
           bg="transparent",height=4*(1.4),width=10,dpi=ppi)
  } else {
    print(paste0("No ", SectorToPlot, " data to plot."))
  }
}

sector_techshare <- function(plotnumber,ChartType,SectorToPlot){
  
  if (ChartType == "EQ"){
    Combin <- EQCombin
    Batch <- EQBatchTest
  }else if (ChartType == "CB"){
    Combin <- CBCombin
    Batch <- CBBatchTest
  }
  
  #Remove all portfolios other than Market, Average
  Batch1 <- subset(Batch, Type != "Portfolio")
  # Batch2 <- Batch %>%
  #   filter(Year == Startyear & Technology != "OilCap" & Type =="Portfolio") %>%
  #   select("PortName","Sector","Technology","Scen.WtProduction.Market","Type") %>%
  #   rename(WtProduction=Scen.WtProduction.Market )
  # 
  # Batch2$Type <-"2Ãƒ‚° Market"
  #Add our target portfolio back
  # Portfolios <- rbind(Combin,Batch1)
  
  #Filter and select
  Production <- subset(Portfolios, Year == Startyear +5 &
                         Technology != "OilCap",
                       select=c("PortName","Sector","Technology","WtProduction","Type"))
  Production <- rbind(Production,Batch2)
  Production$Sector <- as.factor(Production$Sector)
  levels(Production$Sector)[levels(Production$Sector)=="Coal"] <-"Fossil Fuels"
  levels(Production$Sector)[levels(Production$Sector)=="Oil&Gas"] <-"Fossil Fuels"
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","Type")
  Production <- Production %>% gather(key=Metric, value=Value, "WtProduction")
  Production <- aggregate(Production["Value"],by=Production[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  Production <- subset(Production, Production$Type != "Market")
  
  
  if(nrow(Production)>0){
    
    technologyorder <- technology_order
    colours <- as.vector(ColourPalette[["Colours"]])
    names(colours) <- technologyorder
    labels <- c("Renewables","Hydro","Nuclear","Gas","Coal","Electric","Hybrid","ICE","Gas","Oil","Coal")
    names(labels) <- technologyorder
    
    
    Production$Technology <- factor(Production$Technology, levels = technologyorder)
    Production$Sector <- factor(Production$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
    
    Production$Type <- wrap.labels(Production$Type,20)
    Production$Type <- factor(Production$Type, levels=c("Portfolio","MetaPortfolio","2° Market"))
    xlabels = c("Portfolio", "All\nInsurers", "2°\nTarget")
    
    titles = c("Fossil Fuel Production", "Power Capacity", "Automotive Production")
    names(titles) <- c("Fossil Fuels", "Power", "Automotive")
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    #Production$Type <- factor(Production$Type)
    
    Production <- subset(Production, select = c("Type", "Sector", "Technology", "Value"))
    
    plottheme<- stacked_bar_chart(Production, colours, xlabels, labels)+
      ylab("Share of Sector Production")+
      theme(plot.title = element_text(hjust =0.5,colour=textcolor,size=11,margin = unit(c(0,0,1,0),"lines")),
            legend.position = "bottom",
            legend.title = element_blank())
    
    
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
          ggtitle("Automotive Production")
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
                        p3+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")),
                        p1+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")), nrow=1)
      dev.off()
      if(PrintPlot){print(cmd)}
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
      
    }
  }
}

sector_techshare_3yr <- function(plotnumber,ChartType,SectorToPlot){
  
  if (ChartType == "EQ"){
    Combin <- EQCombin
    #Batch <- EQBatchTest
  }else if (ChartType == "CB"){
    Combin <- CBCombin
    #Batch <- CBBatchTest
  }
  
  #Filter and select
  Combin <- subset(Combin, (Year ==2018|Year ==2019|Year ==2020) &
                     Technology != "OilCap",
                   select=c("PortName","Sector","Technology","WtProduction","Year"))
  Combin$Sector <- as.factor(Combin$Sector)
  levels(Combin$Sector)[levels(Combin$Sector)=="Coal"] <-"Fossil Fuels"
  levels(Combin$Sector)[levels(Combin$Sector)=="Oil&Gas"] <-"Fossil Fuels"
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","Year")
  Combin <- Combin %>% gather(key=Metric, value=Value, "WtProduction")
  Combin <- aggregate(Combin["Value"],by=Combin[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  
  
  if(nrow(Combin)>0){
    
    technologyorder <- technology_order
    colours <- as.vector(ColourPalette[["Colours"]])
    names(colours) <- technologyorder
    labels <- c("Renewables","Hydro","Nuclear","Gas","Coal","Electric","Hybrid","ICE","Gas","Oil","Coal")
    names(labels) <- technologyorder
    
    
    Combin$Technology <- factor(Combin$Technology, levels = technologyorder)
    Combin$Sector <- factor(Combin$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
    
    xlabels = c("2018","2019","2020")
    titles = c("Fossil Fuel Production", "Power Capacity", "Automotive Production")
    names(titles) <- c("Fossil Fuels", "Power", "Automotive")
    
    Combin$Year <- as.factor(Combin$Year)
    Combin <- subset(Combin, select = c("Year", "Sector", "Technology", "Value"))
    
    plottheme<- stacked_bar_chart(Combin, colours, xlabels, labels)+
      ylab("Share of Sector Production")+
      theme(plot.title = element_text(hjust =0.5,colour=textcolor,size=11,margin = unit(c(0,0,1,0),"lines")),
            legend.position = "bottom",
            legend.title = element_blank())
    
    
    if (SectorToPlot %in% c("Automotive","Power","Fossil Fuels")){
      dat <- subset(Combin, Sector == SectorToPlot )
      p1 <- template %+% dat +
        ggtitle(titles[names(titles) == SectorToPlot])
      
      # print(p1)
      
      if (SectorToPlot == "Fossil Fuels"){
        SectorToPlot <- "FossilFuels"
      }
      ggsave(p1,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3,width=3,dpi=ppi)
      
    } else if (SectorToPlot == "All"){
      dat <- subset(Combin,Sector=="Automotive" )
      if (nrow(subset(dat)) > 0) {  
        p1 <- plottheme %+% dat +
          ggtitle("Automotive Production")
      } 
      
      
      dat <- subset(Combin,Sector=="Fossil Fuels")
      if (nrow(subset(dat)) > 0) {  
        p2 <- plottheme %+% dat +
          ggtitle("Fossil Fuel Production")
        
      } 
      
      dat <- subset(Combin,Sector=="Power")
      if (nrow(subset(dat)) > 0) {  
        p3 <- plottheme %+% dat +
          ggtitle("Power Capacity")
      } 
      
      cmd<-grid.arrange(p2,
                        p3+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")),
                        p1+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")), nrow=1)
      dev.off()
      if(PrintPlot){print(cmd)}
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
      
    }
  }
}

Graph246 <- function(plotnumber,ChartType,TechToPlot){
  
  ### EQUITY PRODUCTION
  if (ChartType =="EQ") {
    ALD <- EQALDAggProd
    ALD$Asset.Type <- "Equity"
    Combin <- EQCombin
  } else{
    ### BOND PRODUCTION
    ALD <- CBALDAggProd
    ALD$Asset.Type <- "Bonds"
    Combin <- CBCombin
  }
  
  ### PORT PRODUCTION
  #ALD <- bind_rows(Aldprod1, Aldprod2)
  table(ALD$Asset.Type, useNA="always")
  ALD <- subset(ALD, Aggregation=="GlobalAggregate" & BenchmarkRegion=="GlobalAggregate" & Scenario %in% c("450S","NPS","CPS"))
  tech.order <- c("Coal","Oil","Gas", "CoalCap", "OilCap","GasCap","NuclearCap", "HydroCap","RenewablesCap","ICE","Hybrid","Electric")
  ALD$Technology <- factor(ALD$Technology, levels=tech.order, ordered=TRUE)
  #ALD$Tech.Type <- ifelse(ALD$Technology %in% c("Hybrid","Electric","NuclearCap", "RenewablesCap","HydroCap"),
  #                        "Low Carbon", "High Carbon")
  ALD$Scenario <- factor(ALD$Scenario, levels=c("450S","NPS","CPS"), ordered=TRUE)
  
  ### Separate into CurrentPlans and Scenario, get into same column headers
  ALD <- subset(ALD,select = c(InvestorName, PortName,Sector,Year, Technology, Scenario, WtProduction, Scen.WtProduction))
  ALD.cp <- ALD %>%
    select(InvestorName, PortName, Sector, Year, Technology,  WtProduction) %>%   #Tech.Type,
    distinct() %>%
    rename(Production=WtProduction) %>% mutate(Line.Type="CurrentPlan")
  
  ALD.sc <- ALD %>%
    select(InvestorName, PortName, Scenario, Sector, Year, Technology, Scen.WtProduction) %>%  #Tech.Type,
    rename(Production=Scen.WtProduction) %>%
    mutate(Line.Type="Scenario")
  
  ### Calculate GROWTH
  
  ALD2 <- bind_rows(ALD.cp, ALD.sc)
  ALD2 <- ALD2 %>% ungroup() %>%
    arrange(InvestorName, PortName, Scenario, Sector, Technology, Line.Type, Year) %>%  #Tech.Type,
    group_by(InvestorName, PortName, Scenario, Sector, Technology, Line.Type) %>%  #Tech.Type,
    mutate(Growth=Production/first(Production))
  
  # b<-a %>%
  #    arrange(Scenario,Sector,Technology,Year) %>%
  #    group_by(InvestorName,Scenario,Sector,Technology)%>%
  #    mutate(ha =Scen.WtProduction/first(Scen.WtProduction))
  
  
  ylims <- ALD2 %>%
    filter(PortName=="MetaPort" | PortName=="Listed Market" | PortName=="Bond Universe") %>%
    group_by(Technology, Scenario) %>%  # Tech.Type,
    summarise(min=min(Growth), max=max(Growth))
  a<-data.frame(Technology=c("Electric", "Electric","ICE","ICE"), Scenario=c("CPS","NPS","CPS","NPS"),min=c(1,1,1,0.929878),max=c(1,9.7801,1,1))   #Tech.Type=c("Low Carbon", "Low Carbon","High Carbon","High Carbon"),
  ylims<-as.data.frame(rbind(as.data.frame(ylims),a))
  ### SEPARATE AGAIN
  
  ALD.cp <- ALD2 %>% filter(Line.Type=="CurrentPlan")
  ALD.sc <- ALD2 %>% filter(Line.Type=="Scenario")
  
  ### GET SCENARIOS INTO RIBBON FORMAT
  
  ALD.sc.wide <- ALD.sc %>%
    ungroup() %>%
    select(InvestorName, PortName, Scenario, Sector, Technology, Line.Type, Year, Growth) %>%   # Tech.Type,
    spread(key=Scenario, value=Growth)
  ALD.sc.wide[which(ALD.sc.wide$Technology=="ICE" & ALD.sc.wide$PortName=="MetaPort"),]$CPS<-1
  ALD.sc.wide[which(ALD.sc.wide$Technology=="ICE" & ALD.sc.wide$PortName=="MetaPort"),]$NPS<- c(1, 0.989,0.973,0.96,0.953,0.946)
  ALD.sc.wide[which(ALD.sc.wide$Technology=="Electric" & ALD.sc.wide$PortName=="MetaPort"),]$CPS<-1
  ALD.sc.wide[which(ALD.sc.wide$Technology=="Electric" & ALD.sc.wide$PortName=="MetaPort"),]$NPS<-ifelse(ALD.sc.wide[which(ALD.sc.wide$Technology=="Electric" & ALD.sc.wide$PortName=="MetaPort"),]$`450S`==1,1,ALD.sc.wide[which(ALD.sc.wide$Technology=="Electric" & ALD.sc.wide$PortName=="MetaPort"),]$`450S`*0.5)
  
  
  ALD.sc.wide <- ALD.sc.wide %>%
    arrange(InvestorName, PortName, Sector, Technology, Line.Type, Year) %>%   #Tech.Type,
    group_by( InvestorName, PortName, Sector, Technology, Line.Type) %>%  #Tech.Type,
    mutate(Green=ifelse(last(`450S`) > last(CPS), 1, 0))
  
  ### IDENTIFY LIMITS of the Y axis
  
  MIN.Y <- -2
  if (TechToPlot =="Electric"){MAX.Y=20}else{MAX.Y=2.5}
  
  ALD.sc.wide <- ALD.sc.wide %>% mutate(Line1=ifelse(Green == 1, CPS, `450S`),
                                        Line2=NPS,
                                        Line3=ifelse(Green == 1 , `450S`, CPS),
                                        Line4=MAX.Y)
  
  ALD.sc.tall <- ALD.sc.wide %>%
    select(-`450S`, -NPS, -CPS) %>%
    gather(key="Target", value="Value",-InvestorName, -PortName, -Sector,-Technology,-Green, -Line.Type,-Year)   #-Tech.Type
  
  
  
  ALD.sc.tall <- ALD.sc.tall %>%
    group_by(InvestorName, PortName, Sector, Technology, Line.Type, Green, Year) %>%   #Tech.Type
    mutate(lower=lag(Value),
           lower=ifelse(is.na(lower), MIN.Y, lower))
  
  GoodBad <- GreenBrown(TechToPlot)
  PORTFOLIO <- c("MetaPort")
  ALD.sc.tall<- as.data.frame(ALD.sc.tall)
  ALD.cp <- as.data.frame(ALD.cp)
  
  green.fill <- c("Line4"=area_2,
                  "Line3"=area_2_4,
                  "Line2"=area_4_6,
                  "Line1"=area_6)
  
  brown.fill <- c("Line4"=area_6,
                  "Line3"=area_4_6,
                  "Line2"=area_2_4,
                  "Line1"=area_2)
  
  green.labels <- c("Line4"="2D",
                    "Line3"="2D-4D",
                    "Line2"="4D-6D",
                    "Line1"="6D")
  
  brown.labels <- c("Line4"="6D",
                    "Line3"="4D-6D",
                    "Line2"="2D-4D",
                    "Line1"="2D")
  
  a<- ifelse(TechToPlot=="Electric",-0.1,min(subset(ylims, Technology==TechToPlot, select="min")))
  
  b<-max(subset(ylims, Technology==TechToPlot, select="max"))
  calbreak <- function(ymin, ymax)
  {
    if ((ymax-ymin)<1)
    {unit <- 0.05}
    else if ((ymax-ymin)>1 && (ymax-ymin)<2)
    {unit <- 0.5}
    else
    {unit <-5}
    roundmax <- (as.integer(ymax/unit)+1)*unit
    roundmin <- (as.integer(ymin/unit))*unit
    breaks <- sort(unique(c(seq(roundmin, roundmax , unit),1)))
    return(breaks)
  }
  
  
  unit <- 100
  outputplot <- ggplot(data = subset(ALD.sc.tall, Technology == TechToPlot & PortName %in% PORTFOLIO)) +
    geom_ribbon(aes(ymin=lower*unit, ymax=Value*unit, x=Year,fill=Target),alpha=0.75) +
    scale_fill_manual(labels=eval(parse(text = paste(GoodBad,".labels",sep = ""))), values=eval(parse(text = paste(GoodBad,".fill",sep = "")))) +
    scale_x_continuous(name="Year", expand=c(0,0),limits=c(2018, 2023.6)) +
    scale_y_continuous(name="Index of Production (2018=100)",
                       expand=c(0,0),
                       breaks=calbreak(a,b)*unit) +
    theme_246() + theme(legend.position = "none") +
    #labs(title=paste0("Growth of ", "names[x]", " Allocated to Portfolio, 2018-2023"),
    #     subtitle = "Trajectory of Portfolio's Current Plans compared to IEA 2ÃƒƒƒÃƒ‚ƒÃƒ‚‚ÃƒƒƒÃƒ‚‚Ãƒ‚°, 4ÃƒƒƒÃƒ‚ƒÃƒ‚‚ÃƒƒƒÃƒ‚‚Ãƒ‚°, 6ÃƒƒƒÃƒ‚ƒÃƒ‚‚ÃƒƒƒÃƒ‚‚Ãƒ‚° Degree Scenarios") +
    coord_cartesian(ylim=c(calbreak(a,b)[1]*unit, calbreak(a,b)[length(calbreak(a,b))]*unit))
  
  if (ChartType =="CB"){
    outputplot <- outputplot +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == unique(Combin$PortName)),
                aes(x=Year, y=Growth*unit), color=cb_line, size=.75) +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == "Bond Universe"),
                aes(x=Year, y=Growth*unit), color=cb_line, size=.75, linetype="dashed")
    
  }else{
    outputplot <- outputplot +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == unique(Combin$PortName)),
                aes(x=Year, y=Growth*unit), color=eq_line, size=.75) +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == "Listed Market"),
                aes(x=Year, y=Growth*unit), color=eq_line, size=.75, linetype="dashed")}
  
  if(PrintPlot){print(outputplot)}
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",TechToPlot,"_246.png",sep=""),height=3.6,width=4.6,dpi=ppi*2)
  
}

# # ------------ Oil and Gas Charts ----------- #
Oilshare <- function(plotnumber, companiestoprint, ChartType){
  # ChartType = "CB"
  # # plotnumber = 99
  # companiestoprint = 20
  # SectorToPlot = "Power"
  if (ChartType == "EQ"){
    CompProdSS <- EQCompProdSnapshot
    OilCompProdSS<- subset(CompProdSS,(Technology %in% "Oil")&Year == (Startyear+5),select=c("Name","Year","PortWeightEQYlvl", "Technology","EQY_FUND_TICKER")) 
    colnames(OilCompProdSS)[which(names(OilCompProdSS) == "EQY_FUND_TICKER")] <- "Ticker"
    OilOG<- subset(OGData,Year ==(Startyear+5)& (Technology %in% "Oil") &(RollUpType %in% "Equity"))
    
    
  } else if(ChartType == "CB"){
    CompProdSS <- CBCompProdSnapshot
    OilCompProdSS<- subset(CompProdSS,(Technology %in% "Oil")&Year == (Startyear+5),select=c("Name","Year","PortWeightEQYlvl", "Technology","COMPANY_CORP_TICKER")) 
    colnames(OilCompProdSS)[which(names(OilCompProdSS) == "COMPANY_CORP_TICKER")] <- "Ticker"
    
    OilOG<- subset(OGData,Year ==(Startyear+5)& (Technology %in% "Oil") &(RollUpType %in% "Debt"))
    
  }
  
  
  # ------------ Oil Chart -------------------- #
  
  
  ####Oil
  #OilCompProdSS$EQY_FUND_TICKER <- gsub(" US","",as.character(OilCompProdSS$EQY_FUND_TICKER))
  
  OilCompanies <- right_join(OilOG,OilCompProdSS, by=c("Technology","Ticker"))
  OilCompanies <- subset(OilCompanies, select=c("Production","PortWeightEQYlvl","Resource.Type","Name"))
  OilCompanies1 <- OilCompanies %>%
    group_by(Resource.Type,Name,PortWeightEQYlvl) %>%
    summarise("Oilsum" = sum(Production))%>%
    ungroup() 
  OilCompanies2 <- OilCompanies1 %>%  
    group_by(Name) %>%
    summarise("Oiltotal" =sum(Oilsum))
  
  OilCompanies <- left_join(OilCompanies1,OilCompanies2,by=c("Name"))
  if (nrow(OilCompanies) > 0) {
    OilCompanies$OilShare <- (OilCompanies$Oilsum/OilCompanies$Oiltotal) 
    OilCompanies$Classification <- "Companies"
    
    OilCompanies$Resource.Type <- as.factor(OilCompanies$Resource.Type)
    levels(OilCompanies$Resource.Type)[levels(OilCompanies$Resource.Type)=="Conventional Gas"] <- "Other & Unknown"
    levels(OilCompanies$Resource.Type)[levels(OilCompanies$Resource.Type)=="Unconventional Gas"] <- "Other & Unknown"
    levels(OilCompanies$Resource.Type)[levels(OilCompanies$Resource.Type)=="-"] <- "Other & Unknown"
    
    
    techorder <- c("Oil Sands","Heavy Oil","Conventional Oil","Unconventional Oil","Other & Unknown")
    tech_labels <- c(paste0(" ", GT["Oil_Sands"][[1]]),paste0(" ",GT["Heavy_Oil"][[1]]), paste0(" ", GT["Conv_Oil"][[1]]),
                     paste0(" ", GT["Unconv_Oil"][[1]]),paste0(" ", GT["Other_Oil"][[1]]))
    
    
    colors <- c("#72755e","#8d9176","#a5a792", "#bcbeae","#d3d5ca")
    
    
    
    #AllData <- filter(AllData, Technology %in% techorder)
    OilCompanies$Resource.Type <- factor(OilCompanies$Resource.Type, levels=techorder)
    
    
    OilCompanies <- OilCompanies %>% 
      filter(Resource.Type %in% techorder) %>%
      arrange(-PortWeightEQYlvl) 
    OilCompanies <- OilCompanies %>%
      filter(Name %in% unique(OilCompanies$Name)[1:min(companiestoprint,length(unique(OilCompanies$Name)))])
    
    
    
    colnames(OilCompanies)[which(names(OilCompanies) == "Resource.Type")] <- "Oil.Type"
    OilCompanies <- subset(OilCompanies,select = c("Oil.Type","Name","OilShare","Classification","PortWeightEQYlvl"))
    
    dummy <- data.frame(c("Oil.Type", NA),
                        c("Name", ""),
                        c("OilShare", 0),
                        c("Classification", NA),
                        c("PortWeightEQYlvl", NA))
    colnames(dummy) <- as.character(unlist(dummy[1,]))
    dummy = dummy[-1, ]
    dummy$OilShare <- as.numeric(dummy$OilShare)
    
    OilCompanies <- rbind(OilCompanies,
                     dummy)
    
    OilCompanies$Oil.Type <- factor(OilCompanies$Oil.Type, levels=techorder)
    OilCompanies$PortWeightEQYlvl <- as.numeric(OilCompanies$PortWeightEQYlvl)
    names(colors) <- techorder
    names(tech_labels) <- techorder
    
    # scaleFUN <- function(x) {
    #   x <- round(x, digits = 1)
    #   x<-as.numeric(x)
    #   x[x<10] <- paste0("  ",x[x<10])
    #   return(x)
    # }
    perc <- function(x, digits = 1, format = "f", ...) {
      x<-paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
      
      return(x)
    }
    
    oil<-na.omit(OilCompanies)

    bar_labels = c(paste0(substr(unique(oil$Name), 1, 15),"..."),"")
    
    PortPlot <- ggplot(data=OilCompanies, aes(x=reorder(Name,PortWeightEQYlvl), y=OilShare,
                                              fill=factor(Oil.Type,levels=c("Oil Sands","Heavy Oil","Conventional Oil","Unconventional Oil","Other & Unknown"))),
                       show.guide = TRUE)+
      geom_bar(stat = "identity", position = "fill", width = .6)+
      #geom_hline(yintercept = c(.25,.50,.75), color="white")+
      scale_fill_manual(values=colors,labels = rev(paste(tech_labels, " ")), breaks = rev(techorder))+
      scale_y_continuous(expand=c(0,0), labels=percent)+
      scale_x_discrete(labels = bar_labels)+
      guides(fill=guide_legend(nrow = 1))+
      theme_barcharts()+
      
      geom_text(data=oil,
                aes(x = Name, y = 1),
                label = perc(oil$PortWeightEQYlvl),
                hjust = -1, color = textcolor, size=textsize*(5/14))+
      geom_text(data=oil,
                aes(x="",y=1),
                label = "Weight",
                hjust = -0.5, color =textcolor, size =textsize*(5/14))+
      xlab("")+
      ylab("TechShare")+
      coord_flip()+
      theme(legend.position = "bottom",legend.title = element_blank(),
            plot.margin = unit(c(1, 6, 0, 0), "lines"), axis.line.x = element_line(colour = textcolor,size=0.5))+
       guides(fill = guide_legend(ncol = 5,keywidth=1))
      # annotation_custom(
      #   grob = textGrob(label = "Weight", hjust =-0.9,gp=gpar(fontsize=textsize,col=textcolor)),
      #   xmin = n_distinct(OilCompanies$Name)+0.5, xmax = n_distinct(OilCompanies$Name)+1, ymin = 1, ymax = 1.05)
      # 
    
    
    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    if(PrintPlot){
      dev.off()
      grid.draw(gt)
    }
    
    if(length(unique(OilCompanies$Name))<3){
      h=length(unique(OilCompanies$Name))
    }else{h=3}
    
    ggsave(gt,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_OilShare.png', sep=""),
           bg="transparent",height=h,width=10,dpi=ppi)}
  else {
    print("No oil data to plot.")
  }
}

carboninout <- function(plotnumber, companiestoprint, ChartType){
  # ChartType = "CB"
  # # plotnumber = 99
  # companiestoprint = 20
  # SectorToPlot = "Power"
  if (ChartType == "EQ"){
    CompProdSS <- EQCompProdSnapshot %>%
      filter(Year == Startyear +5) %>%
      select(Name,PortWeightEQYlvl, EQY_FUND_TICKER) %>%
      unique()
    ccap<- CarbonCap
    
    
  } else if(ChartType == "CB"){
    CompProdSS <- CBCompProdSnapshot %>%
      filter(Year == Startyear +5) %>%
      select(Name,PortWeightEQYlvl, COMPANY_CORP_TICKER) %>%
      rename(EQY_FUND_TICKER = COMPANY_CORP_TICKER) %>%
      unique()
    
    ccap<- CarbonCap
    ccap$EQY_FUND_TICKER <-gsub(' [A-z ]*', '' , as.character(ccap$EQY_FUND_TICKER))
    
  }
  
  
  ccap$InsideCarbonBudget <- ccap$TotalCarbonBudget - ccap$OutsideCarbonBudget
  ccap <- subset(ccap,select= c("EQY_FUND_TICKER","TotalCarbonBudget","OutsideCarbonBudget","InsideCarbonBudget" ))
  
  colnames(ccap) <- c("EQY_FUND_TICKER","TotalCarbonBudget","Outside Carbon Budget","Inside Carbon Budget")
  ccap$Inside.Carbon.Budget <- ccap$`Inside Carbon Budget`/ccap$TotalCarbonBudget
  ccap$Outside.Carbon.Budget <- ccap$`Outside Carbon Budget`/ccap$TotalCarbonBudget
  
  portfolio <- left_join(CompProdSS,ccap, by="EQY_FUND_TICKER") %>%
    select(Name,PortWeightEQYlvl,`Inside Carbon Budget`,`Outside Carbon Budget`)
  portfolio1 <- melt(portfolio, id.vars = c( "Name","PortWeightEQYlvl"), variable.name = "CarbonBudget")
  portfolio1 <- subset(portfolio1, !is.na(value))
  
  if (nrow(portfolio1)>1) {
    carbonorder <- c("Inside Carbon Budget","Outside Carbon Budget")
    
    colors <- c(OilProdColour,area_6)
    
    
    #AllData <- filter(AllData, Technology %in% techorder)
    portfolio1$CarbonBudget  <- factor(portfolio1$CarbonBudget, levels=carbonorder)
    
    portfolio1 <- portfolio1 %>% 
      arrange(-PortWeightEQYlvl) 
    
    portfolio1 <- portfolio1 %>%
      filter(Name %in% unique(portfolio1$Name)[1:min(companiestoprint,length(unique(portfolio1$Name)))])
    
    dummy <- data.frame(c("Name", ""),
                        c("PortWeightEQYlvl", NA),
                        c("CarbonBudget", NA),
                        c("value", 0))
    colnames(dummy) <- as.character(unlist(dummy[1,]))
    dummy = dummy[-1, ]
    dummy$value<- as.numeric(dummy$value)
    
    portfolio1<-rbind(portfolio1,dummy)
    portfolio1$Name <- factor(portfolio1$Name, levels=rev(unique(c("",portfolio1$Name))))
    portfolio1$PortWeightEQYlvl<-as.numeric(portfolio1$PortWeightEQYlvl)
    names(colors) <- carbonorder
    
    perc <- function(x, format = "f", ...) {
      x<-ifelse(x*100 >=10,
                paste0(formatC(100 * x, format = format, digits = 0, ...), "%"),
                paste0(formatC(100 * x, format = format, digits = 1, ...), "%"))
      return(x)
    }
    
    port<-na.omit(portfolio1)
   
    
    bar_labels = c(paste0(substr(unique(port$Name), 1, 15),"..."),"")
    PortPlot <- ggplot(data=portfolio1, aes(x=Name, y=value,
                                            fill=factor(CarbonBudget,levels=c("Outside Carbon Budget","Inside Carbon Budget" ))),
                       show.guide = TRUE)+
      geom_bar(stat = "identity", position = "fill", width = .6)+
      #geom_hline(yintercept = c(.25,.50,.75), color="white")+
      scale_fill_manual(values=colors,labels = paste(carbonorder," "), breaks = (carbonorder))+
      scale_y_continuous(expand=c(0,0),labels=percent)+
      scale_x_discrete(labels = bar_labels)+
      guides(fill=guide_legend(nrow = 1))+
      theme_barcharts()+
      coord_flip()+
      geom_text(data=port,
                aes(x = Name, y = 1),
                label = perc(port$PortWeightEQYlvl),
                hjust = -1, color = textcolor, size=12*(5/14))+
      geom_text(data=port,
                aes(x="",y=1),
                label = "Weight",
                hjust = -0.5, color =textcolor, size =12*(5/14))+
      xlab("")+
      ylab("TechShare")+
      
      theme(legend.position = "bottom",legend.title = element_blank(),
            plot.margin = unit(c(1, 6, 0, 0), "lines"), axis.line.x = element_line(colour = textcolor,size=0.5))
    #annotation_custom(
    #  grob = textGrob(label = "Weight",
    #                  gp=gpar(fontsize=12,col=textcolor),
    #                  hjust = -0.1),
    #  xmin = n_distinct(portfolio1$Name)+0.5, xmax = n_distinct(portfolio1$Name)+1, ymin = 1, ymax = 1.05)
    
    
    gt <- ggplot_gtable(ggplot_build(PortPlot))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    
    if(PrintPlot){
      dev.off()
      grid.draw(gt)
    }
    
    if(length(unique(portfolio1$Name))<=6){
      h=length(unique(portfolio1$Name))
    }else{h=6.5}
    
    ggsave(gt,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_CarboninnoutShare.png', sep=""),
           bg="transparent",height=h,width=11,dpi=ppi)
    return(TRUE)
  } else {
    print("No Carbon Budget data to plot.")
    return(FALSE)
  }
  
}


# ---------------Tech Share Geom_area ---------------------------- #
sector_techshare_area <- function(plotnumber,ChartType,SectorToPlot){
  
  if (ChartType == "EQ"){
    Combin <- EQCombin
  }else if (ChartType == "CB"){
    Combin <- CBCombin
  }
  
  #Filter and select
  Combin <- subset(Combin, Technology != "OilCap",
                   select=c("PortName","Sector","Technology","WtProduction","Year"))
  Combin$Sector <- as.factor(Combin$Sector)
  levels(Combin$Sector)[levels(Combin$Sector)=="Coal"] <-"Fossil Fuels"
  levels(Combin$Sector)[levels(Combin$Sector)=="Oil&Gas"] <-"Fossil Fuels"
  # Aggregate and rename CarstenMetric_Port
  ID.COLS = c("Sector","Technology","Year")
  Combin <- Combin %>% gather(key=Metric, value=Value, "WtProduction")
  Combin <- aggregate(Combin["Value"],by=Combin[c(ID.COLS)],FUN=sum)
  #Created an average for the peers (or even just use fill!)
  
  
  
  if(nrow(Combin)>0){
    
    technologyorder <- technology_order
    colours <- as.vector(ColourPalette[["Colours"]])
    names(colours) <- technologyorder
    labels <- c("Renewables","Hydro","Nuclear","Gas","Coal","Electric","Hybrid","ICE","Gas","Oil","Coal")
    names(labels) <- technologyorder
    
    colour<-data.frame(cbind(colours,technologyorder,labels))
    colnames(colour)[2]<-"Technology"
    colour$Technology<- factor(colour$Technology, levels = technologyorder)
    Combin$Technology <- factor(Combin$Technology, levels = technologyorder)
    Combin$Sector <- factor(Combin$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
    Combin <- left_join(Combin,colour,by="Technology")
    # xlabels = c("2018","2019","2020")
    # titles = c("Fossil Fuel Production", "Power Capacity", "Automotive Production")
    # names(titles) <- c("Fossil Fuels", "Power", "Automotive")
    
    #Combin$Year <- as.factor(Combin$Year)
    tot <-Combin %>%
      select (Year,Sector,Technology,Value) %>%
      group_by(Year,Sector) %>%
      summarise(tot = sum(Value)) 
    
    Combin <- left_join(Combin,tot, by =c("Year","Sector"))
    Combin$per <- Combin$Value/Combin$tot
    
    #dat <- subset(Combin,Sector==SectorToPlot)
    plottheme<- ggplot(data=Combin, aes(x =Year,y=per, fill = Technology))+
      geom_area( position = 'stack')+
      scale_fill_manual(name="",values = colours,labels=labels)+
      theme_barcharts()+
      theme(plot.title = element_text(hjust =0.5,colour=textcolor,size=11,margin = unit(c(0,0,1,0),"lines")),
            legend.position = "bottom",
            legend.title = element_blank())
    
    
    if (SectorToPlot == "All"){
      dat <- subset(Combin,Sector=="Automotive")
      if (nrow(subset(dat)) > 0) {  
        p1 <- plottheme %+% dat +
          scale_y_continuous(labels = scales::percent)+
          ylab("Share of Sector Production (Vehicles)")+
          ggtitle("Automotive Production")
      } else {
        dat <- data.frame("Sector" = "Automotive", "Technology" = "ICE", "per" = 0, "Year"=2018)
        dat$per <- as.numeric(dat$per)
        p1 <- plottheme %+% dat +
          xlim(2018,2023)+
          scale_y_continuous(labels = scales::percent,limits = c(0,1))+
          ylab("Share of Sector Production (Vehicles)")+
          ggtitle("Automotive Production") +
          geom_text(x=2021, y = .5, label = "No Automotive Production")
      }
      
      dat <- subset(Combin,Sector=="Fossil Fuels")
      if (nrow(subset(dat)) > 0) {  
        p2 <- plottheme %+% dat +
          scale_y_continuous(labels = scales::percent)+
          ylab("Share of Sector Production (Joules)")+
          ggtitle("Fossil Fuel Production")
        
      } else {
        dat <- data.frame("Sector" = "Fossil Fuels","Technology" = "OilProd",
                          "per" = 0,"Year"=2018)
        dat$per <- as.numeric(dat$per)
        p2 <- plottheme %+% dat +
          xlim(2018,2023)+
          scale_y_continuous(labels = scales::percent,limits = c(0,1))+
          ylab("Share of Sector Production (Joules)")+
          ggtitle("Fossil Fuel Production") +
          geom_text(x=2021, y = 0.5, label = "No Fossil Fuel Production")
      }
      
      dat <- subset(Combin,Sector=="Power")
      if (nrow(subset(dat)) > 0) {  
        p3 <- plottheme %+% dat +
          scale_y_continuous(labels = scales::percent)+
          ylab("Share of Sector Production (MW)")+
          ggtitle("Power Capacity")
      } else {
        dat <- data.frame("Sector" = "Power","Technology" = "CoalCap","per" = 0,"Year"=2018)
        dat$per <- as.numeric(dat$per)
        p3 <- plottheme %+% dat +
          xlim(2018,2023)+
          scale_y_continuous(labels = scales::percent,limits = c(0,1))+
          ylab("Share of Sector Production (MW)")+
          ggtitle("Power Capacity") +
          geom_text(x=2021, y = .5, label = "No Power Capacity")
      }
      
      cmd<-grid.arrange(p2,
                        p3+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")),
                        p1+theme(axis.text.y = element_text(color="white"), axis.title.y = element_text(color="white")), nrow=1)
      dev.off()
      
      
      if(PrintPlot){print(cmd)}
      ggsave(cmd,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_Stackedbar.png', sep=""),bg="transparent",height=3.2,width=9.7,dpi=ppi)
    }
  }
}


Graph246_new <- function(plotnumber,ChartType,TechToPlot){
  
  filternames <- c("Listed Market", "Bond Universe",PortName,"MetaPort")
  PortNames <-PortName
  ### EQUITY PRODUCTION
  if (ChartType =="EQ") {
    ALD <- EQALDAggProd[EQALDAggProd$PortName %in% filternames & EQALDAggProd$Technology %in% TechToPlot,]
    ALD$Asset.Type <- "Equity"
    Combin <- EQCombin
  } else{
    ### BOND PRODUCTION
    ALD <- CBALDAggProd[CBALDAggProd$PortName %in% filternames & CBALDAggProd$Technology %in% TechToPlot,]
    ALD$Asset.Type <- "Bonds"
    Combin <- CBCombin
  }
  
  ### PORT PRODUCTION
  #ALD <- bind_rows(Aldprod1, Aldprod2)
  table(ALD$Asset.Type, useNA="always")
  ALD <- subset(ALD, Aggregation=="GlobalAggregate" & BenchmarkRegion=="GlobalAggregate" & Scenario %in% c("450S","NPS","CPS"))
  ALD[which(ALD$Technology=="Oil"),]$WtProduction <- ALD[which(ALD$Technology=="Oil"),]$WtProduction
  ALD[which(ALD$Technology=="Oil"),]$Scen.WtProduction<- ALD[which(ALD$Technology=="Oil"),]$Scen.WtProduction
  
  ALD[which(ALD$Technology=="Gas"),]$WtProduction <- ALD[which(ALD$Technology=="Gas"),]$WtProduction
  ALD[which(ALD$Technology=="Gas"),]$Scen.WtProduction<- ALD[which(ALD$Technology=="Gas"),]$Scen.WtProduction
  tech.order <- c("Coal","Oil","Gas", "CoalCap", "OilCap","GasCap","NuclearCap", "HydroCap","RenewablesCap","ICE","Hybrid","Electric")
  ALD$Technology <- factor(ALD$Technology, levels=tech.order, ordered=TRUE)
  #ALD$Tech.Type <- ifelse(ALD$Technology %in% c("Hybrid","Electric","NuclearCap", "RenewablesCap","HydroCap"),
  #                        "Low Carbon", "High Carbon")
  ALD$Scenario <- factor(ALD$Scenario, levels=c("450S","NPS","CPS"), ordered=TRUE)
  
  
  
  ### Separate into CurrentPlans and Scenario, get into same column headers
  ALD <- subset(ALD,select = c(InvestorName, PortName,Sector,Year, Technology, Scenario, WtProduction, Scen.WtProduction))
  ALD.cp <- ALD %>%
    select(InvestorName, PortName, Sector, Year, Technology,  WtProduction) %>%   #Tech.Type,
    distinct() %>%
    rename(Production=WtProduction) %>% mutate(Line.Type="CurrentPlan")
  
  
  ALD.sc <- ALD %>%
    select(InvestorName, PortName, Scenario, Sector, Year, Technology, Scen.WtProduction) %>%  #Tech.Type,
    rename(Production=Scen.WtProduction) %>%
    mutate(Line.Type="Scenario")
  
  ### Calculate GROWTH
  
  ALD2 <- bind_rows(ALD.cp, ALD.sc)
  
  if (TechToPlot %in% c("Electric","ICE")){
    PortNames<-"MetaPort"
  }else {
    PortNames<-PortName
  } 
  ### Add in Car Data
  if (TechToPlot %in% c("Electric","ICE")){
    ALD.temp <- ALD.sc %>% 
      select(-Production) %>%
      filter(PortName ==PortNames)
      if (ChartType =="EQ") {
        ALD.temp1 <- ALD.temp
        ALD.temp1$Scenario[ALD.temp1$Technology  %in% c("ICE","Electric") & ALD.temp1$PortName == PortNames] <- "CPS"
        ALD.temp1$Production[ALD.temp1$Technology == "ICE" & ALD.temp1$PortName == PortNames] <- 60987.94
        ALD.temp1$Production[ALD.temp1$Technology == "Electric" & ALD.temp1$PortName == PortNames] <- 268.9853
        
        
        ALD450 <- ALD.sc[ALD.sc$PortName == PortNames & ALD.sc$Year != 2018,]
        ALD.temp2 <- ALD.temp
        ALD.temp2$Scenario[ALD.temp2$Technology %in% c("ICE","Electric") & ALD.temp2$PortName == PortNames] <- "NPS"
        ALD.temp2$Production[ALD.temp2$Technology == "ICE" & ALD.temp2$PortName == PortNames] <- c(60987.94, 60317.07,59341.27,58548.42,58121.51,57694.59)
        ALD.temp2$Production[ALD.temp2$Technology == "Electric" & ALD.temp2$PortName == PortNames & ALD.temp2$Year == "2018"] <- 268.9853
        ALD.temp2$Production[ALD.temp2$Technology == "Electric" & ALD.temp2$PortName == PortNames & ALD.temp2$Year != "2018"] <- ALD450$Production*.5
        
  
        ALD.temp <- rbind(ALD.temp1,ALD.temp2)
      }else{
        ALD.temp1 <- ALD.temp
        ALD.temp1$Scenario[ALD.temp1$Technology  %in% c("ICE","Electric") & ALD.temp1$PortName == PortNames] <- "CPS"
        ALD.temp1$Production[ALD.temp1$Technology == "ICE" & ALD.temp1$PortName == PortNames] <- 77423.17
        ALD.temp1$Production[ALD.temp1$Technology == "Electric" & ALD.temp1$PortName == PortNames] <- 149.1076
        
        
        ALD450 <- ALD.sc[ALD.sc$PortName == "MetaPort" & ALD.sc$Year != 2018,]
        ALD.temp2 <- ALD.temp
        ALD.temp2$Scenario[ALD.temp2$Technology %in% c("ICE","Electric") & ALD.temp2$PortName == PortNames] <- "NPS"
        ALD.temp2$Production[ALD.temp2$Technology == "ICE" & ALD.temp2$PortName == PortNames] <- c(77423.17, 76571.52,75332.74,74326.24,73784.28,73242.32)
        ALD.temp2$Production[ALD.temp2$Technology == "Electric" & ALD.temp2$PortName == PortNames & ALD.temp2$Year == "2018"] <- 149.1076
        ALD.temp2$Production[ALD.temp2$Technology == "Electric" & ALD.temp2$PortName == PortNames & ALD.temp2$Year != "2018"] <- ALD450$Production*.5
        
        
        ALD.temp <- rbind(ALD.temp1,ALD.temp2)
      }
      ALD2 <- bind_rows(ALD2,ALD.temp)
    }
 
  ### Normalisation of market to Portfolio
  ALD.cp <- ALD2 %>% filter(Line.Type=="CurrentPlan")
  
  var <- ifelse(ALD.cp[which(ALD.cp$PortName==PortNames & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production ==0,0,
                ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production/ ALD.cp[which(ALD.cp$PortName==PortNames & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production)
  
  #ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ifelse(var ==0,0,ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production/var)
  if (var ==0){
    ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<-0
  }else{
    ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production/var}
  
  if (TechToPlot %in% c("Electric","ICE")){
    var1<- ALD.cp[which(ALD.cp$PortName==PortName & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production/ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production
    ALD.cp[which(ALD.cp$PortName==PortName &  ALD.cp$Technology ==TechToPlot),]$Production <- ALD.cp[which(ALD.cp$PortName==PortName &ALD.cp$Technology ==TechToPlot),]$Production/var1
  }
  ALD.sc <- ALD2 %>% filter(Line.Type=="Scenario")
  
  ALD2 <- bind_rows(ALD.cp, ALD.sc)
  
   ylims <- ALD2 %>%
      filter( InvestorName != "Market" | Line.Type != "Scenario") %>%
      summarise(min=min(Production), max=max(Production))
  
  
   
  ### Units
  unitscaleval <- max(ylims$max, ALD.cp$Production)   
  unitscalefactor <- 1
  green.unit <- c("Electric" = "Vehicles",
                  "RenewablesCap"="MW",
                  "NuclearCap" ="MW")
  
  brown.unit <- c("CoalCap" = "MW",
                  "GasCap"="MW",
                  "Oil" ="bbl",
                  "Gas" = "m3",
                  "ICE" ="Vehicles")
  
  if (unitscaleval >1e3){
    unitscalefactor <- 1e3
    green.unit <- c("Electric" = "1000 Vehicles",
                    "RenewablesCap"="GW",
                    "NuclearCap" ="GW")
    
    brown.unit <- c("CoalCap" = "GW",
                    "GasCap"="GW",
                    "Oil" ="Mbbl",
                    "Gas" = "1000 m3",
                    "ICE" ="1000 Vehicles")
    
    
  } 
  if (unitscaleval > 1e6){
    unitscalefactor <- 1e6
    green.unit <- c("Electric" = "million Vehicles",
                    "RenewablesCap"="TW",
                    "NuclearCap" ="TW")
    
    brown.unit <- c("CoalCap" = "TW",
                    "GasCap"="TW",
                    "Oil" ="MMbbl",
                    "Gas" = "million m3",
                    "ICE" ="million Vehicles")
  } 
  if (unitscaleval > 1e9){
    unitscalefactor <- 1e9
    green.unit <- c("Electric" = "billion Vehicles",
                    "RenewablesCap"="PW",
                    "NuclearCap" ="PW")
    
    brown.unit <- c("CoalCap" = "PW",
                    "GasCap"="PW",
                    "Oil" ="Gbbl",
                    "Gas" = "billion m3",
                    "ICE" ="billion Vehicles")
  }
  
  ALD2$Production <- ALD2$Production/unitscalefactor  
 
  ylims <- ALD2 %>%
    filter( InvestorName != "Market" | Line.Type != "Scenario") %>%
    summarise(min=min(Production), max=max(Production))
  
  ### GET SCENARIOS INTO RIBBON FORMAT
  ALD.sc <- ALD2 %>% filter(Line.Type=="Scenario")
  ALD.cp <- ALD2 %>% filter(Line.Type=="CurrentPlan")
  
  ALD.sc.wide <- ALD.sc %>%
    ungroup() %>%
    select(InvestorName, PortName, Scenario, Sector, Technology, Line.Type, Year, Production) %>%   # Tech.Type,
    spread(key=Scenario, value=Production)
  
  ALD.sc.wide <- ALD.sc.wide %>%
    arrange(InvestorName, PortName, Sector, Technology, Line.Type, Year) %>%   #Tech.Type,
    group_by( InvestorName, PortName, Sector, Technology, Line.Type) %>%  #Tech.Type,
    mutate(Green=ifelse(last(`450S`) > last(CPS), 1, 0))
  
  ALD.sc.wide <- ALD.sc.wide %>% mutate(Line1=ifelse(Green == 1, CPS, `450S`),
                                        Line2=NPS,
                                        Line3=ifelse(Green == 1 , `450S`, CPS)) #Line4=MAX.Y)
  

  ### IDENTIFY LIMITS of the Y axis
  
  ymin<- min(ylims$min)
  ymax<- max(ylims$max)
  
  
  
  
  if ((ymax-ymin)<1){
    unit <- .1
  }else if ((ymax-ymin)>1 & (ymax-ymin)<10){
    unit <- 1
  }else if ((ymax-ymin)>=10 & (ymax-ymin)<150){
    unit <- 10
  }else if ((ymax-ymin)>=150 & (ymax-ymin)<1000){
    unit <- 100
  }else if ((ymax-ymin)>=1000 & (ymax-ymin)<10000){
    unit <-1000
  }else if ((ymax-ymin)>=10000 & (ymax-ymin)<100000){
    unit <-10000
  }
  
  MAX.Y <- ceiling(ymax/unit)*unit
  MIN.Y <- floor(ymin/unit)*unit
  
  
  
  ALD.sc.wide$Line4 <- MAX.Y
  
  ALD.sc.tall <- ALD.sc.wide %>%
    select(-`450S`, -NPS, -CPS) %>%
    gather(key="Target", value="Value",-InvestorName, -PortName, -Sector,-Technology,-Green, -Line.Type,-Year)   #-Tech.Type
  
  
  
  ALD.sc.tall <- ALD.sc.tall %>%
    group_by(InvestorName, PortName, Sector, Technology, Line.Type, Green, Year) %>%   #Tech.Type
    mutate(lower=lag(Value),
           lower=ifelse(is.na(lower), MIN.Y, lower))
  
  GoodBad <- GreenBrown(TechToPlot)
  PORTFOLIO <- c("MetaPort")
  ALD.sc.tall<- as.data.frame(ALD.sc.tall)
  ALD.cp <- as.data.frame(ALD.cp)
  
  green.fill <- c("Line4"=area_2,
                  "Line3"=area_2_4,
                  "Line2"=area_4_6,
                  "Line1"=area_6)
  
  brown.fill <- c("Line4"=area_6,
                  "Line3"=area_4_6,
                  "Line2"=area_2_4,
                  "Line1"=area_2)
  
  green.labels <- c("Line4"="2D",
                    "Line3"="2D-4D",
                    "Line2"="4D-6D",
                    "Line1"="6D")
  
  brown.labels <- c("Line4"="6D",
                    "Line3"="4D-6D",
                    "Line2"="2D-4D",
                    "Line1"="2D")
  
  
  # 
  # MIN.Y <- ceiling(ymax/10)*10
  # MAX.Y <- floor(ymin/10)*10
  
  outputplot <- ggplot(data = subset(ALD.sc.tall, Technology == TechToPlot & ALD.sc.tall$PortName == PortNames )) +
    geom_ribbon(aes(ymin=lower, ymax=Value, x=Year,fill=Target),alpha=0.75) +
    scale_fill_manual(labels=eval(parse(text = paste(GoodBad,".labels",sep = ""))), values=eval(parse(text = paste(GoodBad,".fill",sep = "")))) +
    scale_x_continuous(name="Year", expand=c(0,0),limits=c(2018, 2023.6)) +
    scale_y_continuous(name=paste0("Weighted Production ","(",eval(parse(text = paste(GoodBad,".unit",sep = "")))[TechToPlot],")"),
                       expand=c(0,0),
                       breaks=seq(MIN.Y,MAX.Y,length.out = 5)) +
    theme_246() + theme(legend.position = "none") +
    #labs(title=paste0("Growth of ", "names[x]", " Allocated to Portfolio, 2018-2023"),
    #     subtitle = "Trajectory of Portfolio's Current Plans compared to IEA 2ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚‚Ãƒ‚Ã‚°, 4ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚‚Ãƒ‚Ã‚°, 6ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚ƒÃƒ‚Ã‚ƒÃƒƒÃ‚‚Ãƒ‚Ã‚‚ÃƒƒÃ‚‚Ãƒ‚Ã‚° Degree Scenarios") +
    coord_cartesian(ylim=c(MIN.Y, MAX.Y))
  
  if (ChartType =="CB"){
    outputplot <- outputplot +
       geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == unique(Combin$PortName)),
                 aes(x=Year, y=Production), color=cb_line, size=.75) +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == "Bond Universe"),
                aes(x=Year, y=Production), color=cb_line, size=.75, linetype="dashed")
    
  }else{
    outputplot <- outputplot +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == unique(Combin$PortName)),
                aes(x=Year, y=Production), color=eq_line, size=.75) +
      geom_line(data=subset(ALD.cp, Technology == TechToPlot & PortName == "Listed Market"),
                aes(x=Year, y=Production), color=eq_line, size=.75, linetype="dashed")}
  
  if(PrintPlot){print(outputplot)}
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",TechToPlot,"_246.png",sep=""),height=3.6,width=4.6,dpi=ppi*2)
  
  #return(outputplot)
}
  
 

# --------------- newly added graphs--------------------------------------#
bar_246 <- function(plotnumber,ChartType) {
  filternames <- c("Listed Market", "Bond Universe",PortName)
  if(ChartType == "CB") {
    Portfolio <- CBCombin
    ALD <- CBALDAggProd[CBALDAggProd$PortName %in% filternames,]
    ALD$Asset.Type <- "Bonds"
  } else if (ChartType == "EQ") {
    Portfolio <- EQCombin
    ALD <- EQALDAggProd[EQALDAggProd$PortName %in% filternames,]
    ALD$Asset.Type <- "Equity"
  }
  
  ALD <- subset(ALD, Aggregation=="GlobalAggregate" & BenchmarkRegion=="GlobalAggregate" & Scenario %in% c("450S","NPS","CPS"))
  ALD[which(ALD$Technology=="Oil"),]$WtProduction <- ALD[which(ALD$Technology=="Oil"),]$WtProduction/6.12
  ALD[which(ALD$Technology=="Oil"),]$Scen.WtProduction<- ALD[which(ALD$Technology=="Oil"),]$Scen.WtProduction/6.12
  
  ALD[which(ALD$Technology=="Gas"),]$WtProduction <- ALD[which(ALD$Technology=="Gas"),]$WtProduction/0.0372
  ALD[which(ALD$Technology=="Gas"),]$Scen.WtProduction<- ALD[which(ALD$Technology=="Gas"),]$Scen.WtProduction/0.0372
  
  tech.order <- c("Coal","Oil","Gas", "CoalCap", "OilCap","GasCap","NuclearCap", "HydroCap","RenewablesCap","ICE","Hybrid","Electric")
  ALD$Technology <- factor(ALD$Technology, levels=tech.order, ordered=TRUE)
  #ALD$Tech.Type <- ifelse(ALD$Technology %in% c("Hybrid","Electric","NuclearCap", "RenewablesCap","HydroCap"),
  #                        "Low Carbon", "High Carbon")
  ALD$Scenario <- factor(ALD$Scenario, levels=c("450S","NPS","CPS"), ordered=TRUE)
  
  ### Separate into CurrentPlans and Scenario, get into same column headers
  ALD <- subset(ALD,select = c(InvestorName, PortName,Sector,Year, Technology, Scenario, WtProduction, Scen.WtProduction))
  ALD.cp <- ALD %>%
    select(InvestorName, PortName, Sector, Year, Technology,  WtProduction) %>%   #Tech.Type,
    distinct() %>%
    rename(Production=WtProduction) %>% mutate(Line.Type="CurrentPlan")
  
  ### Calculate GROWTH
  
  var <- ifelse(ALD.cp[which(ALD.cp$PortName==PortName & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production ==0,0,
                ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production/ ALD.cp[which(ALD.cp$PortName==PortName & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production)
  
  ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ifelse(var ==0,0,ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production/var)
  
  ALD.cp_p <- subset(ALD.cp,(Year ==2018 | Year == 2023 ) &InvestorName!="Market")
  
  ALD.cp_p <-ALD.cp_p %>%
    ungroup() %>%
    select(InvestorName, Sector, Technology, Line.Type, Year, Production) %>%   # Tech.Type,
    spread(key=Year, value=Production)
  ALD.cp_p$delta <- ALD.cp_p$`2023` - ALD.cp_p$`2018`
  
  ALD.cp_m <- subset(ALD.cp,(Year ==2018 | Year == 2023 ) & InvestorName =="Market")
  
  ALD.cp_m <-ALD.cp_m %>%
    ungroup() %>%
    select(InvestorName, Sector, Technology, Line.Type, Year, Production) %>%   # Tech.Type,
    spread(key=Year, value=Production)
  ALD.cp_m$delta <- ALD.cp_m$`2023` - ALD.cp_m$`2018`
  
  ALD<-left_join(ALD.cp_p,ALD.cp_m, by=c("Sector","Technology")) %>%
    select(Technology,Sector,delta.x,delta.y) %>%
    rename(denominator =delta.y,
           numerator = delta.x) %>%
    mutate(y=numerator/denominator)
  ALD$y<-ifelse(is.infinite((ALD$y)*-1),0,ALD$y)
  ALD$y2<- 1-ALD$y
  ALD$y2<-ifelse(ALD$y2<0,(ALD$y2)*-1,(ALD$y2))
  ALD$y3 <- ifelse(ALD$y>1,1,ALD$y)
  ALD$Name <- "Name"
  
  ALD <-gather(ALD,
               key = "input",
               value = "Value",
               -Technology)
  ALD1G<- subset(ALD, input=="y" & Technology %in% c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap"))
  ALD1B<- subset(ALD, input=="y" & Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE"))
  ALD2G<- subset(ALD,(input=="y2"|input=="y3") &Technology %in% c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap"))
  ALD2B<- subset(ALD, (input=="y2"|input=="y3") &Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE"))
  ALD1G$Value <-as.numeric(ALD1G$Value)
  ALD2G$Value <-as.numeric(ALD2G$Value)
  ALD1B$Value <-as.numeric(ALD1B$Value)
  ALD2B$Value <-as.numeric(ALD2B$Value)
  #ALD[which(ALD$Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE")),]$Value <- ALD[which(ALD$Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE")),]$Value *(-1)
  
  ALD2B$input<-as.factor(ALD2B$input)
  levels(ALD2B$input)[levels(ALD2B$input)=="y3"] <- "y4"
  levels(ALD2B$input)[levels(ALD2B$input)=="y2"] <- "y5"
  plot <- ggplot() +
    geom_bar(data=subset(ALD2G,Technology %in% c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap")),
             aes(x = Technology, y = Value, fill = as.factor(input)), stat = "identity",color="green")+
    #scale_fill_manual(values=c("y2" = "white", "y3" = "green"))+
    geom_bar(data=subset(ALD1G,Technology %in% c("Electric","Hybrid","RenewablesCap","HydroCap","NuclearCap")),
             aes(x = Technology, y = Value), stat = "identity",alpha=0.2,fill="green",color="green")+
    geom_bar(data=subset(ALD2B,Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE")),
             aes(x = Technology, y = Value*-1, fill = as.factor(input)), stat = "identity",color="red")+
    scale_fill_manual(values=c("y5" = "white", "y4" = "red","y2" = "white", "y3" = "green"))+
    geom_bar(data=subset(ALD1B,Technology %in% c("Coal","Oil","Gas","CoalCap","GasCap","ICE")),
             aes(x = Technology, y = Value*-1), stat = "identity",alpha=0.2,fill="red",color="red")+
    
    #geom_text(size=textsize*(5/14),aes(x = Technology, y = Show,label = paste0(round(100*Exposure),"%"),vjust = ifelse(Exposure >= 0, -.3, 1)))+
    #facet_grid(. ~ Sector, scales = "free", space = "free")+
    #geom_hline(yintercept = 0, size = 1, color = textcolor)+
    #ylim(0,1)
    #scale_y_continuous( limits = c(0,1),expand = c(0.08,0.08))+
    #scale_x_discrete(expand=c(0,0))+
    ylab("Index of 2D Production Change")+
    theme_barcharts()+
    theme(panel.spacing.x = unit(.5,"cm"),
          strip.text = element_text(size=textsize,colour=textcolor),
          strip.background = element_blank(),
          plot.margin = (unit(c(0, 0, 0.1, 0), "lines")))
}



# ---------------table data frame ----------------------------------------#
total_vol_change_2040<-c("354% (4081667)","59.3% (1291000)","89.8% (388000)","31.4% (537333)","-41.0% (-829333)","-23.3% (-6435830000)",
                             "8.08% (3e+11)","-46.4% (-2.48e+09)","-21.1% (-14378565)","440% (51630364)","352% (9157470)")
total_vol_change_2023<-c("69.2% (798167)","13.0% (168000)","17.3% (74900)","8.26% (141633)","-2.54% (-51433)","-2.14% (-590930000)",
                             "4.58% (1.7e+11)","-10.9% (-5.84e+08)","-8.61% (-5862210)","97.0% (11387480)","105% (2733523)")

tech<-c("Renewables","Hydropower","Nuclear Power","Gas Power","Coal Power","Oil","Gas",
        "Coal","ICE Vehicle","Hybrid Vehicle","Electric Vehicle")
table<-data.frame(tech,total_vol_change_2023,total_vol_change_2040)
