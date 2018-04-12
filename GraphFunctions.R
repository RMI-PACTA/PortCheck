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
  InsuranceCompanyNameWrapped <- wrap.labels(PortfolioNameLong,18)
  # SizeofPortfolio <- PortfolioBreakdown$comma.PortfolioSize.[PortfolioBreakdown$PortName == PortName]
  if(PortfolioName == "MetaPort"){SizeofPortfolio <-1000}else{
  SizePortfolio <-  Ports.Overview %>%
      filter(Portfolio.Name == PortName) %>%
        distinct(Port.ValueUSD)
  SizeofPortfolio<- SizePortfolio[[1]]
  }
  
  SizeofPortfolio <- prettyNum(SizeofPortfolio,big.mark = ",")
  TodaysDate <- format(Sys.Date(),format = "%m.%d.%Y")

  
  NoPeers <- nrow(TestList)-1
    
  if(HasEquity & HasDebt){AssetClass <- "Corporate Bonds plus Bonds of Largest Government/Municipal Power Producers and Listed Equity"}
  else if(HasEquity & !HasDebt){AssetClass <- "Listed Equity"}
  else if(!HasEquity & HasDebt){AssetClass <- "Corporate Bonds plus Bonds of Largest Government/Municipal Power Producers "}
  
  ### Sector Check
  SectorCheck <- TestList[TestList$PortName == PortName,]

  HasPower <- SectorCheck$HasPower.CB| SectorCheck$HasPower.EQ
  HasAuto <- SectorCheck$HasAuto.CB| SectorCheck$HasAuto.EQ
  HasOG <- SectorCheck$HasOilGas.CB| SectorCheck$HasOilGas.EQ
  HasCoal <- SectorCheck$HasCoal.CB| SectorCheck$HasCoal.EQ
  
  ### Sector Weights ###
  FFSectorPortEQ <- 4
  PowerSectorPortEQ <- 4
  AutoSectorPortEQ <- 4
  
  FFSectorPortCB <- 4
  PowerSectorPortCB <- 4
  AutoSectorPortCB <- 4
  
  
  ### MERGE ALL RESULTS ###
  reportdata <<- data.frame(
           c("InsuranceCompanyName",InsuranceCompanyName),
           c("InsuranceCompanyNameWrapped",InsuranceCompanyNameWrapped),
           c("SizeofPortfolio",SizeofPortfolio),
           c("TodaysDate",TodaysDate),
           c("HasPower",HasPower),
           c("HasAuto",HasAuto),
           c("HasOG",HasOG),
           c("HasCoal",HasCoal),
           c("NoPeers",NoPeers),
           c("AssetClass", AssetClass),
           c("FFSectorPortEQ",FFSectorPortEQ),
           c("PowerSectorPortEQ",PowerSectorPortEQ),
           c("AutoSectorPortEQ",AutoSectorPortEQ),
           c("FFSectorPortCB",FFSectorPortCB),
           c("PowerSectorPortCB",PowerSectorPortCB),
           c("AutoSectorPortCB",AutoSectorPortCB)
           )

  colnames(reportdata) <- as.character(unlist(reportdata[1,]))
  reportdata = reportdata[-1, ]
 
  
  return(reportdata)
  
}

CAReport <- function(){
  
  reportdata <-CAReportData()
  HasAuto <- reportdata$HasAuto[[1]]
  HasPower <- reportdata$HasPower[[1]]
  HasOG <- reportdata$HasOG[[1]]
  
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
  
  # Remove EQ/CB sectors 
  if (!HasEquity){
    text <- removetextlines("EQSpecific")
  }

  if (!HasDebt){
    text <- removetextlines("CBSpecific")
  }  
  
  if(HasAuto == "FALSE"){
    text <- removetextlines("AutoSector")
  }
  if(HasPower == "FALSE"){
    text <- removetextlines("PowerSector")
  }
  if(HasOG == "FALSE"){
    text <- removetextlines("FossilFuelSector")
  }
  
  if(paste0(HasAuto,HasPower) == "FALSEFALSE"){
    text <- removetextlines("PowerAutomotiveSector")
  }
  
  # Replace Sector Weight Values
  a<-data.frame("SectorList"=paste0(rep(c("FF","Power","Auto"),1,each=2),"Sector","Port",rep(c("EQ","CB"),3)))
  for (j in 1:nrow(a)){
    text$text <- gsub(as.character(a$SectorList[j]),reportdata[as.character(a$SectorList[j])][[1]],text$text)
  }  
  
  # Replace Insurer Name
  text$text <- gsub("InsuranceCompanyName",PortfolioNameLong,text$text)
  text$text <- gsub("SizeofPortfolio",reportdata$SizeofPortfolio,text$text)
  text$text <- gsub("TodaysDate",reportdata$TodaysDate,text$text)
  text$text <- gsub("NoPeers",reportdata$NoPeers,text$text)
  text$text <- gsub("AssetClass",reportdata$AssetClass,text$text)
  
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
  TemplateNameNew <- paste0("Template_",PortfolioName,"_",Languagechoose)
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
    file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",Languagechoose,".pdf"))}else{
      file.rename(paste0(TemplateNameNew,".pdf"),paste0("AlignmentReport_",InvestorName,"_",PortfolioName,"_",Languagechoose,".pdf"))}
  
  
  
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

#----------- Distribution Chart ------------- #

distribution_chart <- function(plotnumber, ChartType, df, ID.COLS, MetricCol, ylim,
                               Title, Labels, LineHighl, LineLabels, LineColors, BarColors){

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
  
  x_coord <- length(unique(order$Name))
  
  arrow <- sum(filter(dfagg, Name == PortName)$Value)
  
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
                ifelse(ChartType == "EQ", "Equity", "Bond"),
                " Portfolios"))
  
  arrowlength <- ylimval/5
  
  if (PortName %in% dfagg$Name) {
    distribution_plot <- distribution_plot +
      geom_segment(data=filter(dfagg, Name == PortName),
                   aes(x=Name,xend=Name,y=arrow+arrowlength,yend=arrow+.001),
                   size = 1, arrow = arrow(length = unit(.4,"cm")))
  }

  return(distribution_plot)
  
}

# -------------STACKED BAR CHARTS ---------- #

stacked_bar_chart <- function(dat, colors, legend_labels){
  # "item", "family", "score", "value"
  colnames <- colnames(dat)
  
  plottheme <- ggplot(data=dat, aes_string(x=colnames[1], y=colnames[4], fill=colnames[3]),
                      show.guide = TRUE)+
    geom_bar(stat = "identity", position = "fill", width = .6)+
    geom_hline(yintercept = c(.25,.50,.75), color="white")+
    scale_fill_manual(values=colors,labels = legend_labels, breaks = names(legend_labels))+
    scale_y_continuous(expand=c(0,0), labels=percent)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()
  
  return(plottheme)
}
          

# -------- GRAPHS AND CHARTS -----------------                 

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
    do(data.frame(t(quantile(.$Exp.Carsten.Plan.Port.Scen.Market, probs = c(0.25, 0.75)))))
  
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
  Exposures$X25.<-ifelse(Exposures$X25. < -1,-1,Exposures$X25.)
  Exposures$X75.<-ifelse(Exposures$X75. < -1,-1,Exposures$X75.)
  
  
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
    
    # centre alignment line    # xmax
    annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour="black",fill = "transparent")+
    annotate(geom="rect",xmin =-1,xmax=1,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
    
    # Company Circles
    geom_point(data=Exposures,aes(x=comploc/100,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
    annotate(geom="text",label=Exposures$complabel, x= Exposures$comploc/100, y= Exposures$Locations, colour="white",size=rel(3))+ 
    
    # Distribution Range 
    annotate(geom="text",x= -1.03, hjust=1 , y= locations,label=Exposures$minlabel,size=rel(3),colour=textcolor)+     # Minimum
    annotate(geom="text",x= 1.03, hjust=0 , y= locations,label=Exposures$maxlabel,size=rel(3),colour=textcolor)+     # Maximum
    
    # Ranking box and label

    annotate("text", label = GT["RankTitle"][[1]], x= 1.3,y = max(locations)+ 0.5, size=rel(3),colour=textcolor,fontface = "bold")+ # Rank Heading

    annotate("text", label = paste0(Exposures$my_ranks," ",GT["RankOF"][[1]]," ",Exposures$mx), x= 1.3,hjust=0.5, y = locations,size=rel(3),colour=textcolor)+ # Company Ranking
    
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

  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_',ChartType,'_rankingchart.png', sep=""),bg="transparent",height=7.2,width=9.7,dpi=ppi)
  

  # outputplot <- ggplot_gtable(ggplot_build(outputplot))
  # outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
  # grid.draw(outputplot)  
  if (PrintPlot) {print(outputplot)}
  #return()
}

ranking_chart_alignment_original <- function(plotnumber,ChartType,SectorToPlot){
  
  
  if (ChartType == "EQ"){
    Exposures <- EQExposureRange
    AUMData <- EQAUMDatarange
    Ranks <-EQRanks
    
  }else if (ChartType == "CB"){
    Exposures <- CBExposureRange
    AUMData <- CBAUMDatarange
    Ranks <-CBRanks
  }
  
  
  TechList <- c("Electric","Hybrid","ICE","Coal","Oil","Gas","RenewablesCap","HydroCap","NuclearCap","GasCap","CoalCap")
  
  
  # if(PortfolioNameLong %in% c(Exposures$PortName, "PK","V") ){
  if(PortName %in% c(Exposures$PortName, "PK","V") ){
    
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
      
      # Rank <- Ranks[Ranks$PortName %in% PortfolioNameLong,]
      Rank <- Ranks[Ranks$PortName %in% PortName,]
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
    df <- df[df$PortName %in% c(PortName,"Minimum","Maximum","WeightedMean","Rank"),]
    
    PlotData <- setNames(data.frame(t(df[,-1])), df[,1]) 
    PlotData$Technology <- rownames(PlotData)
    PlotData <- merge(PlotData,sectors,by="Technology")
    
    PlotData$PortLoc <- PlotData[,PortName]/100
    
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
    PlotData$comploc <- PlotData[,PortName]/100
    PlotData$comploc[PlotData$comploc < 0] <- 0
    PlotData$comploc[PlotData$comploc > 2] <- 2
    
    PlotData$complabel<-PlotData[,PortName]
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
      
      # write.csv(PlotData,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
      
      graphheight <- 7.2
    }
  
    
    outputplot <- ggplot_gtable(ggplot_build(outputplot))
    outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
    grid.draw(outputplot)  
    
    if (PortfolioNameLong %in% c("PK","V")){
      PortfolioName<- PortfolioNameLong}
    
    if (SectorToPlot == "Fossil Fuels"){SectorToPlot <- "FossilFuels"}
    
    ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",SectorToPlot,'_RankingChart.png', sep=""),bg="transparent",height=graphheight,width=7,plot=outputplot)
    
  }
  
  return()
}

# -------- PORTFOLIO SUMMARY -------- #

sector_processing <- function(){
  
  ID.COLS = c("PortName","Year","Sector","Technology","CarstenMetric_Port","Type")
  
  EQ <- EQCombin
  if(HasEquity) {
    EQ$Type <- "Equity Portfolio"
    EQ <- unique(subset(EQ, Year == Startyear,
                        select = c(ID.COLS)))
  }
  CB <- CBCombin
  if(HasDebt) {
    CB$Type <- "Bond Portfolio"
    CB <- unique(subset(CB, Year == Startyear, 
                        select = c(ID.COLS)))
  }
  
  #Aggregate by sector, breaking down by the type (equity vs debt)
  df <- rbind(CB,EQ)
  df <- df %>% gather(key=Type, value=Value, -c(ID.COLS))
  df$Sector<-as.factor(df$Sector)
  levels(df$Sector)[levels(df$Sector)=="Coal"] <- "Fossil Fuels"
  levels(df$Sector)[levels(df$Sector)=="Oil&Gas"] <- "Fossil Fuels"
  levels(df$Sector)[levels(df$Sector)=="Power"] <- "Utility Power"
  dfagg <- aggregate(df["CarstenMetric_Port"],by=df[c("Sector","Type","PortName")],FUN=sum)
  
  dfagg <- dfagg %>%
    group_by(Sector) %>%
    complete(Type=c("Bond Portfolio","Equity Portfolio"), fill=list(CarstenMetric_Port = 0, PortName = PortName))
  
  return(dfagg)
}                                       

portfolio_sector_stack <- function(plotnumber){
  
  dfagg <- sector_processing()
  sectorpalette <- c(energy,pow,trans)
  sectororder <-c("Fossil Fuels","Utility Power","Automotive")
  colourdf <- data.frame(colour=sectorpalette, Sector =sectororder)
  dfagg$Sector<-as.factor(dfagg$Sector)
  combined <- sort(union(levels(dfagg$Sector), levels(colourdf$sectororder)))
  dfagg <- merge(dfagg, colourdf, by= "Sector") 
  orderofchart <- c("Bond Portfolio","Equity Portfolio")
  dfagg$Type <- factor(dfagg$Type,levels=orderofchart)
  dfagg$Sector<- factor(dfagg$Sector,levels = sectororder)
  dfagg <- dfagg[order(dfagg$Sector,dfagg$Type),]
  temp <-sum(dfagg$CarstenMetric_Port)
  ylabel = ""
  
  a<-ggplot(dfagg, aes(x=Type, y=CarstenMetric_Port,fill=Sector),show.guide = TRUE)+
    geom_bar(stat = "identity",width = .6)+
    scale_fill_manual(labels=unique(as.character(dfagg$Sector)),values=unique(as.character(dfagg$colour)))+
    scale_y_continuous(expand=c(0,0), limits = c(0,temp+0.01), labels=percent)+
    expand_limits(0,0)+
    ylab(ylabel)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()+
    theme(legend.position = "bottom", legend.title = element_blank())

  if(PrintPlot){print(a)}
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_SectorBarChart.png',sep=""),bg="transparent",height=3,width=4,plot=a,dpi=ppi)
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
    group_by(PortName) %>%
    complete(Technology=technologyorder, fill=list(Exposure = 0))
  Portfolio[Portfolio$Technology %in% c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap"),"Sector"] <- "Power"
  Portfolio[Portfolio$Technology %in% c("Coal","Gas","Oil"),"Sector"] <- "Fossil Fuels"
  Portfolio[Portfolio$Technology %in% c("Electric","Hybrid","ICE"),"Sector"] <- "Automotive"
  
  Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"] <- -1*Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"]
  
  Portfolio$Sector <- factor(Portfolio$Sector, levels = c("Fossil Fuels", "Power", "Automotive"))
  Portfolio$Technology <- factor(Portfolio$Technology, levels = technologyorder)
  
  Portfolio <- arrange(Portfolio, desc(Technology))
  
  TechLabels <- gsub("Cap","",technologyorder)
  names(TechLabels) <- technologyorder
  
  plot <- ggplot(Portfolio) +
    geom_bar(aes(x = Technology, y = Exposure, fill = Exposure >= 0), stat = "identity")+
    scale_fill_manual(values=c(area_6,area_2))+
    geom_text(size=textsize*(5/14),aes(x = Technology, y = Exposure,label = paste0(round(100*Exposure),"%"),vjust = ifelse(Exposure >= 0, -.3, 1)))+
    facet_grid(. ~ Sector, scales = "free", space = "free")+
    geom_hline(yintercept = 0, size = 1, color = textcolor)+
    scale_y_continuous(labels=percent, expand = c(0.08,0.08))+
    scale_x_discrete(labels=TechLabels,expand=c(0,0))+
    ylab("Exposure of Portfolio to 2° Market Benchmark")+
    theme_barcharts()+
    theme(panel.spacing.x = unit(.5,"cm"),
          strip.text = element_text(size=textsize,colour=textcolor),
          strip.background = element_blank(),
          plot.margin = (unit(c(0, 0, 0.1, 0), "lines")))
  
  if(PrintPlot){print(plot)}
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_ExposureSummary.png', sep=""),
         bg="transparent",height=3,width=7.5, units="in",dpi=ppi)
}

analysed_summary <- function(plotnumber){
  
  if (PortName != "MetaPort") {
    over <- Ports.Overview %>%
      filter(Portfolio.Name == PortName)
  } else {
    over <- Ports.Overview %>%
      group_by(Asset.Type, Valid) %>%
      summarise("ValueUSD" = sum(ValueUSD)) %>%
      ungroup() %>%
      mutate("Portfolio.Name" = "MetaPort")
  }
  
  names(over) <- gsub("TwoD\\.","",names(over))
  over$Asset.Type <- ifelse(over$Asset.Type=="Debt", "Bonds", over$Asset.Type)
  
  over$Asset.Type <- plyr::revalue(over$Asset.Type, 
                                   c("Bonds"="Bond Portfolio", "Equity" = "Equity Portfolio", "Other"="Other Holdings"),warn_missing = F)
  
  ## "steelblue" color below should be changed to whatever our Portfolio color is
  plot <- ggplot(over, aes(x=Asset.Type, y=ValueUSD/1000000, fill=factor(Valid))) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(name="", labels=c("Excluded", "In Analysis"), values=c("#d9d9d9",YourportColour)) +
    scale_x_discrete(name="Asset Type") +
    scale_y_continuous(name="", labels=dollar_format(suffix = " Million"), expand=c(0,0)) +
    theme_barcharts() + 
    theme(legend.position = "bottom")
  
  if(PrintPlot){print(plot)}
  
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,'_AnalysedSummary.png', sep=""),
         bg="transparent",height=3,width=4,dpi=ppi)
}

# ------------- DISTRIBUTIONS --------------- #

Risk_Distribution <- function(plotnumber, ChartType){
  Title <- "Percent of Bond Portfolio Value"
  MetricCol <- c("Risk3", "Risk2", "Risk1")
  
  if(ChartType == "CB") {
    RiskData <- Moodys
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
  
  BarColors <- c(LowRisk, MedRisk, HighRisk)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Emerging Moderate", "Emerging Elevated","Immediate Elevated")
  ylim = .5
  
  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol, ylim,
                     Title, Labels, LineHighl, LineLabels, LineColors, BarColors)
  
  if(PrintPlot){print(plot)}
  
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_Risk_Distribution.png"),
         height=3,width=7,dpi=ppi, bg="transparent")
}

Fossil_Distribution <- function(plotnumber, ChartType){
  Title <- "Percent of Portfolio Exposed to Fossil Fuels"
  if (ChartType == "EQ"){
    Batch <- EQBatchTest
  }else if (ChartType == "CB"){
    Batch <- CBBatchTest
  }
  
  #Tag Target portfolio, benchmark
  Batch <- subset(Batch, Year == Startyear & Sector %in% c("Coal","Oil&Gas") & Type != "Market",
                       select=c("PortName","Technology","CarstenMetric_Port"))
  ID.COLS = c("PortName")
  MetricCol <- "CarstenMetric_Port"
  
  BarColors <- c(energy)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Fossil Fuels")
  df <- unique(subset(Batch, select = c(ID.COLS,MetricCol)))

  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol, 1,
                     Title, Labels, LineHighl, LineLabels, LineColors, BarColors) +
    theme(legend.position = "none")

    
  # print(plot)
  # png(filename = paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",'Fossil_Distribution.png')
  
  ggsave(plot=plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",'Fossil_Distribution.png', sep=""),
         height=3,width=7,dpi=ppi, bg="transparent")
  
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
    Portfoliomix$Name <- "Your Portfolio"
    Portfoliomix <- subset(Portfoliomix, select =c("Name","Classification","Technology","WtProduction"))
    Portfoliomix$WtProduction <- Portfoliomix$WtProduction / sum(Portfoliomix$WtProduction)
    colnames(Portfoliomix) <- c("Name","Classification","Technology","TechShare")
    
    # Add 2D Target (Global Market under 2D Scenario)
    Targetmix <- subset(combin, select = c("Technology","Scen.WtProduction"))
    Targetmix$Classification <- "Portfolio"
    Targetmix$Name<-GT["X2Target"][[1]]
    Targetmix <- subset(Targetmix, select =c("Name","Classification","Technology","Scen.WtProduction"))
    Targetmix$Scen.WtProduction <- Targetmix$Scen.WtProduction / sum(Targetmix$Scen.WtProduction)
    colnames(Targetmix) <- c("Name","Classification","Technology","TechShare")
    
    # Add Benchmark / Global Market
    Marketmix <- subset(market, select=c("Technology","WtProduction"))
    Marketmix$Classification <- "Portfolio"
    Marketmix$Name <- "Market"
    Marketmix <- subset(Marketmix, select=c("Name","Classification","Technology","WtProduction"))
    Marketmix$WtProduction <- Marketmix$WtProduction / sum(Marketmix$WtProduction)
    colnames(Marketmix) <- c("Name","Classification","Technology","TechShare")

    PortfolioData <- rbind(Marketmix, Targetmix, Portfoliomix)

    # Percentage share of each technology for each company in the portfolio
    Companies <- subset(CompProdSS, select=c("Name","Technology","CompanyLvlProd","CompanyLvlSecProd","PortWeightEQYlvl"))
    Companies$TechShare <- (Companies$CompanyLvlProd/Companies$CompanyLvlSecProd)
    Companies$Classification <- "Companies"
    Companies <- subset(Companies, select = c("Name","Classification","Technology","TechShare","PortWeightEQYlvl"))
    colnames(Companies) <- c("Name","Classification","Technology","TechShare","PortWeight")
    Companies[is.na(Companies$Name),"Name"] <- "No Name"

    if (SectorToPlot == "Power"){  
      techorder <- technology_order[1:5]
      
      tech_labels <- c(paste0("% ", GT["T_CoalCap"][[1]]),paste0("% ", GT["T_GasCap"][[1]]),
                      paste0("% ", GT["T_NuclearCap"][[1]]),paste0("% ", GT["T_HydroCap"][[1]]),
                      paste0("% ", GT["T_RenewablesCap"][[1]]))
      colors <- as.vector(ColourPalette$Colours[1:5])
    }
    
    if (SectorToPlot == "Automotive"){
      techorder <- technology_order[6:8]
      tech_labels <- c(paste0("% ", GT["T_ICE"][[1]]),paste0("% ", GT["T_Hybrid"][[1]]),paste0("% ", GT["T_Electric"][[1]]))
      colors <- as.vector(ColourPalette$Colours[6:8])
    }
    
    if (SectorToPlot == "Fossil Fuels") {
      techorder <- technology_order[9:11]
      tech_labels <- c(paste0("% ", GT["T_CoalProd"][[1]]),paste0("% ", GT["T_GasProd"][[1]]),paste0("% ", GT["T_CoalProd"][[1]]))
      colors <- as.vector(ColourPalette$Colours[9:11])
    }
  
    if (SectorToPlot == "Oil"){
      #----------------TO DO
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
    AllData$Technology <- factor(AllData$Technology, levels=techorder)
    
    names(colors) <- techorder
    names(tech_labels) <- techorder

    scaleFUN <- function(x) sprintf("%.1f", x)
    
    PortPlot <- stacked_bar_chart(AllData, colors, tech_labels)+
      geom_text(aes(x = "", y = 1),
                label = "% in Portfolio",
                hjust = -.1, color = textcolor)+
      geom_text(data=filter(AllData,Classification=="Companies"),
                aes(x = Name, y = 1),
                label = paste0(scaleFUN(100*Companies$PortWeight),"%"),
                hjust = -1, color = textcolor, size=textsize*(5/14))+
      xlab("Companies")+
      ylab("TechShare")+
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
           bg="transparent",height=4,width=10,dpi=ppi)
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

# ------------ 246 Chart -------------------- #
Inputs246 <- function(TechToPlot){
  #EQ
  Combin1 <- EQCombin
  if (nrow(Combin1)>0){Combin1$Label <- "Equity"}
  Aldprod1 <- EQALDAggProd
  Combin1<- merge(Combin1,Aldprod1, by =c("PortName","Technology","Year"))
  Combin1<- Combin1 %>%
    filter(Scenario.y %in% Scenariochoose)
  
  
  #CB
  Combin2 <- CBCombin
  if (nrow(Combin2)>0){Combin2$Label <- "Bond"}
  Aldprod2 <- CBALDAggProd
  Combin2<- merge(Combin2,Aldprod2, by =c("PortName","Technology","Year"))
  Combin2<- Combin2 %>%
    filter(Scenario.y %in% Scenariochoose)
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
      
      df<-as.data.frame(df)
      df$Diff<-ifelse(df$Diff < 0,-df$Diff,df$Diff)
      
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
  
  if ((nrow(Combin1) >0) &  (nrow(Combin2) >0)){
    Production1 <- subset(Combin1, Technology %in% TechToPlot & Year %in% Startyear:(Startyear+5))
    Production1 <- subset (Production1, select=c("Year","Plan.Pct.Build.Out","Label"))
    Production2 <- subset(Combin2, Technology %in% TechToPlot & Year %in% Startyear:(Startyear+5))
    Production2 <- subset (Production2, select=c("Year","Plan.Pct.Build.Out","Label"))
    
    Production <- as.data.frame(rbind(Production1,Production2))
  } else if (((nrow(Combin1) ==0) &  (nrow(Combin2) >0))){
    Production <- subset(Combin2, Technology %in% TechToPlot & Year %in% Startyear:(Startyear+5))
    Production <- subset (Production, select=c("Year","Plan.Pct.Build.Out","Label"))
    
    
  } else if(((nrow(Combin2) ==0) &  (nrow(Combin1) >0))){
    Production <- subset(Combin1, Technology %in% TechToPlot & Year %in% Startyear:(Startyear+5))
    Production <- subset (Production, select=c("Year","Plan.Pct.Build.Out","Label"))
    
    
  }
  
  
  
  ## Stock Market Build Out
  
  MarketBuildOut1 <- subset(Aldprod1, InvestorName == "Market"& Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
  MarketBuildOut1 <- subset(MarketBuildOut1,select = c("Year","Plan.Pct.Build.Out"))
  MarketBuildOut1$Label <- "Stock Market"
  MarketBuildOut2 <- subset(Aldprod2, InvestorName == "Market"& Technology %in% TechToPlot  & Scenario %in% Scenariochoose & Year %in% Startyear:(Startyear+5))
  MarketBuildOut2 <- subset(MarketBuildOut2,select = c("Year","Plan.Pct.Build.Out"))
  MarketBuildOut2$Label <- "Debt Market"
  
  ### Inputs to the 246 chart.
  
  IEATargets246 <- subset(AllIEATargets, BenchmarkRegion == "Global" & Year %in% Startyear:(Startyear+5)  &
                            Scenario %in% c("450S","NPS","CPS"), select = c("Sector","Technology","Scenario","Year","AnnualvalIEAtech"))
  
  IEATargets <- subset(IEATargets246, Technology %in% TechToPlot)
  IEATargetsRef <- subset(IEATargets, Scenario == "450S", select=c("Year","AnnualvalIEAtech"))
  names(IEATargetsRef)[names(IEATargetsRef)=="AnnualvalIEAtech"] <- "TargetProd"
  IEATargets <- merge(IEATargets,IEATargetsRef, by="Year")
  
  IEATargets <- BuildOutCalc(IEATargets,TechToPlot)
  
  IEATargets <- subset(IEATargets, select = c("Label","Year","Plan.Pct.Build.Out"))
  
  
  df <- rbind(Production,MarketBuildOut1,MarketBuildOut2,IEATargets)
  
  
  return(df)
}


Graph246 <- function(plotnumber, TechToPlot){
  
  
  # BatchTest1 <- EQBatchTest
  # Combin1 <- EQCombin
  LinesToPlot <- c("Equity","Bond","Stock Market","Debt Market")
  # BatchTest2 <- CBBatchTest
  # Combin2 <- CBCombin
  
  
  # Check whether the tech is a green or brown technology
  GoodBad <- GreenBrown(TechToPlot)
  
  df <- Inputs246(TechToPlot)
  
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
    
    # dfwide$Line1 <- dfwide$`450S`
    # dfwide$Line2 <- dfwide$NPS - dfwide$`450S`
    # dfwide$Line3 <- dfwide$CPS - dfwide$NPS
    # dfwide$Line4 <- dfwide$MaxValue - dfwide$CPS
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
  dftar <- melt(dfwide, id.vars =  "Year", variable.name = "Lab")
  # # dftargets <- rev(dftargets)
  # 
  # # AreaNames <-  c( "< 2°C","2-4°C","4-6°C","> 6°C")
  # # Palette <- c(DarkGreen,LightGreen,LightRed,DarkRed)
  #brown lineorder <-c("Line4","Line3","Line2","Line1")
  #lineorder <-c("Line1","Line2","Line3","Line4")
  colourdf <- data.frame(colour=Palette, Target =lineorder, Labels = AreaNames)
  # 
  # #dftargets$Target<-as.factor(dftargets$Target, lestr
  combined <- sort(union(levels(dftargets$Target), levels(colourdf$Target)))
  dftargets <- merge(dftargets, colourdf, by= "Target")
  dftargets$Target<- factor(dftargets$Target,levels = lineorder, ordered=TRUE)
  # 
  # 
  # maxval <- max(dftargets$value) +0.1
  # minval <- min(dftargets$value) -0.1
  # 
  LineColours <- c(eq_line, cb_line,peer_group,peer_group)
  
  
  year_lab = Startyear
  LineVector <- setNames(LineColours,LinesToPlot)
  
  
  
  #ylabel <- "Normalized Built Out"
  
  #if(('Portfolio' %in% colnames(dfwide)) == TRUE)  {
  
  if (GoodBad == "Brown"){
    dftargets$lower <-c(rep(-2.4,6),dfwide$Line1,dfwide$Line2,dfwide$Line3)

    a<- min(dftar$value)
    if (a< -2){a<- -2}

    outputplot <- ggplot(data = dftargets)+
      geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target),alpha=0.6)+
      geom_line(aes(x=dftar[which(dftar$Lab=="Debt Market"),]$Year,y=dftar[which(dftar$Lab=="Debt Market"),]$value,colour =  "Debt Market"), data=subset(dftar,Lab=="Debt Market"), size = linesize,linetype=2)+   # Market
      geom_line(aes(x=dftar[which(dftar$Lab=="Stock Market"),]$Year,y=dftar[which(dftar$Lab=="Stock Market"),]$value,colour =  "Stock Market"), data=subset(dftar,Lab=="Stock Market"), size = linesize,linetype=2)+ 
      scale_color_manual(name="",values = c("Debt Market"=cb_line,"Stock Market"=eq_line))+
      scale_fill_manual(labels=unique(dftargets$Labels),
                        values=unique(as.character(dftargets$colour)))+
      scale_x_continuous(expand=c(0,0), limits=c(2018,2023)) +

      scale_y_continuous(name="Ratio of Currently Planned Production \nTo Production Levels Specified by 2D Scenario",breaks = seq(-2, 2, 1))+

      coord_cartesian(ylim=c(a,2))+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line = element_line(),
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"),
            axis.ticks = element_blank())
  } else if (GoodBad =="Green"){
    dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line3)
    outputplot <- ggplot(data = dftargets)+
      geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target),alpha=0.6)+
      geom_line(aes(x=dftar[which(dftar$Lab=="Debt Market"),]$Year,y=dftar[which(dftar$Lab=="Debt Market"),]$value,colour =  "Debt Market"), data=subset(dftar,Lab=="Debt Market"), size = linesize,linetype=2)+   # Market
      geom_line(aes(x=dftar[which(dftar$Lab=="Stock Market"),]$Year,y=dftar[which(dftar$Lab=="Stock Market"),]$value,colour =  "Stock Market"), data=subset(dftar,Lab=="Stock Market"), size = linesize,linetype=2)+   # Market
      scale_color_manual(name="",values = c("Debt Market"=cb_line,"Stock Market"=eq_line))+
      scale_fill_manual(labels=(unique(dftargets$Labels)),
                        values=(unique(as.character(dftargets$colour))))+
      scale_x_continuous(expand=c(0,0), limits=c(2018,2023)) +

      scale_y_continuous(name="Ratio of Currently Planned Production \nTo Production Levels Specified by 2D Scenario",breaks = seq(0, 1, 0.25))+

      coord_cartesian(ylim=c(0,1))+
      theme_bw()+
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"),
            axis.ticks = element_blank())
  }
  
  
  
  
  if((('Bond' %in% colnames(dfwide)) == TRUE)& (('Equity' %in% colnames(dfwide)) == FALSE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Bond"),]$Year,y=dftar[which(dftar$Lab=="Bond"),]$value,colour =  "Bond"), data=subset(dftar,Lab=="Bond"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Bond"=cb_line,"Debt Market"=cb_line,"Stock Market"=eq_line))+
      theme(legend.position="none",
            text=element_text(family="Arial"))
    
  }else if ((('Bond' %in% colnames(dfwide)) == FALSE)& (('Equity' %in% colnames(dfwide)) == TRUE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Equity"),]$Year,y=dftar[which(dftar$Lab=="Equity"),]$value,colour = "Equity"), data=subset(dftar,Lab=="Equity"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Equity"=eq_line,"Debt Market"=cb_line,"Stock Market"=eq_line))+
      theme(legend.position="none",
            text=element_text(family="Arial"))
  }else if ((('Bond' %in% colnames(dfwide)) == TRUE)& (('Equity' %in% colnames(dfwide)) == TRUE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Bond"),]$Year,y=dftar[which(dftar$Lab=="Bond"),]$value,colour =  "Bond"), data=subset(dftar,Lab=="Bond"), size = linesize,linetype=1)+   # Market
      
      geom_line(aes(x=dftar[which(dftar$Lab=="Equity"),]$Year,y=dftar[which(dftar$Lab=="Equity"),]$value,colour =  "Equity"), data=subset(dftar,Lab=="Equity"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Equity"=eq_line,"Bond"=cb_line,"Debt Market"=cb_line,"Stock Market"=eq_line))+
      theme(legend.position="none",
            text=element_text(family="Arial"))
  }else if ((('Bond' %in% colnames(dfwide)) == FALSE)& (('Equity' %in% colnames(dfwide)) == FALSE) ){
    outputplot <- outputplot+
      theme(legend.position="none",
            text=element_text(family="Arial"))
  }
  
  
  
  if(PrintPlot){print(outputplot)}
  
  

  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,"_246.png",sep=""),height=3.6,width=4.6,dpi=ppi*2)


  
  #ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),height=3.6,width=4.6,plot=outputplot,dpi=ppi*2)	+  #ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),bg="transparent",height=3.6,width=4.6,plot=outputplot,dpi=ppi*2)
                    #height=3.6,width=4.6,plot=outputplot,dpi=ppi*2) #bg="transparent",
}
          
