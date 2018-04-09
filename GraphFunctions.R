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
  SizeofPortfolio <-10000000
  
  NoPeers <- nrow(TestList)-1
    
  if(HasEquity & HasDebt){AssetClass <- "Corporate Debt and Equity"}
  else if(HasEquity & !HasDebt){AssetClass <- "Equity"}
  else if(!HasEquity & HasDebt){AssetClass <- "Corporate Debt"}
  
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
  
  # Replace Sector Weight Values
  a<-data.frame("SectorList"=paste0(rep(c("FF","Power","Auto"),1,each=2),"Sector","Port",rep(c("EQ","CB"),3)))
  for (j in 1:nrow(a)){
    text$text <- gsub(as.character(a$SectorList[j]),reportdata[as.character(a$SectorList[j])][[1]],text$text)
  }  
  
  # Replace Insurer Name
  text$text <- gsub("InsuranceCompanyName",PortfolioNameLong,text$text)
  text$text <- gsub("SizeofPortfolio",reportdata$SizeofPortfolio,text$text)
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
        axis.text.x=element_text(face="bold",colour=textcolor,size=textsize),
        axis.text.y=element_text(face="bold",colour=textcolor,size=textsize),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = textcolor,size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        # legend.position=c(0.5,0),#legend.position = "none",
        legend.position = "none",
        legend.direction="horizontal",
        legend.text = element_text(face="bold",size=textsize,colour=textcolor),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key.size=unit(0.4,"cm"),
        #legend.title=element_blank(),
        legend.title = element_text(colour = textcolor, size = textsize),
        legend.key = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(1,1, 0, 0), "lines")
        # plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}

theme_linecharts <- function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(), 
        axis.text.x=element_text(face="bold",colour=textcolor,size=textsize),
        axis.text.y=element_text(face="bold",colour=textcolor,size=textsize),
        axis.title.x=element_text(face="bold",colour=textcolor,size=textsize),
        axis.title.y=element_text(face="bold",colour=textcolor,size=textsize),
        axis.line = element_line(colour = "black",size=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        # legend.position=c(0.5,0),#legend.position = "none",
        legend.position = "none",
        legend.direction="horizontal",
        legend.text = element_text(face="bold",size=textsize,colour=textcolor),
        legend.background = element_rect(fill = "transparent",colour = NA),
        legend.key.size=unit(0.4,"cm"),
        #legend.title=element_blank(),
        legend.title = element_text(colour = textcolor, size = textsize),
        legend.key = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = unit(c(1,1, 0, 0), "lines")
        #plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}    

theme_distribution <- function(base_size = textsize, base_family = "") {
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(face="bold",colour=textcolor,size=textsize),
        axis.text.y=element_text(face="bold",colour=textcolor,size=textsize),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line.x = element_line(colour = textcolor,size=1),
        axis.line.y = element_line(colour = textcolor,size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(color = textcolor, size = textsize),
        legend.title = element_blank(),
        plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines"),
        # plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, color = textcolor)
  )
}

#----------- Distribution Chart ------------- #

distribution_chart <- function(plotnumber, ChartType, df, ID.COLS, MetricCol,
                               Title, Labels, LineHighl, LineLabels, LineColors, BarColors){
 
  df <- df %>% gather(key=Metric, value=Value, -c(ID.COLS))
  
  dfagg <- aggregate(df["Value"],by=df[c("PortName","Metric", "Type")],FUN=sum)
  dfagg$Value <- as.numeric(dfagg$Value)
  
  dfagg <- dfagg %>%
    group_by(PortName,Type) %>%
    summarise("Value" = 1-sum(Value), "Metric" = "Unexposed") %>%
    ungroup() %>%
    mutate("Metric" = "Unexposed") %>%
    select(PortName,Metric,Type,Value) %>%
    rbind(dfagg)
  
  dfagg <- rename(dfagg, "Name" = PortName)
  order <- dfagg %>% filter(Metric == "Unexposed") %>% arrange(Value)
  dfagg$Name <- factor(dfagg$Name, levels=unique(order$Name))
  dfagg <- filter(dfagg, Metric != "Unexposed", Type != "Market")
  if (PortName != "MetaPort") {
    dfagg <- filter(dfagg, Type != "MetaPortfolio")
  }
  dfagg$Metric <- factor(dfagg$Metric, levels=c(MetricCol))
  
  x_coord <- length(unique(order$Name))
  
  distribution_plot<- ggplot(dfagg)+
    geom_bar(aes(x=Name, y=Value, fill=Metric),
             stat = "identity", width=1)+
    geom_segment(data=filter(dfagg, Name == PortName, Metric == MetricCol[2]),
                 aes(x=Name,xend=Name,y=Value+.05,yend=Value+.001),
                 size = 1, arrow = arrow(length = unit(.5,"cm")))+
    # geom_rect(data=filter(dfagg, Name == PortName, Metric == MetricCol[2]),
    #              aes(xmin=Name,xmax=Name,ymin=0,ymax=Value+0.05),
    #              size = .5)+
    
    scale_fill_manual(values=BarColors,labels=Labels, breaks=c(MetricCol))+
    scale_y_continuous(expand=c(0,0), limits = c(0,1.001), labels=percent)+
    scale_x_discrete(labels=NULL)+
    expand_limits(0,0)+
    # ggtitle(Title)+
    coord_cartesian(ylim=c(0,min(1, 1.1*max(dfagg$Value))))+
    theme_distribution()
  
  
  return(distribution_plot)
  
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
  
  Mins<- aggregate(Exp.Carsten.Plan.Port.Scen.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], min)  #
  Maxs <- aggregate(Exp.Carsten.Plan.Port.Scen.Market ~ Technology, data = BatchTest[which(BatchTest$Year==Startyear+5),], max) # need define variable
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
  
  Exposures$xlowloc <- Exposures$LowLim
  Exposures$xupploc <- Exposures$UppLim
  Exposures$xlowloc[Exposures$xlowloc < -1] <- -1
  Exposures$xupploc[Exposures$xupploc > 1] <- 1
  # PlotData$comploc <- PlotData[,PortName]/100
  # PlotData$comploc[PlotData$comploc < 0] <- 0
  # PlotData$comploc[PlotData$comploc > 2] <- 2
  
  Exposures$comploc<-Exposures$Exp.Carsten.Plan.Port.Scen.Market*100
  # PlotData$complabel[PlotData$complabel>200]<-200
  # PlotData$complabel[PlotData$complabel<0]<-0    
  Exposures$comploc[Exposures$comploc >10000]<-10000
  Exposures$comploc[Exposures$comploc < -10000]<- -10000
  
  Exposures$complabel <- paste0(round(Exposures$comploc,0),"%")
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
    geom_segment(data=Exposures,aes(x=xlowloc, xend=xupploc,y=Locations,yend=Locations), linetype="dashed",colour="black")+
    geom_point(data=Exposures,aes(x=xlowloc,y=Locations), fill="black",colour="black", size=2)+
    geom_point(data=Exposures,aes(x=xupploc,y=Locations),  fill="black",colour="black",size=2)+
    
    # centre alignment line    # xmax
    annotate(geom="rect",xmin = 0,xmax=1,ymin = locations-bh/2,ymax=locations+bh/2,colour=Tar2DColour ,fill = "transparent")+
    annotate(geom="rect",xmin =-1,xmax=1,ymin=(locations-bh/2),ymax=(locations+bh/2), fill="transparent",colour="black")+ # Box around the bars
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
    geom_point(data=Exposures,aes(x=comploc/100,y=Locations),  fill=YourportColour,colour=YourportColour,size=10)+
    annotate(geom="text",label=Exposures$complabel, x= Exposures$comploc/100, y= Exposures$Locations, colour="white",size=rel(3))+ 
    
    # Distribution Range 
    annotate(geom="text",x= -1.1, hjust=1 , y= locations,label=Exposures$minlabel,size=rel(3),colour=textcolor)+     # Minimum
    annotate(geom="text",x= 1.1, hjust=0 , y= locations,label=Exposures$maxlabel,size=rel(3),colour=textcolor)+     # Maximum
    
    # Ranking box and label
    annotate("text", label = GT["RankTitle"][[1]], x= 1.5,y = max(locations)+ 0.5, size=rel(3),fontface = "bold",colour=textcolor)+ # Rank Heading
    annotate("text", label = paste0(Exposures$my_ranks," ",GT["RankOF"][[1]]," ",Exposures$mx), x= 1.5,hjust=0.5, y = locations,size=rel(3),fontface = "bold",colour=textcolor)+ # Company Ranking
    
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
  
  
    labelloc <- -1.5
    leafloc <- c(11,12,2,3)
    if (any((Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"))) & any(Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")))
    
    outputplot<-    outputplot+
      labs(x=NULL,y= NULL)+
      annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],12), size=rel(3), hjust=0, fontface = "bold",colour=textcolor)+  # Technology Label - Black
      annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")+ 
      geom_hline(yintercept = c((tail(a,1)+0.75),(d[1]-0.75)))
    
    else if  (any((Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal"))) & (any(Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear"))==FALSE)){
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL)+
        annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in%  c("CoalCap","GasCap","ICE","Oil","Gas","Coal")],12), size=rel(3), hjust=0, fontface = "bold",colour="black")
    }else if  (any((Exposures$Technology %in%  c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear"))) & (any(Exposures$Technology %in% c("CoalCap","GasCap","ICE","Oil","Gas","Coal"))==FALSE)){
      outputplot<-    outputplot+
        labs(x=NULL,y= NULL)+
        annotate(geom="text",x=labelloc,y=Exposures$Locations[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],label=wrap.labels(Exposures$TechLabel[Exposures$Technology %in% c("Electric", "Hybrid","RenewablesCap", "Hydro", "Nuclear")],12), size=rel(3), hjust=0, fontface = "bold",colour="darkgreen")
      }
    #write.csv(Exposures,paste0("RankingChartData_",ChartType,"_",PortfolioName,".csv"),row.names = F)
    
    graphheight <- 7.2
  
  
ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_rankingchart.png', sep=""),bg="transparent",height=7.2,width=9.7,dpi=ppi)


  
  # outputplot <- ggplot_gtable(ggplot_build(outputplot))
  # outputplot$layout$clip[outputplot$layout$name == "panel"] <- "off"
  # grid.draw(outputplot)  
  print(outputplot)
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

portfolio_pie_chart <- function(plotnumber,ChartType){                           
  
  if (ChartType == "EQ"){
    pieshare <- EQCombin
  }else if(ChartType == "CB"){
    pieshare <- CBCombin
  }
  
  # Data 
 
  pieshare$Label <- "NeedsALabel"
  pieshare$Colour <- RenewablesColour
  pieshare$perc <- 20
  
  # PieChart<- ggplot(pieshare, aes(x="", y=WtProduction, fill=Sector))+
    # geom_bar(stat = "identity",color=NA, width = 1)
  
  # PieChart <- PieChart + coord_polar("y", start=0, direction=-1)+ xlab('') +  ylab('')
  
  PieChart<- ggplot(pieshare, aes(x="", y=WtProduction, fill=Sector))+
    geom_bar(stat = "identity",color=NA, width = 0.5)+
    geom_bar(stat = "identity",color='white',show.legend = FALSE, lwd = .25,width = 1)+
    scale_fill_manual(values= pieshare$Colour,labels=paste(pieshare$Label,": ",pieshare$perc,"%",sep=""))+
    guides(fill = guide_legend(override.aes = list(colour = NULL)))+
    theme(axis.ticks=element_blank(), axis.text.y=element_blank(),axis.title=element_blank(),
          axis.text.x=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_blank(), plot.margin = unit(c(0,0, 0, 0), "lines"),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.text = element_text(size=textsize,colour="black"),
          legend.key.size=unit(0.4,"cm"),legend.title=element_blank())
  
  PieChart <- PieChart + coord_polar("y", start=0, direction=-1)#+ xlab('') #+  ylab('')
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_',ChartType,'_PieChart.png',sep=""),
         bg="transparent",height=4,width=4,plot=PieChart,dpi=ppi)
  
}

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

portfolio_sector_stack <- function(plotnumber){
  
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
    scale_y_continuous(expand=c(0,0), limits = c(0,temp+0.05), labels=percent)+
    expand_limits(0,0)+
    ylab(ylabel)+
    guides(fill=guide_legend(nrow = 1))+
    theme_barcharts()+
    theme(legend.position = "bottom",
          axis.title=element_blank(),
          axis.line.x = element_line(colour = "black",size=1),
          axis.line.y = element_blank(),
          panel.background = element_blank(),
          legend.text = element_text(face="bold",size=textsize,colour=textcolor),
          legend.background = element_rect(fill = "transparent",colour = NA),
          legend.key.size=unit(0.4,"cm"),
          legend.title=element_blank(),
          plot.margin = unit(c(0.6,1.0, 2.5, 0), "lines")
    )
  # print(a)
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,'_SectorBarChart.png',sep=""),bg="transparent",height=3,width=4,plot=a,dpi=ppi)
}

exposure_summary <- function(plotnumber,ChartType){
  
  if(ChartType == "CB") {
    Portfolio <- CBCombin
  } else if (ChartType == "EQ") {
    Portfolio <- EQCombin
  }
  
  BrownTech = c("ICE","Coal","Gas","Oil","CoalCap","GasCap", "OilCap")
  
 technologyorder <- c("CoalCap","GasCap","NuclearCap","HydroCap","RenewablesCap","Electric","Hybrid","ICE","Coal","Gas","Oil")
  
  Portfolio <- Portfolio %>%
    filter(Year == Startyear+5, Technology != "OilCap") %>%   #!= "OilCap"
    select(PortName, Sector, Technology, Exp.Carsten.Plan.Port.Scen.Market) %>%
    rename("Exposure" = Exp.Carsten.Plan.Port.Scen.Market)
  
  Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"] <- -1*Portfolio[Portfolio$Technology %in% BrownTech, "Exposure"]
  
  
  Portfolio$Sector <- recode(Portfolio$Sector, Coal = "Fossil Fuels", `Oil&Gas` = "Fossil Fuels")
  Portfolio$Technology <- factor(Portfolio$Technology, levels = technologyorder)
  
  ### This is the correct colour palette, I just didn't work out how to implement this. 
  repval = 200
  redgreen<- colorRampPalette(c(area_6,area_2_4, area_2))(repval) 
  
  Portfolio$Technology <- gsub("Cap","",Portfolio$Technology)
  
  plot <- ggplot(Portfolio) +
    geom_bar(aes(x = Technology, y = Exposure), fill = YourportColour, stat = "identity") +
    facet_grid(. ~ Sector, scales = "free", space = "free") +
    geom_hline(yintercept = 0, size = 1, color = textcolor)+
    scale_y_continuous(labels=percent,limits=(c(-1,1)))+
    scale_fill_manual(values = colours, labels = labels) +
    theme_barcharts() +
    theme(panel.border = element_rect(color=textcolor,fill=NA,size=1),
          panel.spacing.x = unit(0,"cm"),
          strip.text = element_text(colour=textcolor),
          strip.background = element_blank())
  
  # print(plot)
  ggsave(plot,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_ExposureSummary.png', sep=""),
         bg="transparent",height=4,width=10,dpi=ppi)
}

# ------------- DISTRIBUTIONS --------------- #

# Carstens_Distribution <- function(plotnumber, ChartType){
  # Title <- "Exposure of Portfolios to Climate Relevent Sectors"
  # if(ChartType == "CB") {
  #   BatchTest <- CBBatchTest
  # } else if (ChartType == "EQ") {
  #   BatchTest <- EQBatchTest
  # }
  # ID.COLS = c("PortName","Year","Sector","Technology", "Type")
  # MetricCol <- "CarstenMetric_Port"
  # 
  # BarColors <- c("Grey", "Black","White")
  # names(BarColors) <- c(MetricCol,"Comparison","Unexposed")
  # Labels <- c("Exposed", "Your Portfolio")
  # df <- unique(subset(BatchTest, Year == Startyear, 
  #                     select = c(ID.COLS,MetricCol)))
  # 
  # LineHighl <- c("MetaPortfolio")
  # LineLabels <- c("Average")
  # names(LineLabels) <- LineHighl
  # LineColors <- c("Green")
  # names(LineColors) <- LineLabels
  # 
  # distribution_chart(plotnumber, "Carsten", ChartType, df, ID.COLS, MetricCol,
  #                    Title, Labels, LineHighl, LineLabels, LineColors, BarColors)
  # 
# }

Risk_Distribution <- function(plotnumber, ChartType){
  Title <- "Risk Exposure of Portfolios"
  MetricCol <- c("Risk1", "Risk2")
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
  # metaport[cols] <- 0
  # metaport <- metaport[colnames(PortSS)]
  # 
  # PortSS <- rbind(PortSS,metaport)
  
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
  
  BarColors <- c(HighRisk, MedRisk)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Immediate Elevated", "Emerging Elevated")
  
  LineHighl <- c("MetaPortfolio")
  LineLabels <- c("Average")
  names(LineLabels) <- LineHighl
  LineColors <- c("Green")
  names(LineColors) <- LineLabels
  
  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol,
                     Title, Labels, LineHighl, LineLabels, LineColors, BarColors)
  
  print(plot)
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",'Risk_Distribution.png', sep=""),
         height=4,width=10,plot=plot,dpi=ppi, bg="transparent")
  
}

Fossil_Distribution <- function(plotnumber, ChartType){
  Title <- "Fossil Fuel Breakdown of Portfolios"
  if (ChartType == "EQ"){
    Batch <- EQBatchTest
  }else if (ChartType == "CB"){
    Batch <- CBBatchTest
  }
  
  #Tag Target portfolio, benchmark
  Batch <- subset(Batch, Year == Startyear & Sector %in% c("Coal","Oil&Gas"),
                       select=c("PortName","Technology","CarstenMetric_Port","Type"))
  ID.COLS = c("PortName","Type","Technology")
  MetricCol <- "CarstenMetric_Port"
  
  BarColors <- c(energy)
  names(BarColors) <- c(MetricCol)
  Labels <- c("Fossil Fuels")
  df <- unique(subset(Batch, select = c(ID.COLS,MetricCol)))
  
  LineHighl <- c("MetaPortfolio")
  LineLabels <- c("Average")
  names(LineLabels) <- LineHighl
  LineColors <- c("Green")
  names(LineColors) <- LineLabels
  
  plot <- distribution_chart(plotnumber, ChartType, df, ID.COLS, MetricCol,
                     Title, Labels, LineHighl, LineLabels, LineColors, BarColors) +
    theme(legend.position = "none")
  
  print(plot)
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,"_",'Fossil_Distribution.png', sep=""),
         height=4,width=10,plot=plot,dpi=ppi, bg="transparent")
  
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
  
  if (SectorToPlot == "Oil"){
    techorder <- c("Conventional Oil","Heavy Oil","Oil Sands", "Unconventional Oil","Other")
    CompProdSS <- subset(CompProdSS, Technology == "Oil")
    combin <- subset(combin, Technology == "Oil")
    market <- subset(market, Technology == "Oil")
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
  
    if (SectorToPlot == "Oil"){
      
    }
  
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
    Production$Type <- factor(Production$Type, levels=c("Portfolio","MetaPortfolio","Market"))
    
    chartorder <- c(PortfolioNameLong,GT["AveragePort"][[1]],GT["X2Target"][[1]])
    chartorder <- as.factor(chartorder)
    #Production$Type <- factor(Production$Type)

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
      
      # print(p1)
      
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
    
    xaxismin <- max(-2, min(dftar$value[dftar$Lab %in% c("Equity","Debt Market","Bond","Stock Market")]))
    
    dftargets$lower <-c(rep(-2,6),dfwide$Line1,dfwide$Line2,dfwide$Line1)
    outputplot <- ggplot(data = dftargets)+
      geom_ribbon(aes(ymin=lower, ymax=value, x=Year,fill=Target))+
      geom_line(aes(x=dftar[which(dftar$Lab=="Debt Market"),]$Year,y=dftar[which(dftar$Lab=="Debt Market"),]$value,colour =  "Debt Market"), data=subset(dftar,Lab=="Debt Market"), size = linesize,linetype=3)+   # Market
      geom_line(aes(x=dftar[which(dftar$Lab=="Stock Market"),]$Year,y=dftar[which(dftar$Lab=="Stock Market"),]$value,colour =  "Stock Market"), data=subset(dftar,Lab=="Stock Market"), size = linesize,linetype=5)+ 
      #scale_color_manual(name="",values = c("Debt Market"=peer_group,"Stock Market"=peer_group))+
    
      scale_fill_manual(labels=unique(dftargets$Labels),
                        values=rep(unique(as.character(dftargets$colour)),1))+
      
      #scale_color_manual(name="",values = c("Portfolio"=eq_port,"Market"=stock_market))+
      #scale_y_continuous(minor_breaks = seq(2018 ,2023 , 4), breaks = seq(-2, 2, 1))
      #labels=unique(dftargets$Labels)
      xlab("") +
      ylab("")+
      coord_cartesian(ylim=c(xaxismin,2))+
      theme_minimal()+
      theme(panel.grid.major = element_line(color="black",size=0.5),
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
      geom_line(aes(x=dftar[which(dftar$Lab=="Debt Market"),]$Year,y=dftar[which(dftar$Lab=="Debt Market"),]$value,colour =  "Debt Market"), data=subset(dftar,Lab=="Debt Market"), size = linesize,linetype=3)+   # Market
      geom_line(aes(x=dftar[which(dftar$Lab=="Stock Market"),]$Year,y=dftar[which(dftar$Lab=="Stock Market"),]$value,colour =  "Stock Market"), data=subset(dftar,Lab=="Stock Market"), size = linesize,linetype=5)+   # Market
      scale_color_manual(name="",values = c("Debt Market"=peer_group,"Stock Market"=peer_group))+
      scale_fill_manual(labels=(unique(dftargets$Labels)),
                                             values=(unique(as.character(dftargets$colour))))+
      
      #scale_color_manual(name="",values = c("Portfolio"=eq_port,"Market"=stock_market))+
      #scale_y_continuous(minor_breaks = seq(2018 ,2023 , 4), breaks = seq(-2, 2, 1))
      #labels=unique(dftargets$Labels)
      xlab("") +
      ylab("")+
      coord_cartesian(ylim=c(0,1))+
      theme_minimal()+
      theme(panel.grid.major = element_line(color="black",size=0.5),
            panel.grid.minor = element_blank(),
            axis.ticks=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.margin = unit(c(.5,1,0.5,.5), "cm"))
  }
  
  
  
  
  if((('Bond' %in% colnames(dfwide)) == TRUE)& (('Equity' %in% colnames(dfwide)) == FALSE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Bond"),]$Year,y=dftar[which(dftar$Lab=="Bond"),]$value,colour =  "Bond"), data=subset(dftar,Lab=="Bond"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Bond"=cb_line,"Debt Market"=peer_group,"Stock Market"=peer_group))+
      theme(legend.position="none")
  
  }else if ((('Bond' %in% colnames(dfwide)) == FALSE)& (('Equity' %in% colnames(dfwide)) == TRUE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Equity"),]$Year,y=dftar[which(dftar$Lab=="Equity"),]$value,colour = "Equity"), data=subset(dftar,Lab=="Equity"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Equity"=eq_line,"Debt Market"=peer_group,"Stock Market"=peer_group))+
      theme(legend.position="none")
  }else if ((('Bond' %in% colnames(dfwide)) == TRUE)& (('Equity' %in% colnames(dfwide)) == TRUE) ){
    outputplot <- outputplot+ 
      geom_line(aes(x=dftar[which(dftar$Lab=="Bond"),]$Year,y=dftar[which(dftar$Lab=="Bond"),]$value,colour =  "Bond"), data=subset(dftar,Lab=="Bond"), size = linesize,linetype=1)+   # Market
      
      geom_line(aes(x=dftar[which(dftar$Lab=="Equity"),]$Year,y=dftar[which(dftar$Lab=="Equity"),]$value,colour =  "Equity"), data=subset(dftar,Lab=="Equity"), size = linesize,linetype=1)+   # Market
      scale_color_manual(name="",values = c("Equity"=eq_line,"Bond"=cb_line,"Debt Market"=peer_group,"Stock Market"=peer_group))+
      theme(legend.position="none")
  }
  
  
  
  print(outputplot)
  
  
  ggsave(filename=paste0(plotnumber,"_",PortfolioName,"_",TechToPlot,'_246.png', sep=""),bg="transparent",height=3.6,width=4.6,plot=outputplot,dpi=ppi*2)
  
}                       
