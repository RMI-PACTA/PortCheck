@@ -2431,7 +2431,7 @@ carboninout <- function(plotnumber, companiestoprint, ChartType){
  grid.draw(gt)
}

h <- max(3, length(unique(OilCompanies$Name)))
h <- max(3, length(unique(portfolio1$Name))*.5)

ggsave(gt,filename=paste0(plotnumber,"_",PortfolioName,"_",ChartType,'_CarboninnoutShare.png', sep=""),
       bg="transparent",height=h,width=10,dpi=ppi)
@ -2625,7 +2625,7 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
}else {
  PortNames<-PortName
} 
### Add in Car Data
### Add in Car Data (NPS & CPS Scenarios) 
if (TechToPlot %in% c("Electric","ICE")){
  ALD.temp <- ALD.sc %>% 
    select(-Production) %>%
    @ -2669,23 +2669,53 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
      ### Normalisation of market to Portfolio
      ALD.cp <- ALD2 %>% filter(Line.Type=="CurrentPlan")
      
      var <- ifelse(ALD.cp[which(ALD.cp$PortName==PortNames & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production ==0,0,
                    ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production/ ALD.cp[which(ALD.cp$PortName==PortNames & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production)
      StartPort <- FALSE
      if (PortName %in% ALD.cp$PortName){StartPort <- ALD.cp[which(ALD.cp$PortName==PortName & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production}
      
      #ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ifelse(var ==0,0,ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production/var)
      if (var ==0){
        ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<-0
      }else{
        ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production/var}
      
      if (TechToPlot %in% c("Electric","ICE")){
        var1<- ALD.cp[which(ALD.cp$PortName==PortName & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production/ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production
        ALD.cp[which(ALD.cp$PortName==PortName &  ALD.cp$Technology ==TechToPlot),]$Production <- ALD.cp[which(ALD.cp$PortName==PortName &ALD.cp$Technology ==TechToPlot),]$Production/var1
        StartMarket <- ALD.cp[which(ALD.cp$InvestorName == "Market" & ALD.cp$Year=="2018"  & ALD.cp$Technology ==TechToPlot),]$Production
        
        var <- ifelse(StartPort == FALSE, FALSE, StartPort)
        
        # Var = 0 means no Portfolio production in this tech - just show market - everything is already normalised to the Market
        # Var != 0 Portfolio Production. Scale everything to the portfolio start point. (*Port/(Market or Scenario) Production)
        
        ### Normalise the Market 
        # if (var == FALSE){
        #   ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<-0
        # }else{
        #   ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production<- ALD.cp[which(ALD.cp$InvestorName=="Market" & ALD.cp$Technology ==TechToPlot),]$Production*(StartPort/StartMarket)
        #   }
        
        
        ### Normalisation of NPS & CPS Scenarios to Portfolio for Electric and ICE
        ALD.sc <- ALD2 %>% filter(Line.Type=="Scenario") 
        
        if(TechToPlot %in% c("Electric","ICE")){
          StartScen <- ALD.sc[which(ALD.sc$Scenario == "CPS" & ALD.sc$Year=="2018"  & ALD.sc$Technology ==TechToPlot),]$Production
          
          # Additional Scenarios
          ALD.temp3 <- ALD.sc[ALD.sc$Scenario %in% c("NPS","CPS"),]
          
          # if Var == 0 then don't scale, just use the Meta Port results
          if (var != 0){ALD.temp3$Production <- ALD.temp3$Production*(StartPort/StartScen) }
          ALD.temp3$PortName <- PortName    
          ALD.sc  <- bind_rows(ALD.sc,ALD.temp3)
        }
        ALD.sc <- ALD2 %>% filter(Line.Type=="Scenario")
        
        
        ALD2 <- bind_rows(ALD.cp, ALD.sc)
        
        if (StartPort == TRUE){  
          if (PortName != "MetaPort"){
            ALD2 <- subset(ALD2, ALD2$PortName != "MetaPort")
            ALD2 <- ALD2[-which(ALD2$InvestorName == "Market" & ALD2$Line.Type == "Scenario"),]
          }}else{
            ALD2 <- ALD2[which(ALD2$InvestorName == "Market" | ALD2$PortName == "MetaPort"),]
            ALD2 <- ALD2[-which(ALD2$InvestorName == "Market" & ALD2$Line.Type == "Scenario"),]
            
          }
        
        ylims <- ALD2 %>%
          filter( InvestorName != "Market" | Line.Type != "Scenario") %>%
          summarise(min=min(Production), max=max(Production))
        @ -2774,9 +2804,6 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
          ymin<- min(ylims$min)
          ymax<- max(ylims$max)
          
          
          
          
          if ((ymax-ymin)<1){
            unit <- .1
          }else if ((ymax-ymin)>1 & (ymax-ymin)<10){
            @ -2794,16 +2821,12 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
              MAX.Y <- ceiling(ymax/unit)*unit
              MIN.Y <- floor(ymin/unit)*unit
              
              
              
              ALD.sc.wide$Line4 <- MAX.Y
              
              ALD.sc.tall <- ALD.sc.wide %>%
                select(-`450S`, -NPS, -CPS) %>%
                gather(key="Target", value="Value",-InvestorName, -PortName, -Sector,-Technology,-Green, -Line.Type,-Year)   #-Tech.Type
              
              
              
              ALD.sc.tall <- ALD.sc.tall %>%
                group_by(InvestorName, PortName, Sector, Technology, Line.Type, Green, Year) %>%   #Tech.Type
                mutate(lower=lag(Value),
                       @ -2839,7 +2862,7 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
                         # MIN.Y <- ceiling(ymax/10)*10
                         # MAX.Y <- floor(ymin/10)*10
                         
                         outputplot <- ggplot(data = subset(ALD.sc.tall, Technology == TechToPlot & ALD.sc.tall$PortName == PortNames )) +
                           outputplot <- ggplot(data = subset(ALD.sc.tall, Technology == TechToPlot & ALD.sc.tall$PortName == PortName )) +
                             geom_ribbon(aes(ymin=lower, ymax=Value, x=Year,fill=Target),alpha=0.75) +
                             scale_fill_manual(labels=eval(parse(text = paste(GoodBad,".labels",sep = ""))), values=eval(parse(text = paste(GoodBad,".fill",sep = "")))) +
                             scale_x_continuous(name="Year", expand=c(0,0),limits=c(2018, 2023.6)) +
                             @ -2847,8 +2870,6 @@ Graph246_new <- function(plotnumber,ChartType,TechToPlot){
                               expand=c(0,0),
                               breaks=seq(MIN.Y,MAX.Y,length.out = 5)) +
                theme_246() + theme(legend.position = "none") +
                #labs(title=paste0("Growth of ", "names[x]", " Allocated to Portfolio, 2018-2023"),
                #     subtitle = "Trajectory of Portfolio's Current Plans compared to IEA 2Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???, 4Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???, 6Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â???Ã???Â??? Degree Scenarios") +
                coord_cartesian(ylim=c(MIN.Y, MAX.Y))
              
              if (ChartType =="CB"){