dyCSScool <- function(dygraph){
  
  dygraph$x$css <- '
  .dygraph-legend {
  width: auto !important;
  min-width: 150px;
  color: white;
  background-color: #BABABA !important;
  padding-left:5px;
  border-color:#BABABA;
  border-style:solid;
  border-width:thin;
  transition:0s 4s;
  z-index: 80 !important;
  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  border-radius: 3px;
  }
  
  .dygraph-legend:hover{
  transform: translate(-110%);
  transition: 0s;
  }
  
  .dygraph-legend > span {
  color: white;
  padding-left:5px;
  padding-right:2px;
  margin-left:-5px;
  background-color: black !important;
  display: block;
  }
  
  .dygraph-legend > span:first-child {
  margin-top:2px;
  }
  
  .dygraph-legend > span > span{
  display: inline;
  }
  
  .highlight {
  border-left: 2px solid #BABABA;
  padding-left:3px !important;
  }
  '
  dygraph
}

Affiche_TRIGGER_Plot <- function (Trig, Freq) {
  Data_Trig <- Mkt_data_xts[,(colnames(Mkt_data_xts) == as.character(Trig))]
  
  # Triggers
  Triggers_Ind <- Triggers %>% 
              filter (Indicators == as.character(Trig))
  
  Triggers_Ind[1:nrow(Data_Trig),] <- Triggers_Ind[1,]
  
  Triggers_Ind<- xts(Triggers_Ind[,-1],order.by = Mkt_data$Date,frequency = "Daily")
  
  Data_Trig <- cbind(Data_Trig,Triggers_Ind)
  
  if (Freq == "WTW") {
    Freq = 5
    Triggers_n <- 2
  } else if(Freq == "MTM") {
    Freq = 20
    Triggers_n <- 3
  } else if(Freq == "QTQ") {
    Freq = 60
    Triggers_n <- 4
  } else if(Freq == "YTY") {
    Freq = 250
    Triggers_n <- 5
  }
  
  # Rdt
  if (grepl("Action",as.character(Trig))) {
    Yield <- cbind(TTR::ROC(Data_Trig[,1],n = Freq,"continuous") * 100,Data_Trig[,Triggers_n] * 100,Data_Trig[,Triggers_n+4] * 100)
  } else {
    Yield <- cbind(diff(Data_Trig[,1],lag = Freq,differences = 1),Data_Trig[,Triggers_n] / 100,Data_Trig[,Triggers_n+4] / 100)
  }
  
  dy <- dygraph(Yield) %>%
    dyOptions(stackedGraph = F,
              colors = list("yellow","orange" ,"red"),
              axisLabelColor = "white") %>%
    dyRangeSelector(height = 45, strokeColor = "")%>%
    dyCSScool()
  
  return(dy)
}

