############### Feuille Market ##########
Affiche_MARKET_Plot <- function (Mkt_Rela = F, sliderDays, indices) {
  # Caracteristiques axes
  x <- list(
    title = "Dates"
  )
  y <- list(
    title = "Indice"
  )
  # Si evolution en relatif
  if (Mkt_Rela==TRUE) {
    Mkt_data <- Mkt_data[(nrow(Mkt_data)-sliderDays):nrow(Mkt_data),]
    Mkt_data[,2:ncol(Mkt_data)] <- apply(Mkt_data[,2:ncol(Mkt_data)],2,function(x) x[1:nrow(Mkt_data)]/x[1])
  }
  
  # Graphique de l'evolution (ploty)
  N_indices <- length(indices)
  
  ifelse(N_indices==0,
         p <- plot_ly(Mkt_data,x=~Date,y=~get("Action CAC 40"), type = 'scatter', mode = 'lines',name = indices[1]) %>%
           layout(xaxis = x, 
                  yaxis = y,
                  title = "Indice marche",
                  showlegend = TRUE),
         ifelse(N_indices==1,
                p <- plot_ly(Mkt_data,x=~Date,y=~get(indices[1]), type = 'scatter', mode = 'lines',name = indices[1]) %>%
                  layout(xaxis = x, 
                         yaxis = y,
                         title = "Indice marche",
                         showlegend = TRUE),
                ifelse(N_indices==2,
                       p <- plot_ly(Mkt_data,x=~Date,y=~get(indices[1]), type = 'scatter', mode = 'lines',name = indices[1]) %>%
                         layout(xaxis = x, 
                                yaxis = y,
                                title = "Indice marche",
                                showlegend = TRUE) %>%
                         add_trace(y = ~get(indices[2]), type = 'scatter', mode = 'lines', name = indices[2]),
                       ifelse(N_indices==3,
                              p <- plot_ly(Mkt_data,x=~Date,y=~get(indices[1]), type = 'scatter', mode = 'lines',name = indices[1]) %>%
                                layout(xaxis = x, 
                                       yaxis = y,
                                       title = "Indice marche",
                                       showlegend = TRUE) %>%
                                add_trace(y = ~get(indices[2]), type = 'scatter', mode = 'lines', name = indices[2])%>%
                                add_trace(y = ~get(indices[3]), type = 'scatter', mode = 'lines', name = indices[3]),
                              ifelse(N_indices==4,
                                     p <- plot_ly(Mkt_data,x=~Date,y=~get(indices[1]), type = 'scatter', mode = 'lines',name = indices[1]) %>%
                                       layout(xaxis = x, 
                                              yaxis = y,
                                              title = "Indice marche",
                                              showlegend = TRUE) %>%
                                       add_trace(y = ~get(indices[2]), type = 'scatter', mode = 'lines', name = indices[2]) %>%
                                       add_trace(y = ~get(indices[3]), type = 'scatter', mode = 'lines', name = indices[3]) %>%
                                       add_trace(y = ~get(indices[4]), type = 'scatter', mode = 'lines', name = indices[4]),
                                     ifelse(N_indices>=5,
                                            p <- plot_ly(Mkt_data,x=~Date,y=~get(indices[1]), type = 'scatter', mode = 'lines',name = indices[1]) %>%
                                              layout(xaxis = x, 
                                                     yaxis = y,
                                                     title = "Indice marche",
                                                     showlegend = TRUE) %>%
                                              add_trace(y = ~get(indices[2]), type = 'scatter', mode = 'lines', name = indices[2]) %>%
                                              add_trace(y = ~get(indices[2]), type = 'scatter', mode = 'lines', name = indices[2]) %>%
                                              add_trace(y = ~get(indices[3]), type = 'scatter', mode = 'lines', name = indices[3]) %>%
                                              add_trace(y = ~get(indices[4]), type = 'scatter', mode = 'lines', name = indices[4]) %>%
                                              add_trace(y = ~get(indices[5]), type = 'scatter', mode = 'lines', name = indices[5]),
                                            ""
                                     ))))))
  return(p)
}


Calc_Var <- function (Data,n) {
  Calc_Var <- Data[nrow(Data),]
  if (n > 0) {
    VAbs <- sort(c(grep("Taux",colnames(Data)),
                   grep("Volatilite",colnames(Data)),
                   grep("Spread",colnames(Data)),
                   grep("Indice",colnames(Data))))
    VRel <- sort(c(grep("Action",colnames(Data))))
    
    Calc_Var [,VAbs] <- (Data[nrow(Data),VAbs] - Data[nrow(Data)-n,VAbs])
    Calc_Var [,VRel] <-  (Data[nrow(Data),VRel] - Data[nrow(Data)-n,VRel]) / Data[nrow(Data)-n,VRel] * 100
    
  } 
  return(Calc_Var)
}


Affiche_MARKET_Table <- function (num) {
  Mkt_data_table <- Mkt_data %>% 
                      select(one_of(Mkt_index$Name[Mkt_index$FERP==1]))
  
  MARKET_Table <- rbind(Mkt_data_table[nrow(Mkt_data_table),],
                        round(Calc_Var(Mkt_data_table,num), 2),
                        round(Calc_Var(Mkt_data_table,5),2),
                        round(Calc_Var(Mkt_data_table,10),2))
  
  MARKET_Table <-  data.frame(t(MARKET_Table))
  MARKET_Table <- sapply(1:ncol(MARKET_Table),
                         function(i) {MARKET_Table[,i] <<- as.numeric(as.character(MARKET_Table[,i]))})
  
  rownames(MARKET_Table) <- Mkt_index$Name[Mkt_index$FERP==1]
  
  MARKET_Table <- data.frame(names = row.names(MARKET_Table), MARKET_Table)
  rownames(MARKET_Table) <- NULL
  
  colnames(MARKET_Table) <- c("Index", 
                              "Current Lvl",
                              paste(num, "Days", sep = " "),
                              "vs 5 Days",
                              "vs 10 Days")
  rm(Mkt_data_table)
  MARKET_Table_Perc <- list(MARKET_Table[c(1, 2, 3, 14, 15, 16, 17),],
                            MARKET_Table[-c(1, 2, 3, 14, 15, 16, 17),])
  
  rownames(MARKET_Table_Perc[[1]]) <- NULL
  rownames(MARKET_Table_Perc[[2]]) <- NULL
  MARKET_Table_Perc[[1]] <- DT::datatable(MARKET_Table_Perc[[1]],options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}")))%>%
                      formatStyle(1:ncol(MARKET_Table), color = 'black')%>%
                      formatStyle(3, backgroundColor = '#FFA000')
  
  MARKET_Table_Perc[[2]] <- DT::datatable(MARKET_Table_Perc[[2]],options = list(
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")))%>%
    formatStyle(1:ncol(MARKET_Table), color = 'black')%>%
    formatStyle(3, backgroundColor = '#FFA000')
  
  return(MARKET_Table_Perc)
}

# 
# Affiche_TRIGGER_Plot <- function () {
#   Data_Trig <- Mkt_data_xts[,(colnames(Mkt_data_xts) == as.character(input$Trig))]
#   # Triggers
#   Triggers_Ind <- Triggers %>% filter (Indicators == as.character(input$Trig))
#   Triggers_Ind[1:nrow(Data_Trig),] <- Triggers_Ind[1,]
#   Triggers_Ind<- xts(Triggers_Ind[,-1],order.by = Mkt_data$Date,frequency = "Daily")
#   Data_Trig <- cbind(Data_Trig,Triggers_Ind)
#   if (input$Freq == "WTW") {
#     Freq = 5
#     Triggers_n <- 2
#   } else if(input$Freq == "MTM") {
#     Freq = 20
#     Triggers_n <- 3
#   } else if(input$Freq == "QTQ") {
#     Freq = 60
#     Triggers_n <- 4
#   } else if(input$Freq == "YTY") {
#     Freq = 250
#     Triggers_n <- 5
#   }
#   # Rdt
#   if (grepl("Action",as.character(input$Trig))) {
#     Yield <- cbind(TTR::ROC(Data_Trig[,1],n = Freq,"continuous") * 100,Data_Trig[,Triggers_n] * 100,Data_Trig[,Triggers_n+4] * 100)
#   } else {
#     Yield <- cbind(diff(Data_Trig[,1],lag = Freq,differences = 1),Data_Trig[,Triggers_n] / 100,Data_Trig[,Triggers_n+4] / 100)
#   }
#   
#   dy <- dygraph(Yield) %>%
#     dyOptions(stackedGraph = FALSE) %>%
#     dyRangeSelector(height = 20)
#   
#   return(dy)
# }

