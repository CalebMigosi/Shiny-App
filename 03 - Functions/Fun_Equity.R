
sector_country_plot <- function(countries, camembertEquity){
  
  Data_EQ <- Data_EQ_CLUST[Data_EQ_CLUST$COUNTRY %in% countries,]%>%
            dcast(RATING~COUNTRY, value.var = "MARKET VALUE", fun.aggregate = sum, na.rm = F)
  
  names(Data_EQ) <- c("RATING", "C1", "C2")
  
  ifelse(camembertEquity == "pie",
  p <- plot_ly()%>%
      add_pie(data = Data_EQ, labels = ~RATING, values = ~C1, name = countries[1],
               domain = list(x = c(0, 0.45), y = c(1, 1)),
                marker = list(colors = colors_Aviva))%>%
      add_pie(data = Data_EQ, labels = ~RATING, values = ~C2, name = countries[2],
              domain = list(x = c(0.55, 1), y = c(1, 1)),
              marker = list(colors = colors_Aviva))%>%
      layout(paper_bgcolor = '#8dacc3',                  #Color of the background
               plot_bgcolor = '#8dacc3'),
  
  p <- plot_ly(Data_EQ, x = ~RATING, y = ~C1, type = 'bar', name = countries[1],
               marker = list(color = '#ffdf00',
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    add_trace(y = ~C2, name =countries[2], marker = list(color = "#4F9F31")) %>%
    layout(xaxis = list(title = "Country",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'Allocation (%)',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')))
  return(p)
  
}


country_plot<- function(camembertEquity){
  Data_EQ_CLUST2 <- Data_EQ_CLUST%>%
                    group_by(COUNTRY)%>%
                    summarise(Value = sum(`MARKET VALUE`))
  
  ifelse(camembertEquity == "pie",
  p <- plot_ly(Data_EQ_CLUST2, labels = ~COUNTRY, values = ~Value, type = "pie",
          marker = list(colors = colors_Aviva))%>%
    layout(paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3'),
  
  p <- plot_ly(Data_EQ_CLUST2, x = ~COUNTRY, y = ~Value, type = "bar",
               marker = list(color = colors_Aviva))%>%
    layout(xaxis = list(title = "Value",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'GICS Sector',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')))

return (p)
}

sector_plot<- function(camembertEquity){
  Data_EQ_CLUST2 <- Data_EQ_CLUST%>%
                      group_by(GICS_SECTOR_NAME)%>%
                      summarise(Value = sum(`MARKET VALUE`))
  
  ifelse(camembertEquity == "pie",
  p <- plot_ly(Data_EQ_CLUST2, labels = ~GICS_SECTOR_NAME, values = ~Value, type = "pie",
          marker = list(colors = colors_Aviva))%>%
    layout(paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')),
  
  
  p <- plot_ly(Data_EQ_CLUST2, x = ~GICS_SECTOR_NAME, y = ~Value, type = "bar",
               marker = list(color = colors_Aviva))%>%
    layout(xaxis = list(title = "Value",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'GICS Sector',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')))
  
  return(p)
}

