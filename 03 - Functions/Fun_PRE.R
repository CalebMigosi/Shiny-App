############### Feuille Risque PRE ##########
AffichePRECAT <- function (PREInput, DatesRep) {
  # Données PMVL
  PRECAT <- Data2 %>%
    filter(DR %in% as.Date(DatesRep, tryFormats = "%Y-%m-%d")) %>%
    filter(EntPRE == PREInput) %>%
    filter(CatPRE==1) %>%
    group_by(CatALM1,DR) %>%
    summarise(PMVL_PRE_DR=(sum(PMVL_PRE)/1000000))
  
  PRECAT$CatALM1 <- factor(PRECAT$CatALM1)
  PRECAT$DR <- factor(PRECAT$DR)
  
  #Change structure of the dataframe to plot grouped bargraphs
  PRECAT <- dcast(PRECAT, CatALM1~DR, value.var = "PMVL_PRE_DR")
  
  PRECAT <- cbind(PRECAT$CatALM1,
                     PRECAT[, DatesRep])
  
  colnames(PRECAT) <- c("CatALM", "Date1", "Date2")
  
  # Graphique des allocations (Might be better as a circle graph or horizontal)
  p <- plot_ly(PRECAT, x = ~CatALM, y = ~Date1, type = 'bar', name = DatesRep[1],
               marker = list(color = '#ffdf00',
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    add_trace(y = ~Date2, name = DatesRep[2], marker = list(color = "#4F9F31")) %>%
    layout(xaxis = list(title = "Asset Class",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'PRE',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')) 
  return(p)
}

AffichePRETOT <- function (PREInput, DatesRep) {
  # Données PMVL
  PRETOT <- Data2 %>%
    filter(DR %in% as.Date(DatesRep, tryFormats = "%Y-%m-%d")) %>%
    filter(EntPRE== PREInput) %>%
    filter(CatPRE==1) %>%
    group_by(DR) %>%
    summarise(PMVL_PRE_DR=(sum(PMVL_PRE)/1000000)) #%>% 
  #group_by(DR) %>% summarise(PMVL_PRE_DR=sum(PMVL_PRE))
  PRETOT$DR <- factor(PRETOT$DR)
  
  p <- plot_ly(PRETOT, x = ~PMVL_PRE_DR, y = ~DR, type = 'bar',  orientation = 'h',marker = list(color = '#4F9F31'))%>%
    layout(xaxis = list(color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'PVML',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff')) 
  return(p)
}
