AfficheHistAlloc <- function (PerHistIn,RepDatesIn) {
      Tot <- Data2 %>% 
              filter(DR %in% as.Date(RepDatesIn , tryFormats = "%Y-%m-%d")) %>%
              filter(Per %in% PerHistIn) %>% 
              group_by(DR) %>%
              summarise (TotalMV = sum(VB))
  
  AllocT <- Data2 %>%
              filter(Per %in% PerHistIn) %>%
              left_join(Tot,by = "DR") %>%
              group_by(CatALM2,DR) %>%
              mutate(Allocation = VB / TotalMV) %>%
              summarise(Allocation = sum(Allocation)*100)
  
  AllocT$CatALM2 <- factor(AllocT$CatALM2)
  AllocT$DR <- factor(AllocT$DR)
  
  # Frequence couleur plot (nombre de categories)
  Ncolor <- nrow(AllocT %>% group_by(CatALM2) %>% summarise(n()))
  
  #Change structure of the dataframe to plot grouped bargraphs
  AllocT <- dcast(AllocT, CatALM2~DR, value.var = "Allocation")
  
  AllocT <- cbind(AllocT$CatALM2,
                  AllocT[as.character(RepDatesIn)])
  
  colnames(AllocT) <- c("CatALM", "Date1", "Date2")
  
  p <- plot_ly(AllocT, x = ~CatALM, y = ~Date1, type = 'bar', name = RepDatesIn[1],
               marker = list(color = '#FFD900',
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    add_trace(y = ~Date2, name = RepDatesIn[2], marker = list(color = "#4F9F31")) %>%
    layout(xaxis = list(title = "Asset Class",
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
             color = '#ffffff'))
  
  return(p)
}

pieAfficheHistAlloc <- function(PerHistIn,RepDatesIn){
Totpie <- Data2 %>% 
  filter(DR %in% as.Date(RepDatesIn , tryFormats = "%Y-%m-%d")) %>%
  filter(Per %in% PerHistIn) %>% 
  group_by(DR) %>%
  summarise (TotalMV = sum(VB))

AllocTpie <- Data2 %>%
  filter(Per %in% PerHistIn) %>%
  left_join(Totpie,by = "DR") %>%
  group_by(CatALM2,DR) %>%
  mutate(Allocation = VB / TotalMV) %>%
  summarise(Allocation = sum(Allocation)*100)

AllocTpie$CatALM2 <- factor(AllocTpie$CatALM2)
AllocTpie$DR <- factor(AllocTpie$DR)

# Frequence couleur plot (nombre de categories)
NcolorP <- nrow(AllocTpie %>% group_by(CatALM2) %>% summarise(n()))

#Change structure of the dataframe to plot grouped bargraphs
AllocTpie <- dcast(AllocTpie, CatALM2~DR, value.var = "Allocation")

colnames(AllocTpie)
AllocTpie <- cbind(AllocTpie$CatALM2,
                AllocTpie[as.character(RepDatesIn)])

colnames(AllocTpie) <- c("CatALM", "Date1", "Date2")

p <- plot_ly() %>%
  add_pie(data = AllocTpie, labels = ~CatALM, values = ~Date1,
          marker = list(color = colors_Aviva),
          name = RepDatesIn[1], 
          domain = list(x = c(0, 0.45), y = c(0.45, 0.45))) %>%
  add_pie(data = AllocTpie, labels = ~CatALM, values = ~Date2,
          marker = list(color = colors_Aviva),
          name = RepDatesIn[2], 
          domain = list(x = c(0.55, 1), y = c(1, 1)))%>%
  layout(paper_bgcolor = '#8dacc3',                  #Color of the background
         plot_bgcolor = '#8dacc3') 

return(p)
}