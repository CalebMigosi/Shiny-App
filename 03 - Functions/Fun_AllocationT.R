AfficheAlloc <- function(Perimeters_Alloc,DateP, plot_type){
#   
# Perimeters_Alloc <- c("Afer conso", "Aviva Vie conso", "SHF")
# DateP = "2018-12-31"

Tot <- Data2 %>% 
  filter(Per %in% Perimeters_Alloc) %>% 
  filter(DR==DateP) %>% 
  summarise (Tot =sum(VB))

Tot<-data.frame(Tot)

AllocT <- Data2 %>%
  filter(Per %in% Perimeters_Alloc) %>%
  filter(DR == DateP) %>%
  mutate(Alloc = VB / Tot[1,1]) %>%
  group_by(CatALM1) %>%
  summarise(Allocation = sum(Alloc)*100)

AllocT$CatALM1 <- factor(AllocT$CatALM1,levels =c("TF","TV","Loans","OPC HY","OPC EM","OPC IG",
                                                  "OPC Conv","OPC Autres","Action","PE","HF","Struct Action","Immobilier",
                                                  "Cash"))
# Frequence couleur plot (max nombre catÃ©gori)
Ncolor <- nrow(AllocT)

ifelse(plot_type == "pie", #If pie is selected then switch to pie plot

p <- plot_ly(AllocT, 
            labels = ~CatALM1, 
            values = ~Allocation, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            marker = list(colors = colors_Aviva),
            text = ~paste(CatALM1, paste(round(Allocation, 2), "%", sep = ""), sep = " - "))%>%
 layout(paper_bgcolor = '#8dacc3',                  #Color of the background
        plot_bgcolor = '#8dacc3')             

       ,
#Graphique des allocations
p <- plot_ly(AllocT,
        x = ~CatALM1,
        y = ~Allocation,
        type = "bar",
        marker = list(color = colors_Aviva,
                      line = list(width = 1,
                                  color = '#00000')))%>%       # We define the color of the bars
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
          color = '#ffffff')))



# p <- ggplot(AllocT, aes(x = CatALM1,y = Allocation))+
#           geom_bar(stat="identity")
  
return(p)
}


### Donnees Credit ###
AfficheCredit <- function (Perimeters_Alloc,DateP, plot_type) {
# 
  # Perimeters_Alloc <- c("Afer conso", "Aviva Vie conso", "SHF")
  # DateP = "2018-12-31"
  
  # Données Credit 
  TotC <- Data2 %>% filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter(CatALM1=="TF"|CatALM1=="TV") %>%
    summarise (TotC =sum(VB))
  TotC<-data.frame(TotC)
  
  AllocC <- Data2 %>%
    filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter(CatALM1=="TF"|CatALM1=="TV") %>%
    mutate(AllocC= VB / TotC[1,1]) %>%
    group_by(CS) %>%
    summarise(Allocation=sum(AllocC) *100)
  # Frequence couleur plot (max nombre catégori)
  Ncolor <- nrow(AllocC)
  
  AllocC <- AllocC %>% arrange(CS,-Allocation)
  AllocC$CS <- factor(AllocC$CS,levels=c("GOUV","FIN","NFIN","TIT","NA")) 
  
  # Graphique credit
ifelse(plot_type == "pie",  
  
#We plot a pie chart if the condition is met
  p <- plot_ly(AllocC, 
          labels = ~CS, 
          values = ~Allocation, 
           type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',
          marker = list(colors = colors_Aviva),
          text = ~paste(CS, paste(round(Allocation, 2), "%", sep = ""), sep = " - "))%>%
    layout(paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3'),


#We draw a bargraph otherwise
p <- plot_ly(AllocC,
             x = ~CS,
             y = ~Allocation,
             type = "bar",
             marker = list(color = colors_Aviva,
                           line = list(width = 1,
                                       color = '#00000')))%>%       # We define the color of the bars
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

)   

  return(p)
}

