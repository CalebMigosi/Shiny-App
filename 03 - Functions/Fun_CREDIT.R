############### Feuille Risque CREDIT ##########
AfficheCREDIT_UPC <- function(Perimeters_Alloc, DateP) {
  Perimeters_Alloc <- c("Afer conso", "Aviva Vie conso")
  DateP <- "2018-12-31"
CREDIT_UPC <- Data2 %>%
    filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
    group_by(UPC,CS, PAYS, Rat)%>%
    summarise(VB_UPC=round(sum(VB) /1000000,2))

CREDIT_UPC2 <- Data2 %>%
  filter(Per==Perimeters_Alloc) %>%
  filter(DR==DateP) %>%
  filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
  group_by(UPC,CS)%>%
  summarise_at(c("Dur", "PMVL", "PMVL_PRE"), mean, na.rm = T)


CREDIT_UPC <- CREDIT_UPC%>%
        merge(CREDIT_UPC2[, -2], by = "UPC", all = T)
  
  # Selection des 10 emetteurs les plus importants
  CREDIT_UPC <- ungroup(CREDIT_UPC) %>% 
                  group_by(CS) %>% 
                  arrange (CS,-VB_UPC) %>%
                  rename("Secteur"=CS, 
                         "Market Value (mil)"=VB_UPC, 
                         "Mean Duration" = Dur,
                         "PMVL PRE (mil)" = PMVL_PRE,
                         "PMVL (mil)" = PMVL,
                         "Country" = PAYS,
                         "Rating" = Rat)
  
  CREDIT_UPC[, 6:8] <- round(CREDIT_UPC[, 6:8], 2)
  CREDIT_UPC[, 7:8] <- CREDIT_UPC[, 7:8]/1000000
  
  #Convert CREDIT_UPC into a DT::datatable
  CREDIT_UPCFinal <- DT::datatable(CREDIT_UPC,options = list(
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}")))%>%
                  formatCurrency(c(3, 7, 8), '\U20AC', digits = 2)%>%
                  formatStyle(1:8, color = 'black')
                  
  
  
  
  return (CREDIT_UPCFinal)
}

AfficheCREDIT_PAYS <- function(Perimeters_Alloc, DateP, plot_type) {
  
  Tot <- Data2 %>% filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
    group_by(CS) %>%
    summarise (VB_PAYS =sum(VB)/1000000)
  
  CREDIT_PAYS <- Data2 %>%
    filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
    group_by(CS,ZONEGEO) %>%
    summarise(VB_PAYS=sum(VB) /1000000)
  CREDIT_PAYS <- CREDIT_PAYS %>% full_join (Tot,by='CS')
  CREDIT_PAYS <- CREDIT_PAYS %>% ungroup()
  colnames(CREDIT_PAYS) <- c("Secteur","Zone Geographique","VBPAYS","VBTOT")
  CREDIT_PAYS <- CREDIT_PAYS %>% mutate(Allocation=VBPAYS/VBTOT*100)
  CREDIT_PAYS <- CREDIT_PAYS %>% arrange(Secteur,-Allocation)
  CREDIT_PAYS$`Zone Geographique` <- factor(CREDIT_PAYS$`Zone Geographique`,
                                            levels=c("France/Belgium","Supranational",
                                                     "Core Europe","IPIGS","Eastern Europe",
                                                     "UK/canada/US","Australia/NZ","Others"))
  
  # CREDIT_PAYS$Secteur <- replace_na(CREDIT_PAYS$Secteur,"missing")
  CREDIT_PAYS <- CREDIT_PAYS %>% filter((Secteur=="FIN")|(Secteur=="NFIN")|(Secteur=="GOUV"))  
  
  #Change structure of the dataframe to plot grouped bargraphs
  CREDIT_PAYS <- dcast(CREDIT_PAYS, `Zone Geographique`~Secteur, value.var = "Allocation")
  
  if (plot_type == "Financial") {
    p <- plot_ly(data = CREDIT_PAYS, labels = ~`Zone Geographique`, values = ~FIN, type = "pie",
                 marker = list(color = colors_Aviva),
                 name = "Financial",
                 title = "Financial")%>%
      layout(paper_bgcolor = '#8dacc3',                  #Color of the background
             plot_bgcolor = '#8dacc3')
  }
  
  else if(plot_type == "Non-Financial"){
    p <- plot_ly(data = CREDIT_PAYS, labels = ~`Zone Geographique`, values = ~NFIN, type = "pie",
                 marker = list(color = colors_Aviva),
                 name = "Non-Financial",
                 title = "Non-Financial") %>%
      layout(paper_bgcolor = '#8dacc3',                  #Color of the background
             plot_bgcolor = '#8dacc3') }
  
  else if(plot_type == "Sovereign")
    {p <- plot_ly(data = CREDIT_PAYS, labels = ~`Zone Geographique`, values = ~GOUV, type = "pie",
                     marker = list(color = colors_Aviva),
                     name = "Sovereign",
                     title = "Sovereign") %>%
    layout(paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3')}
  else{
  p <- plot_ly(CREDIT_PAYS, x = ~`Zone Geographique`, y = ~FIN, type = 'bar', name = "Financial",
               marker = list(color = '#ffdf00',
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    add_trace(y = ~GOUV, name = "Sovereign", marker = list(color = "#4F9F31")) %>%
    add_trace(y = ~NFIN, name = "Non Financial", marker = list(color = "#001E60")) %>%
    layout(xaxis = list(title = "Allocation (%)",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = 'Geographical Zone',
                        color = '#ffffff'),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#ffffff'))}
  return (p)
}


AfficheCREDIT_RAT <- function(Perimeters_Alloc, DateP, plot_type) {
  
  Tot <- Data2 %>% filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
    group_by(CS) %>%
    summarise (VB_RAT =sum(VB)/1000000)
  
  CREDIT_RAT <- Data2 %>%
    filter(Per==Perimeters_Alloc) %>%
    filter(DR==DateP) %>%
    filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
    group_by(CS,Rat,RatNUM) %>%
    summarise(VB_RAT=sum(VB) /1000000)
  CREDIT_RAT <- CREDIT_RAT %>% full_join (Tot,by='CS')
  CREDIT_RAT <- CREDIT_RAT %>% ungroup()
  colnames(CREDIT_RAT) <- c("Secteur","Rating","RatingNum","VBPAYS","VBTOT")
  CREDIT_RAT<- CREDIT_RAT %>% mutate(Allocation=VBPAYS/VBTOT*100)
  CREDIT_RAT<- CREDIT_RAT %>% arrange(Secteur,RatingNum)
  CREDIT_RAT$Rating <- factor(CREDIT_RAT$Rating, levels=c("AAA","AA+","AA","AA-","A+","A","A-",
                                                          "BBB+","BBB","BBB-","BB+","BB","BB-",
                                                          "B+","B","B-","C")) 
  # On ne presente que les fin/Nfin / gouv
  CREDIT_RAT <- CREDIT_RAT %>% filter((Secteur=="FIN")|(Secteur=="NFIN")|(Secteur=="GOUV"))  
  CREDIT_RAT <- dcast(CREDIT_RAT, Rating~Secteur, value.var = "Allocation", fun.aggregate = sum, na.rm= T)
  
  if (plot_type == "Financial") {
  p <- plot_ly(data = CREDIT_RAT, labels = ~Rating, values = ~FIN, type = "pie",
            marker = list(colors = colors_Aviva),
            name = "Financial",
            title = "Financial")%>%
              layout(paper_bgcolor = '#8dacc3',                  #Color of the background
                     plot_bgcolor = '#8dacc3')
  }
  
  else if(plot_type == "Non-Financial"){
    p <- plot_ly(data = CREDIT_RAT, labels = ~Rating, values = ~NFIN, type = "pie",
                 marker = list(colors = colors_Aviva),
                 name = "Non-Financial",
                 title = "Non-Financial") %>%
      layout(paper_bgcolor = '#8dacc3',                  #Color of the background
             plot_bgcolor = '#8dacc3') }
  
  else {p <- plot_ly(data = CREDIT_RAT, labels = ~Rating, values = ~GOUV, type = "pie",
                         marker = list(colors = colors_Aviva),
                         name = "Sovereign",
                     title = "Sovereign") %>%
          layout(paper_bgcolor = '#8dacc3',                  #Color of the background
               plot_bgcolor = '#8dacc3')}

  return (p)
}
