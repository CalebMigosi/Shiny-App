############################################################################################################
#'''''''''''''''''''''''''''''''''''''''''''''''''' Données marché à facteur de risque'''''''''''''''''''''#
############################################################################################################

############################ Bloomberg à Facteur de risque #################################################

year_bef <- max(year(Mkt_data$Date))-1
Ref_RF <- Mkt_data %>% filter(year(Date) == year_bef  & month(Date) == "12") %>% top_n(2,Date)
Last_RF<- Mkt_data %>% top_n(1,Date)

#### Facteur de risque EConv ###
# Perf yield %
Perf_EConv <- (Last_RF$"Action Eurostoxx 50"[1] / Ref_RF$"Action Eurostoxx 50"[1] - 1)*100

#### Facteur de risque Taux ###
#' Taux Swap Euro 5Y'
Perf_TauxSWAP_5Y <- Last_RF$ 'Taux Swap Euro 5Y'[1] - Ref_RF$'Taux Swap Euro 5Y'[1]


#### Facteur de risque credit ###
Cred_pond <- Data2 %>% filter(Per== "AER SHF"|Per== "Afer conso"|Per== "Aviva Vie conso"|Per== "SHF"|Per== "Victoire retraite") %>%
  filter((CatALM1=="TF")|(CatALM1=="TV")) %>%
  filter(DR==max(Data2$DR)) %>%
  group_by(CS,Rat) %>%
  summarize(VB = sum(VB)) %>% ungroup()
Cred_pond <- Cred_pond[grep("FIN",Cred_pond$CS),]  

Cred_pond <- Cred_pond %>% mutate(Rat = gsub("AAA","A",Rat)) %>%
  mutate(Rat = gsub("AA","A",Rat)) %>%
  mutate(Rat = gsub("BBB","B",Rat)) %>%
  mutate(Rat = gsub("BB","B",Rat)) %>%
  mutate(Rat = gsub("CCC","B",Rat)) %>%
  mutate(Rat = gsub("CC","B",Rat)) %>%
  mutate(Rat = gsub("[:+:]","",Rat)) %>%
  mutate(Rat = gsub("-","",Rat)) %>% group_by(Rat,CS) %>% summarize (VB=sum(VB)) %>%
  ungroup() %>% filter(Rat=="A"|Rat=="B") %>%
  mutate(allocation = VB / sum(VB))

# Perf en delta spread
Perf_CCorp <- ((Last_RF$`Financial BBB 5 ans`[1] - Ref_RF$`Financial BBB 5 ans`[1] ) - Perf_TauxSWAP_5Y) * Cred_pond$allocation[3] +
  ((Last_RF$`Financial A 5 ans`[1] - Ref_RF$`Financial A 5 ans`[1] ) - Perf_TauxSWAP_5Y) * Cred_pond$allocation[1] +
  ((Last_RF$`Non Financial A 5 ans`[1] - Ref_RF$`Non Financial A 5 ans`[1] ) - Perf_TauxSWAP_5Y) * Cred_pond$allocation[2] +
  ((Last_RF$`Non Financial BBB 5 ans`[1] - Ref_RF$`Non Financial BBB 5 ans`[1] ) - Perf_TauxSWAP_5Y) * Cred_pond$allocation[4]

