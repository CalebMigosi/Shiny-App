##### Vecteur des perimetres  #####
Per<-c("AER SHF","Afer conso","Afer Conso Eurocroissance","Aviva Vie conso",
       "Aviva Vie PERP","GI","Victoire retraite","SHF","ARP conso","ARP Aviva retraite","ARP SHF")

##### Ajout categorie ALM pour allocation instant T : niveau micro  #####
Data2 <- Data2 %>% mutate(CatALM1=LC3)

# TF
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$CatALM1," TF"),"TF")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Obligation Autre R343-10","TF")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Fonds de couverture Obligataire","TF")

# TV
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$CatALM1," TV"),"TV")

# OPCVMs
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$CatALM1,"OPCVM"),"OPC Autres")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="OPCVM Taux Convertibles","OPC Conv")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="OPCVM Taux Emerging Markets","OPC EM")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="OPCVM Taux Credit Investment Grade","OPC IG")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="OPCVM Taux High Yield","OPC HY")

# Loans
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$CatALM1,"(?i)pret"),"Loans")

# Actions
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$LC3,"Action"),"Action")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Titres de Participation","Action")
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Fonds de couverture action","Action")

# Hedge Fund
Data2$CatALM1<- replace(Data2$CatALM1,Data2$CatALM1=="Hedge Fund","HF")

# PE
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Private Equity","PE")

# Structuré action
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="Produit Structure Action","Struct Action")

# Immobilier
Data2$CatALM1 <- replace(Data2$CatALM1,str_detect(Data2$CatALM1,"Immobilier"),"Immobilier")

# Cash
Data2$CatALM1 <- replace(Data2$CatALM1,Data2$LC3=="OPCVM Tresorerie Court","Cash")


