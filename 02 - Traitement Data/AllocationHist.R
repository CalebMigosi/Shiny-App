##### Ajout catégorie ALM pour historique allocation: Niveau macro  #####
Data2 <- Data2 %>% mutate(CatALM2=CatALM1)

Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"TF"),"Obligations")
Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"TV"),"Obligations")
Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"OPC"),"OPCVMs")
Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"HF"),"Equities")
Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"PE"),"Equities")
Data2$CatALM2 <- replace(Data2$CatALM2,str_detect(Data2$CatALM2,"Action"),"Equities")

