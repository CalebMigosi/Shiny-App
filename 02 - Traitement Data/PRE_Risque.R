##### Ajout catégorie ALM risque de PRE  #####
## Entite PRE
Data2 <- Data2 %>% mutate(EntPRE=paste(Data2$EL,Data2$Per,sep=" "))
# Retrait des UCs,Eurocroissance, et N/A
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"UC"),"NO_PRE")
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"(?i)eurocroissance"),"NO_PRE")
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"N/A"),"NO_PRE")

# Association ID aux entites AvivaVie, AER, PERP, GI et ARP 
# PERP
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"(?i)PERP"),"PERP")
#AvivaVie
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"(?i)Aviva (?i)Vie"),"AvivaVie")
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"EPARGNE ACTUELLE SHF"),"AvivaVie")
# AER
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"(?i)AER"),"AER")
#GI
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"GI"),"GI")
#ARP
Data2$EntPRE <- replace(Data2$EntPRE,str_detect(Data2$EntPRE,"(?i)Aviva (?i)Retraite (?i)Professionelle"),"ARP")


## Categorie actif eligible PRE
Data2 <- Data2 %>% mutate(CatPRE=LC1)
Data2$CatPRE<- replace(Data2$CatPRE,str_detect(Data2$LC1,"R343-10"),1)
Data2$CatPRE<- replace(Data2$CatPRE,str_detect(Data2$LC1,"R332-20"),1)
Data2$CatPRE<- replace(Data2$CatPRE,is.na(Data2$LC1),1)  # immobilier            
Data2$CatPRE<- replace(Data2$CatPRE,str_detect(Data2$LC1,"R343-9"),0)
Data2$CatPRE<- replace(Data2$CatPRE,str_detect(Data2$LC1,"R332-19"),0)
Data2$CatPRE<-as.double(Data2$CatPRE)

## PMVL pour le calcul de la PRE (yc PDD) 
Data2$VB [is.na(Data2$VB)] <-0
Data2$VC [is.na(Data2$VC)] <-0
Data2$PDD [is.na(Data2$PDD)] <-0
Data2 <- Data2 %>% mutate(PMVL_PRE=VB-VC+PDD)

#  Choix des entites pour la feuille risque de pRE
PerPRE<-c("AvivaVie","AER","ARP","PERP","GI")
