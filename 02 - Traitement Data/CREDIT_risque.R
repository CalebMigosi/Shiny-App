##### Ajout categorie pour le risque de credit  #####
## Zone geographique'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Data2 <- Data2 %>% mutate('ZONEGEO'=1)
# France - Belgique
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)France"),"France/Belgium")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Belgique"),"France/Belgium")
# IPIGS - Irlande, Portugal, italie, Grece, Espagne
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Italie"),"IPIGS")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Irlande"),"IPIGS")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Grece"),"IPIGS")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Portugal"),"IPIGS")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Espagne"),"IPIGS")
# Core European countries (incl Norvege et suisse hors UE)
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Allemagne"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Autriche"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Suede"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Danemark"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Finlande"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Luxembourg"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Pays-Bas"),"Core Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Suisse"),"Core Europe")


# Eastern Europe
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Republique Tcheque"),"Eastern Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Estonie"),"Eastern Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Pologne"),"Eastern Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Slovaquie"),"Eastern Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Slovenie"),"Eastern Europe")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Estonie"),"Eastern Europe")

# UK/Canada/US
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Etats-Unis d'Amerique"),"UK/canada/US")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Royaume-Uni"),"UK/canada/US")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Canada"),"UK/canada/US")

# Australie / Nouvelle zelande
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Australie"),"Australia/NZ")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)New Zealand"),"Australia/NZ")

# Supranational
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)European Union"),"Supranational")
Data2$ZONEGEO <- replace(Data2$ZONEGEO,str_detect(Data2$PAYS,"(?i)Supranational"),"Supranational")

# Autres
Data2$ZONEGEO <- replace(Data2$ZONEGEO,Data2$ZONEGEO==1,"Others")

## Rating ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat," "),"")
# Rating AAA
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Aaa"),"AAA")
# Rating AA
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Aa1"),"AA+")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Aa2"),"AA")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Aa3"),"AA-")
# Rating A
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"A1"),"A+")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"A2"),"A")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"A3"),"A-")
# Rating BBB
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Baa1"),"BBB+")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Baa2"),"BBB")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Baa3"),"BBB-")
# Rating BB
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Ba1"),"BB+")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Ba2"),"BB")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Ba3"),"BB-")
# Rating B
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"B1"),"B+")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"B2"),"B")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"B3"),"B-")

# Rating C
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Caa1"),"C")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Caa2"),"C")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Caa3"),"C")
Data2$Rat<- replace(Data2$Rat,str_detect(Data2$Rat,"Ca"),"C")

# Ajout variable Rating Numerique
Data2 <- Data2 %>% mutate (RatNUM=9999)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="AAA",1)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="AA+",2)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="AA",3)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="AA-",4)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="A+",5)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="A",6)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="A-",7)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BBB+",8)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BBB",9)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BBB-",10)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BB+",11)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BB",12)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="BB-",13)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="B+",14)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="B",15)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="B-",16)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="C",17)
Data2$RatNUM<- replace(Data2$RatNUM,Data2$Rat=="NR",18)

