############################################################################################################
#'''''''''''''''''''''''''''''''''''''''''''''''''' Importation données '''''''''''''''''''''''''''''''''''#
############################################################################################################

############################ Importation BDA ##############################################################

#setwd("D:/Charles/Documents/Projet R/01 - Data Base")
path="./01 - Data/01 - BDA"
NameFiles <- dir(path)
NameFiles <- NameFiles[str_detect(NameFiles,"^BDA_Rapport_")]

### Importation premier fichier ###
file <-NameFiles[1] #Modified to 2
file<-paste(path,file,sep = "/")
suppressWarnings(Data1<-read_xlsx(file))

# Retrait Accents variables, type actif et pays/UPC tiers #
colnames(Data1)<-iconv(colnames(Data1),from="UTF-8", to="ASCII//TRANSLIT")
Data1$`Categorie Actifs Niveau 3`<-iconv(Data1$`Categorie Actifs Niveau 3`,from="UTF-8", to="ASCII//TRANSLIT")
Data1$`Libelle UPC Tiers`<-iconv(Data1$`Libelle UPC Tiers`,from="UTF-8", to="ASCII//TRANSLIT")
Data1$`Libelle Pays Tiers`<-iconv(Data1$`Libelle Pays Tiers`,from="UTF-8", to="ASCII//TRANSLIT")


# Ajout Variable Date reporting (id immo et autres)
Data1<-Data1 %>% mutate(DR = max(`Date d'arrete`, na.rm = TRUE))
Data1$DR <- as.Date(Data1$DR)

# Nom de colonne qui ont été modifié au cours du temps
names(Data1)[grepl ("PB recalcule",names(Data1))] <- "PB recalcule"

### Importation fichiers suivants ###
if (length(NameFiles)>1){
  for (i in 2:length(NameFiles)){
    file <-NameFiles[i]
    file<-paste(path,file,sep = "/")
    DataTemp<-suppressWarnings(read_xlsx(file))
    
    # Retrait Accents variables, type actif et pays/UPC tiers #
    colnames(DataTemp)<-iconv(colnames(DataTemp),from="UTF-8", to="ASCII//TRANSLIT")
    DataTemp$`Categorie Actifs Niveau 3`<-iconv(DataTemp$`Categorie Actifs Niveau 3`,from="UTF-8", to="ASCII//TRANSLIT")
    DataTemp$`Libelle UPC Tiers`<-iconv(DataTemp$`Libelle UPC Tiers`,from="UTF-8", to="ASCII//TRANSLIT")
    DataTemp$`Libelle Pays Tiers`<-iconv(DataTemp$`Libelle Pays Tiers`,from="UTF-8", to="ASCII//TRANSLIT")
    
    
    # Ajout Variable Date reporting (id immo et autres)
    DataTemp<-DataTemp %>% mutate(DR = max(`Date d'arrete`, na.rm = TRUE))
    DataTemp$DR <- as.Date(DataTemp$DR)
    
    # Nom de colonne qui ont été modifié au cours du temps
    names(DataTemp)[grepl ("PB recalcule",names(DataTemp))] <- "PB recalcule"
    
    
    Data1 <-bind_rows(Data1,DataTemp)
  }
}

### Fin Importation fichiers    ###


############################ Importation donnees real Estate ############################################

path="./01 - Data/03 - Immo"
fileRE <- paste( path,"Portefeuille Immo_Adresses _tot.xlsx",sep="/")
suppressWarnings(DataRE <- read_xlsx(fileRE))
DataRE <- DataRE %>% select("VB","Per","latitude","longitude","Type")
# colnames(DataRE) <- c("VB","Per","latitude","longitude","Type")
DataRE$latitude <- as.numeric(DataRE$latitude)
DataRE$longitude <- as.numeric(DataRE$longitude)
DataRE$VB <- as.numeric(DataRE$VB)
DataRE$Type <- as.factor(DataRE$Type)
DataRE$Per <- as.factor(DataRE$Per)

### Fin Importation donnees real estate ###


############################ Importation donnees marché ###################################################

# Importaton nom indices
path="./01 - Data/02 - Market"
Mkt_index <- read_xlsx(paste(path,"Mkt_index.xlsx",sep='/'))
N_indices <- nrow(Mkt_index)

# creation variables et importation Mkt_data
NameFiles <- dir(path)
NameMkt_data <- NameFiles[str_detect(NameFiles,"Mkt_data")]
if (length(NameMkt_data) != 0) {
  Mkt_data <- readRDS(paste(path,"Mkt_data.rds",sep='/'))
  date_rec <- as.Date(max(Mkt_data$Date))
} else {
  date_rec <- as.Date("2007-01-01")
  }

N_days <- difftime (Sys.Date() - 1, date_rec)

# Boucle recuperation si nombre de jour superieur à 0
if ( N_days > 0) {
  tryCatch ( {
    blpConnect()
    opt <- c("periodicitySelection" = "DAILY") # Can be WEEKLY 
    Mkt_data_temp <- clean_data(tickers = Mkt_index$Index, date_init = date_rec + 1, opt = opt)
    colnames(Mkt_data_temp)[2:ncol(Mkt_data_temp)] <- Mkt_index$Name[match(colnames(Mkt_data_temp)[2:ncol(Mkt_data_temp)],Mkt_index$Index,nomatch = 0)]
    
    if(length(NameMkt_data) != 0) {
      Mkt_data <- Mkt_data %>% bind_rows(Mkt_data_temp)
    } else {
      Mkt_data <- Mkt_data_temp
    }

    saveRDS(file="./01 - Data/02 - Market/Mkt_data.rds",Mkt_data)
    
    },
    error = function(c) {
      c$message <- "Pas de connection bloomberg"
      c$message
    }
  )
}
# Triggers
Mkt_data_xts <- xts(Mkt_data[,-1],order.by = Mkt_data[,1], frequency = "Daily")
Triggers <- read_xlsx(paste(path,"Triggers.xlsx",sep='/'))


### Fin  Importation donnees marche ###


############################ Importation loss functions ###################################################
path <- "./01 - Data/04 - SCR/02 - Loss Functions"
List_RF <- c("EConv","FL","CCorp","CSov","P","EqVol","FVol","Inf")

#Find the names of all elements in the file
names_DataLF <- list.files(paste(path, "RDS Files", sep = "/"))%>%
  str_remove("\\.rds$")

#Extract all the files in RDS Files folder
list <- list.files(paste(path, "RDS Files", sep = "/"))%>%
  lapply(function(i) readRDS(paste(path, paste("RDS Files", i,sep = "/"), sep = "/")))

#Rename your files
names(list) <- c(names_DataLF)

#Save to environment
list2env(list, envir = .GlobalEnv) 

### Fin  Importation donnees loss functions ###


############################ Importation Own Funds / Cover Ratio ##############################################

path="./01 - Data/04 - SCR/01 - OFs_CR"
file_OFs_CR <- paste( path,"Own_Funds_CR.xlsx",sep="/")
Data_OFs_CR <- read_xlsx(file_OFs_CR)
Data_OFs_CR$Date <-  as.Date(Data_OFs_CR$Date, "%Y-%m-%d")
Date_LF <- as.Date("2018-12-31", "%Y-%m-%d")

### Fin  Importation Own Funds / Cover Ratio  ###


############################ Importation Analyse EQUITY ##############################################

path="./01 - Data/05 - Equity"
file_EQ_CLUST <- paste( path,"EQ_CLUST.rds",sep="/")
Data_EQ_CLUST <- readRDS(file_EQ_CLUST)
file_EQ_CLUST_RESUM <- paste( path,"EQ_CLUST_RESUM.rds",sep="/")
Data_EQ_CLUST_RESUM <- readRDS(file_EQ_CLUST_RESUM)
file_EQ_CLUST_VAR <- paste( path,"EQ_CLUST_VAR.rds",sep="/")
Data_EQ_CLUST_VAR <- readRDS(file_EQ_CLUST_VAR)


### Fin  Importation Own Funds / Cover Ratio  ###
