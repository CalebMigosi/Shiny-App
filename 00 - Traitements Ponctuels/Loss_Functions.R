##############################################################################################################
#''''''''''''''''''''''''''' Importation  et traitement Coefficients loss function '''''''''''''''''''''''''''
##############################################################################################################
library(tidyverse)
require(dplyr, stringr, tidyverse, matrixStats)

# Liste des facteurs de risque mis à jour
List_RF <- c("EConv","FL","CCorp","CSov","P","EqVol","FVol","Inf")

# Chemin d'exportation
Path <- "./01 - Data/04 - SCR/02 - Loss Functions"

################################ Singlewise Loss Functions  ##################################################

location <- paste(Path,"Singlewise.csv",sep="/")

Ind_Equations <- read.csv2(location)

# Remove empty rows
Ind_Equations <- Ind_Equations[rowSums(is.na(Ind_Equations)) < 265, ]

# Aggregation information
Ind_Equations$Entity <- paste(Ind_Equations[, 1],Ind_Equations[, 2],
                              Ind_Equations[, 3] , Ind_Equations[, 4])%>%
  sapply(function(x) gsub("MPan","Mpan",as.character(x)))

# Identify each function
all_names <- as.logical(rowSums(sapply(List_RF, grepl, Ind_Equations$Entity)))
Ind_Equations <- Ind_Equations[all_names,-c(2:5)]%>%
  filter(!grepl("Liability Tax Relief", Entity))%>%
  arrange(Entity)

# Singlewise Inputs_RF 
Inputs_RF_Sing <- cbind(Ind_Equations$Entity, 0) %>% data.frame()
names(Inputs_RF_Sing) <- c("names", "inputs")
Inputs_RF_Sing$inputs <- as.numeric(as.character(Inputs_RF_Sing$inputs)) 
Inputs_RF_Sing$names <-  as.character(levels(Inputs_RF_Sing$names))


#Extract the boundaries 
boundaries_Sing <- Ind_Equations[, grepl("Piecewise", names(Ind_Equations))]
boundaries_Sing[, 1] <- -Inf
boundaries_Sing[is.na(boundaries_Sing)] <- Inf

#Extract coefficients and their corresponding powers
coefs_Sing <-Ind_Equations[, grepl("Final", names(Ind_Equations))]
coefs_Sing[is.na(coefs_Sing)] <- 0

powers_Sing <- Ind_Equations[, grepl("Power", names(Ind_Equations))]
powers_Sing[is.na(powers_Sing)] <- 1

# Exportation loss function RdS
saveRDS(Inputs_RF_Sing,paste(Path,"Inputs_RF_Sing.rds",sep="/"))
saveRDS(boundaries_Sing,paste(Path,"boundaries_Sing.rds",sep="/"))
saveRDS(coefs_Sing,paste(Path,"coefs_Sing.rds",sep="/"))
saveRDS(powers_Sing,paste(Path,"powers_Sing.rds",sep="/"))
saveRDS(Ind_Equations,paste(Path,"Ind_Equations.rds",sep="/"))




################################ Pairwise Loss Functions  ####################################################

location <- paste(Path,"Pairwise.csv",sep="/")

#Find all possible combinations of the names and their corresponding inputs
combinations <- matrix(0, nrow = length(List_RF)^2, ncol = 3)%>%
  data.frame()
k = 0
for (i in 1:length(List_RF)){
  for (j in 1:length(List_RF)){
    combinations[k+1, 1] <- paste(List_RF[i],List_RF[j]) #Combination of names 
    # combinations[k+1, 2] <- inputs[i]                #X input
    # combinations[k+1, 3] <- inputs[j]                #Y input
    k = k+1
  }
} 

#Name our columns
names(combinations) <- c("names","X","Y") 

Pairwise <- read.csv2(location)

#Remove empty rows
Pairwise <- Pairwise[rowSums(is.na(Pairwise)) < 160, ]

#Unique ID for each function
Pairwise$Entity <- paste(Pairwise[, 1],Pairwise[, 2],
                         Pairwise[, 3] , Pairwise[, 4], Pairwise[, 5])


#Identifying the pairwise functions we want
#Use function ID to identify all the functions we want
all_names <- sapply(combinations$names, grep, Pairwise$Entity)

#Remove all 0 length lists
all_names <- all_names[sapply(all_names, length) != 0]

#filter by all_indices  
Pairwise <- Pairwise[unlist(all_names),-c(2,3,6)]%>%
  filter(!grepl("Liability Tax Relief", Entity))%>%
  arrange(Entity)

all_names <- sapply(combinations$names, grep, Pairwise$Entity) 
all_names <- all_names[sapply(all_names, length) != 0]

#Find all non NA combinations
non_NA_index <- match(names(all_names), combinations$names)
non_NA_index <- cbind(non_NA_index,combinations[non_NA_index,])

#Declare matrix of our final inputs
Inputs_RF_Pair <- matrix(0, nrow = sum(sapply(all_names, length)), ncol = 3)%>%
  data.frame()

Inputs_RF_Pair[, 1] <- Pairwise$Entity

for (n in 1:nrow(non_NA_index)){
  Inputs_RF_Pair[grepl(non_NA_index[n, 2], Inputs_RF_Pair[, 1]), -1] <- non_NA_index[n,c(3, 4)]
}

#Extract the boundaries 
boundaries_Pair <- Pairwise[, grepl("Piecewise", names(Pairwise))]
right <- boundaries_Pair[,grepl("R", names(boundaries_Pair))]
right[is.na(right)] <- Inf
boundaries_Pair[,grepl("R", names(boundaries_Pair))] <- right
boundaries_Pair[is.na(boundaries_Pair)] <- -Inf

#Clean up the boundaries
for (R in 1:nrow(boundaries_Pair)){
  for (C in seq(1, ncol(boundaries_Pair), 2)){
    if ((boundaries_Pair[R, C] == -Inf) && (boundaries_Pair[R, C+1] == Inf)){
      boundaries_Pair[R, C] = Inf
    }
  }
}

#Extract coefficients and their corresponding powers
coefs_Pair <-Pairwise[, grepl("Final", names(Pairwise))]
coefs_Pair[is.na(coefs_Pair)] <- 0

powers_Pair <- Pairwise[, grepl("Power", names(Pairwise))]
powers_Pair[is.na(powers_Pair)] <- 1
powers_x_Pair <- powers_Pair[grepl("x", names(powers_Pair))]
powers_y_Pair <- powers_Pair[grepl("y", names(powers_Pair))]

# Exportation loss function RdS
saveRDS(Inputs_RF_Pair,paste(Path,"Inputs_RF_Pair.rds",sep="/"))
saveRDS(boundaries_Pair,paste(Path,"boundaries_Pair.rds",sep="/"))
saveRDS(coefs_Pair,paste(Path,"coefs_Pair.rds",sep="/"))
saveRDS(powers_Pair,paste(Path,"powers_Pair.rds",sep="/"))
saveRDS(powers_x_Pair,paste(Path,"powers_x_Pair.rds",sep="/"))
saveRDS(powers_y_Pair,paste(Path,"powers_y_Pair.rds",sep="/"))
saveRDS(Pairwise,paste(Path,"Pairwise.rds",sep="/"))





################################ Threewise Loss Functions  ################################################

location <- paste(Path,"Threewise.csv",sep="/")

#Find all possible combinations of the names and their corresponding inputs
combinations <- matrix(0, nrow = length(List_RF)^3, ncol = 4)%>%
  data.frame()
m = 0
for (i in 1:length(List_RF)){
  for (j in 1:length(List_RF)){
    for(k in 1:length(List_RF)){
      combinations[m+1, 1] <- paste(List_RF[i],List_RF[j], List_RF[k]) #Combination of names 
      # combinations[m+1, 2] <- inputs[i]                #X input
      # combinations[m+1, 3] <- inputs[j]               #Y input
      # combinations[m+1, 4] <- inputs[k]               #Z input
      m = m+1
    }
  }
} 

#Name our columns
names(combinations) <- c("names","X","Y", "Z") 

#Extract File
Threewise <- read.csv2(location)

#Remove empty rows
Threewise <- Threewise[rowSums(is.na(Threewise)) < 100, ]

#Unique ID for each function
Threewise$Entity <- paste(Threewise[, 1],Threewise[, 3],
                          Threewise[, 4] , Threewise[, 5], Threewise[, 6])

#Identifying the Threewise functions we want
#Use function ID to identify all the functions we want
all_names <- sapply(combinations$names, grep, Threewise$Entity)

#Remove all 0 length lists
all_names <- all_names[sapply(all_names, length) != 0]

if (length(all_names) == 0){return(0)
}else{ 
  #filter by all_indices  
  Threewise <- Threewise[unlist(all_names),-c(2,3,7)]%>%
    filter(!grepl("Liability Tax Relief", Entity))%>%
    arrange(Entity)
  
  all_names <- sapply(combinations$names, grep, Threewise$Entity) 
  all_names <- all_names[sapply(all_names, length) != 0]
  
  #Find all non NA combinations
  non_NA_index <- match(names(all_names), combinations$names)
  non_NA_index <- cbind(non_NA_index,combinations[non_NA_index,])
  
  #Declare matrix of our final inputs
  Inputs_RF_Three <- matrix(0, nrow = sum(sapply(all_names, length)), ncol = 4)%>%
    data.frame()
  
  Inputs_RF_Three[, 1] <- Threewise$Entity
  
  for (n in 1:nrow(non_NA_index)){
    Inputs_RF_Three[grepl(non_NA_index[n, 2], Inputs_RF_Three[, 1]), -1] <- non_NA_index[n,c(3:5)]
  }
  
  #Extract the boundaries 
  boundaries_Three <- Threewise[, grepl("Piecewise", names(Threewise))]
  right <- boundaries_Three[,grepl("R", names(boundaries_Three))]
  right[is.na(right)] <- Inf
  boundaries_Three[,grepl("R", names(boundaries_Three))] <- right
  boundaries_Three[is.na(boundaries_Three)] <- -Inf
  
  for (R in 1:nrow(boundaries_Three)){
    for (C in seq(1, ncol(boundaries_Three), 2)){
      if ((boundaries_Three[R, C] == -Inf) && (boundaries_Three[R, C+1] == Inf)){
        boundaries_Three[R, C] = Inf
      }
    }
  }
  
  #Extract coefficients and their corresponding powers
  coefs_Three <-Threewise[, grepl("Final", names(Threewise))]
  coefs_Three[is.na(coefs_Three)] <- 0
  
  powers_Three <- Threewise[, grepl("Power", names(Threewise))]
  powers_Three[is.na(powers_Three)] <- 1
  powers_x_Three <- powers_Three[grepl("x", names(powers_Three))]
  powers_y_Three <- powers_Three[grepl("y", names(powers_Three))]
  powers_z_Three <- powers_Three[grepl("z", names(powers_Three))]
} 

saveRDS(Inputs_RF_Three,paste(Path,"Inputs_RF_Three.rds",sep="/"))
saveRDS(boundaries_Three,paste(Path,"boundaries_Three.rds",sep="/"))
saveRDS(coefs_Three,paste(Path,"coefs_Three.rds",sep="/"))
saveRDS(powers_Three,paste(Path,"powers_Three.rds",sep="/"))
saveRDS(powers_x_Three,paste(Path,"powers_x_Three.rds",sep="/"))
saveRDS(powers_y_Three,paste(Path,"powers_y_Three.rds",sep="/"))
saveRDS(powers_z_Three,paste(Path,"powers_z_Three.rds",sep="/"))
saveRDS(Threewise,paste(Path,"Threewise.rds",sep="/"))


