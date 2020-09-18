y <- c("openxlsx","dplyr", "stringr", "tidyverse", "matrixStats", "compiler", "foreach", "doSNOW", "parallel", "doParallel")
lapply(y, require, character.only = TRUE) 
wd <- "//cerata/DGCU/Risques/6. Risques Métiers/5. Investissements financiers/Estimation Cover Ratio/03 - Application Shiny/01 - Data/04 - SCR/02 - Loss Functions"

#Source the piecewise function
source(paste(wd, 
             "Piecewise Function.R", sep = "/"))

#Extract the pairwise spreadsheet
Pairwise <- read.xlsx(paste(wd, 
                              "Pairwise.xlsx", sep = "/"))

#Clean the file
Pairwise <- Pairwise[rowSums(is.na(Pairwise)) <  160, ]

#Create a unique ID for each function
Pairwise$Entity <- paste(Pairwise[, 1],Pairwise[, 2],
                                 Pairwise[, 3] , Pairwise[, 4], Pairwise[, 5])

#Remove the liability tax relief
Pairwise <- Pairwise[,-c(2:6)]%>%
  filter(!grepl("Liability Tax Relief", Entity))%>%
  arrange(Entity)

#Extract the boundaries_pairwise
boundaries_pairwise <- Pairwise[, grepl("Piecewise", names(Pairwise))]

#Extract coefficients and their corresponding powers_pairwise
asst_pairwise <- grepl("Asset", Pairwise$Entity)
coefs_pairwise_asst<- grepl("Final", names(Pairwise))
Pairwise[asst_pairwise,coefs_pairwise_asst] <- -Pairwise[asst_pairwise, coefs_pairwise_asst]

coefs_pairwise <-Pairwise[, grepl("Final", names(Pairwise))]
coefs_pairwise[is.na(coefs_pairwise)] <- 0

#Extract powers_pairwise. NA Values converted to 1
powers_pairwise <- Pairwise[, grepl("Power", names(Pairwise))]
powers_pairwise[is.na(powers_pairwise)] <- 1
powers_pairwise_x <- powers_pairwise[grepl("x", names(powers_pairwise))]
powers_pairwise_y <- powers_pairwise[grepl("y", names(powers_pairwise))]

#Extract the left boundaries_pairwise
left_bounds_x <- boundaries_pairwise[,seq(1, ncol(boundaries_pairwise), 4)]
left_bounds_y <- boundaries_pairwise[,seq(3, ncol(boundaries_pairwise), 4)]
right_bounds_x <- boundaries_pairwise[,seq(2, ncol(boundaries_pairwise), 4)]
right_bounds_y <- boundaries_pairwise[,seq(4, ncol(boundaries_pairwise), 4)]

#Create a Pairwise_wDVA index_wDVA for the boundaries_pairwise_wDVA of the inputs
index_x <- matrix(0, ncol = ncol(coefs_pairwise), nrow = nrow(coefs_pairwise))
index_y <- matrix(0, ncol = ncol(coefs_pairwise), nrow = nrow(coefs_pairwise))

#Define the final inputs
final_inputs_2wise <- matrix(0, nrow = ncol(coefs_pairwise)*nrow(coefs_pairwise), ncol = 2)

#Determine the index of every single interval in out matrix
sapply(1:nrow(boundaries_pairwise), function(i){
  sapply(seq(1, ncol(boundaries_pairwise), 4), 
     function(x){
       "We subdivide our boundaries_pairwise by the conditions they fulfil"
       "X BOUNDS"
       #CASE1: None of the boundaries_pairwise are Inf or -Inf
       index_x[i, ((x+3)/4)] <<- ((!is.na(boundaries_pairwise[i,x]) & !is.na(boundaries_pairwise[i,x+1]))*1)+
         
         #CASE2: Right boundary is Inf
         ((!is.na(boundaries_pairwise[i,x]) & is.na(boundaries_pairwise[i,x+1]))*2)+
         
         #CASE3: Left Boundary is -Inf
         ((is.na(boundaries_pairwise[i,x]) & !is.na(boundaries_pairwise[i,x+1]))*3)+
         
         #CASE4: Right boundary is Inf and Left is -Inf
         ((is.na(boundaries_pairwise[i,x]) & is.na(boundaries_pairwise[i,x+1]))*4)
       
       "Y BOUNDS"
       #CASE1: None of the boundaries_pairwise are Inf or -Inf
       index_y[i,((x+3)/4)] <<- ((!is.na(boundaries_pairwise[i,x+2]) & !is.na(boundaries_pairwise[i,x+3]))*1)+
         
         #CASE2: Right boundary is Inf
         ((!is.na(boundaries_pairwise[i,x+2]) & is.na(boundaries_pairwise[i,x+3]))*2)+
         
         #CASE3: Left Boundary is -Inf
         ((is.na(boundaries_pairwise[i,x+2]) & !is.na(boundaries_pairwise[i,x+3]))*3)+
         
         #CASE4: Right boundary is Inf and Left is -Inf
         ((is.na(boundaries_pairwise[i,x+2]) & is.na(boundaries_pairwise[i,x+3]))*4)
     })
})
          
#Construct the inputs we require for each function
bounds_x <- cbind(as.vector(t(left_bounds_x)), 
                          as.vector(t(right_bounds_x)),
                          as.vector(t(index_x)), 
                          as.vector(t(powers_pairwise_x)))

bounds_y <- cbind(as.vector(t(left_bounds_y)), 
                          as.vector(t(right_bounds_y)),
                          as.vector(t(index_y)), 
                          as.vector(t(powers_pairwise_y)))

#Vectorize all the coefficients
coefs_pairwise_flat <-as.vector(t(coefs_pairwise)) 

#Find the number of columns in the coefs
col <- ncol(coefs_pairwise)

#Find the risk factor in the X and Y inputs
x_factor <- sapply(Pairwise$Entity, function(x){
                   strsplit(x, split = " ")[[1]][4]})

y_factor <- sapply(Pairwise$Entity, function(x){
                  strsplit(x, split = " ")[[1]][5]})



"Compute the loss function on the pairwise"
pairwise <- cmpfun(function(names, inputs){
  "Compute loss for each of the intervals in the matrix and sum them"
  sapply(1:nrow(Pairwise), function(i){
    final_inputs_2wise[(col*(i-1)+1):(col*i), 1] <<- ifelse(x_factor[i] %in% names, 
                                                                     inputs[grep(x_factor[i], names)],
                                                                     0)
    
    final_inputs_2wise[(col*(i-1)+1):(col*i), 2] <<- ifelse(y_factor[i] %in% names, 
                                                                        inputs[grep(y_factor[i], names)],
                                                                        0)})
    
  sum(sapply(1:length(coefs_pairwise_flat), function(i){
      #Find the piecewise result for the first risk factor
      piecewise(index = bounds_x[i, 3], input = final_inputs_2wise[i,1], bd = bounds_x[i, 1:2], pw = bounds_x[i, 4])*
        
      #Find the piecewise result for the second
      piecewise(index = bounds_y[i, 3], input = final_inputs_2wise[i,2], bd = bounds_y[i, 1:2], pw = bounds_y[i, 4])})*
      
      #Multiply by the coefficients and sum with the exception of the NAs    
      coefs_pairwise_flat, na.rm = T)
  })

