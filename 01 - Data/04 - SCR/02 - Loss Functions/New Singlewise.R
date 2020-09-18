
# names = c("EConv", "CCorp", "CSov", "EqVol", "FL", "FS", "FT") 
# inputs = c(0.01, 0.03, 0.04, 0.04, 0.04, 0.02, 0.01)
y <- c("openxlsx","dplyr", "stringr", "tidyverse", "matrixStats", "compiler", "foreach", "doSNOW", "parallel", "doParallel")
lapply(y, require, character.only = TRUE) 
wd <- "//cerata/DGCU/Risques/6. Risques Métiers/5. Investissements financiers/Estimation Cover Ratio/03 - Application Shiny/01 - Data/04 - SCR/02 - Loss Functions"

#Source the piecewise function
source(paste(wd, 
             "Piecewise Function.R", sep = "/"))

#Extract the singlewise spreadsheet
singlewise <- read.xlsx(paste(wd, 
                             "Singlewise.xlsx", sep = "/"))

singlewise <- singlewise[rowSums(is.na(singlewise)) < 265, ]

#Identify each function
singlewise$Entity <- paste(singlewise[, 1],singlewise[, 2],
                           singlewise[, 3] , singlewise[, 4]) %>%
  sapply(function(x) gsub("MPan","Mpan",as.character(x)))

#Identify each function
loss_functions <- singlewise[,-c(2:5)]%>%
  filter(!grepl("Liability Tax Relief", Entity))%>%
  arrange(Entity)


#Change all coefficients of a liability function to -ve
asst_functions <- grepl("Asset", loss_functions$Entity)
coefs_asst <- grepl("Final", names(loss_functions))
loss_functions[asst_functions,coefs_asst ] <- -loss_functions[asst_functions, coefs_asst]

#Establisht the boundaries
boundaries <- loss_functions[, grepl("Piecewise", names(loss_functions))]
left_bounds <- boundaries[,seq(1, ncol(boundaries), 2)]
right_bounds <- boundaries[,seq(2, ncol(boundaries), 2)]

#Extract coefficients and their corresponding powers
coefs <-loss_functions[, grepl("Final", names(loss_functions))]
coefs[is.na(coefs)] <- 0

powers <- loss_functions[, grepl("Power", names(loss_functions))]
powers[is.na(powers)] <- 1

#Check if out input is between any of the 2 boundaries
index <- matrix(0, ncol = ncol(coefs), nrow = nrow(coefs))
final_inputs <- matrix(0, nrow = ncol(coefs)*nrow(coefs))

#We identify the case associated with each interval
sapply(1:nrow(boundaries), function(i){
  "The first sapply loops over all the rows/functions"
  
    sapply(seq(1, ncol(boundaries), 2),
           function(x){
             "We subdivide our boundaries by the conditions they fulfil"
             "SOV BOUNDS"
             #CASE1: None of the boundaries are Inf or -Inf
             index[i, ((x+1)/2)] <<- ((!is.na(boundaries[i,x]) & !is.na(boundaries[i,x+1]))*1)+
               
               #CASE2: Right boundary is Inf
               ((!is.na(boundaries[i,x]) & is.na(boundaries[i,x+1]))*2)+
               
               #CASE3: Left Boundary is -Inf
               ((is.na(boundaries[i,x]) & !is.na(boundaries[i,x+1]))*3)+
               
               #CASE4: Right boundary is Inf and Left is -Inf
               ((is.na(boundaries[i,x]) & is.na(boundaries[i,x+1]))*4)
      })
})

bounds <- cbind(as.vector(t(left_bounds)), 
                            as.vector(t(right_bounds)),
                            as.vector(t(index)), 
                            as.vector(t(powers)))

#Vectorize all the coefficients
coefs_flat <-as.vector(t(coefs)) 





"Singlewise Loss Function"
singlewise <- cmpfun(function(names, inputs){
  #Assign the appropriate input to each row
  sapply(1:nrow(loss_functions), function(i){
    final_inputs[((ncol(coefs)*(i-1))+1):(ncol(coefs)*i)] <<- ifelse(strsplit(loss_functions$Entity[i], split = " ")[[1]][4] %in% names, 
                                                                     inputs[match(strsplit(loss_functions$Entity[i], split = " ")[[1]][4], names)],
                                                                     0)
  })
  "Compute loss for each of the intervals in the matrix and sum them"
  sum(sapply(1:nrow(bounds), function(i){
    #Find the piecewise result for the first risk factor
    piecewise(index = bounds[i, 3], input = final_inputs[i], bd = bounds[i, 1:2], pw = bounds[i, 4])})*
      coefs_flat, na.rm = T)
})

