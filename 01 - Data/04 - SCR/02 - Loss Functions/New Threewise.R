y <- c("openxlsx","dplyr", "stringr", "tidyverse", "matrixStats", "compiler", "foreach", "doSNOW", "parallel", "doParallel")
lapply(y, require, character.only = TRUE) 
wd <- "//cerata/DGCU/Risques/6. Risques Métiers/5. Investissements financiers/Estimation Cover Ratio/03 - Application Shiny/01 - Data/04 - SCR/02 - Loss Functions"

#Source the piecewise function
source(paste(wd, 
             "Piecewise Function.R", sep = "/"))

#Extract the singlewise spreadsheet
Threewise <- read.xlsx(paste(wd, 
                            "Threewise.xlsx", sep = "/"))


#Create a unique ID for each function
Threewise$Entity <- paste(Threewise[, 1],Threewise[, 2],
                         Threewise[, 3] , Threewise[, 4], 
                         Threewise[, 5], Threewise[, 6])

#Remove the liability tax relief
Threewise <- Threewise[,-c(2:7)]%>%
  filter(!grepl("Liability Tax Relief", Entity))%>%
  arrange(Entity)

#Extract the boundaries_Threewise
boundaries_Threewise <- Threewise[, grepl("Piecewise", names(Threewise))]

#Convert the Asset function coefficients to negative
asst_Threewise <- grepl("Asset", Threewise$Entity)
coefs_Threewise_asst<- grepl("Final", names(Threewise))
Threewise[asst_Threewise,coefs_Threewise_asst] <- -Threewise[asst_Threewise, coefs_Threewise_asst]

#Extract the coefficients
coefs_Threewise <-Threewise[, grepl("Final", names(Threewise))]
coefs_Threewise[is.na(coefs_Threewise)] <- 0

#Extract powers_Threewise. NA Values converted to 1
powers_Threewise <- Threewise[, grepl("Power", names(Threewise))]
powers_Threewise[is.na(powers_Threewise)] <- 1
powers_Threewise_x <- powers_Threewise[grepl("x", names(powers_Threewise))]
powers_Threewise_y <- powers_Threewise[grepl("y", names(powers_Threewise))]
powers_Threewise_z <- powers_Threewise[grepl("z", names(powers_Threewise))]

#Extract the left boundaries_Threewise
left_bounds_3wise_x <- boundaries_Threewise[,seq(1, ncol(boundaries_Threewise), 6)]
left_bounds_3wise_y <- boundaries_Threewise[,seq(3, ncol(boundaries_Threewise), 6)]
left_bounds_3wise_z <- boundaries_Threewise[,seq(5, ncol(boundaries_Threewise), 6)]
right_bounds_3wise_x <- boundaries_Threewise[,seq(2, ncol(boundaries_Threewise), 6)]
right_bounds_3wise_y <- boundaries_Threewise[,seq(4, ncol(boundaries_Threewise), 6)]
right_bounds_3wise_z <- boundaries_Threewise[,seq(6, ncol(boundaries_Threewise), 6)]

#Create a Threewise_wDVA index_wDVA for the boundaries_Threewise_wDVA of the inputs
index_3wise_x <- matrix(0, ncol = ncol(coefs_Threewise), nrow = nrow(coefs_Threewise))
index_3wise_y <- matrix(0, ncol = ncol(coefs_Threewise), nrow = nrow(coefs_Threewise))
index_3wise_z <- matrix(0, ncol = ncol(coefs_Threewise), nrow = nrow(coefs_Threewise))

#Define the final inputs
final_inputs_3wise <- matrix(0, nrow = ncol(coefs_Threewise)*nrow(coefs_Threewise), ncol = 3)

#Determine the index of every single interval in out matrix
sapply(1:nrow(boundaries_Threewise), function(i){
  sapply(seq(1, ncol(boundaries_Threewise), 6), 
         function(x){
           "We subdivide our boundaries_Threewise by the conditions they fulfil"
           "X BOUNDS"
           #CASE1: None of the boundaries_Threewise are Inf or -Inf
           index_3wise_x[i, ((x+5)/6)] <<- ((!is.na(boundaries_Threewise[i,x]) & !is.na(boundaries_Threewise[i,x+1]))*1)+
             
             #CASE2: Right boundary is Inf
             ((!is.na(boundaries_Threewise[i,x]) & is.na(boundaries_Threewise[i,x+1]))*2)+
             
             #CASE3: Left Boundary is -Inf
             ((is.na(boundaries_Threewise[i,x]) & !is.na(boundaries_Threewise[i,x+1]))*3)+
             
             #CASE4: Right boundary is Inf and Left is -Inf
             ((is.na(boundaries_Threewise[i,x]) & is.na(boundaries_Threewise[i,x+1]))*4)
           
           "Y BOUNDS"
           #CASE1: None of the boundaries_Threewise are Inf or -Inf
           index_3wise_y[i,((x+5)/6)] <<- ((!is.na(boundaries_Threewise[i,x+2]) & !is.na(boundaries_Threewise[i,x+3]))*1)+
             
             #CASE2: Right boundary is Inf
             ((!is.na(boundaries_Threewise[i,x+2]) & is.na(boundaries_Threewise[i,x+3]))*2)+
             
             #CASE3: Left Boundary is -Inf
             ((is.na(boundaries_Threewise[i,x+2]) & !is.na(boundaries_Threewise[i,x+3]))*3)+
             
             #CASE4: Right boundary is Inf and Left is -Inf
             ((is.na(boundaries_Threewise[i,x+2]) & is.na(boundaries_Threewise[i,x+3]))*4)
           
           "Z BOUNDS"
           #CASE1: None of the boundaries_Threewise are Inf or -Inf
           index_3wise_z[i,((x+5)/6)] <<- ((!is.na(boundaries_Threewise[i,x+4]) & !is.na(boundaries_Threewise[i,x+5]))*1)+
             
             #CASE2: Right boundary is Inf
             ((!is.na(boundaries_Threewise[i,x+4]) & is.na(boundaries_Threewise[i,x+5]))*2)+
             
             #CASE3: Left Boundary is -Inf
             ((is.na(boundaries_Threewise[i,x+4]) & !is.na(boundaries_Threewise[i,x+5]))*3)+
             
             #CASE4: Right boundary is Inf and Left is -Inf
             ((is.na(boundaries_Threewise[i,x+4]) & is.na(boundaries_Threewise[i,x+5]))*4)
         })
})



#Construct the inputs we require for each function
bounds_3wise_x <- cbind(as.vector(t(left_bounds_3wise_x)), 
                  as.vector(t(right_bounds_3wise_x)),
                  as.vector(t(index_3wise_x)), 
                  as.vector(t(powers_Threewise_x)))

bounds_3wise_y <- cbind(as.vector(t(left_bounds_3wise_y)), 
                  as.vector(t(right_bounds_3wise_y)),
                  as.vector(t(index_3wise_y)), 
                  as.vector(t(powers_Threewise_y)))

bounds_3wise_z <- cbind(as.vector(t(left_bounds_3wise_z)), 
                  as.vector(t(right_bounds_3wise_z)),
                  as.vector(t(index_3wise_z)), 
                  as.vector(t(powers_Threewise_z)))

#Vectorize all the coefficients
coefs_Threewise_flat <-as.vector(t(coefs_Threewise)) 

#Find the number of columns in the coefs
col3w <- ncol(coefs_Threewise)

#Find the risk factor in the X and Y inputs
x_fac <- sapply(Threewise$Entity, function(x){
  strsplit(x, split = " ")[[1]][4]})

y_fac <- sapply(Threewise$Entity, function(x){
  strsplit(x, split = " ")[[1]][5]})

z_fac <- sapply(Threewise$Entity, function(x){
  strsplit(x, split = " ")[[1]][6]})


"Compute the loss function on the Threewise"
threewise <- cmpfun(function(names, inputs){
  "Compute loss for each of the intervals in the matrix and sum them"
  sapply(1:nrow(Threewise), function(i){
    final_inputs_3wise[((col3w*(i-1))+1):(col3w*i), 1] <<- ifelse(x_fac[i] %in% names, 
                                                            inputs[match(x_fac[i], names)],
                                                            0)

    final_inputs_3wise[((col3w*(i-1))+1):(col3w*i), 2] <<- ifelse(y_fac[i] %in% names, 
                                                              inputs[match(y_fac[i], names)],
                                                              0)
  
    final_inputs_3wise[((col3w*(i-1))+1):(col3w*i), 3] <<- ifelse(z_fac[i] %in% names, 
                                                            inputs[match(z_fac[i], names)],
                                                            0)
  })
  
  
  sum(sapply(1:nrow(bounds_3wise_x), function(i){
    #Find the piecewise result for the first risk factor
    piecewise(index = bounds_3wise_x[i], input = final_inputs_3wise[i,1], bd = bounds_3wise_x[i, 1:2], pw = bounds_3wise_x[i, 4])*
      
      #Find the piecewise result for the second
      piecewise(index = bounds_3wise_y[i], input = final_inputs_3wise[i,2], bd = bounds_3wise_y[i, 1:2], pw = bounds_3wise_y[i, 4])*
      
      #Find the piecewise result for the third
      piecewise(index = bounds_3wise_z[i], input = final_inputs_3wise[i,3], bd = bounds_3wise_z[i, 1:2], pw = bounds_3wise_z[i, 4])})*
      
      #Multiply by the coefficients and sum with the exception of the NAs    
      coefs_Threewise_flat, na.rm = T)
  })

