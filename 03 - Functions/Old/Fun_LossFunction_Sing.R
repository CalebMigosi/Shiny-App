Loss_Singlewise <- function(Inputs_RF_Sing) {
#********Function calculates the loss from a predefined matrix of values******
'**Please install the following packages: dplyr, stringr, tidyverse, matrixStats**'

  inputs_names <- Inputs_RF_Sing
  boundaries <- boundaries_Sing
  coefs <- coefs_Sing
  powers <- powers_Sing
  
  #Check if out input is between any of the 2 boundaries
  index <- matrix(0, nrow = ncol(coefs_Sing), ncol = 1)
  loop_output <- matrix(0,nrow = nrow(Inputs_RF_Sing), ncol = 2)%>%
    data.frame()
  loop_output[, 1] <- Inputs_RF_Sing[, 1]
  final_output <- matrix(0, nrow = (0.5*nrow(Inputs_RF_Sing)), ncol = 3)%>%
    data.frame()
  
  
  #Try using lapply
  for (i in 1:nrow(inputs_names)){
    for (j in seq(1, ncol(boundaries), 2)) {
      index[((j+1)/2)] <- dplyr::between(inputs_names[i, 2], 
                                          boundaries[i,j],boundaries[i,j+1])
    }
    cfs <- coefs[i, as.logical(index)]
    pws <- powers[i, as.logical(index)]
    inpt <-matrix(inputs_names[i, 2], nrow = length(pws), ncol = 1)
    
    result <- sum(cfs*(inpt^pws))
    loop_output[i, 2] <- result
  }

  final_output[, 1] <- sapply(loop_output[, 1], str_replace_all, "Asset|Liability", "")%>%
                        unique()
  ast <- match(loop_output[grepl("Asset",loop_output[, 1]),1], loop_output[,1])
  lia <- match(loop_output[grepl("Liability",loop_output[, 1]),1], loop_output[,1])
  
  final_output[, 2] <- loop_output[ast,2] 
  
  final_output[, 3] <- loop_output[lia,2] 
    
  names(final_output) <- c("Entity","Assets","Liabilities")
  return(final_output)
}

