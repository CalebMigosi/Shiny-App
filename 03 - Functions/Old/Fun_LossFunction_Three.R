Loss_Threewise <- function(Inputs_RF_Three) {
    
  final_inputs <- Inputs_RF_Three
  boundaries <- boundaries_Three
  coefs <- coefs_Three
  powers <- powers_Three
  powers_x <- powers_x_Three
  powers_y <- powers_y_Three
  powers_z <- powers_z_Three
  
  #Check if out input is between any of the 2 boundaries
  index <- matrix(0, nrow = ncol(coefs), ncol = 3)
  loop_output <- matrix(0,nrow = nrow(final_inputs), ncol = 2)%>%
    data.frame()
  loop_output[, 1] <- final_inputs[, 1]
  
  final_output <- matrix(0, nrow = (0.5*nrow(final_inputs)), ncol = 3)%>%
    data.frame()
  
  #Try using lapply
  for (i in 1:nrow(final_inputs)){
    for (j in seq(1, ncol(boundaries), 6)) {
      index[((j+5)/6), 1] <- dplyr::between(final_inputs[i, 2], 
                                            boundaries[i,j],boundaries[i,j+1])
      index[((j+5)/6), 2] <- dplyr::between(final_inputs[i, 3], 
                                            boundaries[i,j+2],boundaries[i,j+3])
      index[((j+5)/6), 3] <- dplyr::between(final_inputs[i, 4], 
                                            boundaries[i,j+4],boundaries[i,j+5])
    }
    ind <- index[, 1]*index[, 2]*index[, 3]
    
    if (sum(ind) == 0) {loop_output[i, 2] <- 0
    }else{
      cfs <- coefs[i, as.logical(ind)]
      pws_x <- powers_x[i, as.logical(ind)]
      pws_y <- powers_y[i, as.logical(ind)]
      pws_z <- powers_z[i, as.logical(ind)]
      inpt <-final_inputs[i, c(2:4)]
      
      prd <- (inpt[,1]^pws_x)*(inpt[,2]^pws_y)*(inpt[,3]^pws_z)
      result <- sum(cfs*prd)
      loop_output[i, 2] <- result
    }
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
