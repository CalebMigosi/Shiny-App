"LOSS FUNCTIONS"
"Singlewise function"
singlewise <- cmpfun(function(names, inputs){
  #Assign the appropriate input to each row
  sapply(1:nrow(boundaries), function(i){
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



"Pairwise function"
pairwise <- cmpfun(function(names, inputs){
  "Compute loss for each of the intervals in the matrix and sum them"
  sapply(1:nrow(boundaries_pairwise), function(i){
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


"Threewise function"
threewise <- cmpfun(function(names, inputs){
  "Compute loss for each of the intervals in the matrix and sum them"
  sapply(1:nrow(boundaries_Threewise), function(i){
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

