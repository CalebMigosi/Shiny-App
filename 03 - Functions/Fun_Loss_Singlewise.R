"1 - DISTRIBUTION DATA"
"We fit all our splines for the probabilities"
#Find all probabilities
proba <- round(seq(0, 1, 0.001), 3)

#Matrix of data
spline_data <- data.frame(matrix(0, nrow = length(proba), ncol = ncol(rf_distrib)))
names(spline_data) <- names(rf_distrib)

#First column is the quantiles
spline_data[, 1] <- proba

sapply(2:ncol(rf_distrib), function(i) {
  sp <- smooth.spline(x = flatten(rf_distrib[1]),
                y = flatten(rf_distrib[i]), cv = TRUE)
  
  spline_data[,i] <<- predict(sp, proba)$y
})


"2 - PLOT FITTING"
"=========================================================================================================="
singlewise_plot <- function(names, inputs){
  "Note that the inputs are actually probabilities. 
  We have to convert them to risk factor values before plotting."
  #Change probabilities into values
  inputs <- sapply(1:length(inputs), function(i){
    spline_data[match(inputs[i], spline_data$Quantile), names[i]]})
  
  #Convert our names into a dataframe
  df <- data.frame(names, inputs)
  
  resDf <- sapply(1:nrow(df), function(i){-singlewise(names[i], inputs[i])
  })
  
  resDf <- data.frame(names, resDf)
  
  resDf$names <- as.factor(resDf$names)
  p <- plot_ly(resDf, x =~resDf , y = ~names, type  = "bar", orientation = 'h', 
               marker = list(color = colors_Aviva,
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    layout(xaxis = list(title = "Loss",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = "",
                        color = '#ffffff',
                        showgrid = T),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#00000'))
  
  return(p)
}

total_loss_plot <- function(names, inputs){
  
  inputs <- sapply(1:length(inputs), function(i){
    spline_data[match(inputs[i], spline_data$Quantile), names[i]]})
  
  df <- data.frame(names, inputs )
  singlewise_loss <- -singlewise(names, inputs)
  pairwise_loss <- -pairwise(names, inputs)
  threewise_loss <- -threewise(names, inputs)
  
  DF <- data.frame(names =c("Singlewise", "Pairwise", "Threewise" ), 
                   Losses = c(singlewise_loss, pairwise_loss, threewise_loss))
  DF$names <- as.factor(DF$names)
  
  p <- plot_ly(DF, y = ~names, x = ~Losses, type = "bar", orientation = 'h',
          marker = list(color = colors_Aviva,
                        line = list(width = 1,
                                    color = '#00000'))) %>%
    layout(xaxis = list(title = "Loss",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = "",
                        color = '#ffffff',
                        showgrid = T),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#00000'))
  
  return(p)
}

total_loss_value <- function(names, inputs){
  inputs <- sapply(1:length(inputs), function(i){
    spline_data[match(inputs[i], spline_data$Quantile), names[i]]})
  
 value <-  -round((singlewise(names,inputs)+
           pairwise(names, inputs)+
           threewise(names, inputs))/1000, 2)
 
 return(value)
}


"PURE RISK FACTORS"
"============================================================================================================="
singlewise_plot2 <- function(names, inputs){
  "Note that the inputs are actually probabilities."
  
  #Convert our names into a dataframe
  df <- data.frame(names, inputs)
  
  resDf <- sapply(1:nrow(df), function(i){-singlewise(names[i], inputs[i])
  })
  
  resDf <- data.frame(names, resDf)
  
  resDf$names <- as.factor(resDf$names)
  p <- plot_ly(resDf, x =~resDf , y = ~names, type  = "bar", orientation = 'h', 
               marker = list(color = colors_Aviva,
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    layout(xaxis = list(title = "Loss",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = "",
                        color = '#ffffff',
                        showgrid = T),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#00000'))
  
  return(p)
}

total_loss_plot2 <- function(names, inputs){
  df <- data.frame(names, inputs )
  singlewise_loss <- -singlewise(names, inputs)
  pairwise_loss <- -pairwise(names, inputs)
  threewise_loss <- -threewise(names, inputs)
  
  DF <- data.frame(names =c("Singlewise", "Pairwise", "Threewise" ), 
                   Losses = c(singlewise_loss, pairwise_loss, threewise_loss))
  DF$names <- as.factor(DF$names)
  
  p <- plot_ly(DF, y = ~names, x = ~Losses, type = "bar", orientation = 'h',
               marker = list(color = colors_Aviva,
                             line = list(width = 1,
                                         color = '#00000'))) %>%
    layout(xaxis = list(title = "Loss",
                        color = '#ffffff',
                        showgrid = T),
           
           yaxis = list(title = "",
                        color = '#ffffff',
                        showgrid = T),
           
           paper_bgcolor = '#8dacc3',                  #Color of the background
           plot_bgcolor = '#8dacc3',                    #Color of the plot background
           
           #Define the font
           font = list(
             #family = "sans serif",
             size = 15,
             color = '#00000'))
  
  return(p)
}

total_loss_value2 <- function(names, inputs){
  value <-  -round((singlewise(names,inputs)+
                      pairwise(names, inputs)+
                      threewise(names, inputs))/1000, 2)
  
  return(value)
}