clean_data <- function(tickers, n=0, date_init, opt) {
  "Convert the list data produced by Bloomberg into a dataframe"
  start.date = as.Date(date_init)
  data <- bdh(as.character(tickers), fields = "PX_LAST", start.date = start.date, end.date = (Sys.Date()-1), options = opt)
  
  #Change the name of each list to the ticker
  for (i in 1:length(data)) names(data[[i]])[2] <- names(data)[i]
  
  #Change the list to a dataframe
  length <- matrix(0, ncol = 2, nrow = length(data))
  
  #Assign the length of each list to a column
  for (i in 1:length(data)) length[i, ] <- c(length(data[[i]][, 2]), i)
  
  #Arrange the columns from longest to shortest and maintain the indices of each stock
  length <- length%>%
    data.frame()%>%
    arrange(desc(X1))
  
  #Remove columns with less than 500 lines
  length <- length[(length[,1]>n), ]
  data <- data[length[, 2]]
  
  #Create a dataframe using join_all
  data <- plyr::join_all(data, by = "date", match = "all")
  data$date <- as.Date(data$date)
  data <- data %>% rename(Date = date)
  return(data)
}
