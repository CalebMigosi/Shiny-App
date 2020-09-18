
Loss_Tot <- function () {
  val_RF <- c(EConv_fun(Perf_EConv/100),
             0, 0, 0, 0, 0, 0, 0)
  Loss_Tot <- Data_OFs_CR %>% 
                  select("Date",contains("Own_Fund Aviva Vie Solo")) %>% 
                  rename ("Own_Fund_Aviva_Vie_Solo" = "Own_Fund Aviva Vie Solo") 
  
  New_Date <- data.frame( `Own_Fund_Aviva_Vie_Solo` = Loss_Tot$Own_Fund_Aviva_Vie_Solo[Loss_Tot$Date == Date_LF] + 
                          (singlewise(List_RF, val_RF) + pairwise(List_RF, val_RF) + 
                             threewise(List_RF, val_RF))/1000,
                          `Date` = Mkt_data$Date[nrow(Mkt_data)])
  
  Loss_Tot <-  Loss_Tot %>% bind_rows(New_Date)
  Loss_Tot$Date <- format(Loss_Tot$Date,'%Y-%m-%d')
  return(Loss_Tot)
}

