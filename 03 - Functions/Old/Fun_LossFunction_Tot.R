############################################################################################################
################# Fonction de de la perte totale en fonction des inputs ####################################
############################################################################################################

Loss_Tot <- function () {
  val_RF <- c(EConv_fun(input$RF_EConv/100),
              FL_fun(input$RF_FL/100),
              CCorp_fun(input$RF_CCorp/100),
              CSov_fun(input$RF_CSov/100),
              P_fun(input$RF_P/100),
              EqVol_fun(input$RF_EqVol/100),
              FVol_fun(input$RF_FVol/100),
              Inf_fun(input$RF_Inf/100))
  
  ###### Singlewise Loss ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  sapply(c(1:length(List_RF)), function(x){
    Inputs_RF_Sing[grep(List_RF[x], Ind_Equations$Entity), 2] <<- val_RF[x]
  })
  
  # Loss_sin <- readRDS("./04 - Data Calc/01 - OFs/Loss_sin.Rds")
  Loss_sin <- Loss_Singlewise(Inputs_RF_Sing)
  Loss_sin <- Loss_sin %>% cbind(matrix(unlist(str_split(Loss_sin$Entity," ")),ncol=4,byrow=TRUE) [,-3]) %>%
    select(-Entity) %>%
    rename(Entity = `1`) %>%
    group_by(Entity) %>%
    summarise (Assets= sum(Assets),Liabilities= sum(Liabilities)) %>%
    mutate (Tot = Assets - Liabilities) %>%
    select(-Assets,-Liabilities)
   
  ###### Pairwise Loss ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  sapply(c(1:length(List_RF)), function(x){
    Inputs_RF_Pair[grep(List_RF[x], Pairwise$Risk.X), 2] <<- val_RF[x]
    Inputs_RF_Pair[grep(List_RF[x], Pairwise$Risk.Y), 3] <<- val_RF[x]
  })
  
  Loss_pair <- Loss_Pairwise(Inputs_RF_Pair)
  Loss_pair <- Loss_pair %>% cbind(matrix(unlist(str_split(Loss_pair$Entity," ")),ncol=5,byrow=TRUE) [,-3]) %>%
    select(-Entity) %>%
    rename(Entity = `1`, RF1 = `3`,RF2 = `4`) %>%
    group_by(Entity) %>%
    summarise (Assets= sum(Assets),Liabilities= sum(Liabilities)) %>%
    mutate (Tot = Assets - Liabilities) %>%
    select(-Assets,-Liabilities)
  Loss_pair$Entity <- c("FRL_AER_Pairwises","FRL_AVS_Pairwises")
  
  ###### Threewise Loss ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  
  sapply(c(1:length(List_RF)), function(x){
    Inputs_RF_Three[grep(List_RF[x], Threewise$Risk.X), 2] <<- val_RF[x]
    Inputs_RF_Three[grep(List_RF[x], Threewise$Risk.Y), 3] <<- val_RF[x]
    Inputs_RF_Three[grep(List_RF[x], Threewise$Risk.Z), 3] <<- val_RF[x]
  })
  
  Loss_three <- Loss_Threewise(Inputs_RF_Three)
  Loss_three <- Loss_three %>% cbind(matrix(unlist(str_split(Loss_three$Entity," ")),ncol=5,byrow=TRUE) [,-2]) %>%
    select(-Entity) %>%
    rename(Entity = `1`, RF1 = `2`,RF2 = `3`,RF3 = `4`) %>%
    group_by(Entity) %>%
    summarise (Assets= sum(Assets),Liabilities= sum(Liabilities)) %>%
    mutate (Tot = Assets - Liabilities) %>%
    select(-Assets,-Liabilities)
  Loss_three$Entity <- c("FRL_AER_Threewises","FRL_AVS_Threewises")
  
  ###### Aggregation Loss''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Loss_tot <- Loss_sin %>% rename(Loss = Tot)
  Loss_tot[,2] <- Loss_sin[,2]/1000 + Loss_pair[,2]/1000 + Loss_three[,2]/1000
  Loss_tot <- Loss_tot %>% bind_rows(Loss_tot %>% ungroup() %>% summarise(Loss = sum(Loss)))
  Loss_tot$Entity <- c("AER","Aviva Vie Contrib","Aviva Vie Solo")
  
  New_Date <- Data_OFs_CR[1,1:4]
  New_Date [1,1] <- Mkt_data$Date[nrow(Mkt_data)]
  New_Date [1,2] <- Data_OFs_CR[Data_OFs_CR$Date== Date_LF,"Own_Fund Aviva Vie Solo"] - Loss_tot[Loss_tot$Entity=="Aviva Vie Solo",2]
  New_Date [1,3] <- Data_OFs_CR[Data_OFs_CR$Date== Date_LF,"Own_Fund AER"] - Loss_tot[Loss_tot$Entity=="AER",2]
  New_Date [1,3] <- Data_OFs_CR[Data_OFs_CR$Date== Date_LF,"Own_Fund Aviva Vie Contrib"] - Loss_tot[Loss_tot$Entity=="Aviva Vie Contrib",2]
  
  Data_OFs_CR <- Data_OFs_CR %>% bind_rows(New_Date) %>% select(-contains("SCR"))
  
  Data_OFs_CR$Date <- format(Data_OFs_CR$Date,'%Y-%m-%d')
  
  saveRDS(Data_OFs_CR,"./04 - Data Calc/01 - OFs/Data_OFs_CR.Rds")
  
  return(Data_OFs_CR)
  
}
