############################################################################################################
################# Fonction de visulatin des résutats par facteur de risque ################################
############################################################################################################

Loss_Gran <- function () {
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
  
  Loss_sin <- Loss_Singlewise (Inputs_RF_Sing)
  Loss_sin <- Loss_sin %>% cbind(matrix(unlist(str_split(Loss_sin$Entity," ")),ncol=4,byrow=TRUE) [,-3]) %>%
    select(-Entity) %>%
    rename(Entity = `1`, RF = `3`) %>%
    group_by(Entity,RF) %>%
    summarise (Assets= sum(Assets),Liabilities= sum(Liabilities)) %>%
    mutate (Tot = Assets/1000 - Liabilities/1000) %>%
    select(-Assets,-Liabilities)
  # saveRDS(Loss_sin,"./04 - Data Calc/01 - OFs/Loss_sin.Rds")
  
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
    mutate (Tot = Assets/1000 - Liabilities/1000) %>%
    select(-Assets,-Liabilities)
  Loss_pair$Entity <- c("FRL_AER_Pairwises","FRL_AVS_Pairwises")
  # saveRDS(Loss_pair,"./04 - Data Calc/01 - OFs/Loss_pair.Rds")
  
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
    mutate (Tot = Assets/1000 - Liabilities/1000) %>%
    select(-Assets,-Liabilities)
  Loss_three$Entity <- c("FRL_AER_Threewises","FRL_AVS_Threewises")
  # saveRDS(Loss_three,"./04 - Data Calc/01 - OFs/Loss_three.Rds")
  
  ###### Aggregation des informations''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  Loss_gran<- Loss_sin %>% bind_rows(Loss_pair) %>% bind_rows(Loss_three) %>% rename (Loss = Tot)
  
  return(Loss_gran)
  
}
