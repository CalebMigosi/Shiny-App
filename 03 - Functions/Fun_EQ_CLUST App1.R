############################################################################################################
################# Analyse ptf Equity #######################################################################
############################################################################################################

# EQ_CLUST_LIST <- function () {
#   EQ_CLUST_LIST <- Data_EQ_CLUST
#   return(EQ_CLUST_LIST)
# }

EQ_CLUST_COUNTRY <- function () {
  EQ_CLUST_COUNTRY <- Data_EQ_CLUST %>% select(COUNTRY,Cluster,`MARKET VALUE`) %>% group_by(COUNTRY,Cluster) %>%
    summarize(`MARKET VALUE` = round(sum(`MARKET VALUE`)/1000000,1)) 
  EQ_CLUST_COUNTRY$Cluster <- factor(EQ_CLUST_COUNTRY$Cluster)
  EQ_CLUST_COUNTRY <- EQ_CLUST_COUNTRY %>% left_join(Data_EQ_CLUST_RESUM , by = 'Cluster') %>%
    mutate(Allocation =  `MARKET VALUE` / `MARKET VALUE TOT` * 100) 
  
  Ncolor <- nrow(EQ_CLUST_COUNTRY %>% group_by(Cluster) %>% summarise(n=n()))
  
  p <- ggplot(EQ_CLUST_COUNTRY,aes(x=COUNTRY,y=Allocation,fill=Cluster)) +
        geom_bar(stat = "identity",colour="black",position=position_dodge()) +
        ggtitle("Allocation geographique")+
        ylab("Allocation (%)")+
        # geom_text(aes(label=round(Allocation,0)), vjust=-0.3, color="black",
        #           position = position_dodge(0.9), size=5)+
        scale_fill_manual(values=scale_Aviva(Ncolor))+
        theme_Aviva()
  p <- ggplotly(p)
  return(p)
}

EQ_CLUST_SECT <- function () {
  EQ_CLUST_SECT <- Data_EQ_CLUST %>% select(GICS_SECTOR_NAME,Cluster,`MARKET VALUE`) %>% group_by(GICS_SECTOR_NAME,Cluster) %>%
    summarize(`MARKET VALUE` = round(sum(`MARKET VALUE`)/1000000,1)) 
  EQ_CLUST_SECT$Cluster <- factor(EQ_CLUST_SECT$Cluster)
  EQ_CLUST_SECT <- EQ_CLUST_SECT %>% left_join(Data_EQ_CLUST_RESUM , by = 'Cluster') %>%
    mutate(Allocation =  `MARKET VALUE` / `MARKET VALUE TOT` * 100) 
  
  Ncolor <- nrow(EQ_CLUST_SECT %>% group_by(Cluster) %>% summarise(n=n()))
  
  p <- ggplot(EQ_CLUST_SECT,aes(x=GICS_SECTOR_NAME,y=Allocation,fill=Cluster)) +
    geom_bar(stat = "identity",colour="black",position=position_dodge()) +
    ggtitle("Allocation sectorielle")+
    ylab("Allocation (%)")+
    # geom_text(aes(label=round(Allocation,0)), vjust=-0.3, color="black",
    #           position = position_dodge(0.9), size=5)+
    scale_fill_manual(values=scale_Aviva(Ncolor))+
    theme_Aviva() +
    theme(axis.text.x = element_text(angle=45))
  
  p <- ggplotly(p)
  return(p)
}

EQ_CLUST_VAR <- function () {
  List_plot <- list()
  
  for ( i in 2:ncol(Data_EQ_CLUST_VAR)){
    name_clust <- colnames(Data_EQ_CLUST_VAR[i])
    EQ_CLUST_VAR <- Data_EQ_CLUST_VAR[,c(1,i)] %>% top_n(10) %>%
      rename("Cluster" = colnames(Data_EQ_CLUST_VAR[i])) %>%
      arrange(desc(`Cluster`)) %>%
      mutate(Name=factor(Name))
    
    Ncolor <- 10
    
    p <- ggplot(EQ_CLUST_VAR,aes(x=reorder(Name,`Cluster`),y= `Cluster`,fill=reorder(Name,`Cluster`))) +
      geom_bar(stat = "identity",colour="black",position=position_dodge()) +
      ggtitle( paste("Variables explicatives",name_clust,sep=" "))+
      ylab("Importance")+
      coord_flip() +
      scale_fill_manual(values=scale_Aviva(Ncolor))+
      theme_Aviva()
    
    List_plot[[i-1]] <- p
      
  }
 
  List_plot <- grid.arrange(grobs = List_plot)
  
  return(List_plot)
}
# EQ_CLUST_RESUM <- function () {
#   TOT <- sum(Data_EQ_CLUST$`MARKET VALUE`)/1000000
#   EQ_CLUST_RESUM <- Data_EQ_CLUST %>% select(Cluster,`MARKET VALUE`) %>% group_by(Cluster) %>%
#     summarize(`MARKET VALUE` = round(sum(`MARKET VALUE`),1)/1000000) %>%
#     mutate(Allocation = round(`MARKET VALUE` / TOT*100,2))
#   return(EQ_CLUST_RESUM)
# }

test <- list()
test[[1]] <- list(1,"a")
test[[2]] <- list(2)
