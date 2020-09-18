##############################################################################################################
#''''''''''''''''''''''''''' Importation  et traitement analyse Equity risk'''''''''''''''''''''''''''
##############################################################################################################

'Pour eviter de refaire un traitement de la base transpa plusieurs fois'

# Creation Fichier resumé EQ_CLUST_RESUM
Path <- "./01 - Data/05 - Equity"
location <- paste(Path,"EQ_CLUST.rds",sep="/")
EQ_CLUST_RESUM <- readRDS(location)
TOT <- sum(Data_EQ_CLUST$`MARKET VALUE`)/1000000
EQ_CLUST_RESUM <- EQ_CLUST_RESUM %>% select(Cluster,`MARKET VALUE`) %>% group_by(Cluster) %>%
  summarize(`MARKET VALUE TOT` = sum(`MARKET VALUE`)/1000000) %>%
  mutate(Allocation = round(`MARKET VALUE TOT` / TOT*100,2)) %>%
  mutate( `MARKET VALUE TOT` = round(`MARKET VALUE TOT`,2))
EQ_CLUST_RESUM$Cluster <- factor(EQ_CLUST_RESUM$Cluster)

saveRDS(EQ_CLUST_RESUM,paste(Path,"EQ_CLUST_RESUM.rds",sep="/"))

