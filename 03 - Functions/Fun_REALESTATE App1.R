############### Feuille Real Estate ##########
AfficheRealEstate <- function () {
  # Données immo
  DataREAff <- DataRE %>%
    group_by(Type) %>%
    summarise(VB=sum(VB)/1000000)
  return(DataREAff)
}