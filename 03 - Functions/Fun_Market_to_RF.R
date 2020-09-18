####################################################################################################################
#''''''''''''''''''''''''''''''''''Function from Market to RF ''''''''''''''''''''''''''''''''''''''''''''''''''''''
####################################################################################################################

EConv_fun <- function (EConv_Mkt) {
  return(-log(EConv_Mkt+1))
}

P_fun <- function (P_Mkt) {
  return(-log(P_Mkt+1))
}

CCorp_fun <- function (CCorp_Mkt) {
  return(CCorp_Mkt)
}

CSov_fun <- function (CSov_Mkt) {
  return(CSov_Mkt)
}


"Interest rate level"
"=================================================================================================================="
pathSCR <- "./01 - Data/07 - SCR"
components_taux <- openxlsx::read.xlsx(paste(pathSCR, "Components Taux.xlsx", sep = "/"))
scaling_components <- openxlsx::read.xlsx(paste(pathSCR, "Scaling Taux.xlsx", sep = "/"))

FL_fun <- function (FL_Mkt, NSS = T) {
  #Introduce a curve in csv form
  if (ncol(FL_Mkt) > 2 & NSS == T){
    #Run NSS if required
    FL_Mkt <- t(FL_Mkt)
    params <- as.xts(Svensson(FL_Mkt[,2], FL_Mkt[,1]), order.by = Sys.Date())
    rates <- Srates(params, as.xts(t(1:40), order.by = Sys.Date()), "Spot")%>%
                as.matrix(ncol = 1)
   
     #Compute the Risk Factors
   composants <-  rates%*%as.matrix(components_taux[, -1], nrow = 40)
   
   #Scale the risk factors
   composants <- composants/scaling_components
   
  } else if (ncol(FL_Mkt) == 2 & NSS == T){
    params <- as.xts(Svensson(FL_Mkt[,2], FL_Mkt[,1]), order.by = Sys.Date())
    rates <- Srates(params, as.xts(t(1:40), order.by = Sys.Date()), "Spot")%>%
      as.matrix(ncol = 1)
    
    #Compute the Risk Factors
    composants <-  rates%*%as.matrix(components_taux[, -1], nrow = 40)
    
    #Scale the risk factors
    composants <- composants/scaling_components
    
  } else if (ncol(FL_Mkt) > 2 & NSS == F){
    composants <-  FL_Mkt[, 2]%*%as.matrix(components_taux[, -1], nrow = 40)
    
    #Scale the risk factors
    composants <- composants/scaling_components
    
  } else {
    composants <-  t(FL_Mkt[, 2])%*%as.matrix(components_taux[, -1], nrow = 40)
    
    #Scale the risk factors
    composants <- composants/scaling_component
  }
  
 FL <- as.numeric(composants[1])
  
  return(FL)
}

Inf_fun <- function (Inf_Mkt) {
  return(Inf_Mkt)
}

EqVol_fun <- function (EqVol_Mkt) {
  return(EqVol_Mkt)
}

FVol_fun <- function (FVol_Mkt) {
  return(FVol_Mkt)
}


