"LOSS FUNCTION DATA"
#Source the Loss Function Simulator with DVA
location <- "//cerata/DGCU/Risques/6. Risques Métiers/5. Investissements financiers/Estimation Cover Ratio/03 - Application Shiny/01 - Data/04 - SCR/02 - Loss Functions"

source(paste(location, 
             "New Singlewise.R", sep = "/"))

#Source the Pairwise with DVA
source(paste(location, 
             "New Pairwise.R", sep = "/"))

#Source the Pairwise with DVA
source(paste(location, 
             "New Threewise.R", sep = "/"))

#Save all files as rds
sapply(ls(.GlobalEnv), function(x) saveRDS(get(x, envir = .GlobalEnv), paste(location, paste("RDS Files", paste(x, ".rds", sep = ""), sep = "/"), sep = "/")))


           
       