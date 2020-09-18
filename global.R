"LIBRARIES TO IMPORT"
"================================================================================================================"
packagesToInstall <- c("shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes" ,"shinyBS", 
              "shinyjs", "dplyr", "tidyverse", "readxl", "lobstr", "data.table", "stringr", "plotly", "ggiraph", 
              "ggiraphExtra", "leaflet", "ggmap", "markdown", "httr", "leaflet.extras", "Rblpapi", "matrixStats",
              "gridExtra", "xts", "TTR", "dygraphs", "DT", "reshape2", "openxlsx", "fda", "YieldCurve")

#success <- lapply(packagesToInstall, install.packages, character.only = TRUE)
success <- lapply(packagesToInstall, require, character.only = TRUE)

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(readxl)
library(lobstr)
library(data.table)
library(stringr)
library(plotly)
library(ggiraph)
library(ggiraphExtra)
library(leaflet)
library(ggmap)
library(markdown)
library(httr)
library('leaflet.extras')
library(Rblpapi)
library(matrixStats)
library("gridExtra")
library(xts)
library(TTR)
library(dygraphs)
library(compiler)
library(Rblpapi)
library(DT)
library(reshape2)
library(openxlsx)
library(fda)
library(YieldCurve)

#Import risk factor distribution
pathxlsx = "./01 - Data"
rf_distrib <- openxlsx::read.xlsx(paste(pathxlsx, "07 - SCR/Risk Factor Distribution.xlsx", sep = "/"))
PCAcurve <- openxlsx::read.xlsx(paste(pathxlsx, "07 - SCR/Swap Curve Q3 2019.xlsx" ,sep = "/"))

"IMPORT DATA"
"================================================================================================================"
pathR=paste(getwd(), "02 - Traitement Data", sep = "/")
source(paste(pathR,"Fun_DataBloom_Cleaner.R", sep = "/"),local = TRUE)
source(paste(pathR,"Importation_Data.R",sep = "/"),local = TRUE)
source(paste(pathR,"Importation_Images.R",sep = "/"),local = TRUE)
source(paste(pathR,"Var_Select.R",sep = "/"),local = TRUE)
source(paste(pathR,"AllocationT.R",sep = "/"),local = TRUE)
source(paste(pathR,"AllocationHist.R",sep = "/"),local = TRUE)
source(paste(pathR,"PRE_Risque.R",sep = "/"),local = TRUE)
source(paste(pathR,"CREDIT_Risque.R",sep = "/"),local = TRUE)
source(paste(pathR,"LF_RFcalc.R",sep = "/"),local = TRUE)


"IMPORT PLOTTING FUNCTIONS"
"================================================================================================================"
pathRFun="./03 - Functions"
source(paste(pathRFun,"Fun_AllocationT.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_AllocationHist.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_PRE.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_CREDIT.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_MARKET.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_FERP.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_Loss_Functions.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_Market_to_RF.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_Loss_Tot.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_Loss_Singlewise.R",sep = "/"),local = TRUE)
source(paste(pathRFun,"Fun_Equity.R",sep = "/"),local = TRUE)


"AVIVA COLORS From: https://standards.aviva.com/global-experience-principles/style-guide/colour/"
"================================================================================================================"
colors_Aviva <- c('#FFD900', 
                  '#001E60', 
                  '#1A61BD',
                  '#E5EDF8', 
                  '#0E573F',
                  "#004FB6",
                  "#44C0FF",
                  "#4F9F31",
                  "#FFA000",
                  "#BD2624",
                  '#421B67',
                  '#87378E',
                  '#C01B83',
                  '#00788A',
                  '#413E45')

