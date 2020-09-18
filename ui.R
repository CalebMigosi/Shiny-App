# Define UI for application that draws a histogram
header <- dashboardHeader(disable = T)
sidebar<- dashboardSidebar(disable = T)

"HTML Preliminary"
#We set the color for the background of our boxes
html_setup <- tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#222d32
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#8dacc3;
                    color: #000000
                    }
                    "))


"DEFINITION OF PAGES"
"======================================================================================================="
"1. Allocation Page "
CurrentAllocation <- dashboardBody(html_setup,
                                   source("./ui Pages/01 - Allocation/Buttons.R", local = T)$value,
                                   source("./ui Pages/01 - Allocation/Current Allocation.R", local = T)$value)
                                   

HistAllocation <- dashboardBody(source("./ui Pages/01 - Allocation/Buttons 2 Dates.R", local = T)$value,
                                source("./ui Pages/01 - Allocation/Hist Allocation.R", local = T)$value)

"2. Accounting Page "
PRE <- dashboardBody(source("./ui Pages/02 - Accounting/Buttons.R", local = T)$value,
                     source("./ui Pages/02 - Accounting/PRE.R", local = T)$value)


"3.Market Risk Page "
CreditRisk <- dashboardBody(source("./ui Pages/03 - Market Risk/Buttons.R", local = T)$value,
                            source("./ui Pages/03 - Market Risk/Credit Risk.R", local = T)$value,
                            source("./ui Pages/03 - Market Risk/Credit Risk Table.R", local = T)$value)

EquityRisk <- dashboardBody(source("./ui Pages/03 - Market Risk/Buttons Equity.R", local = T)$value,
                            source("./ui Pages/03 - Market Risk/Equity Risk.R", local = T)$value,
                            conditionalPanel("input.comparative == 'Show'" , uiOutput("plots")))

Property <- dashboardBody(source("./ui Pages/03 - Market Risk/Real Estate.R", local = T)$value)


"4. Market Indicators/ FERP"
MarketIndicators <- dashboardBody(source("./ui Pages/04 - Market Indicators/Market Indicator Graph.R", local = T)$value,
                                  source("./ui Pages/04 - Market Indicators/Market Indicator Table.R", local = T)$value)

FERP <- dashboardBody(source("./ui Pages/04 - Market Indicators/FERP.R", local = T)$value)

"5. Own Funds Page"
OFVariation <- dashboardBody(source("./ui Pages/05 - Own Funds/Buttons.R", local = T)$value,
                              source("./ui Pages/05 - Own Funds/OF Variation.R", local = T)$value)

Mkt_to_RF <- dashboardBody(source("./ui Pages/05 - Own Funds/Buttons 2.R", local = T)$value,
                           source("./ui Pages/05 - Own Funds/Market To RF.R", local = T)$value)

"==================================================================================================="

"Define the global page. Insert specific elements for each tab"
fluidPage(theme = shinytheme("darkly"),
  navbarPage(title = div(span(img(src="LogoAviva.png",
                                  height = 30,
                                  width = 70))),
    #Allocation Page           
    navbarMenu("Allocation",
                tabPanel("Current Allocation",
                         dashboardPage(header,sidebar,CurrentAllocation)),
                tabPanel("Record",
                        dashboardPage(header,sidebar,HistAllocation))),
    
    #ACcounting Page
    navbarMenu("Accounting",
               tabPanel("PRE Monitoring",
                        dashboardPage(header,sidebar,PRE))),
    #            tabPanel("Cash Flow Timeline",
    #                     dashboardPage(header,sidebar,CurrentAllocation))),

    #Market Risks Page
    navbarMenu("Market Risks",
               tabPanel("Credit Risk",
                        dashboardPage(header,sidebar,CreditRisk)),
    #            tabPanel("Property Risk",
    #                     dashboardPage(header,sidebar,Property)),
               tabPanel("Equity Risk",
                        dashboardPage(header,sidebar, EquityRisk))),
    # ))
    # 
    #Market indicators Page
    navbarMenu("Market Indicators",
               tabPanel("Market Overview",
                        dashboardPage(header,sidebar,MarketIndicators)),
               tabPanel("FERP Indicators",
                        dashboardPage(header,sidebar,FERP))),

    #Solvency Page
    navbarMenu("Solvency",
               tabPanel("OF Variation",
                        dashboardPage(header,sidebar,OFVariation)),
               tabPanel("Market to RF",
                        dashboardPage(header,sidebar,Mkt_to_RF)))
    )
)



