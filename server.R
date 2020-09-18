shinyServer(function(input, output) {
    
    "1. Current Allocation"
"=========================================================================================================="  
    output$AllocPlot <- renderPlotly({
       AfficheAlloc(input$PerCREDIT,input$DateCREDIT, input$camembertCREDIT)
 })
    
    output$CreditPlot <- renderPlotly({
        AfficheCredit(input$PerCREDIT,input$DateCREDIT, input$camembertCREDIT)
    })

    
    "2. Historical Allocation"
"=========================================================================================================="    
    output$HistAllocPlot <- renderPlotly({
        AfficheHistAlloc(input$PerHist, c(input$DateGraph1,input$DateGraph2))
    })
    
    output$pieAfficheHistAlloc <- renderPlotly({
        pieAfficheHistAlloc(input$PerHist, c(input$DateGraph1,input$DateGraph2))
    })

    
    "3. PRE"
"=========================================================================================================="    
    output$PRETOTPlot <- renderPlotly({
        AffichePRETOT(input$PerPRE, DatesRep = c(input$Date1PRE,input$Date2PRE))
    })
    
    output$PRECATPlot <- renderPlotly({
        AffichePRECAT(input$PerPRE, DatesRep = c(input$Date1PRE,input$Date2PRE))
    })

    "4. Credit"
"=========================================================================================================="      
    output$CREDIT_UPC <-  DT::renderDataTable({AfficheCREDIT_UPC(input$PerimCREDIT, input$DatePerimCREDIT)})
    
    output$CREDIT_PAYS <- renderPlotly({
        AfficheCREDIT_PAYS(input$PerimCREDIT, input$DatePerimCREDIT,input$plotCREDITGeo)
    })
    
    output$CREDIT_RAT <- renderPlotly({
        AfficheCREDIT_RAT(input$PerimCREDIT, input$DatePerimCREDIT, input$plotCREDIT)
    })
    

    "5. Equity"
"=========================================================================================================="    
    
    output$Sector_CountryEquity <- renderPlotly({
        if(length(input$SelecetedVars) > 2)
        {
            updateCheckboxGroupInput("countries", selected= tail(input$countries,2))
        }

        sector_country_plot(input$countries, input$camembertEquity)
    })
    
    output$CountryEquity <- renderPlotly({
        country_plot(input$camembertEquity)
    })
    
    output$SectorEquity <- renderPlotly({
        sector_plot(input$camembertEquity)
    })
    
    # output$plots <- renderUI({
    #     plotlyOutput("Sector_CountryEquity")})
    # 
    # 
    "6. Market Indicators"
"=========================================================================================================="    

    output$MARKET_Plot <- renderPlotly({
        Affiche_MARKET_Plot(input$Mkt_Rela_CheckBox, input$Mkt_Rela_Slider, input$mkt_index)
    })
    
    output$MARKET_Table1 <- DT::renderDataTable({Affiche_MARKET_Table(input$num1)[[1]]})
    
    output$MARKET_Table2 <- DT::renderDataTable({Affiche_MARKET_Table(input$num2)[[2]]})
 
       
    "7. FERP"
"=========================================================================================================="
    output$TRIGGERS_Plot <- renderDygraph(Affiche_TRIGGER_Plot(input$Trig, input$Freq))

 
       
    "8. Solvency Quantiles"
"=========================================================================================================="    
    output$OF_Evol_YTD <- DT::renderDataTable({
        
        dfE <- data.frame(Risk_Factor = List_RF,
                        Perf_YTD = c(Perf_EConv, 0, 0, 0, 0, 0, 0, 0))
        
        DT::datatable(dfE, rownames = F, options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))%>%
            formatStyle(1:ncol(dfE), color = 'black')
        })
    
    # # dat$date <- format(dat$date,'%Y-%m-%d')
    output$OF_Am_Tot <- DT::renderDataTable({
        
        dfTot <- Loss_Tot()
        dfTot[, 2] <- round(dfTot[, 2], 2)
        
        DT::datatable(dfTot, rownames = F, options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))%>%
        formatStyle(1:ncol(dfTot), color = 'black')})
    
    output$OF_Var_Gran <- DT::renderDataTable({
        
        dfVar <- data.frame(Risk_Factor = List_RF,
                            Gain_Singlewise = c(round(singlewise(List_RF[1], EConv_fun(Perf_EConv/100)), 1)/1000,
                                                                      0, 0, 0, 0, 0, 0, 0))
        DT::datatable(dfVar, rownames = F, options = list(
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"),
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))%>%
                formatStyle(1:ncol(dfVar), color = 'black')
        })

    
    "1. Singlewise decomposition"
"============================================================================================================="
    output$SinglewisePlot <- renderPlotly({ 
       
         if(input$Quantile == T){
        singlewise_plot(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                        c(input$EConv, input$FL, input$CCorp, input$CSov, input$P, input$EqVol,
                          input$FVol, input$Infl))}
        
        else{
         singlewise_plot2(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                          c(EConv_fun(input$EConv2), FL_fun(PCAcurve+input$FL2) , input$CCorp2, input$CSov2, P_fun(input$P2), input$EqVol2,
                            input$FVol2, input$Infl2))} 
    })
    
 
       
    "2. Loss decomposition"    
"============================================================================================================="
    output$Total_Loss_Plot <- renderPlotly({ 
        
        if(input$Quantile == T){
        total_loss_plot(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                        c(input$EConv, input$FL, input$CCorp, input$CSov, input$P, input$EqVol,
                          input$FVol, input$Infl))}
            else{
            total_loss_plot2(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                             c(EConv_fun(input$EConv2), FL_fun(PCAcurve+input$FL2) , input$CCorp2, input$CSov2, P_fun(input$P2), input$EqVol2,
                               input$FVol2, input$Infl2))} 
        })
    

    "3. OF Variation"    
"============================================================================================================="    
    output$OF_Variation <- renderValueBox({
            
                   
        valueBox(
            #Since Loss Functions give losses as positive
        value = ifelse(input$Quantile == T, 
                       total_loss_value(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                                 c(input$EConv, input$FL, input$CCorp, input$CSov, input$P, input$EqVol,
                                   input$FVol, input$Infl)),
                       total_loss_value2(c("EConv", "FL", "CCorp", "CSov", "P", "EqVol", "FVol", "Infl"), 
                                         c(EConv_fun(input$EConv2), FL_fun(PCAcurve+input$FL2) , input$CCorp2, input$CSov2, P_fun(input$P2), input$EqVol2,
                                           input$FVol2, input$Infl2))),
        
                    subtitle = "Own Funds variation (m€) under defined scenario",
                    icon = icon("fas fa-building"),
                    color = "red")})
    
    
    output$OF_Value <- renderValueBox({
        valueBox(
            #Since Loss Functions give losses as positive
            value = round(Loss_Tot()[1, 2],2),
            subtitle = "Own Funds Aviva Vie Solo (m€) at 31-12-2018",
            icon = icon("fas fa-building"),
            color = "blue") })
    
    output$OF_Var_Tot <- renderValueBox({
        valueBox(
            value = round(singlewise(c("EConv", "FL"),
                                     c(EConv_fun(Perf_EConv/100), 0)) / 1000, FL_fun(PCAcurve)),
            subtitle = paste("YTD variation (m€) from", Date_LF, sep = " "),
            # value = (readRDS("./04 - Data Calc/01 - OFs/Data_OFs_CR.Rds") %>%
            #            select("Own_Fund Aviva Vie Solo") %>% last),
            # subtitle = paste ("variation (m€) sur Aviva Vie Solo depuis le ",Date_LF,sep=""),
            icon = icon("fas fa-building"),
            color = "green"
        )})





"9. Market to risk Factors"
"==========================================================================================================" 
filedata <- reactive({
    inFile <- input$YieldCurve
    if (is.null(inFile)){
        return(NULL)
    }
    
    tbl <- read_excel(inFile$datapath, col_names = FALSE)
    return(tbl)
})

output$Variation_Plot <- renderValueBox({
    df <- filedata()
    EConvVariation <- singlewise("EConv", EConv_fun(input$EConvMkt))
    FLVariation <- ifelse(input$Manual == TRUE,
                singlewise("EConv", FL_fun(PCAcurve+input$ParallelShift, NSS = T)),
           ifelse(is.null(df), 0, singlewise("FL", FL_fun(df, NSS = input$NSS))))
    PVariation <- singlewise("P", P_fun(input$PMkt))

        valueBox(
            value = EConvVariation + FLVariation+PVariation,
            subtitle = "Total Variation",
            icon = icon("fas fa-building"),
            color = "green")
})


observeEvent(input$Manual, {
    if(input$Manual == TRUE){
        shinyjs::enable("ParallelShift")
    }else{
        shinyjs::reset("ParallelShift")
        shinyjs::disable("ParallelShift")
    }
})


output$ImpactPlot <- renderPlotly({
    df <- filedata()
    if(input$Manual == TRUE){
        singlewise_plot2(c("EConv", "FL", "P"), c(EConv_fun(input$EConvMkt), 
                                                  FL_fun(PCAcurve+input$ParallelShift, NSS = T),
                                                  P_fun(input$PMkt)))}
    else{
        singlewise_plot2(c("EConv", "FL", "P"), c(EConv_fun(input$EConvMkt), 
                                             ifelse(is.null(df), 0, FL_fun(df, NSS = input$NSS)),
                                             P_fun(input$PMkt)))}
    })

})