fluidRow(
column(4, 
       box(status = "primary",
        solidHeader = T,
        collapsible = T,
        title = "Index Selection",
        background = "black",
        width=12,
        
      checkboxInput("Mkt_Rela_CheckBox", 
                    "Relative Evolution"),
      
      conditionalPanel(condition = "input.Mkt_Rela_CheckBox==true",
                       sliderInput("Mkt_Rela_Slider", 
                                   label = "Depuis combien de jours", 
                                   min = 1, max = nrow(Mkt_data), 
                                   value = c(1), 
                                   step = 1)),
      div(style = 'color:#000000',
           multiInput("mkt_index",
                 label = 'Market Indicators (maximum 5)', 
                 choices = Mkt_index$Name)))),

column(8, 
    box(status = "primary",
    solidHeader = T,
    collapsible = F,
    title = "PRE Comparative Monitoring",
    #background = "black",
    width=12,
    
    plotlyOutput("MARKET_Plot"))))


