fluidPage(

  column(6,
    box(status = "primary",
            solidHeader = T,
            collapsible = F,
            width = 12,
            title = "Market Indicator Performance (%)",
        sliderTextInput(
          inputId = "num1",
          label = "Relative Performance (In Days):", 
          choices = c(1:100),
          grid = TRUE,
          selected = 5),
              DT::dataTableOutput("MARKET_Table1"))),
  
  column(6,
         box(status = "primary",
             solidHeader = T,
             collapsible = F,
             width = 12,
             title = "Market Indicator Performance (Abs)",
             sliderTextInput(
               inputId = "num2",
               label = "Relative Performance (In Days):", 
               choices = c(1:100),
               grid = TRUE,
               selected = 5),
             DT::dataTableOutput("MARKET_Table2"))))
