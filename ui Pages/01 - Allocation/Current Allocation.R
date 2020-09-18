fluidRow(
        #We plot the pie chart
        column(6, box(width=12,
                      title = "Fixed Income Composition",
                      status = "primary",
                      solidHeader = T,
                      collapsible = F,
                      plotlyOutput("CreditPlot"),
                      downloadButton("dwnCredit","Bonds portfolio (png)"))),
        
        
        #We plot the bar Graph
        column(6, box(status = "primary",
                      solidHeader = T,
                      collapsible = F,
                      title = "Portfolio by Asset Class",
                      width=12,
                      plotlyOutput("AllocPlot"),
                      downloadButton("dwnAlloc","Allocation (png)")))
        
        
        )
