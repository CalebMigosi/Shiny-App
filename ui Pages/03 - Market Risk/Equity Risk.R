fluidRow(bsModal("test", "Graphs", "Comparative", size = "large",  plotlyOutput("Sector_CountryEquity")),
  #We plot the pie chart
  column(6, box(width=12,
                title = "Sector Decomposition",
                status = "primary",
                solidHeader = T,
                collapsible = F,
                plotlyOutput("SectorEquity"))),
  
  
  #We plot the bar Graph
  column(6, box(status = "primary",
                solidHeader = T,
                collapsible = F,
                title = "Country Decomposition",
                width=12,
                plotlyOutput("CountryEquity")))
)