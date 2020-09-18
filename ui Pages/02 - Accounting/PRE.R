fluidRow(

  #Plot Graph 1 ******Remember to connect date to the name*********
column(4, box(status = "primary",
              solidHeader = T,
              collapsible = F,
              title = "PRE Comparative Monitoring",
              #background = "black",
              width=12,
              
              #Plot the graph
              plotlyOutput("PRETOTPlot"),
              downloadButton("dwnPRETOT",
                   "URGL (R3343-10) (png)"))),

column(8, box(status = "primary",
             solidHeader = T,
             collapsible = F,
             title = "Comparative PRE by Asset Class",
             #background = "black",
             width=12,
             
             #Plot the Graph
             plotlyOutput("PRECATPlot"),
             downloadButton("dwnPRECAT","URGLL (R3343-10) by category (png)"))))