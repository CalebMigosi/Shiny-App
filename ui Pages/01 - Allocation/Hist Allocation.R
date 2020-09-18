fluidRow(
  #Plot Graph 1 ******Remember to connect date to the name*********
  column(6, box(status = "primary",
                solidHeader = T,
                collapsible = F,
                title = "Comparison of changes in allocation",
                #background = "black",
                width=12,
                
                #Plot the output at Date1
                plotlyOutput("HistAllocPlot"), #Plot the graph on date 1
                downloadButton("dwnHistAlloc","Record (png)"))),
  
  column(6, box(status = "primary",
                solidHeader = T,
                collapsible = F,
                title = "Comparison of changes in allocation (Pie)",
                #background = "black",
                width=12,
                
                #Plot the output at Date1
                plotlyOutput("pieAfficheHistAlloc"), #Plot the graph on date 1
                downloadButton("dwnpieHistAlloc","Record (png)"))))

  
  # #Plot Graph 2 ******Remember to connect date to the name*********
  # column(6, box(status = "primary",
  #               solidHeader = T,
  #               collapsible = F,
  #               title = "Portfolio by Asset Class2",
  #               #background = "black",
  #               width=12,
  # 
  #               #Plot the output at Date2
  #               plotOutput("HistAllocPlot2"), #Plot the graph on date 2
  #         downloadButton("dwnHistAlloc","Record (png)"))))
