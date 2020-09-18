fluidRow(
  column(6, box(status = "primary",
                solidHeader = T,
                collapsible = F,
                title = "Geographical/Sectorial Decomposition",
                #background = "black",
                width=12,
                
                  #We select the date on all plots in the "Current Allocation" page
                  radioGroupButtons(
                    inputId = "plotCREDITGeo",
                    label = "",
                    choices = list("Financial", "Sovereign", "Non-Financial", "Total"),
                    status = "primary"),
                
                #Plot the graph
                plotlyOutput("CREDIT_PAYS"),
                downloadButton("dwnCredit_PAYS",
                   "Bonds portfolio geo zone (png)"))),

  column(6, box(status = "primary",
                solidHeader = T,
                collapsible = F,
                title = "Sectorial/Rating Decomposition",
                #background = "black",
                width=12,
                  #We select the date on all plots in the "Current Allocation" page
                  radioGroupButtons(
                    inputId = "plotCREDIT",
                    label = "",
                    choices = list("Financial", "Sovereign", "Non-Financial"),
                    status = "primary"),
                plotlyOutput("CREDIT_RAT"),
                downloadButton("dwnCredit_RAT",
                   "Bonds portfolio rating (png)"))))