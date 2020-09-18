fluidRow(
  column(8, box(title = "Decomposition Table",
                  status = "primary",
                  #background = "black",
                  solidHeader = T,
                  collapsible = T,
                  width = 12,
                  DT::dataTableOutput("CREDIT_UPC"),
                  downloadButton("dwnCredit_UPC","Bonds portfolio UPCs (csv)"))))