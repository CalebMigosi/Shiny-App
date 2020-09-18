fluidRow(column(1, tags$h2(),
                dropdownButton(
                  numericInput("EConvMkt", 
                               label = "Equity Return (In Decimal)", 
                               min = -1, max = 2, value = 0, step = 0.01),
                  
                  fileInput("YieldCurve", 
                              "Yield Curve as xlsx",
                            accept = c(
                              ".xlsx")),
                  
                  materialSwitch(
                            inputId = "NSS",
                            label = "Use NSS",
                            value = T,
                            status = "success"),
                  
                  numericInput("PMkt", 
                               label = "Property Index Return (In Decimal)", 
                               min = -1, max = 2, value = 0, step = 0.01),
                  
                  circle = TRUE, 
                  status = "danger",
                  icon = icon("gear"), width = "300px",
                  
                  tooltip = tooltipOptions(title = "Click to set inputs !"))),

  column(4,
              shinyjs::useShinyjs(),
                materialSwitch(
                      inputId = "Manual",
                      label = strong(tag('center', 
                                         list(span(style='color: black; font-size: 20px', 'Use Parallel Shift')))),
                      value = F,
                      status = "success"),
         
                numericInput("ParallelShift", 
                       label = strong(tag('center', 
                                  list(span(style='color: black; font-size: 20px', 'Parallel Move in bps:')))), 
                       min = -1, 
                       max = 1, 
                       value = 0, 
                       step = 0.05)),
  valueBoxOutput("Variation_Plot"))
