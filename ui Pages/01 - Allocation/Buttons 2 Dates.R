fluidRow(
         #Select Perimeter   
    column(10, 
              checkboxGroupButtons(
                      inputId = "PerHist",
                      label = strong(tag('center', 
                                         list(span(style='color: black; font-size: 20px', 'Select Perimeter:')))),
                      choices = Per,
                      status = "primary",
                      selected = "AER SHF",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon"))), offset = 1),
  column(1, 
         tags$h2(),
         br(),
         dropdown(
           tags$h3(),
           
           #We select the date on all plots in the "Current Allocation" page
           pickerInput(inputId = "DateGraph1", 
                       label = strong(tag('center', 
                                          list(span(style='color: black; font-size: 20px', 'Select First Date:')))),
                       choices = list(`Dates disponibles` = as.character(DatesRep)),
                       options = list(`style` = "btn-default btn-lg"),
                       choicesOpt = list(
                         style = rep(("color: black; background: darkgrey; 
                                      font-weight: bold; 
                                      font-size: 18px"),10))),
           
           pickerInput(inputId = "DateGraph2", 
                       label = strong(tag('center', 
                                          list(span(style='color: black; font-size: 20px', 'Select Second Date:')))),
                       choices = list(`Dates disponibles` = as.character(DatesRep)),
                       options = list(`style` = "btn-default btn-lg"),
                       choicesOpt = list(
                         style = rep(("color: black; background: darkgrey; 
                                      font-weight: bold; 
                                      font-size: 18px"),10))),
           style = "gradient",
           circle = T, 
           status = "warning",
           icon = icon("gear"), 
           width = "500px",
           right = T, 
           tooltip = tooltipOptions(title = "Click to change to camembert!"),
           animate = animateOptions(
             enter = animations$fading_entrances$fadeInLeftBig,
             exit = animations$fading_exits$fadeOutRightBig))))
