fluidRow(column(10, checkboxGroupButtons(
  inputId = "countries",
  label = strong(tag('center', 
                     list(span(style='color: black; font-size: 20px', 'Select Countries for Comparative Analysis:')))),
  choices = unique(Data_EQ_CLUST$COUNTRY),
  status = "primary"),
  offset = 1),
  
  column(1, 
         tags$h2(),
         br(),
         dropdown(
           tags$h3(),

           
           radioGroupButtons(
             inputId = "camembertEquity",
             label = strong(tag('center', 
                                list(span(style='color: black; font-size: 20px', 'Choose a graph:')))), 
             choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", 
                         `<i class='fa fa-pie-chart'></i>` = "pie"),
             justified = TRUE),
           
           actionButton("Comparative", "Comparative Plot"),
           
           # radioGroupButtons(
           #   inputId = "comparative",
           #   label = strong(tag('center', 
           #                      list(span(style='color: black; font-size: 20px', 'Comparative UI')))), 
           #   choices = c("Show", "Hide"),
           #   justified = TRUE),
           
           style = "gradient",
           circle = T, 
           status = "warning",
           icon = icon("gear"), 
           width = "500px",
           right = T, 
           tooltip = tooltipOptions(title = "Click to change to camembert!"),
           animate = animateOptions(
             enter = animations$fading_entrances$fadeInLeftBig,
             exit = animations$fading_exits$fadeOutRightBig
           ))))