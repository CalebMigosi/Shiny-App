fluidRow(
column(4,  

box(status = "primary",
                solidHeader = T,
                collapsible = F,
                background = "black",
                title = "FERP Triggers",
                #background = "black",
                width=12,
                height = 800,
    
                radioGroupButtons(
                  inputId = "Freq",
                  label = "Frequence",
                  choices = c("WTW","MTM","QTQ","YTY"), 
                  selected = "WTW",
                  status = "danger"),
                prettyRadioButtons("Trig",
                                   "Trigger", 
                                   choices = Triggers$Indicators, 
                                   selected = "Action Eurostoxx 50"))),

column(8, box(status = "primary",
              solidHeader = T,
              collapsible = F,
              title = "Market Graph",
              background = "black",
              width=12,
              dygraphOutput("TRIGGERS_Plot", 
                            height = 570)))
)

