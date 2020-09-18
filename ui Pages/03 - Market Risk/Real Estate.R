fluidRow(
  # tags$head(tags$style(HTML('/* body */
  #                               .content-wrapper, .right-side {
  #                               background-color: #202020;
  #                               }
  #                               
  #                               .box.box-solid.box-primary>.box-header {
  #                               
  #                               }
  #                               
  #                               .box.box-solid.box-primary{
  #                               
  #                               background:#808080
  #                               }
  #                               '))),
column(8, box(status = "primary",
                solidHeader = F,
                collapsible = F,
                background = "black",
                width=12,
                checkboxInput("heatMapCheckBox", 
                              "Investissements par zone"),
    
                conditionalPanel(condition = "input.heatMapCheckBox==true",
                                 sliderInput("heatMapSlider", 
                                             label = "Slider Range", 
                                             min = 0, max = 1, 
                                             value = c(1), 
                                             step = 0.02)),
    
                leafletOutput("map_RE",height = 800))),
    # absolutePanel(top=20, left=70, textInput("Adresse", "","Ex: Bamako")) manque acces API

valueBoxOutput("MV_RE_known"),

box(width=4,
    tableOutput("MV_RE_type")))

