################## Configurations graphique communes #################
#### Couleurs discretes pour feuille PRE
# Couleurs Aviva
# rgb(0, 79, 182):bleu => en % rgb(0%,31%,71%)
# rgb(255,217,0):jaune => en % rgb(100%,85%,0%)
scale_Aviva <- function(n){
  scale_Aviva <- rgb(1,0.85,0)
  if(n>1) {
    for (i in 2:n) {
        scale_Aviva <- c(scale_Aviva,rgb(1-(i-1)*1/(n-1),0.85-(i-1)*(0.85-0.31)/(n-1),0+(i-1)*0.71/(n-1)))
    }
  }
  return(scale_Aviva)
}


#### Theme global ''''''''''''''''''''''''''''''''''''''
theme_Aviva <- function () {
  theme_minimal() %+replace% 
    theme(
      # panel.background  = element_blank(),
      # plot.background = element_rect(fill="gray96", colour=NA), 
      # legend.background = element_rect(fill="transparent", colour=NA),
      # legend.key = element_rect(fill="transparent", colour=NA),
      axis.text.x  = element_text(size = 10, colour = "black",angle =0),
      plot.title = element_text(color="black", size=15, face="bold.italic",hjust = 0.5),
      axis.title.x = element_text(color="black", size=12, face="italic"),
      axis.title.y = element_text(color="black", size=12, face="italic",angle=90)
    )
}

#### Graphique plot_ly ''''''''''''''''''''''''''''''''''
Axes_plot_ly <- list(
  face="italic",
  size = 16,
  color = "black"
)


# scale_Aviva <- scales::seq_gradient_pal("blue2", "blue4", "Lab") #(seq(0,1,length.out=100))
# 
# 
# scale_Aviva_RAT <- scales::seq_gradient_pal("yellow", "blue4", "Lab") (seq(0,1,length.out=16))
# scale_Aviva_RAT2 <- c("green", "green2", "green4","greenyellow",
#                      "yellow","yellow2","yellow")

# 