Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)
library("ggrepel")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/violation_diagram_1.csv",
                header = TRUE, sep = ",")

data2 = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/violation_diagram_1_single_expert.csv",
                header = TRUE, sep = ",")

pointsToLabel <- c()
                
data$facet <- ifelse(data$violation_uncertainty == 0, 2, 1)
data2$facet <- ifelse(data2$violation_uncertainty == 0, 2, 1)

data$violation_uncertainty <- pmax(.0000001, data$violation_uncertainty)
data2$violation_uncertainty <- pmax(.0000001, data2$violation_uncertainty)

breaks <- subset(data, violation_uncertainty > 0)[,2:3]
#breaks$uncertainty_spread <- round(breaks$uncertainty_spread, digits = 2)
#breaks$violation_uncertainty <- round(breaks$violation_uncertainty, digits = 4)

print(data)

p <- ggplot (data, aes(y = violation_uncertainty, x = uncertainty_spread))

p <- p + geom_point(data = subset(data, facet == 1), aes(colour = "a", shape = "a"))
p <- p + geom_point(data = subset(data, facet == 2), aes(colour = "a", shape = "a"))

p <- p + geom_point(data = subset(data2, facet == 2), aes(colour = "b", shape = "b"))
p <- p + geom_point(data = subset(data2, facet == 1), aes(colour = "b", shape = "b"))
     
p <- p + geom_text_repel(aes(label = combination),
                         data = subset(data, combination %in% pointsToLabel),
    box.padding = unit(5, "pt"),
    point.padding = unit(5, "pt"),
    segment.color = 'grey70',
                         force = 10,
                         family="myriad",size=2.8222222)
                         
p <- p + labs(y = "Violation Uncertainty (Log2)", x = "Uncertainty Spread")

p <- p + scale_x_continuous(breaks = c(0,.2,.4,.6),
                            labels = function (x) round(x, digit=2))

p <- p + scale_y_continuous(breaks = c(0,.0000001,.125,.25,.5,1),
                            labels = function(x) { lapply(x, function(y) { if (is.na(y) | y <= .000001) { percent(0) } else { percent(y) } } ) }, 
                            trans = log2_trans())

p <- p + scale_color_manual(
         name = NULL, 
         labels = c("Experts 1 to 5 combined", "Expert 1"), 
         breaks = c("a", "b"),
         values = cbPalette) +
     scale_shape_discrete(
         name = NULL, 
         labels = c("Experts 1 to 5 combined", "Expert 1"),
         breaks = c("a", "b")
     )
     
p <- p + theme(legend.position = c(.86,.28)) + labs(col=NULL) + 
         theme(legend.key = element_rect(colour = NA, fill = NA), legend.margin=margin(t = c(-.1,.2,0,0), unit='cm')) +
         theme (legend.key = element_rect(size = 5),
                 legend.key.size = unit(.9, 'lines'))

p <- p + facet_grid(facet~., scales = 'free', space = 'free')

p <- p +   theme(strip.background = element_blank(),
        strip.text = element_blank())

p <- p + theme(
            axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank())

p <- p + theme(text = element_text(family="myriad",size=8),
axis.text.x = element_text(angle=45,hjust=0.95),
            panel.grid.minor = element_blank(),
            plot.title = element_text(family="myriad",size=8),
            plot.margin = unit(c(2,2,2,2),"pt"))

img_width = 4.527559
img_height = 2.5 #4.527559

ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/violation_diagram_1.pdf", p, width=img_width, height=img_height, dpi=300)
