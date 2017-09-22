Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)
library("ggrepel")
library(dplyr)

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/violation_diagram_single_v.csv",
                header = TRUE, sep = ",")
rsr <- .99

pointsToLabel <- subset(data, violation_severity > 0)$combination
                

breaks <- subset(data, violation_severity > 0)[,2:3]
#breaks$uncertainty_spread <- round(breaks$uncertainty_spread, digits = 2)
#breaks$violation_uncertainty <- round(breaks$violation_uncertainty, digits = 4)

print(breaks)

data$facet <- ifelse(data$violation_severity == 0, 1, 2)
data$violation_severity <- pmax(.0000001, data$violation_severity)

p <- ggplot (data, aes(x = combination_probability, y = violation_severity))

p <- p + geom_point(data = subset(data, facet == 1))
p <- p + geom_point(data = subset(data, facet == 2))
     
p <- p + geom_text_repel(aes(label = combination),
                         data = subset(data, combination %in% pointsToLabel),
    box.padding = unit(5, "pt"),
    point.padding = unit(5, "pt"),
    segment.color = 'grey70',
                         force = 10,
                         family="myriad",size=2.8222222)
                         
p <- p + labs(x = "Combination Likelihood", y = "Violation Severity (Log10)")


p <- p + scale_y_continuous(breaks = unique(c(0.0000001,round(breaks$violation_severity, digit=2))),
                            labels = function (x) { round(x, digit=2) }, trans=log10_trans())

p <- p + scale_x_continuous(breaks = unique(c(0,breaks$combination_probability)),
                            labels = function(x) { lapply(x, function(y) { if (is.na(y) | y <= .000001) { percent(0) } else { percent(y) } } ) }, 
)#                            trans=log2_trans())

p <- p + facet_grid(~facet, scales = 'free', space = 'free')

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

ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/violation_diagram_single_value.pdf", p, width=img_width, height=img_height, dpi=300)
