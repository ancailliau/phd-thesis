Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)
library("ggrepel")
library(dplyr)

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/iba/sd.csv",
                header = TRUE, sep = ",")

breaks <- subset(data, violation_severity > 0)[,2:3]
pointsToLabel <- c() # subset(data, violation_severity > 0)$combination

p <- ggplot (data, aes(x = combination_probability, y = violation_severity))

p <- p + geom_point()
                         
p <- p + labs(x = "Combination Likelihood", y = "Violation Severity")

p <- p + scale_y_continuous(breaks = unique(breaks$violation_severity),
                            labels = percent)
p <- p + scale_x_continuous(breaks = unique(breaks$combination_probability),
                            labels = percent)

p <- p + theme(strip.background = element_blank(),
               strip.text = element_blank())

p <- p + theme(axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank())

p <- p + theme(text = element_text(family="myriad",size=8),
               axis.text.x = element_text(angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(family="myriad",size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))

img_width = 4.527559
img_height = 2.5 #4.527559

ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/iba/sd.pdf", p, width=img_width, height=img_height, dpi=300)
