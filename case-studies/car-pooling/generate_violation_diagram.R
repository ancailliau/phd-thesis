Sys.setenv(LANG = "en")

library(showtext)
library(scales)
library(ggplot2)
library(gridExtra)
library("ggrepel")
library(dplyr)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

# Variables to specify

current_folder <- "/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/car-pooling/"
data_filename <- "violation_diagram.csv"

img_filename <- "cool_violation_diagram_1.pdf"
img_width <- 4.527559
img_height <- 2.2 #4.527559

rsr <- .95

# Script to generate the plot

data_filepath <- paste(current_folder, data_filename, sep="")
out_filepath <- paste(current_folder, img_filename, sep="")

data <- read.csv(data_filepath, header = TRUE, sep = ",")

pointsToLabel <- subset(data, violation_severity > 0)$combination
breaks <- subset(data, violation_severity > 0)[,2:3]

data$facet <- ifelse(data$violation_severity > 0, 1, 2)
#data$violation_severity <- pmax(0, data$violation_severity)

p <- ggplot (data, aes(x = combination_probability, y = violation_severity))

p <- p + geom_point(data = subset(data, facet == 1))
#p <- p + geom_point(data = subset(data, facet == 2))
     
p <- p + geom_text_repel(aes(label = combination),
                         data = subset(data, combination %in% pointsToLabel),
                         box.padding = unit(5, "pt"),
                         point.padding = unit(5, "pt"),
                         segment.color = 'grey70',
                         force = 10,
                         family="myriad",
                         size=2.8222222)

p <- p + scale_y_continuous(breaks = unique(c(0,round(breaks$violation_severity, digit=2))),
                            labels = function (x) { y <- round(x, digit=2); ifelse(abs(y) < 1e-10, "SV = 0%\nRSR = ESR", percent(y)) })

p <- p + scale_x_continuous(breaks = unique(c(0,breaks$combination_probability)),
                            labels = percent, limits = c(0,.1))


p <- p + geom_hline(yintercept = 0, 
                    color="red", 
                    alpha = .5)
                         
p <- p + labs(x = "Combination Likelihood", 
              y = "Violation Severity")

#p <- p + facet_grid(facet~.,scales="free")

p <- p + theme(strip.background = element_blank(),
               strip.text = element_blank())

p <- p + theme(axis.ticks.y = element_blank())

p <- p + theme(panel.grid.minor.y = element_blank())

p <- p + theme(text = element_text(family = "myriad", size = 8),
               axis.text.x = element_text(angle = 45, hjust = 0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(family = "myriad", size = 8),
               plot.margin = unit(c(2,2,2,2), "pt"))

ggsave(file=out_filepath, p, width=img_width, height=img_height, dpi=300)
