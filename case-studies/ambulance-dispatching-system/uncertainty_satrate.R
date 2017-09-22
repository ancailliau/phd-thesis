Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/uncertainty_sat_rate_achieve_incident_resolved.csv",
                header = TRUE, sep = ",")

img_width = 4.527559
#img_width = 2
img_height = 1.2
#img_height = .8

min_value = 0
max_value = 1

dens <- density(data$achieve_incident_resolved, from=min_value, to=max_value, adjust = .1)
dd <- with(dens,data.frame(x,y))

p <- qplot(x,y,data = dd, geom = "line")

# p <- qplot(x,y,data=dd, geom="line")
# p <- qplot(data$pump_failure, geom="histogram", bins = 10)

#p <- ggplot(data, aes(x=pump_failure)) + 
#     geom_histogram(aes(y=..density..), binwidth=.005, color="black", fill="white")

p <- p + labs(x = NULL, y = NULL)

p <- p + scale_x_continuous(limits = c(min_value,max_value),
                            breaks = c(min_value,max_value,.95),
                            labels = percent)
                            

p <- p + geom_vline(xintercept = .95, 
                    color="red", 
                    alpha = .5)

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())

p <- p + theme(text = element_text(family="myriad",size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(family="myriad",size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))

# print(p)
# ggsave(file="uso_pump_failure_of5.pdf", p, width=img_width, height=img_height, dpi=300)
ggsave(file="uncertainty_sat_rate_achieve_incident_resolved.pdf", p, width=img_width, height=img_height, dpi=300)
