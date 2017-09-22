Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/uso_pump_failure_of5.csv",
                header = TRUE, sep = ",")

generate_plot <- function (filename, item, img_width, img_height, min_value, max_value) {

    data = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", filename, ".csv", sep=""),
                    header = TRUE, sep = ",")

    dens <- density(data[[item]], from=min_value, to=max_value, adjust = .1)
    dd <- with(dens,data.frame(x,y))

    p <- qplot(x,y,data = dd, geom = "line")

    p <- p + labs(x = NULL, y = NULL)

    p <- p + scale_x_continuous(limits = c(min_value,max_value),
                                breaks = c(min_value,max_value),
                                labels = percent)

    p <- p + theme(axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

    p <- p + theme(panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank())

    p <- p + theme(text = element_text(family="myriad",size=8,angle=45,hjust=0.95),
                   panel.grid.minor = element_blank(),
                   plot.title = element_text(family="myriad",size=8),
                   plot.margin = unit(c(2,2,2,2),"pt"))
                   
    ggsave(file=paste(filename, ".pdf", sep=""), p, width=img_width, height=img_height, dpi=300)
}

generate_plot_goal <- function (filename, item, img_width, img_height, min_value, max_value) {

    data = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", filename, ".csv", sep=""),
                    header = TRUE, sep = ",")

    dens <- density(data[[item]], from=min_value, to=max_value, adjust = .1)
    dd <- with(dens,data.frame(x,y))

    p <- qplot(x,y,data = dd, geom = "line")

    p <- p + labs(x = NULL, y = NULL)

    p <- p + scale_x_continuous(limits = c(min_value,max_value),
                                breaks = c(min_value,max_value),
                                labels = percent)

    p <- p + theme(axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

    p <- p + theme(panel.grid.minor.y = element_blank(),
                   panel.grid.major.y = element_blank())

    p <- p + theme(text = element_text(family="myriad",size=8,angle=45,hjust=0.95),
                   panel.grid.minor = element_blank(),
                   plot.title = element_text(family="myriad",size=8),
                   plot.margin = unit(c(2,2,2,2),"pt"))
                   
    ggsave(file=paste(filename, ".pdf", sep=""), p, width=img_width, height=img_height, dpi=300)
}

img_width = 2
img_height = .8

min_value = 0
max_value = .5

generate_plot("quantiles_make_up_water_provided_e1",   "make_up_water_provided", img_width, img_height, min_value, max_value);
generate_plot("quantiles_make_up_water_provided_e2",   "make_up_water_provided", img_width, img_height, min_value, max_value);
generate_plot("quantiles_make_up_water_provided_cook", "make_up_water_provided", img_width, img_height, min_value, max_value);
generate_plot("quantiles_make_up_water_provided_ms",   "make_up_water_provided", img_width, img_height, min_value, max_value);

#img_width = 4.527559
img_height = 1.2

min_value = 0
max_value = 1

# p1 <- generate_plot_goal("quantiles_pump_motor_on_e1",   "pump_motor_on", img_width, img_height, min_value, max_value);
# p2 <- generate_plot_goal("quantiles_pump_motor_on_e2",   "pump_motor_on", img_width, img_height, min_value, max_value);
# p3 <- generate_plot_goal("quantiles_pump_motor_on_cook", "pump_motor_on", img_width, img_height, min_value, max_value);
# p4 <- generate_plot_goal("quantiles_pump_motor_on_ms",   "pump_motor_on", img_width, img_height, min_value, max_value);

library("reshape2")
library("sfsmisc")

item = "make_up_water_provided"
min_value = 0
max_value = 1
adjust = .1

data1 = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", "quantiles_make_up_water_provided_e1", ".csv", sep=""),
                 header = TRUE, sep = ",")
dens <- density(data1[[item]], from=min_value, to=max_value, adjust = adjust)
d1 <- with(dens,data.frame(x,y))
print(integrate.xy(d1$x,d1$y))
area <- integrate.xy(d1$x,d1$y)
d1$y <- d1$y/area

data2 = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", "quantiles_make_up_water_provided_e2", ".csv", sep=""),
                 header = TRUE, sep = ",")
dens <- density(data2[[item]], from=min_value, to=max_value, adjust = adjust)
d2 <- with(dens,data.frame(x,y))
print(integrate.xy(d2$x,d2$y))
area <- integrate.xy(d2$x,d2$y)
d2$y <- d2$y/area

data3 = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", "quantiles_make_up_water_provided_cook", ".csv", sep=""),
                 header = TRUE, sep = ",")
dens <- density(data3[[item]], from=min_value, to=max_value, adjust = adjust)
d3 <- with(dens,data.frame(x,y))
print(integrate.xy(d3$x,d3$y))
area <- integrate.xy(d3$x,d3$y)
d3$y <- d3$y/area

data4 = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", "quantiles_make_up_water_provided_ms", ".csv", sep=""),
                 header = TRUE, sep = ",")
dens <- density(data4[[item]], from=min_value, to=max_value, adjust = adjust)
d4 <- with(dens,data.frame(x,y))
print(integrate.xy(d4$x,d4$y))
area <- integrate.xy(d4$x,d4$y)
d4$y <- d4$y/area

data <- data.frame(x = d1$x, expert1 = d1$y, expert2 = d2$y, ms = d4$y, cook = d3$y)

max_h <- max(data$expert1,data$expert2,data$ms,data$cook)

p <- ggplot(data=data, aes(x=x)) +
  geom_line(aes(y = expert1), alpha = .3, size = .25) +
  geom_line(aes(y = expert2), alpha = .3, size = .25) +
  geom_line(aes(y = ms), size = .5)

p <- p + labs(x = NULL, y = NULL)

p <- p + scale_x_continuous(limits = c(min_value,max_value),
                            breaks = c(min_value,max_value),
                            labels = percent)

p <- p + scale_y_continuous(limits = c(0,max_h))

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())

p <- p + theme(text = element_text(size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))
               
ggsave(file="quantiles_make_up_water_provided_ms.pdf", p, width=img_width, height=img_height, dpi=300)


p <- ggplot(data=data, aes(x=x)) +
  geom_line(aes(y = expert1), alpha = .3, size = .25) +
  geom_line(aes(y = expert2), alpha = .3, size = .25) +
  geom_line(aes(y = cook), size = .5)

p <- p + labs(x = NULL, y = NULL)

p <- p + scale_x_continuous(limits = c(min_value,max_value),
                            breaks = c(min_value,max_value),
                            labels = percent)
                            
p <- p + scale_y_continuous(limits = c(0,max_h))

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())

p <- p + theme(text = element_text(size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))
               
ggsave(file="quantiles_make_up_water_provided_cook.pdf", p, width=img_width, height=img_height, dpi=300)
