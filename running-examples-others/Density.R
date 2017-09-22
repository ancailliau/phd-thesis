library(ggplot2)

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/running-examples-others/fire.csv",
                header = TRUE, sep = ",")

dens <- density(data$battery_out, from=0, to=1, adjust = 1)
dd <- with(dens,data.frame(x,y))

p <- qplot(x,y,data = dd, geom = "line")

p <- qplot(x,y,data=dd, geom="line")
#p <- qplot(data$battery_out, geom="histogram", bins = 10)

p <- p + labs(x = NULL, y = NULL)

p <- p + scale_x_continuous(limits=c(0,.03))

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())

p <- p + theme(text = element_text(size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))

print(p)