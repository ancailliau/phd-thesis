Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)

item = "make_up_water_provided";
#item = "electronic_failure";
#item = "no_power_available";

mydata = read.csv(paste("~/Google Drive/PhD/Dissertation/running-example-sfp/usr_",item,".csv",sep=""),
                  header=TRUE,
                  sep=",")

rsr = .8
#rsr = 0
min_value = 0
max_value = 1
#max_value = .05

img_width = 4.527559
#img_width = 2
img_height = 1.2
#img_height = .8

fancy_xlabel <- function(l) {
    filter_func <- function(x) {
        if (rsr > 0 & x == rsr) { paste("RSR = ", percent(x)) } 
        else { percent(x) }
    }
    l <- lapply(l,filter_func)
    return(l)
}

dens <- density(mydata[[item]])
min_dens <- min(dens$x)
max_dens <- max(dens$x)

dens <- density(mydata[[item]], from=min_value, to=max_value)


dd <- with(dens,data.frame(x,y))

p <- qplot(x,y,data=dd, geom="line")

p <- p + labs(x = NULL, y = NULL)

if (rsr > 0) {
    br <- c(min_value,max_value,min_dens,max_dens,rsr)
} else {
    br <- c(min_value,max_value,min_dens,max_dens)
}

p <- p + scale_x_continuous(limits=c(min_value,max_value),
                            breaks = br,
                            labels = fancy_xlabel)

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())
               
p <- p + theme(text = element_text(family="myriad",size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(family="myriad",size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))

if (rsr > 0) {
    p <- p +  geom_ribbon(data = subset(dd, x > 0 & x < rsr), 
                          aes(ymax = y), 
                          ymin = 0, 
                          fill = "red",
                          colour = NA,
                          alpha = 0.1)
               
    p <- p + geom_vline(xintercept = rsr, 
                        color="red", 
                        alpha = .5)
}

ggsave(file=paste("usr_",item,".pdf",sep=""), p, width=img_width, height=img_height, dpi=300)
