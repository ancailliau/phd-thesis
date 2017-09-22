library("reshape2")
library("ggplot2")
library(scales)
library(gridExtra)
library("sfsmisc")
library(showtext)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()


adjust <- .5
item <- "pump_failure"

data1 = read.csv(paste("/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/", "quantiles_pump_failure_e1", ".csv", sep=""),
                 header = TRUE, sep = ",")
dens <- density(data1[[item]], from=0, to=1, adjust = adjust)

observations_x <- c(9,9,9,11,11,11)
observations_y <- 20 - observations_x

observations <- mapply(c, observations_x, observations_y, SIMPLIFY = FALSE)

mydata <- with(dens,data.frame(x = x, iter0 = y))
mydata$iter0 <- mydata$iter0 / integrate.xy(mydata$x,mydata$iter0)

i <- 0
for (observation in observations) {
  likelihood <- dbeta(mydata$x, observation[1], observation[2])
  likelihood <- likelihood / integrate.xy(mydata$x,likelihood)

  prior <- paste("iter", i, sep = "")
  posterior <- paste("iter", i+1, sep = "")
  mydata[[posterior]] <- (mydata[[prior]] * likelihood)
  mydata[[posterior]] <- mydata[[posterior]] / integrate.xy(mydata$x,mydata[[posterior]])

  i <- i + 1
}

# a <- 4
# b <- 18
# mydata$likelihood <- dbeta(mydata$x, a, b)
# mydata$likelihood <- mydata$likelihood / integrate.xy(mydata$x,mydata$likelihood)
#
# mydata$posterior <- (mydata$prior * mydata$likelihood)
# mydata$posterior <- mydata$posterior / integrate.xy(mydata$x,mydata$posterior)

mdata <- melt(mydata, id=c("x"))

p <- ggplot(data = mdata, aes(x = x, y = value, colour = variable)) +
     geom_line()

p <- p + scale_colour_grey(end = 0, start = .7
     )

p <- p + labs(x = NULL, y = NULL)

#p <- p + scale_x_continuous(limits = c(0,1),
#                            breaks = c(0,1),
#                            labels = percent)
#
#p <- p + scale_y_continuous(limits = c(0,max_h))

p <- p + theme(axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())

p <- p + theme(panel.grid.minor.y = element_blank(),
               panel.grid.major.y = element_blank())

p <- p + theme(text = element_text(size=8,angle=45,hjust=0.95),
               panel.grid.minor = element_blank(),
               plot.title = element_text(size=8),
               plot.margin = unit(c(2,2,2,2),"pt"))
p <- p + theme(legend.position = "none")

img_width = 4.527559
img_height = 1.2 #4.527559    
ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/runtime_update.pdf", p, width=img_width, height=img_height, dpi=300)

#print (p)

# p <- ggplot(data=d1, aes(x=x)) +
#      geom_line(data=d1, aes(y=y)) +
#      geom_line(data=d2, aes(y=y)) +
#      geom_line(data=d3, aes(y=y))
#
# print(p)