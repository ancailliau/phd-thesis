Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)
library("ggrepel")
library("stringr")
library(dplyr)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/Data/PatternVSBDD-report-130917.csv",
                header = TRUE, sep = ",")

print(data)

print(select(filter(data,NbGoals == 10000 & NbObstructions == 1000 & NbObstacles == 10000), Method, NbGoals, NbObstructions, NbObstacles, Mean))
           
data$Mean   = as.numeric(gsub(",", "", gsub(" us", "", data$Mean   )))
data$Error  = as.numeric(gsub(",", "", gsub(" us", "", data$Error  )))
data$StdDev = as.numeric(gsub(",", "", gsub(" us", "", data$StdDev )))
#data$Median = as.numeric(gsub(",", "", gsub(" us", "", data$Median )))

print(select(filter(data,NbGoals == 10000 & NbObstructions == 1000 & NbObstacles == 10000), Method, NbGoals, NbObstructions, NbObstacles, Mean))

#factor(,labels=c("BDD-Based", "BDD-Based Prebuilt", "Pattern-Based")
# print(summary(data))
subdata <- filter(data,TRUE)
p <- ggplot(subdata, aes(x = NbObstacles, y = Mean, colour=Method, shape=Method)) +
     scale_color_manual(labels = c("BDD-Based", "BDD-Based (Prebuilt)", "Pattern-Based"), values=cbPalette) +
     geom_line() +
     geom_point() +
     #geom_errorbar(aes(ymin=Mean-Error, ymax=Mean+Error), width=.1) +
     facet_grid(NbObstructions ~ NbGoals, 
                labeller=labeller(NbObstructions = function(x) paste(x, "obstructions"),
                                  NbGoals = function(x) paste(x, "goals")))

my_seq <- 10^seq(1,10,by=1)
max_bound <- min(my_seq[my_seq > max(subdata$Mean + subdata$Error)])
print(max_bound)

p <- p + scale_y_continuous(label  = function(x) paste(x/1000/1000,"s"),
                            trans  = log10_trans())

p <- p + scale_x_continuous(breaks = c(10,100,1000,10000), trans=log10_trans())

p <- p + theme(panel.grid.minor.y = element_blank())

p <- p + theme(text = element_text(family="myriad",size=8),
                # axis.text.x = element_text(angle=45,hjust=0.95),
            panel.grid.minor = element_blank(),
            plot.title = element_text(family="myriad",size=8),
            plot.margin = unit(c(2,2,2,2),"pt"),legend.key.height=unit(0, "cm"))

p <- p + theme(legend.position = "none")

p <- p + xlab("Number of obstacles (log10)") + ylab("Mean time (log10)") + labs(col="Propagation method: ")

img_width = 4.527559
img_height = 2.9

ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/nb_obstacles.pdf", 
    p, 
    width=img_width, height=img_height, dpi=300)

p2 <- ggplot(subdata, aes(x = NbGoals, y = Mean, colour=Method, shape=Method)) +
     geom_line() +
     geom_point() +
     #geom_errorbar(aes(ymin=Mean-Error, ymax=Mean+Error), width=.1) +
     facet_grid(NbObstructions ~ NbObstacles, 
                labeller=labeller(NbObstructions = function(x) paste(x, "obstructions"),
                                  NbObstacles = function(x) paste(x, "obstacles")))

p2 <- p2 + scale_y_continuous(label  = function(x) paste(x/1000/1000,"s"),
                            trans  = log10_trans())

p2 <- p2 + scale_x_continuous(breaks = c(10,100,1000,10000), trans=log10_trans())

p2 <- p2 +
     scale_color_manual(
         name = "Propagation method", 
         labels = c("BDD-Based", "BDD-Based (Prebuilt)", "Pattern-Based"),
         breaks = c("BDDBasedComputation", "BDDBasedComputationPrebuilt", "PatternBasedComputation"),
         values=cbPalette) +
     scale_linetype_discrete(
         name = "Propagation method", 
         labels = c("BDD-Based", "BDD-Based (Prebuilt)", "Pattern-Based"),
         breaks = c("BDDBasedComputation", "BDDBasedComputationPrebuilt", "PatternBasedComputation")
     ) +
     scale_shape_discrete(
         name = "Propagation method", 
         labels = c("BDD-Based", "BDD-Based (Prebuilt)", "Pattern-Based"),
         breaks = c("BDDBasedComputation", "BDDBasedComputationPrebuilt", "PatternBasedComputation")
     )# +
     #scale_colour_discrete(
     #    name = "Propagation method", 
     #    labels = c("BDD-Based", "BDD-Based (Prebuilt)", "Pattern-Based"),
     #    breaks = c("BDDBasedComputation", "BDDBasedComputationPrebuilt", "PatternBasedComputation")
     #)

p2 <- p2 + theme(panel.grid.minor.y = element_blank())

p2 <- p2 + theme(text = element_text(family="myriad",size=8),
                # axis.text.x = element_text(angle=45,hjust=0.95),
            panel.grid.minor = element_blank(),
            plot.title = element_text(family="myriad",size=8),
            plot.margin = unit(c(2,2,2,2),"pt"),legend.key.height=unit(0, "cm"))

p2 <- p2 + theme(legend.position = "bottom")

p2 <- p2 + xlab("Number of goals (log10)") + ylab("Mean time (log10)")

img_width = 4.527559
img_height = 3.2

ggsave(file="/Users/acailliau/Google Drive/PhD/Dissertation/running-example-sfp/nb_goals.pdf", 
    p2, 
    width=img_width, height=img_height, dpi=300)
