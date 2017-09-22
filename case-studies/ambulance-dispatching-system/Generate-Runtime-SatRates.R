Sys.setenv(LANG = "en")

library(showtext)
library(scales)
library(lubridate)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)

scenario1 = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_1.csv",
                     header=TRUE, sep=",")

scenario1$date = parse_date_time(scenario1$date, "HMS")


scenario2 = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_2.csv",
                     header=TRUE, sep=",")
scenario2$date = as.POSIXct(scenario2$date) #parse_date_time(scenario2$date, "HMS")

scenario3 = read.csv("/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_3.csv",
                     header=TRUE, sep=",")

scenario3$date = parse_date_time(scenario3$date, "HMS")

rds = .8

rds_changing <- data.frame(date = c("4:00:00", "8:15:00", "10:46:00"), rds = c(0.5, 0.8, 0.8))
rds_changing$date = parse_date_time(rds_changing$date, "HMS")

fancy_ylabel_b <- function(l) {
    filter_func <- function(x) {
        if (x == .5) { paste("RSR = ", percent(x)) } 
        else if (x == .8) { paste("RSR = ", percent(x)) } else { percent(x) }
    }
    l <- lapply(l,filter_func)
    return(l)
}

fancy_ylabel <- function(l) {
    filter_func <- function(x) {
        if (x == rds) { paste("RSR = ", percent(x)) } else { percent(x) }
    }
    l <- lapply(l,filter_func)
    return(l)
}

fancy_xlabel <- function(l) {
    filter_func <- function(x) {
        date_format("%H:%M", tz = "CET")(x)
    }
    l <- lapply(l,filter_func)
    return(l)
}

### Plot Scenario 1

p1 <- ggplot(scenario1, aes(date,achieve_incident_resolved)) +
     geom_line () + 
     geom_hline(yintercept=rds, size=.5, color = "red", alpha = .5) +
     geom_vline(xintercept=as.numeric(scenario1$date[703]), size=.5, , linetype=2, alpha = .5)

p1 <- p1 + labs(x = NULL, y = NULL) +
        scale_y_continuous(
            labels = fancy_ylabel, 
            limits=c(.5,1),
            breaks=c(.5, .75, 1, rds)) +
        scale_x_datetime(
            labels = fancy_xlabel
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,2,2,2),"pt")
            )

ggsave(file = "/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_1.pdf", 
       p1, width = 4.527559, height = 1.5, dpi = 300)
       
### Plot Scenario 2

p2 <- ggplot(scenario2, aes(date,achieve_incident_resolved)) +
     geom_line () + 
     geom_hline(yintercept=rds, size=.5, color = "red", alpha = .5) +
     geom_vline(xintercept=as.numeric(scenario2$date[654]), size=.5, , linetype=2, alpha = .5)
     
p2 <- p2 + labs(x = NULL, y = NULL) +
        scale_y_continuous(
            labels = fancy_ylabel, 
            limits=c(.5,1),
            breaks=c(.5, .75, 1, rds)) +
        scale_x_datetime(
            labels = fancy_xlabel
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,2,2,2),"pt")
            )

ggsave(file = "/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_2.pdf", 
       p2, width = 4.527559, height = 1.5, dpi = 300)

### Plot Scenario 3

p3 <- ggplot(scenario3, aes(date,avoid_ambulance_mobilized_on_road)) +
     geom_line ()
     
p3 <- p3 + geom_step(data = rds_changing, aes(y = rds), size=.5, color = "red", alpha = .5) +
     geom_vline(xintercept=as.numeric(scenario3$date[1582]), size=.5, , linetype=2, alpha = .5)

p3 <- p3 + labs(x = NULL, y = NULL) +
        scale_y_continuous(
            labels = fancy_ylabel_b, 
            limits=c(.5,1),
            breaks=c(.5, .75, 1, .8)) +
        scale_x_datetime(
            labels = fancy_xlabel
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,2,2,2),"pt")
            )

ggsave(file = "/Users/acailliau/Google Drive/PhD/Dissertation/case-studies/ambulance-dispatching-system/runtime_scenario_3.pdf", 
       p3, width = 4.527559, height = 1.5, dpi = 300)