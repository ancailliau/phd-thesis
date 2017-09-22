Sys.setenv(LANG = "en")

library(showtext)
library(scales)

font.add("myriad", "/Users/acailliau/Downloads/Adobe Font Folio 11/Western Fonts/Myriad Pro/MyriadPro-Regular.otf")
showtext.auto()

library(ggplot2)
library(gridExtra)

simulation_limits = c(
  as.POSIXct("2017-06-30 8:00:00 CET"),
  as.POSIXct("2017-06-30 10:00:00 CET")
)

generate_small_graph <- function(mydata,line,low,hi) {
    return (ggplot(mydata, aes(date,line)) +
        geom_line() +
        geom_ribbon(data=mydata,aes(ymin=low,ymax=hi),alpha=0.3) +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(labels = percent, limits=c(0,1)) +
        scale_x_datetime(
            labels = date_format("%H:%M", tz = "CET"),
            limits = simulation_limits
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,3,2,2),"pt")
            )
        )
}

generate_large_graph <- function(mydata,line,low,hi) {
    return (ggplot(mydata, aes(date,line)) +
        geom_line() +
        geom_ribbon(data=mydata,aes(ymin=low,ymax=hi),alpha=0.3) +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(labels = percent) +
        scale_x_datetime(
            labels = date_format("%H:%M", tz = "CET"),
            limits = simulation_limits
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,2,2,2),"pt")
            )
        )
}

generate_goal_sat_graph <- function(mydata,line,rds) {
    fancy_label <- function(l) {
        filter_func <- function(x) {
            if (x == rds) { paste("RSR = ", percent(x)) } else { percent(x) }
        }
        l <- lapply(l,filter_func)
        return(l)
    }
    fancy_xlabel <- function(l) {
        filter_func <- function(x) {
            if (x == as.POSIXct("2017-06-30 08:27:36 CET")) { paste(date_format("%H:%M", tz = "CET")(x),"DeployCamera",sep="\n") } 
            else if (x == as.POSIXct("2017-06-30 09:21:36 CET")) { paste(date_format("%H:%M", tz = "CET")(x),"DeployUltrasound",sep="\n") } 
            else { date_format("%H:%M", tz = "CET")(x) }
            # date_format("%H:%M", tz = "CET")(x)
        }
        l <- lapply(l,filter_func)
        return(l)
    }
    return (ggplot(mydata, aes(date,line)) +
        geom_hline(yintercept=rds, size=.5, color = "red", alpha = .5) +
        geom_vline(xintercept=as.numeric(as.POSIXct("2017-06-30 08:27:36 CET")), color="blue", alpha = .5) +
        geom_vline(xintercept=as.numeric(as.POSIXct("2017-06-30 09:21:36 CET")), color="purple", alpha = .5) +
        geom_line() +
        labs(x = NULL, y = NULL) +
        scale_y_continuous(
            labels = fancy_label, 
            limits=c(0,1),
            breaks=c(0, .25, .5, .75, 1, rds)) +
        scale_x_datetime(
            labels = fancy_xlabel,#date_format("%H:%M", tz = "CET"),
            breaks=c(as.POSIXct("2017-06-30 08:00:00 CET"),
                     as.POSIXct("2017-06-30 09:00:00 CET"),
                     as.POSIXct("2017-06-30 10:00:00 CET"),
                     as.POSIXct("2017-06-30 08:27:36 CET"),
                     as.POSIXct("2017-06-30 09:21:36 CET")),
            limits = simulation_limits
        ) +
        theme(text=element_text(family="myriad",size=8),
            panel.grid.minor = element_blank(),
            plot.title=element_text(family="myriad",size=8),
            plot.margin=unit(c(2,2,2,2),"pt")
            )
        )
}

mydata = read.csv("/Users/acailliau/Development/KAOSTools/bin/Debug/data.csv",header=TRUE,sep=",")

mydata$date = as.POSIXlt(mydata$date)

mydata$low_dusty_environment = mydata$dusty_environment-1.96*mydata$sd_dusty_environment
mydata$low_echo = mydata$echo-1.96*mydata$sd_echo
mydata$low_depth_broken = mydata$depth_broken-1.96*mydata$sd_depth_broken
mydata$low_ultrasound_broken = mydata$ultrasound_broken-1.96*mydata$sd_ultrasound_broken
mydata$low_noisy_image = mydata$noisy_image-1.96*mydata$sd_noisy_image
mydata$low_voice_down = mydata$voice_down-1.96*mydata$sd_voice_down

mydata$high_dusty_environment = mydata$dusty_environment+1.96*mydata$sd_dusty_environment
mydata$high_echo = mydata$echo+1.96*mydata$sd_echo
mydata$high_depth_broken = mydata$depth_broken+1.96*mydata$sd_depth_broken
mydata$high_ultrasound_broken = mydata$ultrasound_broken+1.96*mydata$sd_ultrasound_broken
mydata$high_noisy_image = mydata$noisy_image+1.96*mydata$sd_noisy_image
mydata$high_voice_down = mydata$voice_down+1.96*mydata$sd_voice_down

p_dusty <- generate_large_graph(mydata,mydata$dusty_environment, mydata$low_dusty_environment, mydata$high_dusty_environment)
ggsave(file="satrate-dusty_environment.pdf", p_dusty, width=4.527559, height=1.96, dpi=300)

p_dusty <- generate_small_graph(mydata,mydata$dusty_environment, mydata$low_dusty_environment, mydata$high_dusty_environment)
ggsave(file="satrate-dusty_environment-small.pdf", p_dusty, width=2.1, height=1, dpi=300)

p_echo <-  generate_small_graph(mydata,mydata$echo,mydata$low_echo,mydata$high_echo)
ggsave(file="satrate-echo-small.pdf", p_echo,  width=2.1, height=1, dpi=300)

p_depth <- generate_small_graph(mydata,mydata$depth_broken,mydata$low_depth_broken,mydata$high_depth_broken)
ggsave(file="satrate-depth_broken-small.pdf", p_depth,  width=2.1, height=1, dpi=300)

p_ultra <- generate_small_graph(mydata,mydata$ultrasound_broken, mydata$low_ultrasound_broken, mydata$high_ultrasound_broken)
ggsave(file="satrate-ultrasound_broken-small.pdf", p_ultra,  width=2.1, height=1, dpi=300)

p_noisy <- generate_small_graph(mydata,mydata$noisy_image,mydata$low_noisy_image,mydata$high_noisy_image)
ggsave(file="satrate-noisy_image-small.pdf", p_noisy,  width=2.1, height=1, dpi=300)

p_voice <- generate_small_graph(mydata,mydata$voice_down,mydata$low_voice_down,mydata$high_voice_down)
ggsave(file="satrate-voice_dow-small.pdf", p_voice,  width=2.1, height=1, dpi=300)

p_locals_warned_when_risk_imminent <- generate_goal_sat_graph(mydata,
    mydata$locals_warned_when_risk_imminent,.65)
ggsave(file="satrate-locals_warned_when_risk_imminent.pdf", p_locals_warned_when_risk_imminent, width=4.527559, height=1.96, dpi=300)

#g <- grid.arrange (p_dusty,p_echo,p_depth,p_ultra,p_noisy,p_voice,cols=2)
#g <- arrangeGrob (p_dusty,p_echo,p_depth,p_ultra,p_noisy,p_voice, ncol=2)

# par(mfrow=c(3,2))
# print(p_dusty)
# print(p_echo)
# print(p_depth)
# print(p_ultra)
# print(p_noisy)
# print(p_voice)



# ggsave(file="satrate-dusty_environment.pdf", width=4.527559, height=1.96, dpi=300)

# scale_x_datetime(labels = date_format("%a-%d\n%H:%M", tz = "CET")) +

