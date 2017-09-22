library("ggplot2")

filename <- "/tmp/out.csv"

runtime_satrates = read.csv(filename, header = TRUE, sep = ",")
runtime_satrates$date = as.POSIXlt(runtime_satrates$date)

ggplot(runtime_satrates, aes(x = date)) +
  geom_line(aes( y = achieve_incident_resolved , color = "achieve_incident_resolved")) +
  geom_line(aes( y = avoid_ambulance_mobilized_on_road, color="avoid_ambulance_mobilized_on_road" ))