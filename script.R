setwd("C:/Users/kos_c/Desktop/Practicum II1")
load("C:/Users/kos_c/Desktop/Practicum II1/.RData")
library(ggplot2)

flight <- read.csv("2002.csv", header = T)

set.seed(8)
air <- flight[sample(nrow(flight), 10000), ]

attach(air)
# create factors with value labels 
carrier.f<-factor(UniqueCarrier) 


ggplot(air, aes(x = DepTime))+ ggtitle("Departure times of the flights")+
geom_histogram(bins = 24, fill = "lightgreen", colour = "black")+
labs(x="Departure Time",y="Flights")


ggplot(air,  aes(x = factor(Month)) ) +
geom_bar(fill ="steelblue")+
ggtitle("Flights per Month of year 2002")+
labs(x="Month",y="Flights")+
theme(legend.position="none")

ggplot(air,  aes(x = factor(DayOfWeek)) ) +
geom_bar(fill = "orange")+
ggtitle("Flights per Weekday")+
labs(x="Weekday",y="Flights")+
theme(legend.position="none")

ggplot(air, aes(DepTime, ArrTime) ) +
geom_point() + geom_smooth() + ggtitle("DepartureTime versus ArrivalTime")


ggplot(data = air, aes(x = Distance, fill = UniqueCarrier)) + geom_histogram()+ggtitle("Distance by Carrier")

ggplot(flight,  aes(x = UniqueCarrier, fill = DepDelay) ) +geom_bar(fill="lightgreen", colour="lightgreen")+ggtitle("Delay per Carrier")


ggplot(air,aes(Cancelled,UniqueCarrier))  +geom_point() + geom_boxplot()

ggplot(air, aes(DepTime, ArrTime, color = UniqueCarrier)) +
  geom_point(size = 1.2, shape = 16) +
  facet_wrap( ~ UniqueCarrier) +
  theme(legend.key = element_rect(fill = NA),
        legend.position = "right",
        strip.background = element_rect(fill = NA),
        axis.title.y = element_text(angle = 0))

 bwplot(~Distance|carrier.f,
 ylab="Carriers", xlab="Miles", 
 main="Miles per Carrier")
 
  ggplot(air, aes(UniqueCarrier, Distance, color = UniqueCarrier)) + 
  geom_point(size = 1.2, shape = 16) + 
  theme(legend.key = element_rect(fill = NA),
  legend.position = "right",
  strip.background = element_rect(fill = NA),
  axis.title.y = element_text(angle = 0))+
  labs(x="Carrier",y = "Distance")
  
 ggplot(air, aes(UniqueCarrier, DepDelay, color = UniqueCarrier)) +
  geom_line(size = 2.0) +
  theme(legend.key = element_rect(fill = NA),
  legend.position = "right",
  strip.background = element_rect(fill = NA),
  axis.title.y = element_text(angle = 0))+
  labs(x="Carrier",y="Departure Delays")
 
 dotplot(~Distance|carrier.f, 
         main="Dotplot by Number of Miles per Carrier",
         xlab="Miles")