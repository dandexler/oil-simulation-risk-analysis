##############################################
# Oil/Gas Pricing Plots (ggplot2)
# David Andexler
##############################################

library(ggplot2)
library(ggthemes)
library(scales)

setwd("DIRECTORY")
load("drywell.Rdata")
load("wetwell.Rdata")

options(scipen = 999)

drywell <- as.data.frame(drywell)/1000000
profit <- as.data.frame(profit)/1000000

dry_plot <- ggplot(data = drywell, aes(x=drywell))+
  geom_histogram(bins = 100)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Simulated Costs (in millions, USD)") +
  ylab("Frequency") +
  theme(axis.title.x = element_text(size = 12))+
  ggtitle("Simulated Costs for a Single Dry Well \n (in millions, USD)")+
  geom_vline(xintercept = mean(drywell$drywell),col='black',size=1)+
  annotate("text", x = 8.2, y = 58000, label = "Mean: $4.86 million")
dry_plot

wet_plot <- ggplot(data = profit, aes(x=profit))+
  geom_histogram(bins = 100)+
  theme_economist()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Simulated NPV (in millions, USD)") +
  theme(axis.title.x = element_text(size = 12))+
  ylab("Frequency") +
  ggtitle("Simulated Net Present Value of a \n Single Wet Well (in millions, USD)") +
  geom_vline(xintercept = mean(profit$profit),col='black',size=1)+
  annotate("text", x = 30, y = 52000, label = "Mean: $12.82 million")
wet_plot