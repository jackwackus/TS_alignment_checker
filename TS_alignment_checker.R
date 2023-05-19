library(ggplot2)
library(scales)
library(plotly)
library(pracma)
library(modules)

setwd('C:/Users/jconnor/Bay Area Air Quality Management District/Charity Garland - AMSP_shared/Projects/_Mobile Monitoring/Mobile Data/Richmond_North_RIchmond_San_Pablo Study Data/20211206/_QC_Data')
data = read.csv('2021-12-06_RSP_NorthRichmond_V1_3_PTR_corrected.csv')
data$date <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

p = ggplot(data) + ggtitle("Test") + 
  labs(y="ylabel") + labs(x="Time PST") +
  geom_line(aes(date, Benzene, color = "Benzene")) +
  scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))

p <- ggplotly(p)
p

module_dir = 'C:/Users/jconnor/OneDrive - Bay Area Air Quality Management District/R/TS_alignment_checker'
setwd(module_dir)
m = use('ts_alignment_functions.R')
