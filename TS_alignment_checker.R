library(ggplot2)
library(scales)
library(plotly)
library(pracma)

find.peaks <- function(data, parameter, )
{
  for (i in 1:nrow(data))
  {
    if(!is.na(data[i, codeon_col]) & data[i, codeon_col] == 1)
    {
    }
    else if(zero_period == TRUE)
    {
    }
  return(data)
}

setwd('C:/Users/jconnor/Bay Area Air Quality Management District/Charity Garland - AMSP_shared/Projects/_Mobile Monitoring/Mobile Data/Richmond_North_RIchmond_San_Pablo Study Data/20211206/_QC_Data')
data = read.csv('2021-12-06_RSP_NorthRichmond_V1_3_PTR_corrected.csv')
data$date <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")

p = ggplot(data) + ggtitle("Test") + 
  labs(y="ylabel") + labs(x="Time PST") +
  geom_line(aes(date, Benzene, color = "Benzene")) +
  scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))

p <- ggplotly(p)
p
