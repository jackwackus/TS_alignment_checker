library(modules)

module_dir = 'C:/Users/jconnor/OneDrive - Bay Area Air Quality Management District/R/TS_alignment_checker'
setwd(module_dir)
m = use('ts_alignment_functions.R')

setwd('C:/Users/jconnor/Bay Area Air Quality Management District/Charity Garland - AMSP_shared/Projects/_Mobile Monitoring/2021_Richmond_NorthRichmond_SanPablo_CAMP/_Data/Raw_Data')
data = read.csv('20220401/_QC_Data/2022-04-01_RSP_NorthRichmond_V1_3.csv')
data$date <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
parameters = c("CO", "CO2_dry", "NOx", "Benzene", "BC", "PM10", "FMPS_TotConc")
peaks = m$find.common.peaks(data, parameters, 30, 10)
fig = m$plot.time.bounded.stacked.subplots(data, parameters, start_time, end_time)

"
Good Example Days:
20211206
20211210
20211214 11:56:28
20220113
20220131
20220204
2022-02-14 14:24:12
Examples of multiple peaks within window:
20220323
"
