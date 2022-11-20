# Here we calculate the number of reinfections using the 7day moving average & fixed hazard coefficient
library(data.table)

.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('parameters.json'),
  file.path('config_general.json'),
  file.path('data', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

configpth <- .args[3]
attach(jsonlite::read_json(configpth))


target <- tail(.args,1)

data <- readRDS(.args[1])

names(data)[2] <- "cnt"
names(data)[3] <- "ma_cnt"
names(data)[4] <- "reinf"



## 
data[, tot := reinf + cnt]
data[, ma_tot := frollmean(tot, window_days)]
data[, ma_reinf := frollmean(reinf, window_days)]

saveRDS(data, file = target)

