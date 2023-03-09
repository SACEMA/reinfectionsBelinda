# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfection

# File adjusted to include third infections

# Purpose of this file: 
## This file is used to create a dataframe from the CSV file containing
## infection data. 

library('jsonlite')
library('data.table')

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'ts_data.csv'), # input
  file.path('config_general.json'),
  file.path('%s', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

#load data for second and third reinfections
ts <- data.table(read.csv(.args[1], comment.char = '#', stringsAsFactors = FALSE))

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

ts[, date := as.Date(date)]
ts[, ma_cnt := frollmean(cnt, window_days)]
ts[, ma_reinf := frollmean(reinf, window_days)]
ts[, ma_third := frollmean(third, window_days)]
ts[, tot := cnt + reinf + third]
ts[, ma_tot := frollmean(tot, window_days)]
ts[, elig := shift(cumsum(cnt), cutoff-1) - shift(cumsum(reinf), 1, fill = 0)] # eligible for reinfection
ts[, elig.third := shift(cumsum(reinf), cutoff-1) - shift(cumsum(third), 1, fill = 0)] # eligible for second reinfection


saveRDS(ts, file = target)

