suppressPackageStartupMessages({
  require(data.table)
})
###
.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'ts_data.csv'), # input
  file.path('config_general.json'),
  file.path('%s', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

ts <- data.table(read.csv(.args[1], comment.char = '#', stringsAsFactors = FALSE)) # Use to set wave dates as >15% of wave peak

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

system('ls')

ts[, date := as.Date(date)]
ts[, ma_cnt := frollmean(cnt, window_days)]
ts[, ma_reinf := frollmean(reinf, window_days)]
ts[, ma_third := frollmean(third, window_days)]
ts[, ma_fourth := frollmean(fourth, window_days)] 
ts[, tot := cnt + reinf + third + fourth]
ts[, ma_tot := frollmean(tot, window_days)]
ts[, elig := shift(cumsum(cnt), cutoff-1) - shift(cumsum(reinf), 1, fill = 0)] # eligible for reinfection
ts[, elig.third := shift(cumsum(reinf), cutoff-1) - shift(cumsum(third), 1, fill = 0)] # eligible for second reinfection
ts[, elig.fourth := shift(cumsum(third), cutoff-1) - shift(cumsum(fourth), 1, fill = 0)] # eligible for third reinfection
#load data for second and third reinfections

saveRDS(ts, file = target)

