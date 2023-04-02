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
library(english)

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'ts_data.csv'), # input
  file.path('config_general.json'),
  file.path('%s', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

#load data for second and third reinfections
ts <- data.table(read.csv(.args[1], comment.char = '#', stringsAsFactors = FALSE))

number_of_infections <- ncol(ts)-1 #for the date column

if (number_of_infections < 2) 
  stop('Error -- need at least two infections')

if (names(ts)[2] != 'cnt')
  stop('Error -- the second column in the data must be named cnt')

if (names(ts)[3] != 'reinf')
  stop('Error -- the third column in the data must be named reinf')

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

ts[, date := as.Date(date)] #get date value 


ts[, tot := 0]
for (i in 1:number_of_infections) { 
  if (i==1) {
    ts[, ma_cnt := frollmean(cnt, window_days)]
    ts$tot <- ts$tot + ts$cnt
  }
  if (i==2) { 
    ts[, ma_reinf := frollmean(reinf, window_days)]
    ts$tot <- ts$tot + ts$reinf
    ts[, elig := shift(cumsum(cnt), cutoff-1) - shift(cumsum(reinf), 1, fill = 0)] # eligible for reinfection
  }
  if (i>2){ 
    column_name_ma <- paste0('ma_', ordinal(i))
    ts[[column_name_ma]] <- frollmean(ts[[ordinal(i)]], window_days)
    ts$tot <- ts$tot + ts[[ordinal(i)]]
    column_name_elig <- paste0('elig.', ordinal(i))
    if (i==3)
      ts[[column_name_elig]] <- shift(cumsum(ts$reinf), cutoff-1) - shift(cumsum(ts$third), 1, fill = 0)
    else {
      ts[[column_name_elig]] <- shift(cumsum(ts[[ordinal(i-1)]]), cutoff-1) - shift(cumsum(ts[[ordinal(i)]]), 1, fill = 0)
    }
  }
}
ts[, ma_tot := frollmean(tot, window_days)]


saveRDS(ts, file = target)

