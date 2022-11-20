# Here we calculate the number of reinfections using the 7day moving average & fixed hazard coefficient
library(data.table)

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('data', 'infection_data.RDS'), # input
  file.path('parameters.json'),
  file.path('config_general.json'),
  file.path('data', 'ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)



target <- tail(.args,1)

data <- readRDS(.args[1])

configpth_params <- .args[2] 
attach(jsonlite::read_json(configpth_params))
configpth <- .args[3]
attach(jsonlite::read_json(configpth))

set.seed(0)

pscale <- 1


for (day in 1:nrow(data)) {
  data$infections[day] = rbinom(1, data$infections[day], observe_prob_first)
}

data[, reinfections := 0]
data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
data$infections_ma = frollmean(data$infections, window_days)

for (day in (cutoff+1):nrow(data)) { 
  data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
  if (data$date[day]<=wave_split){
    data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
  } else {
    data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale)
  }
  data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
}

saveRDS(data, file=target)