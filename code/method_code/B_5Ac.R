# Here we calculate the number of reinfections using the 7day moving average & fixed hazard coefficient
library(data.table)

.debug <- 'data'
.args <- if (interactive()) sprintf(c(
  file.path('data', 'infection_data.RDS'), # input
  file.path('parameters.json'),
  file.path('config_general.json'),
  value <- read.table(file = .args[4], header = F, nrows = 1)
  pscale <- value$V1[1]
  file.path('data', '5Bc_NF_ts_data_for_analysis.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

load('utils/observe_prob_functions.RData')

target <- tail(.args,1)

data <- readRDS(.args[1])

configpth_params <- .args[2] 
attach(jsonlite::read_json(configpth_params))
configpth <- .args[3]
attach(jsonlite::read_json(configpth))


observe_probability <- logistic_func(min = logistic_first$min, max = logistic_first$max, cases=data$infections, s = logistic_first$steep, x_m = logistic_first$x_m)
plot(data$infections, observe_probability, xlab = "Daily infections", ylab="Observe probability", main="The number of underlying daily infections vs the observe infections")

set.seed(0)

pscale <- 1


data[, reinfections := 0]
data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
data$infections_ma = frollmean(data$infections, window_days)


for (day in 1:nrow(data)) {
  observe_prob_first <-  logistic_func(min=logistic_first$min, max=logistic_first$max, cases = data$infections[day], s =logistic_first$steep, x_m=logistic_first$x_m)
  data$infections[day] <- rbinom(1, data$infections[day], observe_prob_first)
}

for (day in (cutoff+1):nrow(data)) { 
  data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
  if (data$date[day]<=wave_split){
    data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
  } else {
    data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale)
  }
  observe_prob_second <- logistic_func(min=func_second$min, max=func_second$max, cases = data$infections[day], s =func_second$steep, x_m=func_second$x_m)
  data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob_second)
}

data$infections_ma <- frollmean(data$infections, window_days)

saveRDS(data, file=target)