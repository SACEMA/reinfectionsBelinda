require("data.table")
require("jsonlite")

.debug <- '.data'

.args <- if (interactive()) sprintf(c(
  file.path('data', 'inf_for_sbv.RDS'), 
  file.path('config_general.json'),
  file.path('data', 'infection_data.RDS') #data
  ), .debug[1]) else commandArgs(trailingOnly = TRUE)

configpth <- .args[2]

target <- tail(.args, 1)
attach(jsonlite::read_json(configpth))

data <- readRDS(.args[1])
set.seed(0)
data[infections < 0, infections := 0]
data[, infections_ma := frollmean(infections, window_days)]

saveRDS(data, file=target)

