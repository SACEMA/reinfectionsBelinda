
.debug <- '.'
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'fit_functions.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

#Dataframe "data" given with columns cases (the reported number of cases that enters the suceptible "pool"),
#date, and observed (the observed number of reinfections coming from the susceptible pool)

expected <- function(parms = disease_params(), data, delta=cutoff ) with(parms, {
  hz <- lambda * data$ma_tot

  out <- data.frame(date=data$date, expected_infections = rep(0, nrow(data)))

  for (day in 1:(nrow(data)-delta)){
    tmp <- data$cases[day] * (1-exp(-cumsum(hz[(day+delta):nrow(data)])))
    out$expected_infections[(day+delta):nrow(data)] <- out$expected_infections[(day+delta):nrow(data)]+tmp
  }
  return (out)

})


nllikelihood <- function(parms = disease_params(), data) with(parms, {
  tmp <- expected(parms, data)
  log_p <- dnbinom(data$observed, size=1/kappa, mu=c(0,diff(tmp$expected_infections)), log=TRUE)
  -sum(log_p)
  return(-sum(log_p))
})

save(expected, nllikelihood, file = target)
