
.debug <- '.'
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'observe_prob_functions.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


dir.create('utils')
target <- tail(.args, 1)

#Dataframe "data" given with columns cases (the reported number of cases that enters the suceptible "pool"),
#date, and observed (the observed number of reinfections coming from the susceptible pool)

logistic_func <- function(min, max, cases, s, x_m){
  prob = min + (max-min)/(1+exp(s*(cases-x_m)))
  return(prob)
}


save(logistic_func, file = target)
