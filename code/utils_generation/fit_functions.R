
.debug <- '.'
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'fit_functions.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


dir.create('utils')
target <- tail(.args, 1)


expected <- function(parms = disease_params(), data, delta=cutoff ) with(parms, {

  hz <- lambda * data$ma_tot
  
  return ( lapply(1:(nrow(data)-cutoff)
                  , expected_vec
                  , data = data
                  , delta = cutoff
                  , hz = hz)
  )
})

#expected function for lambda 2
expected_l2 <- function(parms = disease_params(), data, delta=cutoff ) with(parms, {
  hz <- lambda * data[date <= omicron_date]$ma_tot
  hz <- c(hz, lambda2 * data[date > omicron_date]$ma_tot)
  
  return ( lapply(1:(nrow(data)-cutoff)
                  , expected_vec
                  , data = data
                  , delta = cutoff
                  , hz = hz)
  )
})


expected_vec <- function(day, data, delta=cutoff, hz)  {
  return(c(rep(0,day-1), data$cases[day] * (1-exp(-cumsum(hz[(day+delta):nrow(data)]))) ))
}


nllikelihood <- function(parms = disease_params(), data) with(parms, {
  tmp <- expected(parms, data)
  tmp <- Reduce("+", tmp)

  tmp <- c(rep(0,90),tmp)
  log_p <- dnbinom(data$observed, size=1/kappa, mu=c(0,diff(tmp)), log=TRUE)
  -sum(log_p)
  return(-sum(log_p))
})

nllikelihood_l2 <- function(parms = disease_params(), data) with(parms, {
  tmp <- expected_l2(parms, data)
  tmp <- Reduce("+", tmp)
  
  tmp <- c(rep(0,90),tmp)
  log_p <- dnbinom(data$observed, size=1/kappa, mu=c(0,diff(tmp)), log=TRUE)
  -sum(log_p)
  return(-sum(log_p))
})

split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

save(expected, expected_l2, nllikelihood_l2, nllikelihood, expected_vec, split_path, file = target)