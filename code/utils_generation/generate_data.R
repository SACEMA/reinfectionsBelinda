#Includes the generic mcmc functions that can be used in any cases (first reinfection, second reinfections, third reinfections)


.debug <- ''

.args <- if (interactive()) sprintf(c(
  file.path('utils', 'generate_data.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

generate_data <- function(method, data_source, seed) {
  
  ##Get the data
  ts <- readRDS(data_source)
  set.seed(seed-1)
  
  ts[, infections_ma := frollmean(infections, window_days)]
  ts[, reinfections := 0]

  if (method==2) {
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    
    underlying <- ts[, c('infections', 'eligible_for_reinf', 'reinfections')]
  
    for (day in (cutoff+1):nrow(ts)) { 
      underlying$eligible_for_reinf[day] = underlying$eligible_for_reinf[day] - sum(underlying$reinfections[1:day-1])
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      if (ts$date[day]<=wave_split) {
        underlying$reinfections[day] = round(reinf_hazard * underlying$infections[day] * underlying$eligible_for_reinf[day])
      } else {
        underlying$reinfections[day] = round(reinf_hazard * underlying$infections[day] * underlying$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
      ts$reinfections[day] = rbinom(1, underlying$reinfections[day], parameters.r$pobs_2[i])
    }
  }
  
  if (method==3) {
    
    underlying <- ts[, c('infections', 'reinfections')]
    
    for (day in (cutoff+1):nrow(ts)){
      ts$infections[day] = rbinom(1, underlying$infections[day], parameters.r$pobs_1[i])
    }
    
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    underlying$eligible_for_reinf= shift(cumsum(ts$infections), cutoff-1)
    
    #adjust infections observed according to observation probability
    
    #distinction: underlying is the underlying 'true' infections, etc. and ts is the observed (what the data can see)
    for (day in (cutoff+1):nrow(ts)) { 
      
      underlying$eligible_for_reinf[day] = underlying$eligible_for_reinf[day] - sum(underlying$reinfections[1:day-1])
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      
      if (ts$date[day]<=wave_split) {
        underlying$reinfections[day] = round(reinf_hazard * underlying$infections[day] * underlying$eligible_for_reinf[day])
      } else {
        underlying$reinfections[day] = round(reinf_hazard * underlying$infections[day] * underlying$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
      
      ts$reinfections[day] = rbinom(1, underlying$reinfections[day], parameters.r$pobs_2[i])
    }
  }
  
  ##Rename column names for MCMC
  names(ts)[2] <- "cases"
  names(ts)[3] <- "ma_cnt"
  names(ts)[4] <- "observed"
  ts[, tot := observed + cases]
  ts[, ma_tot := frollmean(tot, window_days)]
  ts[, ma_reinf := frollmean(observed, window_days)]
  
  return (ts)
}

save(generate_data, file = target)
