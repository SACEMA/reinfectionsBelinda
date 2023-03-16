#Includes the generic mcmc functions that can be used in any cases (first reinfection, second reinfections, third reinfections)


.debug <- ''

.args <- if (interactive()) sprintf(c(
  file.path('utils', 'generate_data.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


dir.create('utils')
target <- tail(.args, 1)

generate_data <- function(method, data_source, seed) {
  
  ##Get the data
  ts <- readRDS(data_source)
  set.seed(seed-1)
  
  ts[, infections_ma := frollmean(infections, window_days)]
  ts[, reinfections := 0]

  if (method==1) {
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    ts$infections_ma = frollmean(ts$infections, window_days)
    
    for (day in (cutoff+1):nrow(ts)) { 
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      if (ts$date[day]<=wave_split) {
        ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day])
      } else {
        ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
    }
  }
  
  
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
    
    ts$infections_ma = frollmean(ts$infections, window_days)
    
    #adjust infections observed according to observation probability
    
    #distinction: underlying is the underlying 'true' infections, etc. and ts is the observed (what the data can see)
    for (day in (cutoff+1):nrow(ts)) { 
      
      underlying$eligible_for_reinf[day] = underlying$eligible_for_reinf[day] - sum(underlying$reinfections[1:day-1])
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      
      ts$infections_ma = frollmean(ts$infections, window_days)
      
      
      if (ts$date[day]<=wave_split) {
        underlying$reinfections[day] = round(reinf_hazard * ts$infections[day] * underlying$eligible_for_reinf[day])
      } else {
        underlying$reinfections[day] = round(reinf_hazard * ts$infections[day] * underlying$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
      
      ts$reinfections[day] = rbinom(1, underlying$reinfections[day], parameters.r$pobs_2[i])
    }
  }
  
  
  if (method==4) {
    #save the number of underlying infections and reinfections (which is currently 0)
    underlying <- ts[, c('infections', 'reinfections')]
    
    #calculate the number of reported primary infections
    for (day in 1:nrow(ts)) {
      ts$infections[day] = rbinom(1, ts$infections[day], parameters.r$pobs_1[i])
    }
    
    #eligible for reinf in both cases starts with the sum of observed primary infections
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    underlying$eligible_for_reinf= shift(cumsum(ts$infections), cutoff-1)
    
    #calculate the moving average of the infections
    ts$infections_ma = frollmean(ts$infections, window_days)
  
    #calulcate the number of deaths from the REPORTED primary infections
    for (day in 1:nrow(ts))
      ts$deaths[day] = rbinom(1, ts$infections[day], parameters.r$dprob[i])
  
    for (day in (cutoff+1):nrow(ts)) { 
      underlying$eligible_for_reinf[day] = max(underlying$eligible_for_reinf[day] - sum(underlying$reinfections[1:day-1]) - sum(ts$deaths[1:day-1]),0)
      ts$eligible_for_reinf[day] = max(ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1]) - sum(ts$deaths[1:day-1]),0)
      
      if (ts$date[day]<=wave_split) {
        underlying$reinfections[day] = round(reinf_hazard * ts$infections[day] * underlying$eligible_for_reinf[day])
      } else {
        underlying$reinfections[day] = round(reinf_hazard * ts$infections[day] * underlying$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
      ts$reinfections[day] = rbinom(1, underlying$reinfections[day], parameters.r$pobs_2[i])
    }
  }
  
  if (method==5) {
    original_infections <- ts[, c('date', 'infections')]
    ## 2: Calculate the number of primary infections and reinfections
    #Method 3 adjustment
    for (day in 1:nrow(ts)) {
      observe_prob_first <-  logistic_func(min=parameters.r$pobs_1_min[i]
                                           , max=parameters.r$pobs_1_max[i]
                                           , cases = original_infections$infections[day]
                                           , s =parameters.r$steep[i]
                                           , x_m=parameters.r$xm[i]
      )
      ts$infections[day] = rbinom(1, ts$infections[day], observe_prob_first)
    }
    
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    ts$infections_ma = frollmean(ts$infections, window_days)
    
    
    for (day in (cutoff+1):nrow(ts)) { 
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      if (ts$date[day]<=wave_split) {
        ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day])
      } else {
        ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day] * parameters.r$pscale[i])
      } 
      #Method 2 adjustment
      observe_prob_second <- logistic_func(min=parameters.r$pobs_2_min[i]
                                           , max=parameters.r$pobs_2_max[i]
                                           , cases = original_infections$infections[day]
                                           , s =parameters.r$steep[i]
                                           , x_m=parameters.r$xm[i]
      )
      ts$reinfections[day] = rbinom(1, ts$reinfections[day], observe_prob_second)
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
