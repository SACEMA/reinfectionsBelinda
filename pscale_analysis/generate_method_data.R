#functions to generate data and store it in the data folder (for each mehod)
generate_m1B <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  set.seed(0)
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}

generate_m2B <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  set.seed(0)
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}

generate_m3B <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  set.seed(0)
  for (day in 1:nrow(data)) {
    data$infections[day] = rbinom(1, data$infections[day], observe_prob_first)
  }
  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}

generate_m4B <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  set.seed(0)
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  
  for (day in 1:nrow(data)) {
    data$infections[day] = rbinom(1, data$infections[day], observe_prob_first)
  }
  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  data[, deaths := 0]

  for (day in 1:nrow(data))
    data$deaths[day] = rbinom(1, data$infections[day], dprob_first)
  
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])- sum(data$deaths[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}


generate_m5aB <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  set.seed(0)
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  
  for (day in 1:nrow(data)) {
    if (data$infections[day]>infections_for_changing_obs)
      data$infections[day] = rbinom(1, data$infections[day], observe_prob_first_changing)
    else
      data$infections[day] = rbinom(1, data$infections[day], observe_prob_first_basic)
  }
  

  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  data[, deaths := 0]
  
  for (day in 1:nrow(data))
    data$deaths[day] = rbinom(1, data$infections[day], dprob_first)
  
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])- sum(data$deaths[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}

generate_m5bB <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  set.seed(0)
  
  for (day in 1:nrow(data)) {
    observe_prob_first <-  logistic_func(min=logistic_first$min, max=logistic_first$max, cases = data$infections[day], s =logistic_first$steep, x_m=logistic_first$x_m)
    data$infections[day] <- rbinom(1, data$infections[day], observe_prob_first)
  }

  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]

  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"
  ## 
  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  #if (save==TRUE)
  #  saveRDS(data,'data/1B_ts_data_for_analysis.RDS')
  
  return (data)
}

generate_m5cB <- function(pscale_param=-1, data, save=FALSE){ 
  attach(jsonlite::read_json('parameters.json'))
  attach(jsonlite::read_json('config_general.json'))
  
  #if (pscale_param==-1)
  #  pscale_param<-pscale
  set.seed(0)
  
  for (day in 1:nrow(data)) {
    observe_prob_first <-  logistic_func(min=logistic_first$min, max=logistic_first$max, cases = data$infections[day], s =logistic_first$steep, x_m=logistic_first$x_m)
    data$infections[day] <- rbinom(1, data$infections[day], observe_prob_first)
  }
  
  
  ifelse(pscale_param==-1, pscale_param<-pscale, pscale_param)
  data[, reinfections := 0]
  data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  
  for (day in (cutoff+1):nrow(data)) { 
    data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
    if (data$date[day]<=wave_split){
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
    } else {
      data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day] * pscale_param)
    }
    observe_prob_second <- logistic_func(min=func_second$min, max=func_second$max, cases = data$infections[day], s =func_second$steep, x_m=func_second$x_m)
    data$reinfections[day] = rbinom(1, data$reinfections[day], observe_prob_second)
  }
  names(data)[2] <- "cases"
  names(data)[3] <- "ma_cnt"
  names(data)[4] <- "observed"

  data[, tot := observed + cases]
  data[, ma_tot := frollmean(tot, window_days)]
  data[, ma_reinf := frollmean(observed, window_days)]
  
  
  return (data)
}


save(generate_m1B, generate_m2B,generate_m3B, generate_m4B, generate_m5aB, generate_m5cB, generate_m5bB, file='utils/generate_method_data.RData')