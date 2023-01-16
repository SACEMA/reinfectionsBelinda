
# Method 3: data (with pscale 1)
df <- data.frame(matrix(ncol = 8, nrow = 0))
x <- c("date", "infections", "infections_ma", "reinfections", "eligible_for_reinf", "pobs1", "pobs2", "reinfections_ma")
colnames(df) <- x

for (pobs1 in seq(0.1, 0.5, 0.1)) { 
  for (pobs2 in seq(0.1, 0.5, 0.1)) { 
    ts <- readRDS('data/infection_data.RDS')
    
    for (day in 1:nrow(ts)) {
      ts$infections[day] = rbinom(1, ts$infections[day], pobs1)
    }
    
    ts[, reinfections := 0]
    ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
    ts$infections_ma = frollmean(ts$infections, window_days)
    
    for (day in (90+1):nrow(ts)) { 
      ts$eligible_for_reinf[day] = ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1])
      ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day])
      #Method 2 adjustment
      ts$reinfections[day] = rbinom(1, ts$reinfections[day], pobs2)
    }
    ts$pobs1 <- pobs1
    ts$pobs2 <- pobs2
    
    ts$reinfections_ma = frollmean(ts$reinfections, window_days)
    df <- rbind(df, ts)
  }
}
saveRDS(df, file='data/m3_ts_data_for_analysis.RDS')

# Method 4 - with constant pscale, pobs_1 and pobs_2 (varying the dprob)
df <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("date", "infections", "infections_ma", "reinfections", "eligible_for_reinf", "dprob", "reinfections_ma")
colnames(df) <- x

for (dprob in c(0.001, 0.01, 0.05)) { 
  ts <- readRDS('data/infection_data.RDS')
  
  for (day in 1:nrow(ts)) {
    ts$infections[day] = rbinom(1, ts$infections[day], 0.2)
  }
  
  ts[, reinfections := 0]
  ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  ts$infections_ma = frollmean(ts$infections, window_days)
  
  #Method 4 adjustment
  for (day in 1:nrow(ts))
    ts$deaths[day] = rbinom(1, ts$infections[day], dprob)
  
  for (day in (cutoff+1):nrow(ts)) { 
    ts$eligible_for_reinf[day] =max(ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1]) - sum(ts$deaths[1:day-1]),0)
    ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day])
    ts$reinfections[day] = rbinom(1, ts$reinfections[day], 0.5)
  }
  ts$dprob = dprob
  ts$reinfections_ma = frollmean(ts$reinfections, window_days)
  df <- rbind(df, ts)
}
saveRDS(df, file='data/m4_ts_data_for_analysis.RDS')
