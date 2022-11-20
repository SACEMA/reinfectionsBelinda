#pscale analysis for specified method
library(foreach)
library(data.table)
library(parallel)
library(coda)
library(dplyr)

method <- "2"

target <- paste0('pscale_analysis/',method,'_pscale_analysis.RDS')

if (file.exists(target)) 
  save_output <- readRDS(target)
if (!file.exists(target)){
  save_output = data.frame(pscale=numeric(0),first_cluster=numeric(0),proportion=numeric(0))
}

parameters <- expand.grid(pscale = seq(1, 3, 0.1))

#get params
attach(jsonlite::read_json('config_general.json'))
attach(jsonlite::read_json('parameters.json'))

#load utils
load('utils/generate_method_data.RData')
load('utils/mcmc_functions.RData')
load('utils/fit_functions.RData')
load('utils/observe_prob_functions.RData')

resultList <- list()
ts_data <- readRDS('data/infection_data.RDS')
  
for (pscale in parameters$pscale){
    results <- list()
    #get data
    if (method==1)
      ts_adjusted <- generate_m1B(pscale_param = pscale, data=ts_data)
    if (method==2)
      ts_adjusted <- generate_m2B(pscale_param = pscale, data=ts_data)
    if (method==3)
      ts_adjusted <- generate_m3B(pscale_param = pscale, data=ts_data)
    if (method==4)
      ts_adjusted <- generate_m4B(pscale_param = pscale, data=ts_data)
    if (method=="5a")
      ts_adjusted <- generate_m5aB(pscale_param = pscale, data=ts_data)
    if (method=="5b")
      ts_adjusted <- generate_m5bB(pscale_param = pscale, data=ts_data)
    if (method=="5c")
      ts_adjusted <- generate_m5cB(pscale_param = pscale, data=ts_data)
    
    set.seed(0)
    
    
    # Create initial bounds
    initBounds <- data.frame(rbind( ## for initial conditions
      c(log(1.2e-09),log(1.75e-07)) ## lambda
      ,c(log(1/1000), log(1/0.5))))## kappa
    colnames(initBounds) <- c('lower','upper')
    rownames(initBounds) <- c('loglambda','logkappa')
    class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'
    
    output <- do.mcmc(mcmc$n_chains, ts_adjusted)
    
    lambda.post <- kappa.post <- numeric(0)
    smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
    niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 
    
    jump <- round(niter/smpls)
    for(ii in 1:mcmc$n_chains){
      #need smpls number of samples from each chain out of niter samples
      lambda.post <- c(lambda.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),1])
      kappa.post <- c(kappa.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),2])
    }
    
    sim_reinf <- function(ii){
      tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
      ex <- expected(data=ts_adjusted, parms = tmp)$expected_infections # Calculate expected reinfections using posterior
      return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
    }
    
    sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)
    
    sri <- data.table(date = ts_adjusted$date, sims)
    sri_long <- melt(sri, id.vars = 'date')
    sri_long[, ma_val := frollmean(value, 7), variable]
    eri <- sri_long[, .(exp_reinf = median(value)
                        , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                        , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]
    
    eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                           , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                           , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]
    eri_ma <- eri_ma[date > fit_through]
    eri <- eri[date > fit_through]
    
    
    
    
    eri_ma <- eri_ma[date > omicron_date]
    eri <- eri[date > omicron_date]
  
    number_of_days <- nrow(eri_ma)
    
    days_diff <- ts_adjusted[date > omicron_date]$ma_reinf - eri_ma$upp_reinf
    days_diff[days_diff<0] <- 0
    days_diff[days_diff>0] <- 1
    
    conseq_diff <- frollsum(days_diff, 5, fill =0)
    proportion <- length(days_diff[days_diff==1])/number_of_days
    first_cluster <- which(conseq_diff==5)[1]
    
    
    save_output[nrow(save_output) + 1,] = c(pscale,first_cluster,proportion)
    saveRDS(save_output, file=target)
    
    gc()
}
