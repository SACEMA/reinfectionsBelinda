#Run MCMC for THIRD infections

suppressPackageStartupMessages({
  library(coda)
  library(parallel)
  library(data.table)
  library(dplyr)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'mcmc_functions.RData'),
  file.path('utils', 'fit_functions.RData'),
  file.path('config_general.json'), 
  file.path('output', 'posterior_90_null_third.RData'), # output
  file.path('output', 'posterior_90_null.RData')
), .debug[1]) else commandArgs(trailingOnly = TRUE)


load(.args[2]) #Load the mcmc functions
load(.args[3]) #Load the fitting functions

ts <- readRDS(.args[1]) #Original observed cases



configpth <- .args[4]



attach(jsonlite::read_json(configpth))



#Adjust the ts to have columns needed for analysis
if (infection=="third"){
  ts_adjusted <- ts[, c("date", "third", "ma_tot", "reinf" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
  target <- tail(.args, 2)
}
if (infection == "second") {
  ts_adjusted <- ts[, c("date", "reinf", "ma_tot", "cnt" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
  target <- tail(.args, 1)
}


#Create disease parameters
#disease_params <-list(lambda=mean(posteriors$lambda), kappa=mean(posteriors$kappa)) 

# This parameters are used as the original 'compare to' values in the MCMC fit. 
disease_params <- function(lambda = .000000015 ## hazard coefficient 0.000000015
                           , kappa = 0.1 ## dispersion (inverse)
) return(as.list(environment()))


## Create initial bounds
initBounds <- data.frame(rbind( ## for initial conditions
  c(log(1.2e-09),log(1.75e-07)) ## lambda
  ,c(log(1/1000), log(1/0.5))))## kappa
colnames(initBounds) <- c('lower','upper')
rownames(initBounds) <- c('loglambda','logkappa')
class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'


#Run MCMC
output <- do.mcmc(4)


#Save posterior
lambda.post <- kappa.post <- numeric(0)
smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 

if (posterior_method == "last") {
  #Takes the last 400 iterations from each chain for the posterior
  for(ii in 1:mcmc$n_chains){
    lambda.post <- c(lambda.post, output$chains[[ii]][(niter+1-smpls):niter, 1])
    kappa.post <- c(kappa.post, output$chains[[ii]][(niter+1-smpls):niter, 2])
  }
}

if (posterior_method == "select") {
  jump <- round(niter/smpls)
  for(ii in 1:mcmc$n_chains){
    #need smpls number of samples from each chain out of niter samples
    lambda.post <- c(lambda.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),1])
    kappa.post <- c(kappa.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),2])
  }
}



save(output, lambda.post, kappa.post,  file=target)

