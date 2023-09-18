# Pipeline in this file adapted from
# Repository: <https://github.com/jrcpulliam/reinfections>
#
# The MCMC sampler is based on code originally written by Steve Bellan as part of the 
# International Clinics on Infectious Disease Dynamics and Data (ICI3D) program, 
# which is made available via a CC-BY International license. (Bellan 2015)
# https://github.com/ICI3D/RTutorials/blob/master/ICI3D_Lab8_MCMC-SI_HIV.R

# File to run MCMC without an additional lambda parameter for nth infections

suppressPackageStartupMessages({
  library(coda)
  library(parallel)
  library(data.table)
  library(dplyr)
  library(english)
})


.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'mcmc_functions.RData'),
  file.path('utils', 'fit_functions.RData'),
  file.path('config_general.json'), 
  3, #which infection? 
  file.path('output', 'posterior_90_null.RData')
), .debug[1]) else commandArgs(trailingOnly = TRUE)


#function that splits path
split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}
output_dir <- './output/'
dir.create(output_dir)

load(.args[2]) #Load the mcmc functions
load(.args[3]) #Load the fitting functions

ts <- readRDS(.args[1]) #Original observed cases

configpth <- .args[4]

attach(jsonlite::read_json(configpth))

# for which number of reinfections are we doing the simulations? 
infections <- .args[5]

target_path <- split_path(tail(.args, 1))
target <- file.path(rev(target_path[2:length(target_path)]), paste0(infections, '_', target_path[1]))


if (infections > 3){
  select <- c("date", ordinal(i), "ma_tot", ordinal(i-1))
  ts_adjusted <- ts[, ..select]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
} 
if (infections == 3){
  select <- c("date", "third", "ma_tot", "reinf")
  ts_adjusted <- ts[, ..select]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
} 
if (infections == 2){
  ts_adjusted <- ts[, c("date", "reinf", "ma_tot", "cnt" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
}


# This parameters are used as the original 'compare to' values in the MCMC fit. 
disease_params <- function(lambda = .000000015 ## hazard coefficient
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

output <- do.mcmc(mcmc$n_chains, ts_adjusted)

#Save posterior
lambda.post <- kappa.post <- numeric(0)
smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 

jump <- round(niter/smpls)
for(ii in 1:mcmc$n_chains){
  #need smpls number of samples from each chain out of niter samples
  lambda.post <- c(lambda.post, output$chains[[ii]][seq(1, niter, jump), 1])
  kappa.post <- c(kappa.post, output$chains[[ii]][seq(1, niter, jump), 2])
}

save(output, lambda.post, kappa.post, file=target)

