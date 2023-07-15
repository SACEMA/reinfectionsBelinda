write("start",file="third_infections.txt",append=TRUE)

args = commandArgs(trailingOnly=TRUE)

results <- list()

#set i, the index in the parameter set
i<-strtoi(args[1])

if (!exists("i") | is.na(i))
  i <- 1


method <- 'third'

write(paste0("set number", i),file="third_infections.txt",append=TRUE)

dir.create(paste0('sbv/raw_output'))
dir.create(paste0('sbv/raw_output/mthird'))

load(file=paste0("sbv/third_infections/parameters.RData"))
data_source <- 'data/inf_for_sbv_v4.RDS'
configpth <- paste0('sbv/third_infections/config_third_infections.json')
settingspth <- 'utils/settings.RData'

load('utils/settings.RData')

# load required packages & files
library(data.table)
library(iterators)
library(foreach)
library(doParallel)
library(coda)
library(parallel)
library(dplyr)
library(ggplot2)

lapply(required_files, load, envir = .GlobalEnv)
load('utils/mcmc_functions.RData')

parameters.r <- save_params

attach(jsonlite::read_json(configpth))

fit_through <- omicron_date #added this line to avoid the number of different config files
wave_split <- omicron_date 

#set seed from argument IF it exits
seed_arg <-strtoi(args[2])
if (!exists("seed_arg") | is.na(seed_arg)) {
  print("Keep seed batch from config -- do nothing")
} else {
  print("Change seed batch from args")
  seed_batch <- seed_arg
}
  

target_mcmc <- paste0('sbv/third_infections/posterior_null_ouput_i_', i, '_seed_', seed_batch, '.RData' )


results <- list()

write('running',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

ts <- generate_data_third(data_source, seed = seed_batch)
ts_adjusted <- ts[, c("date", "observed", "ma_tot", "cases" )]

#Run MCMC
output <- do.mcmc(mcmc$n_chains, ts_adjusted)


write('done mcmc',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

#Save posterior
lambda.post <- kappa.post <- numeric(0)
smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 
jump <- round(niter/smpls)
for(ii in 1:mcmc$n_chains){
  #need smpls number of samples from each chain out of niter samples
  lambda.post <- c(lambda.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),1])
  kappa.post <- c(kappa.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),2])
}

#save mcmc results
save(output, lambda.post, kappa.post, file=target_mcmc)

#5: Run simulations
set.seed(seed_batch+2023)
sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
  answer <- expected(parms = tmp, data = ts_adjusted, delta = cutoff)
  ex2 <- Reduce("+", answer)
  ex2 <- c(rep(0,90),ex2)
  return(rnbinom(length(ex2), size=1/kappa.post[ii], mu =c(0, diff(ex2))))
}

write('start sims',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)

write('end sims',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

#6: analysis
sri <- data.table(date = ts_adjusted$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]


eri_ma <- eri_ma[date > fit_through]


number_of_days <- nrow(eri_ma)

days_diff <- ts[date > fit_through]$ma_reinf - eri_ma$upp_reinf
days_diff[days_diff<0] <- 0
days_diff[days_diff>0] <- 1

conseq_diff <- frollsum(days_diff, 5, fill =0)
proportion <- length(days_diff[days_diff==1])/number_of_days
date_first <- which(conseq_diff==5)[1]

# Diagnostics 
gd <- gelman.diag(output$chains)
gd$psrf <- gd$psrf[ -3,]

lambda_convergence <- gd$psrf[1]
kappa_convergence <- gd$psrf[2]

results <- list(pscale = parameters.r$pscale[i]
                , lambda_con = lambda_convergence
                , kappa_con = kappa_convergence
                , proportion = proportion
                , date_first = which(conseq_diff==5)[1]
                , seed = seed_batch
)
saveRDS(results, file=paste0("sbv/raw_output/mthird/results_", i,".RDS"))
