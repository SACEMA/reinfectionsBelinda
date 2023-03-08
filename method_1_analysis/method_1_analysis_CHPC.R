argos = commandArgs(trailingOnly =TRUE)
min = as.numeric(argos[1])
max = as.numeric(argos[2])
spl = as.numeric(argos[3])
splseq = seq(from=min, to=max-spl+1, length.out=(max-min)/spl)


method <- 1

# load parameter file
load(file=paste0("method_", method, "_analysis/utils/m",method,"_parameters.RData"))

# define config path and data source 
data_source <- 'data/inf_for_sbv.RDS'
configpth <- paste0('method_',method,'_analysis/m',method,'_config_general.json')
settingspth <- 'utils/settings.RData'

#load settings to load the rest of the files
load('utils/settings.RData')

# load required packages & files
lapply(required_packages, require, character.only = TRUE)
lapply(required_files, load, envir = .GlobalEnv)

# attach config parameters from json file
attach(jsonlite::read_json(configpth))

#### creating result list
resultList= list()


funcMakeResults <- function(){
  results <- list()
  
  write('running',file="m1_output.txt",append=TRUE) #comment to confirm that theres not a zombie node
  

  ts <- generate_data(1, data_source, seed = seed_batch)
  ts_adjusted <- ts[, c("date", "observed", "ma_tot", "cases" )]
  
  #Run MCMC
  output <- do.mcmc(mcmc$n_chains, ts_adjusted)
  
  
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
  
  #5: Run simulations
  set.seed(seed_base+2023)
  sim_reinf <- function(ii){
    tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
    ex <- expected(data=ts_adjusted, parms = tmp)$expected_infections # Calculate expected reinfections using posterior
    return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
  }
  
  
  sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)

  
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
  
  ## calculate diagnostics for after wave split: 
  eri_ma <- eri_ma[date > wave_split]
  number_of_days_aw <- nrow(eri_ma)
  days_diff <- ts[date > wave_split]$ma_reinf - eri_ma$upp_reinf
  days_diff[days_diff<0] <- 0
  days_diff[days_diff>0] <- 1
  conseq_diff_aw <- frollsum(days_diff, 5, fill =0)
  proportion_aw <- length(days_diff[days_diff==1])/number_of_days_aw
  date_first_aw <- which(conseq_diff_aw==5)[1]
  
  results <- list(pscale = parameters.r$pscale[i]
          , lambda_con = lambda_convergence
          , kappa_con = kappa_convergence
          , proportion = proportion
          , date_first = which(conseq_diff==5)[1]
          , proportion_after_wavesplit = proportion_aw
          , date_first_after_wavesplit = which(conseq_diff_aw==5)[1]
  )
  saveRDS(results, file=paste0("raw_output/m",method,"/results_", a+i-1,".RDS"))
  return(results)
}


for (a in splseq){
  parameters.r <- save_params[seq(a,(a-1+spl),1),]

  cl <- startMPIcluster()
  registerDoMPI(cl)

  finalMatrix <- foreach(i=1:spl,
                           .packages = c('deSolve','foreach','Rmpi','iterators',
                                         'doMPI','doParallel','data.table', 'dplyr', 'coda')) %dopar% {
                                           tempMatrix = funcMakeResults()

                                           tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
                                         }

  saveRDS(finalMatrix, file=paste0("resultList_CHPC_m",method,"_", a,".RDS"))
  
}

resultList <- vector(mode = "list")
for (a in splseq){
  resultList = c(resultList,readRDS(file=paste0("resultList_CHPC_m",method,"_", a,".RDS")))
}

saveRDS(resultList, file="resultList_CHPC.RData")



