argos = commandArgs(trailingOnly =TRUE)
min = as.numeric(argos[1])
max = as.numeric(argos[2])
spl = as.numeric(argos[3])
splseq = seq(from=min, to=max-spl+1, length.out=(max-min)/spl)

#Parameters defined
window_days <- 7
reinf_hazard <- 1.38866e-08
cutoff <- 90
mcmc <- list(rand_init=TRUE, burnin=2000, n_iter=5000, n_posterior=1600, n_chains=4)
n_sims_per_param <- 100
fit_through <- '2021-02-28'
wave_split <- '2021-05-01'

library(data.table)
library(iterators)
library(Rmpi)
library(doMPI)
library(foreach)
library(doParallel)
library(coda)
library(parallel)
library(dplyr)
library(ggplot2)



#### creating result list
resultList= list()


funcMakeResults <- function(){
  results <- list()
  
  ############## MCMC FUNCTIONS ############
  disease_params <- function(lambda = .000000015 ## hazard coefficient 0.000000015
                             , kappa = 0.1 ## dispersion (inverse)
  ) return(as.list(environment()))
  
  
  lprior <- function(parms=disease_params()) with(parms, {
    lp <- 0;
    return (lp);
  })
  
  ## Sum log-likelihood & log-prior for evaluation inside MCMC sampler
  llikePrior <- function(fit.params = NULL,
                         ref.params = disease_params(),
                         data = ts_adjusted[date <= fit_through]) {
    parms <- within(ref.params, {
      for (nm in names(fit.params))
        assign(nm, as.numeric(fit.params[nm]))
      rm(nm)
    })
    -nllikelihood(parms, data=data) + lprior(parms)
  }
  
  # Want to be able to easily log and unlog parameters
  logParms <- function(alist) {
    alist <- log(alist)
    names(alist) <- paste0('log',names(alist))
    return(alist)
  }
  unlogParms <- function(alist) {
    alist <- exp(alist)
    names(alist) <- sub('log','', names(alist))
    return(alist)
  }
  
  # Create initial bounds
  initBounds <- data.frame(rbind( ## for initial conditions
    c(log(1.2e-09),log(1.75e-07)) ## lambda
    ,c(log(1/1000), log(1/0.5))))## kappa
  colnames(initBounds) <- c('lower','upper')
  rownames(initBounds) <- c('loglambda','logkappa')
  class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'
  
  mcmcSampler <- function(init.params, ## initial parameter guess
                          randInit = TRUE, ## if T then randomly sample initial parameters instead of above value
                          seed = 2, ## RNG seed
                          ref.params=disease_params(), ## fixed parameters
                          data = ts_adjusted[date <= fit_through], ## data
                          proposer = default.proposer(sdProps), ## proposal distribution
                          niter = mcmc$n_iter, ## MCMC iterations
                          nburn = mcmc$burnin){ ## iterations to automatically burn
    set.seed(seed) #Set seed for when generating random numbers
    if(randInit) #randInit = T means we have to use a randomly generated initial value 
      init.params <- initRand(init.params) #Calls initRand function to generate a random uniformly distributed number
    
    current.params <- init.params
    
    nfitted <- length(current.params) # How maby parameters are we trying to fit? 
    
    vv <- 2 # MCMC iteration at which we are currently at. 
    
    accept <- 0 ## initialize proportion of iterations accepted
    
    ## Calculate log(likelihood X prior) for first value
    curVal <- llikePrior(current.params, ref.params = ref.params, data=data) #Use the ref.params(disease.params) to see if we can accept the initial parameters
    
    ## Initialize matrix to store MCMC chain
    # 1000 iterations
    out <- matrix(NA, nr = niter, nc=length(current.params)+1)
    #This creates an array with three entries: the current lambda parameter, the current kappa parameter and the current value
    out[1,] <- c(current.params, ll = curVal) ## add first value
    colnames(out) <- c(names(current.params), 'll') ## name columns
    ## Store original covariance matrix
    #Iterates from 2 to 1000 to complete the matrix with the output of each iteration
    while(vv <= niter) {
      
      
      proposal <- proposer$fxn(logParms(current.params))
      proposal <- unlogParms(proposal)
      propVal <- llikePrior(proposal, ref.params = ref.params, data=data)
      
      lmh <- propVal - curVal ## likelihood ratio = log likelihood difference
      if (is.na(lmh)) { ## if NA, print informative info but don't accept it
        print(list(lmh=lmh, proposal=exp(proposal), vv=vv, seed=seed, ref.params = ref.params, current.params=current.params))
      } else { ## if it's not NA then do acception/rejection algorithm
        ## if MHR >= 1 or a uniform random # in [0,1] is <= MHR, accept otherwise reject
        if ( (lmh >= 0) | (runif(1,0,1) <= exp(lmh)) ) {
          current.params <- proposal
          if (vv>nburn) accept <- accept + 1 ## only track acceptance after burn-in
          curVal <- propVal
        }
      }
      out[vv, ] <- c(current.params, ll = curVal)
      vv <- vv+1
      aratio <- accept/((vv-nburn))
    }
    colnames(out) <- c(names(current.params), 'll')
    #The as.mcmc function is an R function that: Coerces MCMC objects to an mcmc object.
    samp <- as.mcmc(out[1:nrow(out)>(nburn),], start = nburn + 1)
    return(list(ref.params=ref.params
                , seed = seed
                , init.params = init.params
                , aratio = aratio
                , samp = samp
    ))
  }
  
  ## Randomly select a value that is uniformly distributed between these bounds
  initRand <- function(fit.params) {
    fit.params <- logParms(fit.params) #Get the log values for the parameters that we are fitting
    tempnm <- names(fit.params)
    for(nm in tempnm)
      #runif function generates random deviates of the uniform distribution (runif(n, min = 0, max = 1))
      fit.params[nm] <- runif(1,#Generate one deviate
                              min = initBounds[rownames(initBounds)==nm, 'lower'], #Get the lower bound of the respective parameter 
                              max =  initBounds[row.names(initBounds)==nm, 'upper']) #Get the upperbound based on the respective parameter)
    
    return(unlogParms(fit.params))
  }
  
  ## default proposal function
  #Using the normal distribution, this will calculate the proposed next value randomly.
  default.proposer <- function(sdProps) {
    return(list(sdProps, type = 'default',
                fxn = function(current) {
                  proposal <- current
                  proposal <- proposal + rnorm(2, mean = 0, sd = sdProps)
                  proposal
                }))
  }
  
  
  mcmcParams <- list(init.params = c(lambda = NA, kappa = NA)
                     , seed = NA
                     , proposer = default.proposer(sdProps = c(.01, .3))
                     , randInit = TRUE
  )
  
  
  doChains <- function(x, mcmcParams) {
    chains <- mclapply(x, function(x) do.call(mcmcSampler, within(mcmcParams, {seed <- x})))
    aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains
    chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
    chains <- as.mcmc.list(chains) ## make into mcmc.list
    return(list(chains=chains, aratio = aratio))
  }
  
  do.mcmc <- function(n_chains) {
    mcmc.run <- doChains(1:n_chains, mcmcParams)
    return (mcmc.run)
  }
  
  expected <- function(parms = disease_params(), data, delta=cutoff ) with(parms, {
    hz <- lambda * data$ma_tot
    
    out <- data.frame(date=data$date, expected_infections = rep(0, nrow(data)))
    
    for (day in 1:(nrow(data)-delta)){
      tmp <- data$cases[day] * (1-exp(-cumsum(hz[(day+delta):nrow(data)])))
      out$expected_infections[(day+delta):nrow(data)] <- out$expected_infections[(day+delta):nrow(data)]+tmp
    }
    return (out)
    
  })
  
  
  nllikelihood <- function(parms = disease_params(), data) with(parms, {
    tmp <- expected(parms, data)
    log_p <- dnbinom(data$observed, size=1/kappa, mu=c(0,diff(tmp$expected_infections)), log=TRUE)
    -sum(log_p)
    return(-sum(log_p))
  })
  ########### MCMC FUNCTIONS END ############
  
  ###### DATA GENERATION #####
  
  ### 1: Get the data
  
  write(paste0("Starting ", i),file="myfile.txt",append=TRUE)
  
  ts <- readRDS('data/inf_for_sbv.RDS')
  set.seed(1)
  
  #ts[infections < 0, infections := 0]
  ts[, infections_ma := frollmean(infections, window_days)]
  
  
  ## 2: Calculate the number of primary infections and reinfections
  #Method 3 adjustment
  for (day in 1:nrow(ts)) {
    ts$infections[day] = rbinom(1, ts$infections[day], parameters.r$pobs_1[i])
  }
  
  ts[, reinfections := 0]
  ts[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]
  ts$infections_ma = frollmean(ts$infections, window_days)
  
  #Method 4 adjustment
  for (day in 1:nrow(ts))
    ts$deaths[day] = rbinom(1, ts$infections[day], parameters.r$dprob[i])
  
  for (day in (cutoff+1):nrow(ts)) { 
    ts$eligible_for_reinf[day] =max(ts$eligible_for_reinf[day] - sum(ts$reinfections[1:day-1]) - sum(ts$deaths[1:day-1]),0)
    if (ts$date[day]<=wave_split) {
      ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day])
    } else {
      ts$reinfections[day] = round(reinf_hazard * ts$infections[day] * ts$eligible_for_reinf[day] * parameters.r$pscale[i])
    } 
    #Method 2 adjustment
    ts$reinfections[day] = rbinom(1, ts$reinfections[day], parameters.r$pobs_2[i])
  }
  
  
  ## 3: Rename column names for MCMC
  names(ts)[2] <- "cases"
  names(ts)[3] <- "ma_cnt"
  names(ts)[4] <- "observed"
  ts[, tot := observed + cases]
  ts[, ma_tot := frollmean(tot, window_days)]
  ts[, ma_reinf := frollmean(observed, window_days)]
  #Adjust the ts to have columns needed for analysis
  ts_adjusted <- ts[, c("date", "observed", "ma_tot", "cases" )]
  
  write(paste0("Doing MCMC ", i),file="myfile.txt",append=TRUE)
  #Run MCMC
  output <- do.mcmc(mcmc$n_chains)
  write(paste0("MCMC Done ", i),file="myfile.txt",append=TRUE)
  
  
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
  set.seed(2022)
  sim_reinf <- function(ii){
    tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
    ex <- expected(data=ts_adjusted, parms = tmp)$expected_infections # Calculate expected reinfections using posterior
    return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
  }
  
  write(paste0("Starting Sims ", i),file="myfile.txt",append=TRUE)
  
  sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)
  write(paste0("Sims Done ", i),file="myfile.txt",append=TRUE)
  
  
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
  
  results <- list(pobs_1=parameters.r$pobs_1[i] 
                  , pobs_2=parameters.r$pobs_2[i]
                  , pscale = parameters.r$pscale[i]
                  , dprob = parameters.r$dprob[i]
                  , lambda_con = lambda_convergence
                  , kappa_con = kappa_convergence
                  , proportion = proportion
                  , date_first = which(conseq_diff==5)[1]
                  , proportion_after_wavesplit = proportion_aw
                  , date_first_after_wavesplit = which(conseq_diff_aw==5)[1]
  )
  write(paste0("Done with ", i),file="myfile.txt",append=TRUE)
  
  saveRDS(results, file=paste0("raw_output/m4/results_", a+i-1,".RDS"))
  
  return(results)
}


for (a in splseq){
  load(file="method_4_analysis/utils/m4_parameters.RData")
  parameters.r <- save_params[seq(a,(a-1+spl),1),]
  write(paste0(a,"Start batch a"),file="myfile.txt",append=TRUE)
  
  cl <- startMPIcluster()
  registerDoMPI(cl)

  finalMatrix <- foreach(i=1:spl,
                           .packages = c('deSolve','foreach','Rmpi','iterators',
                                         'doMPI','doParallel','data.table', 'dplyr', 'coda')) %dopar% {
                                           tempMatrix = funcMakeResults()

                                           tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
                                         }

    saveRDS(finalMatrix, file=paste0("resultList_CHPC_m4_", a,".RDS"))
    write("Start end batch ",file="myfile.txt",append=TRUE)
}

resultList <- vector(mode = "list")
for (a in splseq){
  resultList = c(resultList,readRDS(file=paste0("resultList_CHPC_m4_", a,".RDS")))
}

saveRDS(resultList, file="resultList_CHPC_m4.RData")



