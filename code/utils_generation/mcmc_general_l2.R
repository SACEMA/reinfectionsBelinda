#Includes the generic mcmc functions that can be used in any cases.

# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfection


# It contains some adjusted functions used in the abovementioned file. 

suppressPackageStartupMessages({
  library(coda)
  library(parallel)
  library(data.table)
  library(dplyr)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'mcmc_functions_l2.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

utils <- './utils/'
dir.create(utils)

# Create initial bounds
initBounds.l2 <- data.frame(rbind( ## for initial conditions
  c(log(1.2e-09),log(1.75e-07)) ## lambda
  ,c(log(1/1000), log(1/0.5))## kappa
  ,c(log(1.2e-09),log(1.75e-07)))) ##lambda2
colnames(initBounds.l2) <- c('lower', 'upper')
rownames(initBounds.l2) <- c('loglambda','logkappa','loglambda2')
class(initBounds.l2[,2]) <- class(initBounds.l2[,1]) <- 'numeric'
target <- tail(.args, 1)

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
  -nllikelihood_l2(parms, data=data) + lprior(parms)
}

# Log and anlog parameters
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

mcmcSampler.l2 <- function(init.params, ## initial parameter guess
                        randInit = mcmc$rand_init, ## if T then randomly sample initial parameters instead of above value
                        seed = 1, ## RNG seed
                        ref.params=disease_params(), ## fixed parameters
                        data = ts_adjusted[date <= fit_through], ## data
                        proposer = default.proposer.l2(sdProps), ## proposal distribution
                        niter = mcmc$n_iter, ## MCMC iterations
                        nburn = mcmc$burnin){ ## iterations to automatically burn

  write(paste0('Hi', str(init.params)),file="third_infections.txt",append=TRUE) 
  
  write(paste0('init', str(init.params)),file="third_infections.txt",append=TRUE) 
  
  
  set.seed(seed) #Set seed for when generating random numbers
  if(randInit) #randInit = T means we have to use a randomly generated initial value 
    init.params <- initRand(init.params) #Calls initRand function to generate a random uniformly distributed number
  
  current.params <- init.params
  
  print(current.params)
  
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
      #print(list(lmh=lmh, proposal=exp(proposal), vv=vv, seed=seed))
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
                            min = initBounds.l2[rownames(initBounds.l2)==nm, 'lower'], #Get the lower bound of the respective parameter 
                            max =  initBounds.l2[row.names(initBounds.l2)==nm, 'upper']) #Get the upperbound based on the respective parameter)
  
  return(unlogParms(fit.params))
}

## default proposal function
#Using the normal distribution, this will calculate the proposed next value randomly.
default.proposer.l2 <- function(sdProps) {
  return(list(sdProps, type = 'default',
              fxn = function(current) {
                proposal <- current
                proposal <- proposal + rnorm(3, mean = 0, sd = sdProps)
                proposal
              }))
}


mcmcParams.l2 <- list(init.params = c(lambda = NA, kappa = NA, lambda2 = NA)
                   , seed = NA
                   , proposer = default.proposer.l2(sdProps = c(.01, .3, .01))
                   , randInit = TRUE
                   )


doChains.l2 <- function(x, mcmcParams.l2) {
  ## Below line uses mclapply to parallelize do.call over seeds
  chains <- mclapply(x, function(x) do.call(mcmcSampler.l2, within(mcmcParams.l2, {seed <- x})))
  aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains
  chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
  chains <- as.mcmc.list(chains) ## make into mcmc.list
  return(list(chains=chains, aratio = aratio))
}

do.mcmc.l2 <- function(n_chains, ts_adjusted) {
  mcmc.run <- doChains.l2(1:n_chains, mcmcParams.l2)
  return (mcmc.run)
}

split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

#Save defined functions
save(split_path, initBounds.l2, doChains.l2, mcmcParams.l2, default.proposer.l2, initRand, mcmcSampler.l2, logParms, unlogParms, lprior, llikePrior, do.mcmc.l2, file = target)

