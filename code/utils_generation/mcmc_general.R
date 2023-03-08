#Includes the generic mcmc functions that can be used in any cases (first reinfection, second reinfections, third reinfections)

suppressPackageStartupMessages({
  library(coda)
  library(parallel)
  library(data.table)
  library(dplyr)
})
traceback()

.debug <- ''

.args <- if (interactive()) sprintf(c(
  file.path('utils', 'mcmc_functions.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


target <- tail(.args, 1)

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
                        seed = 1, ## RNG seed
                        ref.params=disease_params(), ## fixed parameters
                        data = ts_adjusted, ## data
                        proposer = default.proposer(sdProps), ## proposal distribution
                        niter = mcmc$n_iter, ## MCMC iterations
                        nburn = mcmc$burnin){ ## iterations to automatically burn

  data <- data[date<=fit_through]
  
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


doChains <- function(x, mcmcParams, ts_adjusted) {
  args <- mcmcParams
  args$data <- ts_adjusted
  chains <- mclapply(x, function(x) do.call(mcmcSampler, within(args, {seed <- x})))
  aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains

  chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
  

  chains <- as.mcmc.list(chains) ## make into mcmc.list
  
  return(list(chains=chains, aratio = aratio))
}

do.mcmc <- function(n_chains, ts_adjusted) {

  mcmc.run <- doChains(1:n_chains, mcmcParams, ts_adjusted)
  

  return (mcmc.run)
}


save(initBounds, doChains, mcmcParams, default.proposer, initRand, mcmcSampler, logParms, unlogParms, lprior, llikePrior, do.mcmc, disease_params, file = target)
