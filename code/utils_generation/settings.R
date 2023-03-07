#These are settings used by the files that runs on the cluster
#E.g method_2_analysis/method_2_analysis_CHPC.R would use this

required_packages <-  c("data.table"
                        , "iterators"
                        , "Rmpi"
                        , "doMPI"
                        , "foreach"
                        , "doParallel"
                        , "coda"
                        , "parallel"
                        , "dplyr"
                        , "ggplot2"
                        , "jsonlite"
)

#loaded functions to send to the cluster
functions_cl <- c('disease_params'
  ,'lprior'
  ,'llikePrior'
  , 'logParms'
  , 'unlogParms'
  ,'initBounds'
  ,'mcmcSampler'
  ,'initRand'
  ,'default.proposer'
  ,'mcmcParams'
  ,'doChains'
  , 'do.mcmc'
  , 'expected'
  , 'nllikelihood'
  , 'generate_data'
  )

save(functions_cl, required_packages, file='utils/settings.RData')