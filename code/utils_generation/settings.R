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

required_files <- c('utils/observe_prob_functions.RData'
                , 'utils/mcmc_functions.RData'
                ,'utils/fit_functions.RData'
                ,'utils/generate_data.RData'
                ,'utils/sim_functions.RData'
                )

save(required_packages, required_files, file='utils/settings.RData')