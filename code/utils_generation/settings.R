#These are settings used by the files that runs on the cluster
#E.g method_2_analysis/method_2_analysis_CHPC.R would use this

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'settings.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

dir.create('utils')

target <- tail(.args, 1)

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
                )

save(required_packages, required_files, file=target)