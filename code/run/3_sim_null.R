# File, code & functions adapted from
# Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>
# 
# File was adapted to include nth infections
#
suppressPackageStartupMessages({
  library(data.table)
  library(english)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('output', 'posterior_90_null.RData'), # input
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'fit_functions.RData'),
  file.path('config_general.json'), # NOTE: change this to do full run!
  3, 
  file.path('output', 'sim_90_null.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

output_dir <- './output/'
dir.create(output_dir)

#Get infection data
ts <- readRDS(.args[2])

#Load fitting functions
load(.args[3])

#Attach config path
configpth <- .args[4]
attach(jsonlite::read_json(configpth))

infections <- .args[5]

# set target
target_path <- split_path(tail(.args, 1))
target <- file.path(rev(target_path[2:length(target_path)]), paste0(infections, '_', target_path[1]))

# load file
load_path <- split_path(.args[1])
load(file.path(rev(load_path[2:length(load_path)]), paste0(infections, '_', load_path[1])))

if (infections > 3){
  select <- c("date", ordinal(i), "ma_tot", ordinal(i-1))
  ts_adjusted <- ts[, ..select]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
} 

if (infections==2){
  ts_adjusted <- ts[, c("date", "reinf", "ma_tot", "cnt" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
}

if (infections==3){
  ts_adjusted <- ts[, c("date", "third", "ma_tot", "reinf" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
}

#set seed
set.seed(1)


sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii], lambda2 = lambda2.post[ii])
  answer <- expected_l2(parms = tmp, data = ts_adjusted, delta = cutoff)
  ex2 <- Reduce("+", answer)
  ex2 <- c(rep(0,90),ex2)
  return(rnbinom(length(ex2), size=1/kappa.post[ii], mu =c(0, diff(ex2))))
}



sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)
  
  
saveRDS(sims, file = target)


