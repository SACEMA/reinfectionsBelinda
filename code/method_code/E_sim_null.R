# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

suppressPackageStartupMessages({
  library(data.table)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('output', 'posterior_90_null.RData'), # input
  file.path('output', 'posterior_90_null_third.RData'), #input for third infections
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('utils', 'fit_functions.RData'),
  file.path('config_general.json'), #  5
  file.path('output', 'sim_90_null_third.RDS'),
  file.path('output', 'sim_90_null.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


ts <- readRDS(.args[3])

load(.args[4]) # fit functions 

configpth <- .args[5]
attach(jsonlite::read_json(configpth))

if (infection=="second") {
  load(.args[1])
  target <- tail(.args, 1)
  ts_adjusted <- ts[, c("date", "reinf", "ma_tot", "cnt" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
}

if (infection=="third"){
  load(.args[2])
  target <- tail(.args, 2)
  ts_adjusted <- ts[, c("date", "third", "ma_tot", "reinf" )]
  names(ts_adjusted) <- c("date", "observed", "ma_tot", "cases")
}


set.seed(2021)


sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
  ex <- expected(data=ts_adjusted, parms = tmp)$expected_infections # Calculate expected reinfections using posterior
  return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
}

sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)

saveRDS(sims, file = target)
