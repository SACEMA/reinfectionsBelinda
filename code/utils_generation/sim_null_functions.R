sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
  ex <- expected(data=ts_adjusted, parms = tmp)$expected_infections # Calculate expected reinfections using posterior
  return(rnbinom(length(ex), size=1/kappa.post[ii], mu =c(0, diff(ex))))
}

save(sim_reinf, file='utils/sim_functions.RData')