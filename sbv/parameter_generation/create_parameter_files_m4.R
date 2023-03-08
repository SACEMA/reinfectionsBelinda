.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('sbv', 'method_4_analysis', 'parameters.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args,1)

#Create parameter files for method 2 sensitivity analysis
pscale_min = 1
pscale_max = 3
pscale_steps = 0.1

pobs1_min = 0.1
pobs1_max = 0.5
pobs1_steps = 0.1

pobs2_min = 0.1
pobs2_max = 0.5
pobs2_steps = 0.1

dprob = c(0.001, 0.01, 0.05)

parameters <- expand.grid(pscale = seq(pscale_min, pscale_max, pscale_steps), pobs_2 = seq(pobs2_min, pobs2_max, pobs2_steps), pobs_1 = seq(pobs1_min, pobs1_max, pobs1_steps), dprob=dprob)

save_params <- as.data.frame(parameters)
save(save_params, file = target)