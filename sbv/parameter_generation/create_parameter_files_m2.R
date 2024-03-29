#Create parameter files for method 2 sensitivity analysis
.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('sbv', 'method_2_analysis', 'parameters.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args,1)

pscale_min = 1
pscale_max = 3
pscale_steps = 0.1
pobs2_min = 0.1
pobs2_max = 0.5
pobs2_steps = 0.1

parameters <- expand.grid(pscale = seq(pscale_min, pscale_max, pscale_steps), pobs_2 = seq(pobs2_min, pobs2_max, pobs2_steps))

save_params <- as.data.frame(parameters)
save(save_params, file = target)

save_params$index <- seq(1, nrow(save_params))

save_params <- save_params[save_params$pscale==1,]

save(save_params, file = paste('sbv/method_2_analysis/parameters_pscale1.RData'))