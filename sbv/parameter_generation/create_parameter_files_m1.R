#Create parameter files for method 1 sensitivity analysis
pscale_min = 1
pscale_max = 3
pscale_steps = 0.1

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('sbv', 'method_1_analysis', 'parameters.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args,1)

parameters <- expand.grid(pscale = seq(pscale_min, pscale_max, pscale_steps))

save_params <- as.data.frame(parameters)
save_params$extra_row = 1
save(save_params, file = target)
save_params <- save_params[save_params$pscale==1,]
save(save_params, file = paste('sbv/method_1_analysis/parameters_pscale1.RData'))
