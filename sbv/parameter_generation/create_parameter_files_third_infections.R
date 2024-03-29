#Create parameter files for third infections
.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('sbv', 'third_infections', 'parameters.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args,1)

pscale_min = 1
pscale_max = 3
pscale_steps = 0.2
pobs_2 <- 0.5
pobs_1 <- 0.2
pobs_3 <- 0.35

parameters <- expand.grid(pscale = seq(pscale_min, pscale_max, pscale_steps), pobs_2 = pobs_2, pobs_1 = pobs_1, pobs_3 = pobs_3)

save_params <- as.data.frame(parameters)
save(save_params, file = target)

target <- 'sbv/third_infections/parameters_increase.RData'

pscale_1 = 2.8
pscale_2 = c(1.2, 1.5, 2)
pobs_2 <- 0.5
pobs_1 <- 0.2
pobs_3 <- 0.35

parameters <- expand.grid(pscale1 = pscale_1, pscale2 = pscale_2, pobs_2 = pobs_2, pobs_1 = pobs_1, pobs_3 = pobs_3)

save_params <- as.data.frame(parameters)
save(save_params, file = target)
