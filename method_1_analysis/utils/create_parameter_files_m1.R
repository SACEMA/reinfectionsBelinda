#Create parameter files for method 1 sensitivity analysis
pscale_min = 1
pscale_max = 3
pscale_steps = 0.1


parameters <- expand.grid(pscale = seq(pscale_min, pscale_max, pscale_steps))

save_params <- as.data.frame(parameters)
save_params$extra_row = 1
save(save_params, file = 'method_1_analysis/utils/m1_parameters.RData')
