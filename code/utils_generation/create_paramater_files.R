parameters <- expand.grid(pobs_1 = seq(0.05, 1, 0.05), pobs_2 = seq(0.05, 1.0, 0.05))
save_params <- as.data.frame(parameters)
save(save_params, file = 'utils/parameters.RData')
