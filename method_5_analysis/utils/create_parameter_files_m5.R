#Create parameter files for method 5 sensitivity analysis
method <- 5

pscale_min = 1
pscale_max = 3
pscale_steps = 0.5

steep = c(0.0001, 0.0002, 0.00005)

xm_min = 10000
xm_max = 50000
xm_steps = 20000

pobs_1_min = c(0.1, 0.05, 0.025)
pobs_1_max = c(0.1, 0.2, 0.3)


parameters <- expand.grid(
  pscale = seq(pscale_min, pscale_max, pscale_steps)
  , steep=steep
  , xm=seq(xm_min, xm_max, xm_steps)
  , pobs_1_min=pobs_1_min
  , pobs_1_max=pobs_1_max
  , multiplier = c(1,2,3)
)

parameters$pobs_2_min <- parameters$multiplier * parameters$pobs_1_min
parameters$pobs_2_max <- parameters$multiplier * parameters$pobs_1_max

final <- as.data.frame(parameters)

save_params <- data.frame()

for (i in 1:nrow(final)) { 
  row <- final[i, ]
  if ( row$pobs_1_max != row$pobs_1_min && row$pobs_2_max!=row$pobs_2_min )
    save_params <- rbind(save_params, row)
}

save(save_params, file = paste0('method_', method, '_analysis/utils/m', method, '_parameters.RData'))

