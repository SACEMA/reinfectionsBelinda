.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('sbv', 'method_5_analysis', 'parameters.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args,1)


#Create parameter files for method 5 sensitivity analysis
method <- 5

pscale = c(1, 1.2, 1.5)

steep = c(0.00005, 0.0001)

xm_min = 30000
xm_max = 50000
xm_steps = 10000

pobs_1_min = c(0.1, 0.2, 0.3, 0.4, 0.5)
pobs_1_max = c(0.1, 0.2, 0.3, 0.4, 0.5)
pobs_2_min = c(0.1, 0.2, 0.3, 0.4, 0.5)
pobs_2_max = c(0.1, 0.2, 0.3, 0.4, 0.5)



parameters <- expand.grid(
  xm=seq(xm_min, xm_max, xm_steps)
  , pobs_1_min=pobs_1_min
  , pobs_1_max=pobs_1_max
  , pobs_2_min=pobs_2_min
  , pobs_2_max=pobs_2_max
  , steep=steep
  , pscale = pscale
)


final <- as.data.frame(parameters)

save_params <- data.frame()


parameters <- subset(parameters, pobs_1_max > pobs_1_min)
parameters <- parameters[parameters$pobs_2_min >= parameters$pobs_1_min, ]
parameters <- parameters[parameters$pobs_2_max >= parameters$pobs_1_max, ]
parameters <- parameters[parameters$pobs_2_max > parameters$pobs_2_min, ]


save_params <- parameters

rownames(save_params) <- NULL


save(save_params, file = target)

