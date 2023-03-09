
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>


suppressPackageStartupMessages({
  library(data.table)
  library(coda)
  library(ggplot2)
  library(gridExtra)
  library(patchwork)
})

.debug <- ''

.args <- if (interactive()) sprintf(c(
  file.path('output', 'posterior_90_null.RData'),
  file.path('utils', 'fit_functions.RData'),
  file.path('config_general.json'),
  2,
  file.path('output', 'convergence_plot.png')
), .debug[1]) else commandArgs(trailingOnly = TRUE)

output_dir <- './output/'
dir.create(output_dir)


load(.args[2])

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

infections <- .args[4]


# set target
target_path <- split_path(tail(.args, 1))
target <- file.path(rev(target_path[2:length(target_path)]), paste0(infections, '_', target_path[1]))


# load file
load_path <- split_path(.args[1])
load(file.path(rev(load_path[2:length(load_path)]), paste0(infections, '_', load_path[1])))


tmp <- data.table(rbind(
  cbind(chain = 1, iter = (mcmc$burnin + 1):mcmc$n_iter, output$chains[[1]])
  , cbind(chain = 2, iter = (mcmc$burnin + 1):mcmc$n_iter, output$chains[[2]])
  , cbind(chain = 3, iter = (mcmc$burnin + 1):mcmc$n_iter, output$chains[[3]])
  , cbind(chain = 4, iter = (mcmc$burnin + 1):mcmc$n_iter, output$chains[[4]])
))
tmp[, 'log(kappa)' := log10(kappa)]

lambda_trace <- (ggplot(tmp) 
  + aes(x = iter, y = lambda, color = as.character(chain))
  + geom_line(alpha = 0.7)
  + scale_colour_brewer(palette = "Set2")
  + labs(color = 'Chain')
  + ylab(expression(lambda))
  + theme_minimal()
  + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
          , panel.grid.minor = element_blank())
  + theme(axis.title.x=element_blank())
)

logkappa_trace <- (ggplot(tmp) 
               + aes(x = iter, y = `log(kappa)`, color = as.character(chain))
               + geom_line(alpha = 0.7)
               + scale_colour_brewer(palette = "Set2")
               + labs(color = 'Chain')
               + ylab(expression(log(kappa)))
               + theme_minimal()
               + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                       , panel.grid.minor = element_blank())
)

lambda2_trace <- (ggplot(tmp) 
               + aes(x = iter, y = lambda2, color = as.character(chain))
               + geom_line(alpha = 0.7)
               + scale_colour_brewer(palette = "Set2")
               + labs(color = 'Chain')
               + ylab(expression(lambda2))
               + theme_minimal()
               + xlab('iteration')
               + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                       , panel.grid.minor = element_blank())
               + theme(axis.title.x=element_blank())
)

trace_plots <- lambda_trace / logkappa_trace / lambda2_trace + plot_layout(guides = "collect") 

#trace_plots <- grid.arrange(lambda_trace, logkappa_trace, lambda2_trace, nrow=3, ncol=1)

gd <- gelman.diag(output$chains)
gd$psrf <- gd$psrf[ -4,]

convergence_diag_plot <- (ggplot(data.table(cbind(parameter = rownames(gd$psrf), gd$psrf)))
           + aes(x = as.numeric(`Point est.`), y = parameter)
           + geom_point()
           + xlim(1,max(c(1.5, as.numeric(gd$psrf[,1]))))
           + xlab(expression(hat(R)))
           + ylab('parameter')
           + geom_vline(aes(xintercept = 1.1), linetype = 3)
           + theme_minimal()
           + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                   , panel.grid.minor = element_blank()
                   , axis.text.y=element_text(size=14))
)

top_plot <- trace_plots | convergence_diag_plot

#top_plot <- grid.arrange(trace_plots, convergence_diag_plot, ncol=2, nrow=1)

lambda_density.plot <- (ggplot(tmp) 
              + geom_density(aes(x = lambda), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda')))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)

lambda_logkappa_density.plot <- (ggplot(tmp) 
              + geom_hex(aes(x = lambda, y = `log(kappa)`), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda'))) 
              + ylab(parse(text = expression('log(kappa)'))) 
              + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)

logkappa_density.plot <- (ggplot(tmp) 
              + geom_density(aes(x = `log(kappa)`), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('log(kappa)')))
              + theme(axis.title.x=element_blank())
              + theme(axis.text.x=element_blank())
)

lambda_ll.plot <- (ggplot(tmp) 
              + geom_hex(aes(x = lambda, y = ll), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('lambda'))) 
              + ylab('log likelihood') + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
)

logkappa_ll.plot <- (ggplot(tmp) 
              + geom_hex(aes(x = `log(kappa)`, y = ll), bins = 50) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab(parse(text = expression('log(kappa)'))) 
              + ylab('log likelihood') + theme(legend.position = "none") 
              + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
              + theme(axis.title.y=element_blank())
)

ll_ll.plot <- (ggplot(tmp) 
              + geom_density(aes(x = ll), size = 1.2) 
              + theme_minimal()
              + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                      , panel.grid.minor = element_blank())
              + xlab('log likelihood')
)

lambda2_density.plot <- (ggplot(tmp) 
                        + geom_density(aes(x = lambda2), size = 1.2) 
                        + theme_minimal()
                        + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                , panel.grid.minor = element_blank())
                        + xlab(parse(text = expression('lambda2')))
                        + theme(axis.title.x=element_blank())
                        + theme(axis.text.x=element_blank())
)  

lambda2_lambda_density.plot <- (ggplot(tmp) 
                                 + geom_hex(aes(x = lambda2, y = lambda), bins = 50) 
                                 + theme_minimal()
                                 + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                         , panel.grid.minor = element_blank())
                                 + xlab(parse(text = expression('lambda2'))) 
                                 + ylab(parse(text = expression('lambda'))) 
                                 + theme(legend.position = "none") 
                                 + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
                                 + theme(axis.title.x=element_blank())
                                 + theme(axis.text.x=element_blank())
)

lambda2_logkappa_density.plot <- (ggplot(tmp) 
                               + geom_hex(aes(x = lambda2, y = `log(kappa)`), bins = 50) 
                               + theme_minimal()
                               + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                       , panel.grid.minor = element_blank())
                               + xlab(parse(text = expression('lambda2'))) 
                               + ylab(parse(text = expression('log(kappa)'))) 
                               + theme(legend.position = "none") 
                               + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
                               + theme(axis.title.x=element_blank())
                               + theme(axis.text.x=element_blank())
)

lambda2_ll.plot <- (ggplot(tmp) 
                     + geom_hex(aes(x = lambda2, y = ll), bins = 50) 
                     + theme_minimal()
                     + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                             , panel.grid.minor = element_blank())
                     + xlab(parse(text = expression('lambda2'))) 
                     + ylab('log likelihood') + theme(legend.position = "none") 
                     + scale_fill_gradientn(colours = rev(colorspace::heat_hcl(25)))
                     + theme(axis.title.y=element_blank())
)

  

figS4_bottom <- (lambda2_density.plot / lambda2_lambda_density.plot) /lambda2_logkappa_density.plot /lambda2_ll.plot  | 
  (plot_spacer() /lambda_density.plot / lambda_logkappa_density.plot / lambda_ll.plot) |
  (plot_spacer() / plot_spacer() / logkappa_density.plot / logkappa_ll.plot) |
  (plot_spacer() / plot_spacer() / plot_spacer() / ll_ll.plot) 


figS4 <- (top_plot / figS4_bottom) 


if(grepl('RDS', target)){
  saveRDS(figS4, file = target)
}else{
  ggsave(figS4, filename = target, width = 9, height = 7)
}

