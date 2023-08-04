

args = commandArgs(trailingOnly=TRUE)

results <- list()

#set i, the index in the parameter set
i<-strtoi(args[1])

method <- 3

dir.create(paste0('sbv/raw_output'))
dir.create(paste0('sbv/raw_output/m', method))

load(file=paste0("sbv/method_", method, "_analysis/parameters.RData"))
data_source <- 'data/inf_for_sbv.RDS'
configpth <- paste0('sbv/method_',method,'_analysis/m',method,'_config_general.json')
settingspth <- 'utils/settings.RData'

load('utils/settings.RData')

# load required packages & files
library(data.table)
library(iterators)
library(foreach)
library(doParallel)
library(coda)
library(parallel)
library(dplyr)
library(ggplot2)
load('utils/plotting_fxns.RData')


lapply(required_files, load, envir = .GlobalEnv)

parameters.r <- save_params

attach(jsonlite::read_json(configpth))

#Set seed
seed_arg <-strtoi(args[2])
if (!exists("seed_arg") | is.na(seed_arg)) {
  print("Keep seed batch from config -- do nothing")
} else {
  print("Change seed batch from args")
  seed_batch <- seed_arg
}


results <- list()


ts <- generate_data(method, data_source, seed = seed_batch)
ts_adjusted <- ts[, c("date", "observed", "ma_tot", "cases" )]

ts <- ts[ts$date <= fit_through, ]
ts_adjusted <- ts_adjusted[ts_adjusted$date <= fit_through, ]

#Run MCMC
output <- do.mcmc(mcmc$n_chains, ts_adjusted)


#Save posterior
lambda.post <- kappa.post <- numeric(0)
smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 
jump <- round(niter/smpls)
for(ii in 1:mcmc$n_chains){
  #need smpls number of samples from each chain out of niter samples
  lambda.post <- c(lambda.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),1])
  kappa.post <- c(kappa.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),2])
}

#5: Run simulations
set.seed(seed_batch+2023)
sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii])
  answer <- expected(parms = tmp, data = ts_adjusted, delta = cutoff)
  ex2 <- Reduce("+", answer)
  ex2 <- c(rep(0,90),ex2)
  return(rnbinom(length(ex2), size=1/kappa.post[ii], mu =c(0, diff(ex2))))
}


sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)


#6: analysis
sri <- data.table(date = ts_adjusted$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]

number_of_days <- nrow(eri_ma)

days_diff <- ts[date > fit_through]$ma_reinf - eri_ma$upp_reinf
days_diff[days_diff<0] <- 0
days_diff[days_diff>0] <- 1

conseq_diff <- frollsum(days_diff, 5, fill =0)
proportion <- length(days_diff[days_diff==1])/number_of_days
date_first <- which(conseq_diff==5)[1]

# Diagnostics 
gd <- gelman.diag(output$chains)
gd$psrf <- gd$psrf[ -3,]

lambda_convergence <- gd$psrf[1]
kappa_convergence <- gd$psrf[2]

#METRICS 
#Date first below
days_diff <- ts[date <= as.Date(fit_through)]$ma_reinf - eri_ma[date <= as.Date(fit_through)]$low_reinf
days_diff[days_diff>=0] <- 0
days_diff[days_diff<0] <- 1

conseq_diff <- frollsum(days_diff, 10, fill =0)

date_first_below_10 <- which(conseq_diff==10)[1]
date_first_below_5 <- which(conseq_diff==5)[1]

#Date first above
days_diff_above <-  eri_ma[eri_ma$date<=fit_through,]$upp_reinf - ts[ts$date<= fit_through,]$ma_reinf
days_diff_above[days_diff_above>=0] <- 0
days_diff_above[days_diff_above<0] <- 1

conseq_diff <- frollsum(days_diff_above, 10, fill =0)


date_first_above_10 <- which(conseq_diff==10)[1]
date_first_above_5 <- which(conseq_diff==5)[1]


#Proportion outside
number_of_days <- nrow(eri_ma[eri_ma$date<=fit_through,])
proportion_before_ft <- (length(days_diff[days_diff==1])+length(days_diff_above[days_diff_above==1]))/number_of_days

# CONVERGENCE DIAGNOSTICS 
gd <- gelman.diag(output$chains)
gd$psrf <- gd$psrf[ -3,]

lambda_convergence <- gd$psrf[1]
kappa_convergence <- gd$psrf[2]


results <- list(pobs_1=parameters.r$pobs_1[i]
                , pobs_2=parameters.r$pobs_2[i]
                , pscale = parameters.r$pscale[i]
                , lambda_con = lambda_convergence
                , kappa_con = kappa_convergence
                , proportion_before_ft = proportion_before_ft
                , date_first_above_10 = date_first_above_10
                , date_first_above_5 = date_first_above_5
                , date_first_below_10 = date_first_below_10
                , date_first_below_5 = date_first_below_5
)

#Save results
dir.create(paste0("sbv/raw_output/m",method,"/check_data"))
saveRDS(results, file=paste0("sbv/raw_output/m",method,"/check_data/results_", i,".RDS"))

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]


plot_column <- 'observed'
plot_column_ma <- 'ma_reinf'
infections <- 2
infection <-2 

plot_sim <- function(dat, sim, sim_ma) (ggplot(dat) 
                                        + aes(x = date) 
                                        + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3)
                                        + geom_point(aes(y = !!sym(plot_column)), size = .2, color = '1', alpha = .3)
                                        + geom_ribbon(data = sim_ma, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3, fill = '2')
                                        + geom_line(aes(y = !!sym(plot_column_ma)), color = '2')
                                        + ylab(paste0('Infection number: ', infections))
                                        + xlab('Specimen receipt date')
                                        + geom_vline(aes(xintercept = 1 + as.Date(fit_through)), linetype = 3, color = 'red')
                                        + geom_vline(aes(xintercept = 1 + as.Date(wave_split)), linetype = 3, color = 'blue')
                                        + theme_minimal()
                                        + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                                , panel.grid.minor = element_blank()
                                        )
                                        + scale_x_Ms(name = 'Specimen receipt date', labels = function(bs) {
                                          gsub("^(.).+$","\\1", month.abb[month(bs)])
                                        }, minor_breaks = '1 months')
                                        + theme(panel.grid.major.x = element_blank()
                                                , axis.ticks = element_blank()
                                                , panel.grid.minor.x = element_line(color = 'lightgrey', size = .5)
                                                , panel.grid.major.y = element_line(color = 'lightgrey', size = .5)
                                                , panel.grid.minor.y = element_blank()
                                                , panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                        )
                                        + geom_vline(xintercept = c(as.Date('2021-01-01'), as.Date('2022-01-01'))
                                                     , linetype = 2, size = .5, color = '#111111')
                                        + scale_y_sqrt()
)

list_text <- paste(names(parameters.r[i,]), parameters.r[i,], sep = " = ", collapse = "; ")


wrap_title <- function(text, width = 50) {
  paste(strwrap(text, width = width), collapse = "\n")
}

inc_reinf <- (plot_sim(ts, eri, eri_ma) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 14, size = 7*0.35)
              + ggtitle(wrap_title(list_text))
)

dir.create(paste0("sbv/raw_output/m",method,"/check_data/plots"))
ggsave(inc_reinf, filename = paste0("sbv/raw_output/m",method,"/check_data/plots/sim_plot_",i,".png"), width = 6, height = 3)

