
args = commandArgs(trailingOnly=TRUE)

results <- list()

#set i, the index in the parameter set
i<-strtoi(args[1])

if (!exists("i") | is.na(i))
  i <- 1

method <- 'third'

dir.create(paste0('sbv/raw_output'))
dir.create(paste0('sbv/raw_output/mthird_increase'))


load(file=paste0("sbv/third_infections/parameters_increase.RData"))
data_source <- 'data/inf_for_sbv_v4.RDS'
configpth <- paste0('sbv/third_infections/config_third_infections.json')
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

lapply(required_files_third_l2, load, envir = .GlobalEnv)

load('utils/mcmc_functions_l2.RData')
load('utils/plotting_fxns.RData')
parameters.r <- save_params

attach(jsonlite::read_json(configpth))

wave_split <- omicron_date 

seed_arg <-strtoi(args[2])
if (!exists("seed_arg") | is.na(seed_arg)) {
  print("Keep seed batch from config -- do nothing")
} else {
  print("Change seed batch from args")
  seed_batch <- seed_arg
}


results <- list()



ts <- generate_data_third_increase(data_source, seed = seed_batch)
ts_adjusted <- ts[, c("date", "observed", "ma_tot", "cases" )]

#Run MCMC

disease_params <- function(lambda = .000000015 ## hazard coefficient
                           , kappa = 0.1 ## dispersion (inverse)
                           , lambda2 = .000000018
) return(as.list(environment()))


output <- do.mcmc.l2(mcmc$n_chains, ts_adjusted)



#Save posterior
lambda.post <- kappa.post <- lambda2.post <-  numeric(0)
smpls <- mcmc$n_posterior / mcmc$n_chains #number of samples to take from each chain
niter <- mcmc$n_iter - mcmc$burnin  #number of iterations to take into account 
jump <- round(niter/smpls)
for(ii in 1:mcmc$n_chains){
  #need smpls number of samples from each chain out of niter samples
  lambda.post <- c(lambda.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),1])
  kappa.post <- c(kappa.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),2])
  lambda2.post <- c(lambda2.post, output$chains[[ii]][seq(1,mcmc$n_iter-mcmc$burnin,jump),3])
}

#5: Run simulations
set.seed(seed_batch+2023)
sim_reinf <- function(ii){
  tmp <- list(lambda = lambda.post[ii], kappa = kappa.post[ii], lambda2 = lambda2.post[ii])
  answer <- expected_l2(parms = tmp, data = ts_adjusted, delta = cutoff)
  ex2 <- Reduce("+", answer)
  ex2 <- c(rep(0,90),ex2)
  return(rnbinom(length(ex2), size=1/kappa.post[ii], mu =c(0, diff(ex2))))
}



write('start sims',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

sims <- sapply(rep(1:mcmc$n_posterior, n_sims_per_param), sim_reinf)

write('end sims',file="third_infections.txt",append=TRUE) #comment to confirm that theres not a zombie node

#6: analysis
sri <- data.table(date = ts_adjusted$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]


eri_ma <- eri_ma[date > fit_through]


number_of_days <- nrow(eri_ma)

days_diff <- ts[date > fit_through]$ma_reinf - eri_ma$upp_reinf
days_diff[days_diff<0] <- 0
days_diff[days_diff>0] <- 1

conseq_diff <- frollsum(days_diff, 5, fill =0)
proportion <- length(days_diff[days_diff==1])/number_of_days
date_first <- which(conseq_diff==5)[1]


eri_ma <- eri_ma[date > wave_split_2]


number_of_days <- nrow(eri_ma)

days_diff <- ts[date > wave_split_2]$ma_reinf - eri_ma$upp_reinf
days_diff[days_diff<0] <- 0
days_diff[days_diff>0] <- 1

conseq_diff <- frollsum(days_diff, 5, fill =0)
proportion_aw2 <- length(days_diff[days_diff==1])/number_of_days
date_first_aw2 <- which(conseq_diff==5)[1]


# Diagnostics 
gd <- gelman.diag(output$chains)
gd$psrf <- gd$psrf[ -4,]

lambda_convergence <- gd$psrf[1]
kappa_convergence <- gd$psrf[2]
lambda2_convergence <- gd$psrf[3]

# not calculating after wave split because omicron_date/wave_split < fit_through :) 

results <- list(pscale1 = parameters.r$pscale1[i]
                , pscale2 = parameters.r$pscale2[i]
                , lambda_con = lambda_convergence
                , kappa_con = kappa_convergence
                , lambda_2_con = lambda2_convergence
                , proportion = proportion
                , date_first = date_first
                , proportion_aw2 = proportion_aw2
                , date_first_aw2 = date_first_aw2
                , seed = seed_batch
)

#Save results
dir.create(paste0("sbv/raw_output/mthird_increase/check_data"))
dir.create(paste0("sbv/raw_output/mthird_increase/check_data/", seed_batch))

results_dir <- paste0("sbv/raw_output/mthird_increase/check_data/", seed_batch)

saveRDS(results, file=paste0(results_dir, "/results_", i,".RDS"))
#Make the plot

plot_column <- 'observed'
plot_column_ma <- 'ma_reinf'
infections <- 3
infection <-3


eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

rm(sri)
rm(sri_long)

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
                                        + geom_vline(aes(xintercept = 1 + as.Date(wave_split_2)), linetype = 3, color = 'blue')
                                        
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


dir.create(paste0(results_dir, "/plots"))
ggsave(inc_reinf, filename = paste0(results_dir, "/plots/sim_plot_",i,".png"), width = 6, height = 3)

