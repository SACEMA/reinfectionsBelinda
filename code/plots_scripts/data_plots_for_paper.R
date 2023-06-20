# Plot 1 - simulated primary and reinfections
library('gridExtra')
library('ggplot2')
library('data.table')
plot_dir <- 'output/paper_plots'

configpth <- 'sbv/method_1_analysis/m1_config_general.json'
attach(jsonlite::read_json(configpth))

#Scenario 1
dir.create(paste0(plot_dir, '/S1'))
dir <- paste0(plot_dir, '/S1')

load('sbv/method_1_analysis/parameters.RData')
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  data_add <- generate_data(1, 'data/inf_for_sbv_v3.RDS', 1)
  data_add$pscale <- parameters.r[i,]$pscale
  data <- rbind(data, data_add)
}

#original simulated data

fit_through <- '2021-04-01'
omicron_date <- '2021-04-01'
sim_data <- ggplot(data, aes(x=date)) +
                   geom_line(aes(y = ma_cnt)) +
                   xlab('Date') + 
                   ylab('Infections') + 
                   theme(plot.title = element_text(hjust = 0.5)) +
                   theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                  , panel.grid.minor = element_blank()) +
                  theme_minimal() + 
                  scale_colour_brewer(palette = "Set2") +
                  theme(axis.title.x=element_blank()) +
                  geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
                  geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
                  scale_x_date(date_breaks = '3 months', expand = c(0, 0) )
ggsave(sim_data, filename = paste0(dir,'/simulated_data.png'))

#scenario 1 reinfections for different pscales
sim_data_reinf <- ggplot(data[data$pscale %in% c(1, 1.5, 2, 2.5),], aes(x=date)) +
                  geom_line(aes(y = ma_reinf, group=pscale, color= factor(pscale))) +
                  xlab('Date') + 
                  ylab('Reinfections') + 
                  labs(color="Scale") +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                , panel.grid.minor = element_blank()) +
                  theme_minimal() + 
                  scale_colour_brewer(palette = "Set2") +
                  theme(axis.title.x=element_blank()) +
                  geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
                  geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
                  scale_x_date(date_breaks = '4 months', expand = c(0, 0) )

ggsave(sim_data_reinf, filename = paste0(dir,'/calculated_reinfections.png'))

#scenario 1 combined plots
data_method_1 <- grid.arrange(sim_data, sim_data_reinf, nrow=2)
ggsave(data_method_1, filename = paste0(dir,'/combined_S1.png'))



########################### Scenario 2
dir.create(paste0(plot_dir, '/S2'))
dir <- paste0(plot_dir, '/S2')


load('sbv/method_2_analysis/parameters.RData')
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  data_add <- generate_data(2, 'data/inf_for_sbv.RDS', 1)
  data_add$pobs_2 <- parameters.r[i,]$pobs_2
  data_add$pscale <- parameters.r[i,]$pscale
  data <- rbind(data, data_add)
}

#plot for pscale 1, method 2 reinfections calculated
m2 <- ggplot(data[data$pscale==1], aes(x=date)) +
            geom_line(aes(y = ma_reinf, group=pobs_2, color= factor(pobs_2))) +
            labs(color="Observation Probability \n (reinfections)") +
            ylab('Observed Reinfections') + 
            #ggtitle('Observed reinfections for Scenario 2') +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
            , panel.grid.minor = element_blank()) +
            theme_minimal() + 
            scale_colour_brewer(palette = "Set2") +
            theme(axis.title.x=element_blank()) +
            geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
            geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
            scale_x_date(date_breaks = '4 months', expand = c(0, 0) )
  
ggsave(m2, filename = paste0(dir,'/calculated_reinfections.png'))



###################### Method 3

dir.create(paste0(plot_dir, '/S3'))
dir <- paste0(plot_dir, '/S3')
rm(save_params)
Sys.sleep(2)
load('sbv/method_3_analysis/parameters.RData')
rm(paramaters.r)
Sys.sleep(2)
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  data_add <- generate_data(3, 'data/inf_for_sbv.RDS', 1)
  data_add$pobs_2 <- parameters.r[i,]$pobs_2
  data_add$pscale <- parameters.r[i,]$pscale
  data_add$pobs_1 <- paramaters.r[i,]$pobs_1
  data <- rbind(data, data_add)
}

m3_primary_infections <- ggplot(data, aes(x=date)) +
        geom_line(aes(y = ma_cnt, group=pobs_1, color= factor(pobs_1))) +
        xlab('Date') + 
        labs(color="Primary Infections \nObservation Probability") +
        ylab('Observed Primary\nInfections') + 
        #ggtitle('Observed primary infections') +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
        , panel.grid.minor = element_blank()) +
        theme_minimal() + 
        scale_colour_brewer(palette = "Set2") +
        theme(axis.title.x=element_blank()) + 
        geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
        geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
        scale_x_date(date_breaks = '4 months', expand = c(0, 0) )

ggsave(m3_primary_infections, filename = paste0(dir,'/primary_infections_obs.png'))

m3_reinfections <- ggplot(data[data$pscale==1], aes(x=date)) +
            geom_line(aes(y = ma_reinf, group=pobs_1, color= factor(pobs_1))) +
            facet_wrap(~pobs_2) +
            xlab('Date') + 
            #labs(color="Primary infections probability") +
            ylab('Observed Reinfections') + 
            #ggtitle('Observed reinfections') +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
            , panel.grid.minor = element_blank()) +
            theme_minimal() + 
            scale_colour_brewer(palette = "Set2") +
            theme(axis.title.x=element_blank()) + 
            theme(legend.position = "none") +
            geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
            geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
            scale_x_date(date_breaks = '4 months', expand = c(0, 0) )

ggsave(m3_reinfections, filename = paste0(dir,'/reinfections_observed.png'))

data_method_3 <- grid.arrange(m3_primary_infections, m3_reinfections, nrow=2)
ggsave(data_method_3, filename = paste0(dir,'/combined.png'))


#### SCENARIO 4 #########
dir.create(paste0(plot_dir, '/S4'))
dir <- paste0(plot_dir, '/S4')
rm(save_params)
Sys.sleep(2)
load('sbv/method_4_analysis/parameters.RData')
rm(paramaters.r)
Sys.sleep(2)
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  print(i)
  data_add <- generate_data(3, 'data/inf_for_sbv.RDS', 1)
  data_add$pobs_2 <- parameters.r[i,]$pobs_2
  data_add$pscale <- parameters.r[i,]$pscale
  data_add$pobs_1 <- paramaters.r[i,]$pobs_1
  data_add$dprob <- parameters.r[i,]$dprob
  data <- rbind(data, data_add)
}

m4_eligible <- ggplot(data[data$pscale==1 & data$pobs_1==0.5,], aes(x=date)) +
  geom_line(aes(y = eligible_for_reinf , group=pobs_2, color= factor(pobs_2))) +
  facet_wrap(~dprob) +
  xlab('Date') + 
  labs(color="Death probability") +
  ylab('Eligible for reinfection') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
      , panel.grid.minor = element_blank()) +
  theme_minimal() + 
  scale_colour_brewer(palette = "Set2") +
  theme(axis.title.x=element_blank()) + 
  theme(legend.position = "none")
ggsave(m4_eligible, filename = paste0(dir,'/eligible_for_reinf.png'))


m4_reinfections <- ggplot(data[data$pscale==1 & data$pobs_1==0.5,], aes(x=date)) +
  geom_line(aes(y = ma_reinf , group=pobs_2, color= factor(pobs_2))) +
  facet_wrap(~dprob) +
  xlab('Date') + 
  labs(color="Death probability") +
  ylab('Observed') + 
  ggtitle('Observed reinfections for changing deaths probability') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
        , panel.grid.minor = element_blank()) +
  theme_minimal() + 
  scale_colour_brewer(palette = "Set2") +
  theme(axis.title.x=element_blank()) + 
  theme(legend.position = "none")
ggsave(m4_reinfections, filename = paste0(dir,'/observed_reinf.png'))



## Method 5
#function
load('utils/observe_prob_functions.RData')
x <- seq(1, 100000)
y <- logistic_func(0.05, 0.2, x, x_m = 30000, s = 0.00005)
options(scipen=999)
log_plot <- plot(x,y,type='line', xlab='Primary infections', ylab='Observation Probability')

data <- readRDS('data/m5_ts_data_for_analysis.RDS')
datam1 <- readRDS('data/m1_ts_data_for_analysis.RDS')

m5.1 <- ggplot() +
  geom_line(data=data, aes(x=date, y = infections_ma), color='green') + 
  geom_line(data=datam1, aes(x=date, y=infections_ma), color='blue') + 
  xlab('Date') + 
  ylab('Reinfections') + 
  ggtitle('Observed reinfections') +
  theme(plot.title = element_text(hjust = 0.5))


