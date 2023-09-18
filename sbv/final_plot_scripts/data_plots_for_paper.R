# File that can be used to generate plots that represents the data generation process throughout the methods #
# To run this, parameter.RData files must exist for all the scenraios. 

#load packages
library('gridExtra')
library('ggplot2')
library('data.table')
library('patchwork')

#output directory
dir.create('output')
plot_dir <- 'output/paper_plots'
dir.create(plot_dir)

load('utils/generate_data.RData')

#### SCENARIO 1 ####

configpth <- 'sbv/method_1_analysis/m1_config_general.json'
attach(jsonlite::read_json(configpth))

dir.create(paste0(plot_dir, '/S1'))
dir <- paste0(plot_dir, '/S1')

load('sbv/method_1_analysis/parameters.RData')
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  print(i)
  data_add <- generate_data(1, 'data/inf_for_sbv.RDS', i)
  data_add$pscale <- save_params[i,]$pscale
  data <- rbind(data, data_add)
}

#original simulated data

fit_through <- '2021-02-28'
omicron_date <- '2021-05-01'
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
                  scale_x_date(date_breaks = '3 months', expand = c(0, 0),  date_labels = "%b %Y" )
ggsave(sim_data, filename = paste0(dir,'/simulated_data.png'))

#scenario 1 reinfections for different pscales
sim_data_reinf <- ggplot(data[data$pscale %in% c(1, 1.5, 2, 2.5),], aes(x=date)) +
                  geom_line(aes(y = ma_reinf, group=pscale, color= factor(pscale))) +
                  xlab('Date') + 
                  ylab('Reinfections') + 
                  labs(color=expression(sigma)) +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                , panel.grid.minor = element_blank()) +
                  theme_minimal() + 
                  scale_colour_brewer(palette = "Set2") +
                  theme(axis.title.x=element_blank()) +
                  geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
                  geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
                  scale_x_date(date_breaks = '3 months', expand = c(0, 0),  date_labels = "%b %Y" )

ggsave(sim_data_reinf, filename = paste0(dir,'/calculated_reinfections.png'))

#scenario 1 combined plots
data_method_1 <- sim_data/sim_data_reinf + plot_annotation(tag_levels = 'A')

#data_method_1 <- grid.arrange(sim_data, sim_data_reinf, nrow=2)
ggsave(data_method_1, filename = paste0(dir,'/combined_S1.png'))



#### SCENARIO 2 ####
dir.create(paste0(plot_dir, '/S2'))
dir <- paste0(plot_dir, '/S2')


configpth <- 'sbv/method_2_analysis/m2_config_general.json'
attach(jsonlite::read_json(configpth))


load('sbv/method_2_analysis/parameters.RData')
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  data_add <- generate_data(2, 'data/inf_for_sbv.RDS', 1, 'sbv/method_2_analysis/parameters.RData')
  data_add$pobs_2 <- save_params[i,]$pobs_2
  data_add$pscale <- save_params[i,]$pscale
  data <- rbind(data, data_add)
}

#plot for pscale 1, method 2 reinfections calculated
m2 <- ggplot(data[data$pscale==1], aes(x=date)) +
            geom_line(aes(y = ma_reinf, group=pobs_2, color= factor(pobs_2))) +
            labs(color=expression(P[2])) +
            ylab('Observed Reinfections') + 
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
            , panel.grid.minor = element_blank()) +
            theme_minimal() + 
            scale_colour_brewer(palette = "Set2") +
            theme(axis.title.x=element_blank()) +
            geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
            geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
            scale_x_date(date_breaks = '3 months', expand = c(0, 0),  date_labels = "%b %Y" )

ggsave(m2, filename = paste0(dir,'/calculated_reinfections.png'))



#### SCENARIO 3 ####

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
  data_add$pobs_2 <- save_params[i,]$pobs_2
  data_add$pscale <- save_params[i,]$pscale
  data_add$pobs_1 <- save_params[i,]$pobs_1
  data <- rbind(data, data_add)
}

m3_primary_infections <- ggplot(data, aes(x=date)) +
        geom_line(aes(y = ma_cnt, group=pobs_1, color= factor(pobs_1))) +
        xlab('Date') + 
        labs(color=expression(P[1])) +
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
        scale_x_date(date_breaks = '3 months', expand = c(0, 0),  date_labels = "%b %Y" )

ggsave(m3_primary_infections, filename = paste0(dir,'/primary_infections_obs.png'))

m3_reinfections <- ggplot(data[data$pscale==1], aes(x=date)) +
            geom_line(aes(y = ma_reinf, group=pobs_1, color= factor(pobs_1))) +
            facet_wrap(~pobs_2) +
            xlab('Date') + 
            #labs(color="Primary infections probability") +
            ylab('Observed Reinfections') + 
            #ggtitle(expression(P[2])) +
            theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
            , panel.grid.minor = element_blank()) +
            theme_minimal() + 
            scale_colour_brewer(palette = "Set2") +
            theme(axis.title.x=element_blank()) + 
            theme(legend.position = "none") +
            geom_vline(xintercept=as.numeric(as.Date(fit_through)),linetype=2, color="orange") + 
            geom_vline(xintercept=as.numeric(as.Date(omicron_date)),linetype=2, color="red") +
            scale_x_date(date_breaks = '6 months', expand = c(0, 0),  date_labels = "%b %Y" )

ggsave(m3_reinfections, filename = paste0(dir,'/reinfections_observed.png'))
data_method_3 <- m3_primary_infections/m3_reinfections + plot_annotation(tag_levels = 'A')

#data_method_3 <- grid.arrange(m3_primary_infections, m3_reinfections, nrow=2)
ggsave(data_method_3, filename = paste0(dir,'/combined.png'), width=10, height=8)


#### SCENARIO 4 ####
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
  data_add <- generate_data(4, 'data/inf_for_sbv.RDS')
  data_add$pobs_2 <- save_params[i,]$pobs_2
  data_add$pscale <- save_params[i,]$pscale
  data_add$pobs_1 <- save_params[i,]$pobs_1
  data_add$dprob <- save_params[i,]$dprob
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


#### SCENARIO 5 ####
data <- readRDS('data/inf_for_sbv.RDS')
load('utils/observe_prob_functions.RData')

min_infections <- min(data$infections)
max_infections <- max(data$infections)
pobs_min <- 0.1
pobs_max <- 0.5
data <- data[data$infections<100000,]
y_values <- logistic_func(min=pobs_min, max=pobs_max, cases = data$infections, s = 0.0005, x_m = 40000)
data_plot <-  data.frame(x = data$infections, y = y_values)

expand <- expand.grid(steep=c(0.00005, 0.0001), mid=c(30000, 40000, 50000))
plots_list <- list()
grid <- plot_layout(ncol = 3, nrow = 2)

for (i in 1:nrow(expand)){
  tryCatch({
    steep <- expand$steep[i]
    xm <- expand$mid[i]
    y_values <- logistic_func(min=pobs_min, max=pobs_max, cases = data$infections, s = steep, x_m = xm)
    data_plot <-  data.frame(x = data$infections, y = y_values)
    
    
    plot <- ggplot() +
      geom_line(data = data_plot, aes(x = x, y = y), color = "black", linetype = "solid", size = 1) +
      theme_minimal() +
      theme(legend.position = "top") +
      scale_color_manual(values = c("black"), guide = FALSE) + 
      ylim(0, 0.6) + 
      scale_x_continuous(breaks = seq(50000, 50000, by = 0)) + 
      xlab('') + 
      ylab('')
    
    plots_list[[i]] <- plot

  }, error = function(d){print(d)})
}


plot <- grid.arrange(grobs = plots_list, ncol = 3) 
ggsave(plot, file='sbv/plots/log_function.tiff', dpi=1500, compression = "lzw")
