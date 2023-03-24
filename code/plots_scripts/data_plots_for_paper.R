# Plot 1 - simulated primary and reinfections
library('gridExtra')
load('utils/generate_data.RData')

sim_data <- ggplot(data, aes(x=date)) +
                   geom_line(aes(y = infections)) +
                   xlab('Date') + 
                   ylab('Infections') + 
                   ggtitle('Simulated primary infections') +
                   theme(plot.title = element_text(hjust = 0.5))

sim_data_reinf <- ggplot(data, aes(x=date)) +
                  geom_line(aes(y = reinfections, colour='red')) +
                  geom_line(aes(y = reinfections.1.5, colour='blue')) + 
                  geom_line(aes(y = reinfections.2, colour='green')) + 
                  geom_line(aes(y = reinfections.2.5, colour='yellow')) + 
                  geom_line(aes(y = reinfections.3.0, colour='purple')) + 
                  xlab('Date') + 
                  ylab('Reinfections') + 
                  ggtitle('Calculated reinfections') +
                  theme(plot.title = element_text(hjust = 0.5)) +
                  scale_colour_discrete(name='Scale', labels=c('1', '1.5', '2', '2.5', '3'))


data_method_1 <- grid.arrange(sim_data, sim_data_reinf, nrow=2)

###########################

load('sbv/method_2_analysis/parameters.RData')
paramaters.r <- save_params
data <- data.frame()
for (i in 1:nrow(paramaters.r)) {
  data_add <- generate_data(2, 'data/inf_for_sbv.RDS', 1)
  data_add$pobs_2 <- parameters.r[i,]$pobs_2
  data_add$pscale <- parameters.r[i,]$pscale
  data <- rbind(data, data_add)
}


m2 <- ggplot(data[data$pscale==1], aes(x=date)) +
            geom_line(aes(y = ma_reinf, group=pobs_2, color= factor(pobs_2))) +
            labs(color="Observation Probability") +
            ylab('Observed Reinfections') + 
            ggtitle('Observed reinfections for Scenario 2') +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
            , panel.grid.minor = element_blank()) +
            theme_minimal() + 
            scale_colour_brewer(palette = "Set2") +
            theme(axis.title.x=element_blank())
  



###################### Method 3
rm(save_params)
rm(parameters.r)
load('sbv/method_3_analysis/parameters.RData')
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
        labs(color="Primary Infections Observe Prob") +
        ylab('Infections') + 
        ggtitle('Observed primary infections') +
        theme(plot.title = element_text(hjust = 0.5))


m3_reinfections <- ggplot(data, aes(x=date)) +
            geom_line(aes(y = reinfections_ma, group=pobs1, color= factor(pobs1))) +
            facet_wrap(~pobs2) +
            xlab('Date') + 
            #labs(color="Primary infections probability") +
            ylab('Reinfections') + 
            ggtitle('Observed reinfections') +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(legend.position = "none")

data_method_3 <- grid.arrange(m3_primary_infections, m3_reinfections, nrow=2)


#### method 4
data <- readRDS('data/m4_ts_data_for_analysis.RDS')
m4_reinfections <- ggplot(data, aes(x=date)) +
  geom_line(aes(y = reinfections_ma, group=dprob, color= factor(dprob))) +
  xlab('Date') + 
  labs(color="Death probability") +
  ylab('Reinfections') + 
  ggtitle('Observed reinfections for changing deaths probability') +
  theme(plot.title = element_text(hjust = 0.5))


## Method 5
#function
load('utils/observe_prob_functions.RData')
x <- seq(1, 100000)
y <- logistic_func(0.1, 0.5, x, 0.0002, 50000)
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


