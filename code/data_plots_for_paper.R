# Plot 1 - simulated primary and reinfections
library('gridExtra')
data <- readRDS('data/m1_ts_data_for_analysis.RDS')
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

data <- readRDS('data/m2_ts_data_for_analysis.RDS')

m2 <- ggplot(data, aes(x=date)) +
            geom_line(aes(y = reinfections, group=prob, color= factor(prob))) +
            xlab('Date') + 
            labs(color="Observation Probability") +
            ylab('Reinfections') + 
            ggtitle('Observed reinfections') +
            theme(plot.title = element_text(hjust = 0.5))


###################### Method 3
data <- readRDS('data/m3_ts_data_for_analysis.RDS')

m3_primary_infections <- ggplot(data, aes(x=date)) +
        geom_line(aes(y = infections_ma, group=pobs1, color= factor(pobs1))) +
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
