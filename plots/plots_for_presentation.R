### infections plot ### 
library('ggplot2')
data <- readRDS('data/infection_data.RDS')
waves <- readRDS('utils/wave_defs.RDS')
geom_wave <- function(ww, dt = waves){
  geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf), fill = dt[wave == ww, col], alpha = .7, data = dt[wave == ww], inherit.aes = FALSE)
}

#Simulations plot
infections_simulated <- (ggplot(data) 
          + aes(x = date) 
          + geom_wave('W1')
          + geom_wave('W2')
          + geom_point(aes(y = infections), size = .2, alpha = .5)
          + geom_line(aes(y = infections_ma), size = 1)
          + ylab('Simulated infections')
          + ggtitle('Simulated infections over three waves')
          + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", date_minor_breaks = '1 week', limits = data[, range(date)])
)  

infections_simulated
data.1A <- readRDS('data/1A_ts_data_for_analysis.RDS')
data.1B <- readRDS('data/1B_ts_data_for_analysis.RDS')
data.2A <- readRDS('data/2A_ts_data_for_analysis.RDS')
data.2B <- readRDS('data/2B_ts_data_for_analysis.RDS')
data.3A <- readRDS('data/3A_ts_data_for_analysis.RDS')
data.3B <- readRDS('data/3B_ts_data_for_analysis.RDS')
data.4A <- readRDS('data/4A_ts_data_for_analysis.RDS')
data.4B <- readRDS('data/4B_ts_data_for_analysis.RDS')


#Method 1 A &B and reinfections
reinfections_1 <- (ggplot(data) 
                   + aes(x = date) 
                   + geom_wave('W1')
                   + geom_wave('W2')
                   + geom_wave('W3')                   + geom_line(aes(y = infections_ma ), size = 1)
                   + geom_line(aes(y = data.1A$ma_reinf ), size = 0.5, color='green')
                   + geom_point(aes(y = data.1A$reinf ), size = 0.2, color='green')
                   + geom_line(aes(y = data.1B$ma_reinf ), size = 0.5, color='blue')
                   + geom_point(aes(y = data.1B$reinf ), size = 0.2, color='blue')
                   + ylab('Simulated infections')
                   + ggtitle('Method 1: Infections vs reinfections')
                   + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", date_minor_breaks = '1 week', limits = data[, range(date)])
)  
reinfections_1

reinfections_2 <- (ggplot(data.2A) 
                   + aes(x = date) 
                   + geom_line(aes(y = data.1A$ma_reinf ), size = 0.6, color='lightpink')
                   + geom_line(aes(y = data.1B$ma_reinf ), size = 0.6, color='greenyellow')
                   + geom_line(aes(y = data.2A$ma_reinf ), size = 1, color='mediumvioletred')
                   + geom_line(aes(y = data.2B$ma_reinf ), size = 1, color='green4')
                   + geom_point(aes(y=data.2B$reinf), size = 0.2, color='green4')
                   + geom_point(aes(y=data.2A$reinf), size = 0.2, color='mediumvioletred')
                   + ylab('Reinfections')
                   + ggtitle('Method 2: Reinfections vs observed reinfections')
                   + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", date_minor_breaks = '1 week')
)  

reinfections_2



#Method 3 plot (infections after observe probability)
infections_3 <- (ggplot(data) 
                 + aes(x = date) 
                         + geom_point(aes(y = infections ), size = .2, alpha = .5)
                         + geom_line(aes(y = infections_ma ), size = 0.5)
                         + geom_line(aes(y = data.3A$ma_cnt ), size = 0.5, color='green')
                         #+ geom_line(aes(y = data.3A$ma_cnt ), size = 0.5, color='green')
                         + geom_point(aes(y = data.3A$cnt ), size = 0.2, color='green')
                         + ylab('Primary infections')
                         + ggtitle('Method 3: Observed first infections vs underlying infections')
                         + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", date_minor_breaks = '1 week', limits = data[, range(date)])
)  
infections_3


reinfections_3 <- (ggplot(data.3A) 
                   + aes(x = date) 
                   + geom_line(aes(y =data.1A$ma_reinf ), size = 0.5, color='lightpink')
                   + geom_line(aes(y = data.1B$ma_reinf ), size = 0.5, color='greenyellow')
                   + geom_line(aes(y = data.3A$ma_reinf ), size = 0.5, color='mediumvioletred')
                   + geom_line(aes(y = data.3B$ma_reinf ), size = 0.5, color='green4')
                   #+ geom_point(aes(y=data.3B$reinf), size = 0.2, color='green4')
                   #+ geom_point(aes(y=data.3A$reinf), size = 0.2, color='mediumvioletred')
                   + ylab('Reinfections')
                   + ggtitle('Method 3: Reinfections vs observed reinfections')
                   + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", date_minor_breaks = '1 week')
)  

reinfections_3

reinfections_3 <- ggplot(data.2A
                   , aes(x = date, colour=variable)) +
                    geom_line(aes(y = data.1A$ma_reinf ), size = 0.2, colour="blue") + 
                    geom_line(aes(y = data.1B$ma_reinf ), size = 0.2, colour="green") + 
                    geom_line(aes(y = data.3A$ma_reinf ), size = 0.8, colour="blue") + 
                    geom_line(aes(y = data.3B$ma_reinf ), size = 0.8, colour="green") + 
                    ylab('Reinfections') + 
                    ggtitle('Method 3: Reinfections vs observed reinfections') + 
                    scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL, limits = data[, range(date)]) + 
                    scale_colour_manual(values=c("blue", "green", "blue", "green"))
              

reinfections_3

reinfections_4 <- (ggplot(data.2A) 
                   + aes(x = date) 
                   + geom_wave('W1')
                   + geom_wave('W2')
                   + geom_wave('W3')                   
                   + geom_line(aes(y = data.1A$ma_reinf ), size = 0.2, color='blue')
                   + geom_line(aes(y = data.1B$ma_reinf ), size = 0.2, color='green')
                   + geom_line(aes(y = data.4A$ma_reinf ), size = 0.8, color='blue')
                   + geom_line(aes(y = data.4B$ma_reinf ), size = 0.8, color='green')
                   + ylab('Reinfections')
                   + ggtitle('Method 4: Reinfections vs observed reinfections after deaths')
                   + scale_x_date(NULL, date_breaks = "months", date_labels = "%b", minor_breaks = NULL, limits = data[, range(date)])
)  

reinfections_4

data.4A.2 <- readRDS('data/4A_NF_ts_data_for_analysis.RDS')
data.3A.2 <- readRDS('data/3A_NF_ts_data_for_analysis.RDS')

eligb_4 <- (ggplot(data.4A.2) 
            + aes(x = date) 
            + geom_line(aes(y = data.3A.2$eligible_for_reinf ), size = 0.5, color='green')
            + geom_line(aes(y = eligible_for_reinf ), size = 0.5, color='blue')
            #+ geom_line(aes(y = data.4A$ma_reinf ), size = 0.8, color='blue')
            #+ geom_line(aes(y = data.4B$ma_reinf ), size = 0.8, color='green')
            + ylab('Eligible for reinfection')
            + ggtitle('Method 4: Eligible for reinfections vs method 3')
            + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", limits = data[, range(date)])
)  
eligb_4
data.4A.2[, ma_deaths := frollmean(deaths, window_days)]


death4 <- (ggplot(data.4A.2) 
           + aes(x = date) 
           + geom_point(aes(y = data.4A.2$deaths ), size = 0.5, color='black')
           + geom_line(aes(y = data.4A.2$ma_deaths ), size = 1, color='black')
           #+ geom_line(aes(y = data.4A$ma_reinf ), size = 0.8, color='blue')
           #+ geom_line(aes(y = data.4B$ma_reinf ), size = 0.8, color='green')
           + ylab('Deaths')
           + ggtitle('Method 4: Eligible for reinfections vs method 3')
           + scale_x_date(NULL, date_breaks = "2 month", date_labels = "%b %y", limits = data[, range(date)])
)  
death4
  

