library('ggplot2')
data_new <- readRDS('data/5Ab_NF_ts_data_for_analysis.RDS')
data_old <- readRDS('data/infection_data.RDS')
data_new$infections_ma <- frollmean(data_new$infections, 7)

plot <- (ggplot(data_old) 
         + aes(x = date, y = infections_ma)
         + geom_line(alpha = 0.7)
         + geom_point(aes(y = data_new$infections_ma), size = 0.2, color='blue')
         + ylab('Infections')
         + xlab('Date')
)
plot



plot <- (ggplot(data_old) 
         + aes(x = date, y = infections_ma)
         + geom_line(alpha = 0.7)
         + geom_point(aes(y = data_new$infections_ma), size = 0.2, color='blue')
         + ylab('Infections')
         + xlab('Date')
)
