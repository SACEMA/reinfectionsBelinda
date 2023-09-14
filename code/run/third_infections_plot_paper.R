
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

# File to create two simplots for third infections and position them below each other


suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(english)
  library(gridExtra)
  library(grid)
  library(png)
  library(patchwork)
  library(ggplotify)
})

data <- 'data/ts_data_for_analysis.RDS' #Infection data with columns date, 'ma_third', 'third'
configpth <- 'config_general.json'
file_1 <- 'output/3_sim_90_null_l2_correctdata.RDS' #Simulations file for bottom plot
file_2 <- 'output/3_sim_90_null_correctdata.RDS' #Simulations file for top plot

output_dir <- './output/'
output_file <- 'output/sim_plot_fit_through.png'

dir.create(output_dir)

infections <- 'Third'

fit_through_1 <- '2022-01-31'
fit_through_2 <- '2021-10-31'

ts <- readRDS(data)

attach(jsonlite::read_json(configpth))
load('utils/plotting_fxns.RData')

plot_sim <- function(dat, sim, sim_ma) (ggplot(dat) 
                                        + aes(x = date) 
                                        + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3)
                                        + geom_point(aes(y = !!sym(plot_column)), size = .2, color = '1', alpha = .3)
                                        + geom_ribbon(data = sim_ma, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3, fill = '2')
                                        + geom_line(aes(y = !!sym(plot_column_ma)), color = '2')
                                        + ylab(paste0('Third infections'))
                                        + xlab('Specimen receipt date')
                                        + geom_vline(aes(xintercept = 1 + as.Date(fit_through)), linetype = 3, color = 'red')
                                        + theme_minimal()
                                        + theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.25)
                                                , panel.grid.minor = element_blank()
                                        )
                                        + scale_x_Ms(name = 'Specimen receipt date', labels = function(bs) {
                                          c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[month(bs)]
                                        }, date_breaks='2 months')
                                        #+ scale_x_date(
                                        #  date_breaks = "2 months",
                                        #  labels = scales::label_date_short()
                                        #  )
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



infection <- 'Third'
plot_column_ma <- 'ma_third'
plot_column <- 'third'

# load file
sims <- readRDS(file_1)
fit_through <- fit_through_1

sri <- data.table(date = ts$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                    , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]


inc_reinf <- (plot_sim(ts, eri, eri_ma) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

inc_reinf_fit <- (plot_sim(ts[between(date, as.Date('2020-11-01'), as.Date(fit_through))], eri[between(date, as.Date('2020-11-01'), as.Date(fit_through))], eri_ma[between(date, as.Date('2020-11-01'), as.Date(fit_through))])
                  + ggtitle('Fitting period')
                  + geom_text(aes(label = year, y = 0), data = ts[between(date, as.Date('2020-11-01'), as.Date(fit_through)), .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_proj <- (plot_sim(ts[date > as.Date(fit_through)], eri[date > as.Date(fit_through)], eri_ma[date > as.Date(fit_through) ])
                   + ggtitle('Projection period')
                   # + geom_text(aes(label = year, y = 0), data = ts[date > '2021-12-31', .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

fig4_l2 <- grid.arrange(inc_reinf_fit , inc_reinf_proj, nrow=1, ncol=2, widths=c(1, 1.5))



# load file
sims <- readRDS(file_2)
fit_through <- fit_through_2


sri <- data.table(date = ts$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]


inc_reinf <- (plot_sim(ts, eri, eri_ma) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

inc_reinf_fit <- (plot_sim(ts[between(date, as.Date('2020-11-01'), as.Date(fit_through))], eri[between(date, as.Date('2020-11-01'), as.Date(fit_through))], eri_ma[between(date, as.Date('2020-11-01'), as.Date(fit_through))])
                  + ggtitle('Fitting period')
                  + geom_text(aes(label = year, y = 0), data = ts[between(date, as.Date('2020-11-01'), as.Date(fit_through)), .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_proj <- (plot_sim(ts[date > as.Date(fit_through)], eri[date > as.Date(fit_through)], eri_ma[date > as.Date(fit_through) ])
                   + ggtitle('Projection period')
                   + geom_text(aes(label = year, y = 0), data = ts[date > '2021-12-31', .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

fig4 <- grid.arrange(inc_reinf_fit , inc_reinf_proj, nrow=1, ncol=2, widths=c(1, 1.5))


#####





fig4_gg <- ggplotify::as.ggplot(fig4)
fig4_l2_gg <- ggplotify::as.ggplot(fig4_l2)

fig4_gg <- fig4_gg + labs(title = "A")
fig4_l2_gg <- fig4_l2_gg + labs(title = "B")

combined <- grid.arrange(fig4_gg, fig4_l2_gg)

ggsave(combined, filename = output_file, width = 10, height = 8)
