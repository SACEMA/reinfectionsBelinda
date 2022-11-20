suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

.debug <- './reinf'
.args <- if (interactive()) sprintf(c(
  "output/sim_90_null_third.RDS", # input
  "output/4B_sim_90_null.RDS", #input  
  "data/4B_ts_data_for_analysis.RDS",
  "config_general.json",
  "utils/plotting_fxns.RData",
  "output/simplot_90_null_third.png", # output,
  "output/simplot_90_null.png"
), .debug[1]) else commandArgs(trailingOnly = TRUE)


ts <- readRDS(.args[3])

load(.args[5]) # load plotting functions 
configpth <- .args[4] # config file load
attach(jsonlite::read_json(configpth))

if (infection=="third"){
  sims <- readRDS(.args[1])
  target <- tail(.args, 2)
}
if (infection=="second"){
  sims <- readRDS(.args[2])
  target <- tail(.args, 1)
}


sri <- data.table(date = ts$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                    , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]


if (infection=="third"){
  plot_sim <- function(dat, sim, sim_ma) (ggplot(dat) 
                                + aes(x = date) 
                                + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3)
                                + geom_point(aes(y = third), size = .2, color = '1', alpha = .3)
                                + geom_ribbon(data = sim_ma, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3, fill = '2')
                                + geom_line(aes(y = ma_third), color = '2')
                                + ylab('Third infections')
                                + xlab('Specimen receipt date')
                                + geom_vline(aes(xintercept = 1 + as.Date(fit_through)), linetype = 3, color = 'red')
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
)}

if (infection=="second"){
  plot_sim <- function(dat, sim, sim_ma) (ggplot(dat) 
                                          + aes(x = date) 
                                          + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3)
                                          + geom_point(aes(y = reinf), size = .2, color = '1', alpha = .3)
                                          + geom_ribbon(data = sim_ma, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3, fill = '2')
                                          + geom_line(aes(y = ma_reinf), color = '2')
                                          + ylab('Second infections')
                                          + xlab('Specimen receipt date')
                                          + geom_vline(aes(xintercept = 1 + as.Date(fit_through)), linetype = 3, color = 'red')
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
)}

inc_reinf <- (plot_sim(ts, eri, eri_ma) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_fit <- (plot_sim(ts[between(date, as.Date('2020-03-21'), as.Date(fit_through))], eri[between(date, as.Date('2020-03-21'), as.Date(fit_through))], eri_ma[between(date, as.Date('2020-03-21'), as.Date(fit_through))])
                  + ggtitle('Fitting period')
                  + geom_text(aes(label = year, y = 0), data = ts[between(date, as.Date('2020-03-21'), as.Date(fit_through)), .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_proj <- (plot_sim(ts[date > as.Date(fit_through)], eri[date > as.Date(fit_through)], eri_ma[date > as.Date(fit_through)])
                   + ggtitle('Projection period')
                   # + geom_text(aes(label = year, y = 0), data = ts[date > '2021-12-31', .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

fig4 <- (inc_reinf_fit 
         + inc_reinf_proj 
         + plot_layout(widths = c(11, 6))
)


if(grepl('.RDS', target)){
  saveRDS(fig4, file = target)
}else{
  ggsave(fig4, filename = target, width = 6, height = 3)
}
