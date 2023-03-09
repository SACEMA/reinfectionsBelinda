
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>


suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(gridExtra)
})

.debug <- './reinf'
.args <- if (interactive()) sprintf(c(
  "output/sim_90_null.RDS", # input
  "data/ts_data_for_analysis.RDS",
  "config_general.json",
  "utils/plotting_fxns.RData",
  2, 
  "output/simplot_90_null.png" # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

output_dir <- './output/'
dir.create(output_dir)


infections <- .args[5]

ts <- readRDS(.args[2])

configpth <- .args[3]
attach(jsonlite::read_json(configpth))

load(.args[4])

# set target
target_path <- split_path(tail(.args, 1))
target <- file.path(rev(target_path[2:length(target_path)]), paste0(infections, '_', target_path[1]))

# load file
load_path <- split_path(.args[1])
sims <- readRDS(file.path(rev(load_path[2:length(load_path)]), paste0(infections, '_', load_path[1])))


sri <- data.table(date = ts$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                    , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]

plot_sim <- function(dat, sim, sim_ma) (ggplot(dat) 
                                + aes(x = date) 
                                + geom_ribbon(data = sim, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3)
                                + geom_point(aes(y = third), size = .2, color = '1', alpha = .3)
                                + geom_ribbon(data = sim_ma, aes(x = date, ymin = low_reinf, ymax = upp_reinf), alpha = .3, fill = '2')
                                + geom_line(aes(y = ma_third), color = '2')
                                + ylab(paste0('Infection number: ', infections))
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
)

inc_reinf <- (plot_sim(ts, eri, eri_ma) 
              + geom_text(aes(label = year, y = 0), data = ts[, .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -31, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_fit <- (plot_sim(ts[between(date, as.Date('2020-03-20'), as.Date(fit_through))], eri[between(date, as.Date('2020-03-20'), as.Date(fit_through))], eri_ma[between(date, as.Date('2020-12-01'), as.Date(fit_through))])
                  + ggtitle('Fitting period')
                  + geom_text(aes(label = year, y = 0), data = ts[between(date, as.Date('2020-03-20'), as.Date(fit_through)), .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)
inc_reinf_proj <- (plot_sim(ts[date > as.Date(fit_through)], eri[date > as.Date(fit_through)], eri_ma[date > as.Date(fit_through)])
                   + ggtitle('Projection period')
                   # + geom_text(aes(label = year, y = 0), data = ts[date > '2021-12-31', .(year = format(date, '%Y'), date)][, .(date = min(date)), by = year], vjust = -13, hjust = 'left', nudge_x = 14, size = 7*0.35)
)

fig4 <- grid.arrange(inc_reinf_fit , inc_reinf_proj, nrow=1, ncol=2)

if(grepl('.RDS', target)){
  saveRDS(fig4, file = target)
}else{
  ggsave(fig4, filename = target, width = 6, height = 3)
}
