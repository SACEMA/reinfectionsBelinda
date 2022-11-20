# This file is made available under a CC-BY-NC 4.0 International License.
# Details of the license can be found at
# <https://creativecommons.org/licenses/by-nc/4.0/legalcode>. 
# 
# Giving appropriate credit includes citation of the related publication and
# providing a link to the repository:
# 
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

suppressPackageStartupMessages({
  library(data.table)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('output', '1B_sim_90_null.RDS'), # input
  file.path('data', '1B_ts_data_for_analysis.RDS'), # input
  file.path('config_general.json'),
  file.path('pscale.txt'),
  file.path('output', 'pscale_analysis_output.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


target <- tail(.args, 1)
if (file.exists(target)) 
  save_output <- readRDS(target)
if (!file.exists(target)){
  save_output = data.frame(pscale=numeric(0),first_cluster=numeric(0),proportion=numeric(0))
  #x <- c("pscale", "first_cluster", "proportion")
  #colnames(save_output) <- x
}

value <- read.table(file = .args[4], header = F, nrows = 1)
pscale <- value$V1[1]

configpth_params <- .args[3] 
attach(jsonlite::read_json(configpth_params))

ts_data <- readRDS(.args[2])
sims <- readRDS(.args[1])

sri <- data.table(date = ts_data$date, sims)
sri_long <- melt(sri, id.vars = 'date')
sri_long[, ma_val := frollmean(value, 7), variable]

eri <- sri_long[, .(exp_reinf = median(value)
                    , low_reinf = quantile(value, 0.025, na.rm = TRUE)
                    , upp_reinf = quantile(value, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- sri_long[, .(exp_reinf = median(ma_val, na.rm = TRUE)
                       , low_reinf = quantile(ma_val, 0.025, na.rm = TRUE)
                       , upp_reinf = quantile(ma_val, 0.975, na.rm = TRUE)), keyby = date]

eri_ma <- eri_ma[date > omicron_date]
eri <- eri[date > omicron_date]

number_of_days <- nrow(eri_ma)

days_diff <- ts_data[date > fit_through]$ma_reinf - eri_ma$upp_reinf
days_diff[days_diff<0] <- 0
days_diff[days_diff>0] <- 1

conseq_diff <- frollsum(days_diff, 5, fill =0)
proportion <- length(days_diff[days_diff==1])/number_of_days


date_first <- which(conseq_diff==5)[1]

save_output[nrow(save_output) + 1,] = c(pscale,date_first,proportion)

#save_output[nrow(save_output) + 1,]$pscale <- pscale
#save_output[nrow(save_output) + 1,]$first_cluster <- date_first
#save_output[nrow(save_output) + 1,]$proportion <- proportion

saveRDS(save_output, target)