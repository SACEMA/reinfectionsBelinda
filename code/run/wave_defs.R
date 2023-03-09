# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfection

suppressPackageStartupMessages({
  require(data.table)
})

.debug <- ''
.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data_for_analysis.RDS'), # input
  file.path('config_general.json'),
  file.path('utils', 'wave_defs.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)


utils <- './utils/'
dir.create(utils)

ts <- readRDS(.args[1]) # Use to set wave dates as >=15% of wave peak

configpth <- .args[2]
attach(jsonlite::read_json(configpth))

target <- tail(.args, 1)

#Adjust dates to dataset waves.

peak1 <- ts[date <= '2020-09-15', max(ma_tot, na.rm = TRUE)]
peak2 <- ts[date <= '2021-02-15', max(ma_tot, na.rm = TRUE)]
peak3 <- ts[date > '2021-02-15' & date <= '2021-10-15', max(ma_tot, na.rm = TRUE)]
peak4 <- ts[date > '2021-10-15' & date <= '2022-01-31', max(ma_tot, na.rm = TRUE)]

ts[date <= '2020-09-15' & ma_cnt >= wave_thresh * peak1, wave := 'W1']
ts[date > '2020-09-15' & date <= '2021-02-15' & ma_cnt >= wave_thresh * peak2, wave := 'W2']
ts[date > '2021-02-15' & date <= '2021-10-15' & ma_cnt >= wave_thresh * peak3, wave := 'W3']
ts[date > '2021-10-15' & date <= '2022-01-31' & ma_cnt >= wave_thresh * peak4, wave := 'W4']

waves <- ts[!is.na(wave), .(min_date = min(date), max_date = max(date)), keyby = wave]
waves[, col := c('#785EF0', '#DC267F', '#FE6100')]

saveRDS(waves, file = target)
