# This file is used to generate a dataset that represents the `real-world`
# It uses a simulated dataset representing primary infections & calculate the reinfections & third infections based on a constant hazard coefficient
# The purposes of this dataset is to test the code

.args <- if (interactive()) sprintf(c(
  file.path('data', 'ts_data.csv'), # input
  file.path('generate_data', 'inf_for_sbv.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

data_dir <- './data/'
dir.create(data_dir)

target <- tail(.args, 1)
primary_infections <- .args[1] 

reinf_hazard <- 1.38866e-08
cutoff <- 90

library('dplyr')
library('data.table')

data <- readRDS(primary_infections)

data[, reinfections := 0]
data[, eligible_for_reinf := shift(cumsum(infections), cutoff-1)]

for (day in (cutoff+1):nrow(data)) { 
  data$eligible_for_reinf[day] = data$eligible_for_reinf[day] - sum(data$reinfections[1:day-1])
  data$reinfections[day] = round(reinf_hazard * data$infections[day] * data$eligible_for_reinf[day])
}

data[, third_infections := 0]
data[, eligible_for_third := shift(cumsum(reinfections), cutoff-1)]

for (day in (cutoff+1):nrow(data)) { 
  data$eligible_for_third[day] = data$eligible_for_third[day] - sum(data$third_infections[1:day-1])
  data$third_infections[day] = round(reinf_hazard * data$reinfections[day] * data$eligible_for_third[day])
}

data <- data %>% 
  rename(
    cnt = infections,
    reinf = reinfections,
    third = third_infections
  )

write.csv(data, file = target)