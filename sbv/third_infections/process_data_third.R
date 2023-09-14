library(gtools)
library(ggplot2)
library(ggtext)
library(dplyr)

# Prerequisite: scenario results as RDS files in 
#'sbv/third_infections/output/third_2' and 'sbv/third_infections/output/third_2'

load('utils/cleanup_methods.RData')

method <- 'third'

########### GET ALL THE DATA FOR THE INCREASED PSCALE 2 #########


#Get raw result for pscale2 increase 
#dir_save <- paste0('sbv/third_infections/output/third_increase_p2')
#for (i in 1:20) {
#  dir_get <- paste0('sbv/raw_output/mthird_increase/', i)
#  combineRawResultsOld('third', i, directory=dir_get, output_dir=dir_save)
#}

#get all the data
all_data_inc <- getAllResults('third', 'sbv/third_infections/output/third_increase_p2/')
saveRDS(all_data_inc, file = 'sbv/third_infections/output/all_data_inc.RDS')




#exclude results
excluded_results_inc <- excludeResultsNonConverging('third', all_data_inc)
saveRDS(excluded_results_inc, file = 'sbv/third_infections/output/all_data_excluded_inc.RDS')


#Get summarized matrix (for convergence)
summarised_all_inc <- all_data_inc %>% group_by(pscale1, pscale2)
summarised_all_inc <- summarised_all_inc %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , lambda_2_con = proportion_converence(lambda_2_con)
    , proportion_above = median(proportion_aw2)
    , date_first_above = median_cluster(date_first_aw2)
    , .groups = 'drop')

saveRDS(summarised_all_inc, file = 'sbv/third_infections/output/summarised_results_all_inc.RDS')


#Get summarized matrix (for sensitivity)
summarised_inc <- excluded_results_inc %>% group_by(pscale1, pscale2)
summarised_inc <- summarised_inc %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , lambda_2_con = proportion_converence(lambda_2_con)
    , proportion_above = median(proportion_aw2)
    , date_first_above = median_cluster(date_first_aw2)
    , .groups = 'drop')

saveRDS(summarised_inc, file = 'sbv/third_infections/output/summarised_results_inc.RDS')



########### GET ALL THE DATA FOR THE FIXED PSCALE 2 #########


#Get raw result for pscale2 fixed 
dir_save <- paste0('sbv/third_infections/output/third_l2')
for (i in 1:20) {
  dir_get <- paste0('sbv/raw_output/ml2third/', i)
  combineRawResultsOld('third', i, directory=dir_get, output_dir=dir_save)
}

#get all the data
all_data <- getAllResults('third', 'sbv/third_infections/output/third_l2/')
saveRDS(all_data, file = 'sbv/third_infections/output/all_data.RDS')




#exclude results
excluded_results <- excludeResultsNonConverging('third', all_data)
saveRDS(excluded_results, file = 'sbv/third_infections/output/all_data_excluded.RDS')


#Get summarized matrix (for convergence)
summarised_all <- all_data %>% group_by(pscale)
summarised_all <- summarised_all %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , lambda_2_con = proportion_converence(lambda_2_con)
    , proportion_above = median(proportion)
    , date_first_above = median_cluster(date_first)
    , .groups = 'drop')

saveRDS(summarised_all, file = 'sbv/third_infections/output/summarised_results_all.RDS')


#Get summarized matrix (for sensitivity)
summarised <- excluded_results %>% group_by(pscale)
summarised <- summarised %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , lambda_2_con = proportion_converence(lambda_2_con)
    , proportion_above = median(proportion)
    , date_first_above = median_cluster(date_first)
    , .groups = 'drop')

saveRDS(summarised, file = 'sbv/third_infections/output/summarised_results.RDS')


result <- excluded_results %>%
  group_by(pscale) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con <= 1.1 & lambda_con <= 1.1 & lambda_2_con<=1.1),
            count_date_not_na = sum(!is.na(date_first) & kappa_con <= 1.1 & lambda_con <= 1.1 & lambda_2_con<=1.1)) %>%
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, 'sbv/third_infections/output/specificity_matrix.RDS')
write.csv(result, 'sbv/third_infections/output/specificity_matrix.csv' )

#Saves RDS files as CSV files
method <- 'third'
results_dir <- paste0('sbv/third_infections/results')
input_dir <- paste0('sbv/third_infections/output/')
results_dir_1 <- paste0('sbv/third_infections/results/l2')
results_dir_2 <- paste0('sbv/third_infections/results/l2_increase')
dir.create(results_dir)
dir.create(results_dir_1)
dir.create(results_dir_2)

for (i in 1:20) {
  results <- readRDS(paste0(input_dir, 'third_l2/method_third_seed_', i,'_full.RDS'))
  write.csv(results
            , paste0(results_dir_1, '/method_',method,'_seed_', i, '_analysis.csv')
            , row.names=FALSE)
  results <- readRDS(paste0(input_dir, 'third_increase_p2/method_third_seed_', i,'_full.RDS'))
  write.csv(results
            , paste0(results_dir_2, '/method_',method,'_seed_', i, '_analysis.csv')
            , row.names=FALSE)
}
