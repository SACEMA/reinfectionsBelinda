library(gtools)
library(ggplot2)
library(ggtext)
library(dplyr)

load('utils/cleanup_methods.RData')

#combine raw results of all 20 seeds
#for (i in 1:20) {
#  combineRawResults(5, i, paste0('sbv/raw_output/m5/', i), 'sbv/method_5_analysis/output/final_output_data/')
#  print(i)
#}

#get all the data
all_data <- getAllResults(5, 'sbv/method_5_analysis/output/final_output_data/')
saveRDS(all_data, file = 'sbv/method_5_analysis/output/all_data.RDS')

#exclude results
excluded_results <- excludeResultsAll(5, all_data)
saveRDS(excluded_results, file = 'sbv/method_5_analysis/output/all_data_excluded.RDS')

#get specificity matrix
result <- excluded_results[excluded_results$pscale==1 ,] %>%
  group_by(pscale, pobs_1_min, pobs_1_max, pobs_2_min, pobs_2_max, steep, xm) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con <= 1.1 & lambda_con <= 1.1 ),
            count_date_not_na = sum(!is.na(date_first_above_5_aw) & kappa_con <= 1.1 & lambda_con <= 1.1)) %>%
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file = 'sbv/method_5_analysis/output/specificity_matrix.RDS')


#Get summarized matrix (for convergence)
summarised_all <- all_data %>% group_by(pscale, pobs_1_min, pobs_1_max, pobs_2_max, pobs_2_min, xm, steep)
summarised_all <- summarised_all %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_above = median(proportion_above)
    , date_first_above = median_cluster(date_first_above_5_aw)
    , .groups = 'drop')

saveRDS(summarised_all, file = 'sbv/method_5_analysis/output/summarised_results_all.RDS')


#Get summarized matrix (for sensitivity)
summarised <- excluded_results %>% group_by(pscale, pobs_1_min, pobs_1_max, pobs_2_max, pobs_2_min, xm, steep)
summarised <- summarised %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_above = median(proportion_above)
    , date_first_above = median_cluster(date_first_above_5_aw)
    , .groups = 'drop')

saveRDS(summarised, file = 'sbv/method_5_analysis/output/summarised_results.RDS')



