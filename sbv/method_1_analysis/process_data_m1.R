# Prerequisite: scenario results as RDS (dataframes) files in 
#'sbv/method_1_analysis/output/final_output_data' 


library(ggplot2)
library(ggtext)
library(dplyr)
library(gtools)
load('utils/cleanup_methods.RData')

method <- 1

all_data <- getAllResults(method, 'sbv/method_1_analysis/output/final_output_data/')
saveRDS(all_data, file = 'sbv/method_1_analysis/output/all_data.RDS')


#exclude results
excluded_results <- excludeResultsAll(method, all_data)
saveRDS(excluded_results, file = 'sbv/method_1_analysis/output/all_data_excluded.RDS')


result <- excluded_results[excluded_results$pscale==1,] %>%
  group_by(pscale) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0('sbv/method_1_analysis/output/specificity_matrix.RDS'))


#Get summarized matrix (for convergence)
summarised_all <- all_data %>% group_by(pscale)
summarised_all <- summarised_all %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_outside = median(proportion_after_wavesplit)
    , date_first_above = median_cluster(date_first_after_wavesplit)
    , .groups = 'drop')

saveRDS(summarised_all, file = 'sbv/method_1_analysis/output/summarised_results_all.RDS')



#Get summarized matrix (for sensitivity)
summarised <- excluded_results %>% group_by(pscale)
summarised <- summarised %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_outside = median(proportion_after_wavesplit)
    , date_first_above = median_cluster(date_first_after_wavesplit)
    , .groups = 'drop')

saveRDS(summarised, file = 'sbv/method_1_analysis/output/summarised_results.RDS')

#summarised_with_median_convergence
summarised <- excluded_results %>% group_by(pscale)
summarised <- summarised %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = median(kappa_con)
    , lambda_con = median(lambda_con)
    , proportion_outside = median(proportion_after_wavesplit)
    , date_first_above = median_cluster(date_first_after_wavesplit)
    , .groups = 'drop')

saveRDS(summarised, file = 'sbv/method_1_analysis/output/summarised_results_med_con.RDS')


result <- RDS %>%
  group_by(pscale) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0('sbv/method_1_analysis/output/specificity_matrix.RDS'))


#merge the results from method 4 where file1 is the original results, and file2 the results for exclusion
#merge_results_m1 <- function(file1, file2){
#  df1 <- readRDS(file1)
#  df1$date_first_below_10 <- rep(df2$date_first_below_10[1], nrow(df1))
#  df1$date_first_below_5 <- rep(df2$date_first_below_5[1], nrow(df1))
#  df1$date_first_above_5 <- rep(df2$date_first_above_5[1], nrow(df1))
#  df1$date_first_above_10 <- rep(df2$date_first_above_10[1], nrow(df1))
#  df1$converges <- rep(df2$converges,  nrow(df1))
#  
#  saveRDS(df1, file=paste0(file1,'.test.RDS'))  
#}  

#for (i in 1:20) {
#  combineRawResults(1, i
#                    , directory = paste0('sbv/raw_output/m1/', i)
#                    , output_dir = paste0('sbv/method_1_analysis/output/final_output_data/check_data')
#  )
#  merge_results_m1(
#    paste0('sbv/method_1_analysis/output/final_output_data/original/batch_',i,'_results.RDS')
#    , paste0('sbv/method_1_analysis/output/final_output_data/check_data/method_1_seed_',i,'_full.RDS')
#  )
#}


# Save results in output/final_output_data in results directory
#method <- 1 
#results_dir <- paste0('sbv/method_',method,'_analysis/results')
#dir.create(results_dir)
#input_dir <- paste0('sbv/method_',method,'_analysis/output/final_output_data/')
#for (i in 1:20) {
#  results <- readRDS(paste0(input_dir, 'batch_',i,'_results.RDS.test.RDS'))
#  write.csv(results
#            , paste0(results_dir, '/method_1_seed_', i, '_analysis.csv')
#            , row.names=FALSE)
#}
