# Prerequisite: scenario results as RDS (dataframes) files in 
#'sbv/method_4_analysis/output/final_output_data' 

library(ggplot2)
library(ggtext)
library(dplyr)
library(gtools)
load('utils/cleanup_methods.RData')

method <- 4


#get all the data
all_data <- getAllResults(4, 'sbv/method_4_analysis/output/final_output_data/')
saveRDS(all_data, file = 'sbv/method_4_analysis/output/all_data.RDS')

#exclude results
excluded_results <- excludeResultsAll(4, all_data)
saveRDS(excluded_results, file = 'sbv/method_4_analysis/output/all_data_excluded.RDS')


result <- excluded_results[excluded_results$pscale==1,] %>%
  group_by(pscale, pobs_2, pobs_1, dprob) %>%
  summarise(count_kappa_lambda_lt_1.1 = sum(kappa_con < 1.1 & lambda_con < 1.1),
            count_date_not_na = sum(!is.na(date_first_after_wavesplit) & kappa_con < 1.1 & lambda_con < 1.1)) %>%
  # Calculate the result of [ 1 - (count_date_not_na / count_kappa_lambda_lt_1.1) ]
  mutate(specificity = 1 - (count_date_not_na / count_kappa_lambda_lt_1.1))

saveRDS(result, file=paste0('sbv/method_4_analysis/output/specificity_matrix.RDS'))

#Get summarized matrix (for convergence)
summarised_all <- all_data %>% group_by(pscale, pobs_1, pobs_2, dprob)
summarised_all <- summarised_all %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_above = median(proportion_after_wavesplit)
    , date_first_above = median_cluster(date_first_after_wavesplit)
    , .groups = 'drop')

saveRDS(summarised_all, file = 'sbv/method_4_analysis/output/summarised_results_all.RDS')



#Get summarized matrix (for sensitivity)
summarised <- excluded_results %>% group_by(pscale, pobs_1, pobs_2, dprob)
summarised <- summarised %>% 
  summarise(
    convergence = proportion_convergence_both(lambda_con, kappa_con)
    ,  kappa_con = proportion_converence(kappa_con)
    , lambda_con = proportion_converence(lambda_con)
    , proportion_above = median(proportion_after_wavesplit)
    , date_first_above = median_cluster(date_first_after_wavesplit)
    , .groups = 'drop')

saveRDS(summarised, file = 'sbv/method_4_analysis/output/summarised_results.RDS')



#merge the results from method 4 where file1 is the original results, and file2 the results for exclusion
#merge_results_m4 <- function(file1, file2){
#  df1 <- readRDS(file1)
#  df2 <- readRDS(file2)
#  merged <- merge(df1, df2, by = c("pobs_1", "pobs_2", "dprob"), all.x = TRUE)
#  merged <- subset(merged, select = -c(pscale.y, kappa_con.y, lambda_con.y))
#  
#  merged <- merged %>% 
#    rename(
#      kappa_con = kappa_con.x ,
#      lambda_con = lambda_con.x , 
#      pscale = pscale.x 
#    )
#  saveRDS(merged, file=paste0(file1,'.test.RDS'))  
#}  


# Saves RDS files as CSV files for exporting
#method <- 4
#results_dir <- paste0('sbv/method_',method,'_analysis/results')
#dir.create(results_dir)
#input_dir <- paste0('sbv/method_',method,'_analysis/output/final_output_data/')
#for (i in 1:20) {
#  results <- readRDS(paste0(input_dir, 'batch_',i,'_results.RDS.test.RDS'))
#  write.csv(results
#            , paste0(results_dir, '/method_',method,'_seed_', i, '_analysis.csv')
#            , row.names=FALSE)
#}
