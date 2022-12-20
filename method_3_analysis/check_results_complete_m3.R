library('dplyr')

method <-3 
results_dataframe_file <- paste0('method_', method, '_analysis/combined_results.RDS')
parameter_set_file <- paste0('method_', method, '_analysis/utils/m', method, '_parameters.RData')

final_RDS <- readRDS(results_dataframe_file)
load(parameter_set_file)

#remove duplicates
final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, .keep_all = TRUE)

#identify missing entries
pobs <- list()
for (i in 1:nrow(final_RDS)) {
  pobs1 <- final_RDS[i,]$pobs_1
  pobs2 <- final_RDS[i,]$pobs_2
  pscale <- final_RDS[i,]$pscale
  pobs <- append(pobs, paste0(pobs1,';',pobs2, ';', pscale))
}


do_still <- list()
for (i in 1:nrow(save_params)){
  pobs1 <- save_params[i,]$pobs_1
  pobs2 <- save_params[i,]$pobs_2
  pscale <- save_params[i,]$pscale
  search <- paste0(pobs1,';',pobs2, ';', pscale)

  if (!(search %in% pobs))
    do_still <- append(do_still, paste0(search, 'number: ', i))
}

if (length(do_still)>0){
  print("Parameter sets outstanding")
  print(do_still)
}
if (length(do_still)==0){
  print("All Done :) ")
}
