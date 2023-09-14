library('dplyr')

method <-5
seed <- 8

results_dataframe_file <- paste0('sbv/method_', method, '_analysis/output/final_output_data/method_5_seed_', seed,'_full.RDS')
parameter_set_file <- paste0('sbv/method_', method, '_analysis/parameters.RData')

final_RDS <- readRDS(results_dataframe_file)
load(parameter_set_file)

final_RDS <- final_RDS %>% distinct(pobs_1_min
                                    , pobs_1_max
                                    , pobs_2_min
                                    , pobs_2_max
                                    , steep
                                    , xm
                                    , pscale
                                    , .keep_all = TRUE)

#identify missing entries
pobs <- list()
for (i in 1:nrow(final_RDS)) {
  pobs_1_min <- final_RDS[i,]$pobs_1_min
  pobs_1_max <- final_RDS[i,]$pobs_1_max
  pobs_2_min <- final_RDS[i,]$pobs_2_min
  pobs_2_max <- final_RDS[i,]$pobs_2_max
  steep <- final_RDS[i,]$steep
  xm <- final_RDS[i,]$xm
  pscale <- final_RDS[i,]$pscale
  pobs <- append(pobs, paste0(pobs_1_min,';'
                              ,pobs_1_max,';'
                              ,pobs_2_min,';'
                              ,pobs_2_max,';'
                              ,steep,';'
                              ,xm,';'
                              ,pscale
  ))
}

do_still <- list()
for (i in 1:nrow(save_params)){
  pobs_1_min <- save_params[i,]$pobs_1_min
  pobs_1_max <- save_params[i,]$pobs_1_max
  pobs_2_min <- save_params[i,]$pobs_2_min
  
  pobs_2_max <- save_params[i,]$pobs_2_max
  multiplier <- save_params[i,]$multiplier
  steep <- save_params[i,]$steep
  xm <- save_params[i,]$xm
  pscale <- save_params[i,]$pscale
  search <- paste0(
                                pobs_1_min,';'
                                ,pobs_1_max,';'
                                ,pobs_2_min,';'
                                ,pobs_2_max,';'
                                ,steep,';'
                                ,xm,';'
                                ,pscale
          )
  if (!(search %in% pobs))
    do_still <- append(do_still, paste0(search, ', Number: ', i))
}

if (length(do_still)>0){
  print("Parameter sets outstanding")
  print(do_still)
}
if (length(do_still)==0){
  print("All Done :) ")
}

