library('dplyr')

## Combine results that we are currently busy with ###
combineResultsCurrent = function (method) {
  files <- list.files(path=paste0("method_",method,"_analysis/output"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,readRDS(f))
  }
  final_RDS <- do.call(rbind.data.frame, resultList)
  
  if (method == 2)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, .keep_all = TRUE)
  
  saveRDS(final_RDS, file=paste0("method_",method,"_analysis/combined_results.RDS"))
}

#combine raw results in raw folder (method_analysis/output/raw) and save it in output dir
combineResultsInRaw = function (method, name_of_file) { 
  files <- list.files(path=paste0("method_",method,"_analysis/output/raw"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,list(readRDS(f)))
  } 
  saveRDS(resultList, file=paste0("method_",method,"_analysis/output/", name_of_file))
}


#### combine files of specific batch so that we can remove the batch ####
combineBatchFiles = function (method, batch_number) {
  files <- list.files(path=paste0("method_",method,"_analysis/output/batch",batch_number), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,readRDS(f))
  }
  final_RDS <- do.call(rbind.data.frame, resultList)
  if (method == 1)
    final_RDS <- final_RDS %>% distinct(pscale, .keep_all = TRUE)
  
  if (method == 2)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, .keep_all = TRUE)
  
  if (method == 3)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, .keep_all = TRUE)
  
  if (method == 4)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, dprob, .keep_all = TRUE)
  
  saveRDS(final_RDS, file=paste0("method_",method,"_analysis/output/final_output_data/batch_", batch_number,"_results.RDS"))
}

get_median_values <- function(method) {
  files <- list.files(path=paste0("method_",method,"_analysis/output/final_output_data"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  
  
  final_RDS <- data.frame()
  
  for (file in files) {
    final_RDS <- rbind.data.frame(final_RDS, readRDS(file))
  }
  
  if (method==1)
    rds_1 <- final_RDS %>% group_by(pscale) 
  
  if (method==2)
            rds_1 <- final_RDS %>% group_by(pscale, pobs_2) 
  
  if (method==3) {
            rds_1 <- final_RDS %>% group_by(pscale, pobs_1, pobs_2)
            print("method 3")
  }
  calculate_values <- rds_1 %>% 
                            summarise(kappa_con = median(kappa_con)
                            , lambda_con = median(lambda_con)
                            , proportion = median(proportion)
                            , date_first = median(date_first)
                            , proportion_after_wavesplit = median(proportion_after_wavesplit)
                            , date_first_after_wavesplit = median(date_first_after_wavesplit)
                            , .groups = 'drop')

  rm(rds_1)
  summary_RDS <- calculate_values %>% as.data.frame()
  
  saveRDS(summary_RDS, file=paste0("method_",method,"_analysis/combined_results.RDS"))
}
  
  