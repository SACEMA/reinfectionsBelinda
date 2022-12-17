library('dplyr')

## Combine results that we are currently busy with ###
combineResultsCurrent = function (method) {
  files <- list.files(path=paste0("method_",method,"_analysis/output/"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,readRDS(f))
  }
  final_RDS <- do.call(rbind.data.frame, resultList)
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
  
  if (method == 2)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, .keep_all = TRUE)
  
  if (method == 3)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, .keep_all = TRUE)
  
  if (method == 4)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, dprob, .keep_all = TRUE)
  
  saveRDS(final_RDS, file=paste0("method_",method,"_analysis/output/final_output_data/batch_", batch_number,"_results.RDS"))
}

### Combine the RDS in the final_output_data folder and save it for plot usage


## Code that I can reuse
calculate_values <- final_RDS %>% 
  group_by(pscale, pobs_2) %>% 
  summarise(
    kappa_con_med = median(kappa_con)
    , lambda_con_med = median(lambda_con)
    , .groups = 'drop')

summary_RDS <- calculate_values %>% as.data.frame()

#cmd to get distinct rows in combined_results
combined_results <- combined_results %>% distinct(pobs_2, pscale, .keep_all = TRUE)