
## Combine results that we are currently busy with ###
combineResultsCurrent = function (method) {
  #set path:
  path_ <- paste0("sbv/method_",method,"_analysis/")
  output_file <- 'combined_results.RDS'  
  
  files <- list.files(path=paste0(path_, 'output/'), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  
  for (f in files) {
    resultList = c(resultList,readRDS(f))
  }
  final_RDS <- do.call(rbind.data.frame, resultList)
  
  if (method == 2)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, .keep_all = TRUE)
  
  
  saveRDS(final_RDS, file=paste0(paste0(path_, output_file)))
}



#combine raw results in raw folder (method_analysis/output/raw) and save it in output dir
combineResultsInRawCl = function (method, name_of_file, delete=TRUE) { 
  if (method == 'third') {
    path_ <- paste0('sbv/third_infections/')
  } else if (method == 'l2third') {
    path_ <- paste0('sbv/third_infections/')
  } else {
    path_ <- paste0("sbv/method_",method,"_analysis/")
  }
  
  files <- list.files(path=paste0("sbv/raw_output/m",method), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,list(readRDS(f)))
  } 
  saveRDS(resultList, file=paste0(paste0(path_, 'output/'), name_of_file))
  #delete files in raw
  if (delete==TRUE) {
    lapply(files, unlink)
  }
}

#### combine files of specific batch so that we can remove the batch ####
combineBatchFiles = function (method, batch_number, delete=TRUE) {
  files <- list.files(path=paste0("sbv/method_",method,"_analysis/output/batch",batch_number), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,readRDS(f))
  }
  print(method)
  final_RDS <- do.call(rbind.data.frame, resultList)
  if (method == 1)
    final_RDS <- final_RDS %>% distinct(pscale, .keep_all = TRUE)
  
  if (method == 2)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, .keep_all = TRUE)
  
  if (method == 3)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, .keep_all = TRUE)
  
  if (method == 4)
    final_RDS <- final_RDS %>% distinct(pobs_2, pscale, pobs_1, dprob, .keep_all = TRUE)
  
  if (method == 5)
    final_RDS <- final_RDS %>% distinct(pobs_1_min, pobs_2_max, pscale, xm, steep, .keep_all = TRUE)
  
  
  
  saveRDS(final_RDS, file=paste0("sbv/method_",method,"_analysis/output/final_output_data/batch_", batch_number,"_results.RDS"))
  if (delete==TRUE)
    unlink(x = '"sbv/method_",method,"_analysis/output/batch"', recursive = TRUE)
}

median_cluster <- function (values) { 
  values[is.na(values)] <- Inf
  med <- median(values)
  if (med == Inf)
    return (NA)
  return (med)
}

get_median_values <- function(method) {
  files <- list.files(path=paste0("sbv/method_",method,"_analysis/output/final_output_data"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  
  
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
  }
  
  if (method==4){
    rds_1 <- final_RDS %>% group_by(pscale, pobs_1, pobs_2, dprob)
  }
  
  if (method==5){
    rds_1 <- final_RDS %>% group_by(pscale, pobs_1_min, pobs_1_max, multiplier, xm, steep)
  }
  
  calculate_values <- rds_1 %>% 
    summarise(kappa_con = median(kappa_con)
              , lambda_con = median(lambda_con)
              , proportion = median(proportion)
              , date_first = median_cluster(date_first)
              , proportion_after_wavesplit = median(proportion_after_wavesplit)
              , date_first_after_wavesplit = median_cluster(date_first_after_wavesplit)
              , .groups = 'drop')
  
  rm(rds_1)
  summary_RDS <- calculate_values %>% as.data.frame()
  
  saveRDS(summary_RDS, file=paste0("sbv/method_",method,"_analysis/combined_results.RDS"))
}

#check if results in combined_results file is complete - run the getResultsCurrent() method to get the latest update
results_complete <- function(method) {
  script_path <- paste0('sbv/method_', method, '_analysis/check_results_complete_m', method, '.R')
  source(script_path)
}


#combine raw results in raw folder (method_analysis/output/raw) and save it in output dir
combineResultsThird = function (batch, l2, delete=TRUE) { 
  if (l2==TRUE) {
    files <- list.files(path=paste0("sbv/raw_output/ml2third/",batch), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  } else {
    files <- list.files(path=paste0("sbv/raw_output/mlthird/",batch), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  }
  
  resultList <- vector(mode = "list")
  for (f in files) {
    resultList = c(resultList,list(readRDS(f)))
  } 
  
  final_RDS <- do.call(rbind.data.frame, resultList)
  
  name_of_file <- paste0('results_seed_',batch,'.RDS')
  if (l2==TRUE) {
    dir.create("sbv/third_infections/output/ml2third")
    saveRDS(final_RDS, file=paste0("sbv/third_infections/output/ml2third/", name_of_file))
  } else {
    dir.create("sbv/third_infections/output/mlthird")
    saveRDS(final_RDS, file=paste0("sbv/third_infections/output/mlthird", name_of_file))
  }
  
}


get_median_values_third <- function(l2) {
  library()
  if (l2==TRUE) {
    files <- list.files(path=paste0("sbv/third_infections/output/ml2third"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  } else {
    files <- list.files(path=paste0("sbv/third_infections/output/third"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  }
  
  
  final_RDS <- data.frame()
  
  for (file in files) {
    final_RDS <- rbind.data.frame(final_RDS, readRDS(file))
  }
  
  rds_1 <- final_RDS %>% group_by(pscale) 
  
  if (l2==TRUE) {
    calculate_values <- rds_1 %>% 
      summarise(kappa_con = median(kappa_con)
                , lambda_con = median(lambda_con)
                , lambda_2_con = median(lambda_2_con)
                , proportion = median(proportion)
                , date_first = median_cluster(date_first)
                , .groups = 'drop')  
  } else {
    calculate_values <- rds_1 %>% 
      summarise(kappa_con = median(kappa_con)
                , lambda_con = median(lambda_con)
                , proportion = median(proportion)
                , date_first = median_cluster(date_first)
                , .groups = 'drop')  
    
  }
  
  rm(rds_1)
  
  summary_RDS <- calculate_values %>% as.data.frame()
  if (l2==TRUE) {
    saveRDS(summary_RDS, file=paste0("sbv/third_infections/output/median_results_l2.RDS"))
  } else {
    saveRDS(summary_RDS, file=paste0("sbv/third_infections/output/median_results.RDS"))
  }
}

#returns TRUE or FALSE depending on convergence


get_median_results_proportion <- function(method){
  files <- list.files(path=paste0("sbv/method_",method,"_analysis/output/final_output_data"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  
  
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
  }
  
  if (method==4){
    rds_1 <- final_RDS %>% group_by(pscale, pobs_1, pobs_2, dprob)
  }
  
  if (method==5){
    rds_1 <- final_RDS %>% group_by(pscale, pobs_1_min, pobs_1_max, multiplier, xm, steep)
  }
  
  calculate_values <- rds_1 %>% 
    summarise(kappa_con = proportion_converence(kappa_con)
              , lambda_con = proportion_converence(lambda_con)
              , proportion = median(proportion)
              , date_first = median_cluster(date_first)
              , proportion_after_wavesplit = median(proportion_after_wavesplit)
              , date_first_after_wavesplit = median_cluster(date_first_after_wavesplit)
              , .groups = 'drop')
  
  rm(rds_1)
  summary_RDS <- calculate_values %>% as.data.frame()
  
  saveRDS(summary_RDS, file=paste0("sbv/method_",method,"_analysis/combined_results.RDS"))
}

### FUNCTIONS TO IDENTIFY RUNS THAT DID NOT COMPLETE
find_missing_numbers <- function(seed){
  directory <- paste0("sbv/raw_output/m5/test_new_data/round4/", seed, "/plots/")
  prefix <- "sim_plot_"
  extension <- ".png"
  number <- 300
  
  existing_files <- list.files(path = directory, pattern = paste0("^", prefix, "\\d+", extension))
  existing_numbers <- as.integer(gsub(pattern = paste0("^", prefix, "(\\d+)", extension), replacement = "\\1", x = existing_files))
  
  missing_numbers <- setdiff(1:number, existing_numbers)
  
  return(missing_numbers)
}
