.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'cleanup_methods.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)
dir.create('utils')


#Pad the empty rows/columns with NA so the bind_rows can work out
pad_with_na <- function(x, max_length) {
  if (length(x) < max_length) {
    c(x, rep(NA, max_length - length(x)))
  } else {
    x
  }
}


#combine raw results in raw folder (method_analysis/output/raw) and save it in output dir
combineRawResults = function (method, seed, directory="DEFAULT", output_dir="DEFAULT", file_name="DEFAULT") { 
  if (directory=="DEFAULT")
    dir = paste0("sbv/method_",method,"_analysis/output/raw")
  else
    dir = directory
  
  files <- list.files(path=dir, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  #sort files
  files <- mixedsort(files)
  
  resultList <- vector(mode = "list")
  
  for (f in files) {
    resultList = c(resultList,list(readRDS(f)))
  } 
  
  #to know where to pad
  final_RDS <- bind_rows(resultList)
  final_RDS <- final_RDS %>% 
    mutate(converges = ifelse(kappa_con <= 1.1 & lambda_con <= 1.1, TRUE, FALSE)
           , below_5 = is.na(date_first_below_5)
           , below_10 = is.na(date_first_below_10)
           , index_column = 1:nrow(final_RDS)
    )
  
  if (file_name=="DEFAULT") {
    name_of_file <- paste0('method_', method, '_seed_', seed, '_full.RDS')
  } else {
    name_of_file <- file_name
  }
    
  if (output_dir=="DEFAULT")
    saveRDS(final_RDS, file=paste0("sbv/method_",method,"_analysis/output/", name_of_file))
  else 
    saveRDS(final_RDS, file=paste0(output_dir, '/', name_of_file))
}

#combine raw results in raw folder (method_analysis/output/raw) and save it in output dir
combineRawResultsOld = function (method, seed, directory="DEFAULT", output_dir="DEFAULT") { 
  if (directory=="DEFAULT")
    dir = paste0("sbv/method_",method,"_analysis/output/raw")
  else
    dir = directory
  
  files <- list.files(path=dir, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  #sort files
  files <- mixedsort(files)
  
  resultList <- vector(mode = "list")
  
  for (f in files) {
    resultList = c(resultList,list(readRDS(f)))
  } 
  
  #to know where to pad
  final_RDS <- bind_rows(resultList)
  
  name_of_file <- paste0('method_', method, '_seed_', seed, '_full.RDS')
  if (output_dir=="DEFAULT")
    saveRDS(final_RDS, file=paste0("sbv/method_",method,"_analysis/output/", name_of_file))
  else 
    saveRDS(final_RDS, file=paste0(output_dir, '/', name_of_file))
}

getAllResults <- function(method, directory = "DEFAULT") {
  if (directory=="DEFAULT") {
    dir = paste0("sbv/method_",method,"_analysis/output/")
  } else 
    dir = directory
  
  files <- list.files(path=dir, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  files <- mixedsort(files)
  final_RDS <- data.frame()
  
  for (file in files) {
    final_RDS <- rbind.data.frame(final_RDS, readRDS(file))
  }
  return (final_RDS)
}

excludeResultsNonConverging <- function(method, results)  {
  results <- results[results$converges == TRUE,]
  return(results)
}  

excludeResultsAll <- function(method, results)  {
  #1. Remove where cluster below 10 before fit through exists
  results <- results[is.na(results$date_first_below_10),]
  #2. Remove where cluster above 5 before fit through exists
  results <- results[is.na(results$date_first_above_5),]
  #3. Remove non-converging rows
  results <- results[results$converges == TRUE,]
  
  return(results)
}

proportion_converence <- function(data){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data)
  
  return(proportion)
}


proportion_convergence_both <- function(data1, data2){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data1 <= 1.1 & data2 <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data1)
  
  return(proportion)
}

proportion_convergence_three <- function(data1, data2, data3){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data1 <= 1.1 & data2 <= 1.1 & data3 <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data1)
  
  return(proportion)
}

getSummarisedResults <- function(method, directory) {
  files <- list.files(path=paste0(directory), pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  
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
    rds_1 <- final_RDS %>% group_by(pscale, pobs_1_min, pobs_1_max, pobs_2_max, pobs_2_min, xm, steep)
  }
  
  if (method!=5)
  calculate_values <- rds_1 %>% 
    summarise(
                convergence = proportion_convergence_both(lambda_con, kappa_con)
              ,  kappa_con = proportion_converence(kappa_con)
              , lambda_con = proportion_converence(lambda_con)
              , proportion = median(proportion)
              , date_first = median_cluster(date_first)
              , proportion_after_wavesplit = median(proportion_after_wavesplit)
              , date_first_after_wavesplit = median_cluster(date_first_after_wavesplit)
              , .groups = 'drop')
  else 
    calculate_values <- rds_1 %>% 
    summarise(
      convergence = proportion_convergence_both(lambda_con, kappa_con)
      ,  kappa_con = proportion_converence(kappa_con)
      , lambda_con = proportion_converence(lambda_con)
      , .groups = 'drop')
  
  rm(rds_1)
  summary_RDS <- calculate_values %>% as.data.frame()
  
  saveRDS(summary_RDS, file=paste0("sbv/method_",method,"_analysis/combined_results.RDS"))
  
  return (summary_RDS)
}

median_cluster <- function (values) { 
  values[is.na(values)] <- Inf
  med <- median(values)
  if (med == Inf)
    return (NA)
  return (med)
}
  
checkBatchesComplete <- function (expected, directory) {
  files <- list.files(path=directory, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  for (file in files) {
      data <- readRDS(file)
      if (nrow(data) != expected) {
        message(paste("File", file, "does not have", expected, "rows"))
      }
  }
}


save(combineRawResults
     , combineRawResultsOld
     , pad_with_na
     , getAllResults
     , excludeResultsAll
     , excludeResultsNonConverging
      , getSummarisedResults
     , proportion_converence
     , proportion_convergence_both
     , median_cluster
     , checkBatchesComplete
     , proportion_convergence_three
     ,  file='utils/cleanup_methods.RData' )
  