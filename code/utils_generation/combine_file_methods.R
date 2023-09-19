# Methods that are used in the data processing process

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('utils', 'cleanup_methods.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)
dir.create('utils')


# combine raw results in raw folder (method_analysis/output/raw)  OR specified directory
# and save it in output dir as file_name
# This is used where date_first_below_10 and date_first_below_5 is available
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

# combine raw results in raw folder (method_analysis/output/raw)  OR specified directory
# and save it in output dir as file_name
# This is used where date_first_below_10 and date_first_below_5 is not available
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


# Function that conbines all the results in the specified direcoty
# Returns a dataframe
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

# Function that returns results after excluding non-converging results 
# (results where kappa and lambda does not converge)
excludeResultsNonConverging <- function(method, results)  {
  #if converges column doesnt exist, create it
  if (!("converges" %in% colnames(results))){
    if (method=='third')
      results <- results %>% mutate(converges = ifelse(kappa_con <= 1.1 & lambda_con <= 1.1 & lambda_2_con <= 1.1, TRUE, FALSE))
    else 
      results <- results %>% mutate(converges = ifelse(kappa_con <= 1.1 & lambda_con <= 1.1, TRUE, FALSE))
  } 
  results <- results[results$converges == TRUE,]
  return(results[rowSums(is.na(results)) < ncol(results), ])

}  

# Function that returns results after excluding non-converging results
# and results where clusters are outside projection interval during fitting period
# Returns a dataframe
excludeResultsAll <- function(method, results)  {
  #1. Remove where cluster below 10 before fit through exists
  results <- results[is.na(results$date_first_below_10),]
  #2. Remove where cluster above 5 before fit through exists
  results <- results[is.na(results$date_first_above_5),]
  #3. Remove non-converging rows
  results <- results[results$converges == TRUE,]
  
  return(results[rowSums(is.na(results)) < ncol(results), ])
}

# Calculates the proportion of runs that converged (data is a vector of convergence diagnostics)
# returning a number
proportion_converence <- function(data){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data)
  
  return(proportion)
}

# Calculates the proportion of runs that converged (data1 & data2 is a vector of convergence diagnostics)
# returning a number
proportion_convergence_both <- function(data1, data2){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data1 <= 1.1 & data2 <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data1)
  
  return(proportion)
}

# Calculates the proportion of runs that converged (data1 & data2 & data3 is a vector of convergence diagnostics)
# returning a number
proportion_convergence_three <- function(data1, data2, data3){
  # Count how many values in data are less than 1.1
  count_less_than_1_1 <- sum(data1 <= 1.1 & data2 <= 1.1 & data3 <= 1.1)
  
  # Calculate the proportion of values less than 1.1
  proportion <- count_less_than_1_1 / length(data1)
  
  return(proportion)
}

# Calculates the median of the clusters 
# Treats the NA values as  Inf to avoid having NA. 
# If median is Inf, return NA
median_cluster <- function (values) { 
  values[is.na(values)] <- Inf
  med <- median(values)
  if (med == Inf)
    return (NA)
  return (med)
}

# Checks if all the files inthe directory has extexted number of rows. 
checkBatchesComplete <- function (expected, directory) {
  files <- list.files(path=directory, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
  for (file in files) {
      data <- readRDS(file)
      if (nrow(data) != expected) {
        print(paste("File", file, "does not have", expected, "rows"))
      }
  }
}


save(combineRawResults
     , combineRawResultsOld
     , getAllResults
     , excludeResultsAll
     , excludeResultsNonConverging
     , proportion_converence
     , proportion_convergence_both
     , median_cluster
     , checkBatchesComplete
     , proportion_convergence_three
     ,  file='utils/cleanup_methods.RData' )
  