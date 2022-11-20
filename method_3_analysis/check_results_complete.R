results_dataframe_file <- 'method_3_analysis/results_400.RDS'
parameter_set_file <- 'utils/parameters.RData'

load(results_dataframe_file)
load(parameter_set_file)

#identify missing entries
pobs <- list()
for (i in 1:nrow(final_RDS)) {
  pobs1 <- final_RDS[i,]$pobs_1
  pobs2 <- final_RDS[i,]$pobs_2
  pobs <- append(pobs, paste0(pobs1,';',pobs2))
}

load('utils/parameters.RData')

do_still <- list()
for (i in 1:nrow(save_params)){
  pobs1 <- save_params[i,]$pobs_1
  pobs2 <- save_params[i,]$pobs_2
  search <- paste0(pobs1,';',pobs2)
  if (!(search %in% pobs))
    do_still <- append(do_still, search)
}

if (length(do_still)>0){
  print("Parameter sets outstanding")
  print(do_still)
}
if (length(do_still)==0){
  print("All Done :) ")
}
