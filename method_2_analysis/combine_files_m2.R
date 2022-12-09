library('dplyr')

method <- 2



files <- list.files(path=paste0("method_",method,"_analysis/output/"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)


resultList <- vector(mode = "list")
for (f in files) {
  resultList = c(resultList,readRDS(f))
}

rds <- do.call(rbind.data.frame, resultList)

calculate_values <- rds %>% 
                      group_by(pscale, pobs_2) %>% 
                      summarise
                        kappa_con_med = median(kappa_con)
                        , lambda_con_med = median(lambda_con)
                        , .groups = 'drop')

final_RDS <- calculate_values %>% as.data.frame()


save(final_RDS, file=paste0("method_",method,"_analysis/combined_results.RDS"))


