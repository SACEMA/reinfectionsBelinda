method <- 3


files <- list.files(path=paste0("method_",method,"_analysis/output/"), pattern="*.RDS", full.names=TRUE, recursive=FALSE)


resultList <- vector(mode = "list")
for (f in files) {
  resultList = c(resultList,readRDS(f))
}

final_RDS <- do.call(rbind.data.frame, resultList)

save(final_RDS, file=paste0("method_",method,"_analysis/combined_results.RDS"))