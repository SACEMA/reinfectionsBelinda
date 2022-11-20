files <- list.files(path="cluster_code/output_400/", pattern="*.RDS", full.names=TRUE, recursive=FALSE)


resultList <- vector(mode = "list")
for (f in files) {
  resultList = c(resultList,readRDS(f))
}

final_RDS <- do.call(rbind.data.frame, resultList)

save(final_RDS, file='cluster_code/results_400.RDS')