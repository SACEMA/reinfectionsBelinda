args = commandArgs(trailingOnly=TRUE)

method <- strtoi(args[1])

start <- strtoi(args[2])

end <- strtoi(args[3])

load('utils/cleanup_methods.RData')

combineResultsInRaw(method, paste0('m2_results_', start, '_', end, '.RDS'))
combineResultsCurrent(method)