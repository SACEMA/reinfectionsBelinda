
# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfections>

#install.packages('ggplot2', repos = "http://cran.us.r-project.org")
library('ggplot2')

.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'plotting_fxns.RData') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

utils <- './utils/'
dir.create(utils)


target <- tail(.args, 1)

# Functions from Carl A.B. Pearson (modified):
gg_scale_wrapper <- function(
  scale_fun,
  ...
) {
  stopifnot(!missing(scale_fun))
  defs <- list(...)
  if (!length(defs)) warning(
    "provided no default arguments; consider using scale_fun directly."
  )
  
  return(function(...) {
    #' this different ... is how you get a function back that let's you
    #' override defaults, set other arguments to scale_... functions
    .ellipsis <- list(...)
    .args <- defs
    .args[names(.ellipsis)] <- .ellipsis
    do.call(scale_fun, .args)
  })
}

#' then use like this to create your scale-with-defaults:
scale_x_Ms <- gg_scale_wrapper(
  scale_x_date,
  name = NULL,
  breaks = function(l) {
    rl <- round(as.POSIXlt(l), "month")
    as.Date(seq(rl[1] + 60*60*24*15, rl[2], by="month"))
  },
  labels = function(bs) {
    month.abb[month(bs)]
  },
  minor_breaks = 'month'
)

split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(basename(path), split_path(dirname(path))))
}

save(gg_scale_wrapper, scale_x_Ms, split_path, file = target)
