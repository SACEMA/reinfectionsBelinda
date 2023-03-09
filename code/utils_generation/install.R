# Citation: Pulliam, JRC, C van Schalkwyk, N Govender, A von Gottberg, C 
# Cohen, MJ Groome, J Dushoff, K Mlisana, and H Moultrie. (2022) Increased
# risk of SARS-CoV-2 reinfection associated with emergence of Omicron in
# South Africa. _Science_ <https://www.science.org/doi/10.1126/science.abn4947>
# 
# Repository: <https://github.com/jrcpulliam/reinfection


# I have added additional required packages. 
.debug <- 'utils'
.args <- if (interactive()) sprintf(c(
  file.path('%s', 'sessionInfo.RDS') # output
), .debug[1]) else commandArgs(trailingOnly = TRUE)

target <- tail(.args, 1)

packs <- rownames(installed.packages()) # names of all installed packages
req <- c('coda' # for wrangling MCMC output
         , 'colorspace' # for useful color scales / palettes
         , 'data.table' # for data wrangling
         , 'ggplot2' # for plotting
         , 'hexbin' # for plotting posterior parameter sets
         , 'jsonlite' # for reading in json configuration files
         , 'Matrix' # lme4 dependency
         , 'patchwork' # for combining figure panels
         , 'uniformly' # for sampling from plausible range of obs probs (polygon)
         , 'posterior'
         , 'dplyr'
         , 'gridExtra'
) # packages required to run code
toInstall <- req[!is.element(req, packs)] # packages needing installation

if(length(toInstall) > 0){
  print("The following packages are required and will be installed:")
  print(toInstall)
  install.packages(toInstall, repos = "http://cran.us.r-project.org")
}

suppressPackageStartupMessages({
  x <- sapply(req, library, character.only = TRUE)
})

ss <- sessionInfo()

# output version information for comparison to the ones listed README.md that
# were used for manuscript preparation