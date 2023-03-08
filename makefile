

R = Rscript $^ $@
UTILS_SCRIPTS = code/utils_generation
INFECTIONS = 2

#create utils 
mkdir utils

create_utils: utils/settings.RData utils/plotting_fxns.RData \
	utils/observe_prob_functions.RData utils/mcmc_functions.RData \
	utils/generate_data.RData utils/fit_functions.RData \
	

utils/settings.RData: UTILS_SCRIPTS/settings.R
	${R}
	
utils/plotting_fxns.RData: UTILS_SCRIPTS/plotting_fxns.R
	${R}
	
utils/observe_prob_functions.RData: UTILS_SCRIPTS/observe_prob_functions.R
	${R}
	
utils/mcmc_functions.RData: UTILS_SCRIPTS/mcmc_general.R
	${R}
	
utils/generate_data.RData: UTILS_SCRIPTS/generate_data.R
	${R}
	
utils/fit_functions.RData: UTILS_SCRIPTS/fit_functions.R
	${R}

utils/cleanup_methods.RData: UTILS_SCRIPT/combine_file_methods.R
	${R}
