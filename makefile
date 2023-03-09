

R = Rscript $^ $@
UTILS_SCRIPTS = code/utils_generation
INFECTIONS = 2
SBV = TRUE

#create utils 
all: create_utils create_params

sbv: create_utils create_params

create_utils: utils/settings.RData utils/plotting_fxns.RData \
	utils/observe_prob_functions.RData utils/mcmc_functions.RData \
	utils/generate_data.RData utils/fit_functions.RData 

scenarios = 1 2 3 4 5

utils/settings.RData: $(UTILS_SCRIPTS)/settings.R
	${R}
	
utils/plotting_fxns.RData: $(UTILS_SCRIPTS)/plotting_fxns.R
	${R}
	
utils/observe_prob_functions.RData: $(UTILS_SCRIPTS)/observe_prob_functions.R
	${R}
	
utils/mcmc_functions.RData: $(UTILS_SCRIPTS)/mcmc_general.R
	${R}
	
utils/generate_data.RData: $(UTILS_SCRIPTS)/generate_data.R
	${R}
	
utils/fit_functions.RData: $(UTILS_SCRIPTS)/fit_functions.R
	${R}

utils/cleanup_methods.RData: $(UTILS_SCRIPT)/combine_file_methods.R
	${R}

sbv/method_%_analysis/parameters.RData: sbv/parameter_generation/create_parameter_files_m%.R
	${R}
	
create_params: sbv/method_1_analysis/parameters.RData \
	sbv/method_2_analysis/parameters.RData \
	sbv/method_3_analysis/parameters.RData \
	sbv/method_4_analysis/parameters.RData \
	sbv/method_5_analysis/parameters.RData
