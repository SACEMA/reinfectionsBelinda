ifeq ($(infections),)
	infections := 2
endif

R = Rscript $^ $@
UTILS_SCRIPTS = code/utils_generation
INFECTIONS = 2
SBV = TRUE

#all will just create the utils & parameter files 
all: install utils_run

#run does the normal run
run: install utils_run generate_data data mcmc sim plots

#sbv setup for simulation-based-validation
sbv: utils_sbv create_params_sbv

utils: utils/fit_functions.RData \

utils_sbv: utils utils/settings.RData \
	utils/observe_prob_functions.RData \
	utils/generate_data.RData  \
	utils/mcmc_functions.RData

utils_run: utils utils/plotting_fxns.RData utils/mcmc_functions_l2.RData
	
create_params_sbv: sbv/method_1_analysis/parameters.RData \
	sbv/method_2_analysis/parameters.RData \
	sbv/method_3_analysis/parameters.RData \
	sbv/method_4_analysis/parameters.RData \
	sbv/method_5_analysis/parameters.RData


#Install packages
install: $(UTILS_SCRIPTS)/install.R
	${R}

#Create utils
utils/settings.RData: $(UTILS_SCRIPTS)/settings.R
	${R}
	
utils/plotting_fxns.RData: $(UTILS_SCRIPTS)/plotting_fxns.R
	${R}
	
utils/observe_prob_functions.RData: $(UTILS_SCRIPTS)/observe_prob_functions.R
	${R}
	
utils/mcmc_functions.RData: $(UTILS_SCRIPTS)/mcmc_general.R
	${R}
	
utils/mcmc_functions_l2.RData: $(UTILS_SCRIPTS)/mcmc_general_l2.R
	${R}

utils/generate_data.RData: $(UTILS_SCRIPTS)/generate_data.R
	${R}
	
utils/fit_functions.RData: $(UTILS_SCRIPTS)/fit_functions.R
	${R}

utils/cleanup_methods.RData: $(UTILS_SCRIPT)/combine_file_methods.R
	${R}


#Target for parameter files
sbv/method_%_analysis/parameters.RData: sbv/parameter_generation/create_parameter_files_m%.R
	${R}

#Generate data if data is not provided / does not exist
data/ts_data.csv: data/generate_data/generate_data.R data/generate_data/simulated_data.RDS
	${R} 

generate_data: data/ts_data.csv

#Get infections argument to determine for which infections this is done 
$(eval $(infections):;@:)

#Create data for analysis
data/ts_data_for_analysis.RDS: code/run/prep_ts_data.R data/ts_data.csv config_general.json $(infections)
	${R} 
	
data: data/ts_data_for_analysis.RDS

# Run MCMC
output/posterior_90_null.RData: code/run/run_mcmc.R data/ts_data_for_analysis.RDS utils/mcmc_functions_l2.RData utils/fit_functions.RData config_general.json $(infections)
	${R}

mcmc: output/posterior_90_null.RData

# Run Simulations
output/sim_90_null.RDS: code/run/sim_null.R output/posterior_90_null.RData \
data/ts_data_for_analysis.RDS utils/fit_functions.RData config_general.json $(infections)
	${R}

sim: output/sim_90_null.RDS

# Generate plots
output/sim_plot_90_null.png: code/run/sim_plot.R output/sim_90_null.RDS \
data/ts_data_for_analysis.RDS config_general.json utils/plotting_fxns.RData $(infections)
	${R}

output/convergence_plot.png: code/run/convergence_plot.R \
output/posterior_90_null.RData utils/fit_functions.RData config_general.json $(infections)
	${R}

plots: output/sim_plot_90_null.png output/convergence_plot.png
