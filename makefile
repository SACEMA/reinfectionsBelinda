# IF infections argument not provided, set it to 2.
ifeq ($(infections),)
	infections := 2
endif


# IF L2 (second lambda argument) not provided, set it to FALSE. 
ifeq ($(L2),)
	L2 := FALSE 
endif

# Specify based on mcmc run 
ifeq ($(L2),TRUE)
CONFIG_FILE = config_general_l2.json
MCMC_FUNCTIONS = utils/mcmc_functions_l2.RData
MCMC_RUN = code/run/2_run_mcmc.R 
MCMC_OUTPUT = output/posterior_90_null.RData
SIM_RUN = code/run/3_sim_null.R 
SIM_OUTPUT = output/sim_90_null.RDS
CON_PLOT = code/run/convergence_plot.R
CON_PLOT_OUTPUT = output/convergence_plot.png
SIM_PLOT_OUTPUT = output/sim_plot_90_null.png
else 
CONFIG_FILE = config_general.json
MCMC_FUNCTIONS = utils/mcmc_functions.RData
MCMC_RUN = code/run/2_run_mcmc_without_lambda2.R 
MCMC_OUTPUT = output/posterior_90_null_without_l2.RData
SIM_RUN = code/run/3_sim_null_without_lambda2.R 
SIM_OUTPUT = output/sim_90_null_without_l2.RDS
CON_PLOT = code/run/convergence_plot_without_lambda2.R
CON_PLOT_OUTPUT = output/convergence_plot_without_l2.png
SIM_PLOT_OUTPUT = output/sim_plot_90_null_without_l2.png
endif

$(info DOING IT WITH LAMBDA 2: )
$(info $(L2))

R = Rscript $^ $@
UTILS_SCRIPTS = code/utils_generation


#run does the normal run


run: install utils_run data mcmc sim plots


#sbv setup for simulation-based-validation
sbv: utils_sbv create_params_sbv utils/mcmc_functions.RData

l2_sbv: utils_sbv create_params_sbv utils/mcmc_functions_l2.RData

utils: utils/fit_functions.RData \

utils_sbv: utils utils/settings.RData \
	utils/observe_prob_functions.RData \
	utils/generate_data.RData  \
	utils/cleanup_methods.RData

utils_run: utils utils/plotting_fxns.RData $(MCMC_FUNCTIONS)

create_params_sbv: sbv/method_1_analysis/parameters.RData \
	sbv/method_2_analysis/parameters.RData \
	sbv/method_3_analysis/parameters.RData \
	sbv/method_4_analysis/parameters.RData \
	sbv/method_5_analysis/parameters.RData \
	sbv/third_infections/parameters.RData
	


#Install packages
install: code/helper_files/install.R
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

utils/cleanup_methods.RData: $(UTILS_SCRIPTS)/combine_file_methods.R
	${R}


#Target for parameter files
sbv/method_%_analysis/parameters.RData: sbv/parameter_generation/create_parameter_files_m%.R
	${R}
	
sbv/third_infections/parameters.RData: sbv/parameter_generation/create_parameter_files_third_infections.R
	${R}
	

#Get infections argument to determine for which infections this is done 
$(eval $(infections):;@:)

#Create data for analysis
data/ts_data_for_analysis.RDS: code/run/1_prep_ts_data.R data/ts_data.csv $(CONFIG_FILE) $(infections)
	${R} 
	
data: data/ts_data_for_analysis.RDS

# Run MCMC
$(MCMC_OUTPUT): $(MCMC_RUN) data/ts_data_for_analysis.RDS $(MCMC_FUNCTIONS) utils/fit_functions.RData $(CONFIG_FILE) $(infections)
	${R}

mcmc: $(MCMC_OUTPUT)

# Run Simulations
$(SIM_OUTPUT): $(SIM_RUN) $(MCMC_OUTPUT) \
data/ts_data_for_analysis.RDS utils/fit_functions.RData $(CONFIG_FILE) $(infections)
	${R}


sim: $(SIM_OUTPUT)


# Generate plots
output/sim_plot_90_null.png: code/run/3_sim_plot.R $(SIM_OUTPUT) \
data/ts_data_for_analysis.RDS $(CONFIG_FILE) utils/plotting_fxns.RData $(infections)
	${R}

$(CON_PLOT_OUTPUT): $(CON_PLOT) \
$(MCMC_OUTPUT) utils/fit_functions.RData $(CONFIG_FILE) $(infections)
	${R}
	

plots: $(SIM_PLOT_OUTPUT) $(CON_PLOT_OUTPUT)

