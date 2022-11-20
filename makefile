
R = Rscript $^ $@
PLOT_DIR = output/plots_from_methods
MCMC_FUNC = utils/mcmc_functions.RData
FIT_FUNC = utils/fit_functions.RData
PLOT_FUNC = utils/plotting_fxns.RData
OBSERVE_PLOT = utils/observe_prob_functions.RData
VALUE = 2

all: method_1 method_2 method_3 method_4 method_5a

prepdata_all: prepdata_1A prepdata_1B prepdata_2A prepdata_2B prepdata_3A prepdata_3B prepdata_4A prepdata_4B
run_mcmc_all: mcmc_1A mcmc_1B mcmc_2A mcmc_2B mcmc_3A mcmc_3B mcmc_4A mcmc_4B
sim_all: sim_1A sim_1B sim_2A sim_2B sim_3A sim_3B sim_4A sim_4B

### DATA PREPARATION 

data/infection_data.RDS: code/A_get_infection_data.R data/inf_for_sbv.RDS config_general.json
	${R}


#########################################	
### METHOD 1 ###
method_1: method_1A method_1B

## Method 1A ##
method_1A: prepdata_1A mcmc_1A sim_1A
prepdata_1A: data/infection_data.RDS data/1A_NF_ts_data_for_analysis.RDS data/1A_ts_data_for_analysis.RDS
mcmc_1A: prepdata_1A output/1A_posterior_90_null.RData $(PLOT_DIR)/1A_convergence_plot.png 
sim_1A: output/1A_sim_90_null.RDS $(PLOT_DIR)/1A_simplot_90_null.png

## Method 1B ##
method_1B: prepdata_1B mcmc_1B sim_1B
prepdata_1B: data/infection_data.RDS data/1B_NF_ts_data_for_analysis.RDS data/1B_ts_data_for_analysis.RDS
mcmc_1B: prepdata_1B output/1B_posterior_90_null.RData $(PLOT_DIR)/1B_convergence_plot.png
sim_1B: output/1B_sim_90_null.RDS $(PLOT_DIR)/1B_simplot_90_null.png
pscale_analysis_1: output/1B_sim_90_null.RDS output/1_pscale_analysis.RDS 

########################################
### METHOD 2 ###
method_2: method_2A method_2B

## Method 2A ##
method_2A: prepdata_2A mcmc_2A sim_2A
prepdata_2A: data/infection_data.RDS data/2A_NF_ts_data_for_analysis.RDS data/2A_ts_data_for_analysis.RDS
mcmc_2A: prepdata_2A output/2A_posterior_90_null.RData $(PLOT_DIR)/2A_convergence_plot.png
sim_2A: output/2A_sim_90_null.RDS $(PLOT_DIR)/2A_simplot_90_null.png

## Method 2B ## 
method_2B: prepdata_2B mcmc_2B sim_2B
prepdata_2B: data/infection_data.RDS data/2B_NF_ts_data_for_analysis.RDS data/2B_ts_data_for_analysis.RDS
mcmc_2B: prepdata_2B output/2B_posterior_90_null.RData $(PLOT_DIR)/2B_convergence_plot.png
sim_2B: output/2B_sim_90_null.RDS $(PLOT_DIR)/2B_simplot_90_null.png
pscale_analysis_2: output/2B_sim_90_null.RDS output/2_pscale_analysis.RDS 

##############################################	
### METHOD 3 ###
method_3: method_3A method_3B

## Method 3A ##
method_3A: prepdata_3A mcmc_3A sim_3A
prepdata_3A: data/infection_data.RDS data/3A_NF_ts_data_for_analysis.RDS data/3A_ts_data_for_analysis.RDS
mcmc_3A: prepdata_3A output/3A_posterior_90_null.RData $(PLOT_DIR)/3A_convergence_plot.png
sim_3A: output/3A_sim_90_null.RDS $(PLOT_DIR)/3A_simplot_90_null.png

## Method 3B ##
method_3B: prepdata_3B mcmc_3B sim_3B
prepdata_3B: data/infection_data.RDS data/3B_NF_ts_data_for_analysis.RDS data/3B_ts_data_for_analysis.RDS
mcmc_3B: prepdata_3B output/3B_posterior_90_null.RData $(PLOT_DIR)/3B_convergence_plot.png
sim_3B: output/3B_sim_90_null.RDS $(PLOT_DIR)/3B_simplot_90_null.png
pscale_analysis_3: output/3B_sim_90_null.RDS output/3_pscale_analysis.RDS 


##############################################	
### METHOD 4 ###
method_4: method_4A method_4B

## Method 4A ##
method_4A: prepdata_4A mcmc_4A sim_4A
prepdata_4A: data/infection_data.RDS data/4A_NF_ts_data_for_analysis.RDS data/4A_ts_data_for_analysis.RDS
mcmc_4A: prepdata_4A output/4A_posterior_90_null.RData $(PLOT_DIR)/4A_convergence_plot.png
sim_4A: output/4A_sim_90_null.RDS $(PLOT_DIR)/4A_simplot_90_null.png

## Method 4B ##V
method_4B: prepdata_4B mcmc_4B sim_4B
prepdata_4B: data/infection_data.RDS data/4B_NF_ts_data_for_analysis.RDS data/4B_ts_data_for_analysis.RDS
mcmc_4B: prepdata_4B output/4B_posterior_90_null.RData $(PLOT_DIR)/4B_convergence_plot.png
sim_4B: output/4B_sim_90_null.RDS $(PLOT_DIR)/4B_simplot_90_null.png
pscale_analysis_4: output/4B_sim_90_null.RDS output/4_pscale_analysis.RDS 


##############################################	
### METHOD 5 ###
method_5a: method_5Aa method_5Ba

## Method 5Aa ##
method_5Aa: prepdata_5Aa mcmc_5Aa sim_5Aa
prepdata_5Aa: data/infection_data.RDS data/5Aa_NF_ts_data_for_analysis.RDS data/5Aa_ts_data_for_analysis.RDS
mcmc_5Aa: prepdata_5Aa output/5Aa_posterior_90_null.RData $(PLOT_DIR)/5Aa_convergence_plot.png
sim_5Aa: output/5Aa_sim_90_null.RDS $(PLOT_DIR)/5Aa_simplot_90_null.png

## Method 5Ba ##
method_5Ba: prepdata_5Ba mcmc_5Ba sim_5Ba
prepdata_5Ba: data/infection_data.RDS data/5Ba_NF_ts_data_for_analysis.RDS data/5Ba_ts_data_for_analysis.RDS
mcmc_5Ba: prepdata_5Ba output/5Ba_posterior_90_null.RData $(PLOT_DIR)/5Ba_convergence_plot.png
sim_5Ba: output/5Ba_sim_90_null.RDS $(PLOT_DIR)/5Ba_simplot_90_null.png
#pscale_analysis_5a: output/5Ba_sim_90_null.RDS output/5a_pscale_analysis.RDS 
output/5a_pscale_analysis.RDS: code/pscale_analysis.R output/5Ba_sim_90_null.RDS data/5Ba_ts_data_for_analysis.RDS config_general.json pscale.txt 
	${R} 

method_5b: method_5Ab method_5Bb

## Method 5Ab ##
method_5Ab: prepdata_5Ab mcmc_5Ab sim_5Ab
prepdata_5Ab: data/infection_data.RDS data/5Ab_NF_ts_data_for_analysis.RDS data/5Ab_ts_data_for_analysis.RDS
mcmc_5Ab: prepdata_5Ab output/5Ab_posterior_90_null.RData $(PLOT_DIR)/5Ab_convergence_plot.png
sim_5Ab: output/5Ab_sim_90_null.RDS $(PLOT_DIR)/5Ab_simplot_90_null.png

## Method 5Bb ##
method_5Bb: prepdata_5Bb mcmc_5Bb sim_5Bb
prepdata_5Bb: data/infection_data.RDS data/5Bb_NF_ts_data_for_analysis.RDS data/5Bb_ts_data_for_analysis.RDS
mcmc_5Bb: prepdata_5Bb output/5Bb_posterior_90_null.RData $(PLOT_DIR)/5Bb_convergence_plot.png
sim_5Bb: output/5Bb_sim_90_null.RDS $(PLOT_DIR)/5Bb_simplot_90_null.png
#pscale_analysis_5b: output/5Bb_sim_90_null.RDS output/5b_pscale_analysis.RDS 
output/5b_pscale_analysis.RDS: code/pscale_analysis.R output/5Bb_sim_90_null.RDS data/5Bb_ts_data_for_analysis.RDS config_general.json pscale.txt 
	${R} 

method_5c: method_5Ac method_5Bc

## Method 5Ac ##
method_5Ac: prepdata_5Ac mcmc_5Ac sim_5Ac
prepdata_5Ac: data/infection_data.RDS data/5Ac_NF_ts_data_for_analysis.RDS data/5Ac_ts_data_for_analysis.RDS
mcmc_5Ac: prepdata_5Ac output/5Ac_posterior_90_null.RData $(PLOT_DIR)/5Ac_convergence_plot.png
sim_5Ac: output/5Ac_sim_90_null.RDS $(PLOT_DIR)/5Ac_simplot_90_null.png

## Method 5Bc ##
method_5Bc: prepdata_5Bc mcmc_5Bc sim_5Bc
prepdata_5Bc: data/infection_data.RDS data/5Bc_NF_ts_data_for_analysis.RDS data/5Bc_ts_data_for_analysis.RDS
mcmc_5Bc: prepdata_5Bc output/5Bc_posterior_90_null.RData $(PLOT_DIR)/5Bc_convergence_plot.png
sim_5Bc: output/5Bc_sim_90_null.RDS $(PLOT_DIR)/5Bc_simplot_90_null.png
#pscale_analysis_5c: output/5Bc_sim_90_null.RDS output/5c_pscale_analysis.RDS 
output/5c_pscale_analysis.RDS: code/pscale_analysis.R output/5Bc_sim_90_null.RDS data/5Bc_ts_data_for_analysis.RDS config_general.json pscale.txt 
	${R} 


##### RUN LINES #######
##prepare data##
data/%_NF_ts_data_for_analysis.RDS: code/B_%.R data/infection_data.RDS parameters.json config_general.json pscale.txt
	${R}
	
data/%_ts_data_for_analysis.RDS: code/C_adapt_ts_data.R data/%_NF_ts_data_for_analysis.RDS parameters.json config_general.json 
	${R} 
	
##run mcmc##
output/%_posterior_90_null.RData: code/D_run_mcmc.R data/%_ts_data_for_analysis.RDS $(MCMC_FUNC) $(FIT_FUNC) config_general.json 
	${R}

$(PLOT_DIR)/%_convergence_plot.png: code/convergence_plot.R gap_file.txt output/%_posterior_90_null.RData config_general.json
	${R}
	
##run simulations##
output/%_sim_90_null.RDS: code/E_sim_null.R output/%_posterior_90_null.RData gap_file.txt data/%_ts_data_for_analysis.RDS $(FIT_FUNC) config_general.json 
	${R}

$(PLOT_DIR)/%_simplot_90_null.png: code/sim_plot.R gap_file.txt output/%_sim_90_null.RDS data/%_ts_data_for_analysis.RDS config_general.json $(PLOT_FUNC)
	${R}

## pscale analysis ##
output/%_pscale_analysis.RDS: code/pscale_analysis.R output/%B_sim_90_null.RDS data/%B_ts_data_for_analysis.RDS config_general.json pscale.txt 
	${R}
