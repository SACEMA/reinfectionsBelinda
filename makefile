

R = Rscript $^ $@
UTILS_SCRIPTS = code/utils_generation
INFECTIONS = 2

#create utils 
mkdir utils

create_utils: 

utils/

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
