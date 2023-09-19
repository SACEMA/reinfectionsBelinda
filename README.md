# A catalytic model for SARS-CoV-2 reinfections: Performing simulation-based validation and extending the model to include nth infections

Repository for Belinda's MSc work on for simulation-based validation on a catalytic model by Pulliam et al. 

The work in this repositoy consists of two parts:
1.  A pipeline to run the extended model to detect increases in the risk of an n<sup>th</sup> infection. 
2.  Simluation-based validation on the catalytic model

If you have questions or comments, please contact the repository maintainer, Belinda Lombard, at belindalombard@gmail.com.

## Software requirements 
-    R - a statistical programming language
-    R Studio (recommended)- a user interface for R

R packages required are indicated seperately in the two sections

## General pipeline files
The following files are applicable to the entire project and contained in the main directory: 
-   LICENSE - license information
-   README.md - this current file.
-   .gitignore - files and file types that the version control should ignore.
-   `makefile`: full pipeline via GNU Make (requires use of Unix-like command line); see https://www.gnu.org/software/make/ for more information

## Utils generation files
The following files are applicable to the entire project (both the extended model & simulation-based validation). It can be found in the `code/utils_generation` directory. 
-   `wave_defs.R`: utility functions for defining wave periods in the South African data
-   `fit_functions.R`:  utility functions for likelihood calculations (to calculate the expected n<sup>th</sup> infections & the associated likelihoods)
-   `mcmc_general.R`: creates the functions that will run during the fitting period (MCMC Sampler and the associated functions). This is for when only Lambda and Kappa are fitted. In addition to the extended method, this is used in the simulation-based validation for reinfections. 
-    `mcmc_general_l2.R`: creates the functions that will run during the fitting period (MCMC Sampler and the associated functions). This is for when Lambda and Kappa and an additional Lambda parameter, Lambda2 are fitted. In addition to the extended method, this is used in the simulation-based validation for reinfections. 
-    `plotting_fxns.R`: creates the functions needed to create associated plots

 The `utils` directory is ignored by the version control system (with the `.gitignore` file). 


## Extended model 
### Software requirements (for extended model)
The following R packages are required to run the extended model (indicated versions were used during manuscript preperation): 
-    jsonlite: v1.8.5
-    data.table: v1.14.8
-    english: v1.2.6
-    coda: v0.19.4
-    parallel: v4.3.1
-    dplyr: v1.1.2
-    ggplot2: v3.4.2
-    gridExtra: v2.3
-    patchwork: v1.1.2

The file `install.R` which is available in `code/helpers` will install the necessary packages. 

### Pipeline files
These files located are in the main directory: 
-   `config_general.json.example`:  configuration file used for manuscript preparation when fitting third infections and not adding an fitting a reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._
-   `config_general_l2.json.example`: configuration file used for manuscript preparation when fitting third infections and adding an fitting reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._

In `config_general.json`, the "omicron_date" refers to the date from which lambda2 should be fitted. 

### Data files
NB: Create a data directory
The below file is in available from https://zenodo.org/record/7426515 and should be placed in the `data` directory
-   `ts_data.csv` - national daily time series of newly detected putative primary infections (cnt), suspected second infections (reinf), suspected third infections (third), and suspected fourth infections (fourth) by specimen receipt date (date). This dataset was obtained from https://zenodo.org/record/7426515.
Scripts that prepare the data for the model will place the outputs in this directory.

### Code files
#### Data preparation scripts
The following file can be found in `code/run` is used to prepare the `ts_data.csv` file and save an output RDS file for further processing in the `data` directory:
-  `1_prep_ts_data.R` - creates an RDS file with time series data (used in analysis / plotting scripts)

The output is ignored by the version control system (with the `.gitignore` file). 

#### Analysis scripts
Before running the analysis, the number of reinfection that needs to applied to the model needs to be specified in the `config_general.json` file by changing the `infection` field. For second infections, use "second". For third infections, use "third". For infections above three infections, use the accociated number (e.g. fourth infections are 4). 

The following scripts that can be found in `code/run` should be run in the order specified below (after running the data preperation files & the utils generation files): 
1. `2_run_mcmc.R` OR `2_run_mcmc_without_lambda2.R`: This will implement the MCMC fitting.
    - `2_run_mcmc`: file for fitting Kappa, Lambda and Lambda2.
    - `2_run_mcmc_without_lambda2.R`: file for fitting Kappa, Lambda.
1. `3_sim_null.R` OR `3_sim_null_without_lambda2.R`: simulates projections from the null model using a simplified simulation approach
     - `3_sim_null.R`: file for simulation when Kappa and Lambda and Lambda2 were fitted.
     - `3_sim_null_without_lambda2.R`: file for simulation when Kappa and Lambda were fitted.

Visualisation scripts: 
1. `convergence_plot.R` OR `convergence_plot_without_lambda2.R`: creates plot of convergence diagnostics using output of the MCMC fitting procedure.
     - `convergence_plot.R`: file for plot when Kappa and Lambda and Lambda2 were fitted. This was used in the thesis Chapter 3 using the file `output/3_posterior_90_null_correctdata.RData` to create the plots. 
     - `convergence_plot_without_lambda2.R`: file for plot when Kappa and Lambda were fitted. This was used in the thesis Chapter 3 using the file `output/3_posterior_90_null_correctdata.RData` to create the plots.
1. `3_sim_plot.R`: creates plot of observed data with model fits and projections.
1. `ts_plot_third.R`: creates plot to show timeseries of observed first, second and third infections in addition to the number of people eligible for second & third infections.
1. `third_infections_plot_paper`: creates simulations shown in paper for when lambda2 was not fitted and when lambda2 was fitted in addition to lambda and kappa for third infections. The data here are output from the MCMC fitting: `output/3_sim_90_null_correctdata.RDS` and `3_sim_90_null_l2_correctdata.RDS`. 

#### Output files 
The output files below are saved in `output`. n denotes the infection number being applied to the model (e.g. if n=3, the output file will be `3_posterior_90_null.RData` and third infections are being projected. 
 -   `%n_posterior_90_null.RData` - posterior samples from the MCMC fitting procedure
 -   `%n_sim_90_null.RDS` - simulation results

The following files were used in the paper and can be found on https://zenodo.org/record/8354838. They should be placed in the output directory if the scripts will be used for plot creation.  
 -   `3_sim_90_null_l2_correctdata.RDS`
 -   `3_sim_90_null_correctdata.RDS`
 -   `3_posterior_90_null_l2_correctdata.RData`
 -   `3_posterior_90_null_correctdata.RData`

### Pipeline with the makefile
The provided `makefile` can be used to do the above pipeline (`config_general.json` must exist by copying one of the example files)
The makefile will: 
1. Run the package install script
1. Create the utilities with the utils_generation scripts
1. Prepare the data in `data/ts_data.csv`
1. Run the MCMC fitting procedure
1. Run the simulations
1. Create a convergence plot & simulation plot. 

To run the makefile pipeline, the following command can be used: 
- `make infections=%i L2=%l `: This will run the pipeline for fitting lambda, lambda2 and kappa. 

The infections and L2 settings flags are optional. `infections` is number of infections that should be applied to the model (if not provided, it will default to 2) and `L2` is a TRUE or FALSE flag that specifies whether an increasing reinfection risk parameter should be fitted to the data after Omicron date specified in the JSON file (if not provided, it will default to FALSE) 

## Simulation-based validation for reinfections
Code to run the simulation-based validation presented in Chapter are provided in this section. 

### Software requirements
R Packages must be installed (versions indicates the versions used for manuscript preperation):
-    jsonlite: v1.8.5
-    data.table: v1.14.8
-    english: v1.2.6
-    coda: v0.19.4
-    parallel: v4.3.1
-    dplyr: v1.1.2
-    ggplot2: v3.4.2
-    gridExtra: v2.3
-    patchwork: v1.1.2

_(Optional: this is for if you want to run the simulation-based validation for high computing)_ 
 CHPC cluster or a cluster that you can run PBS jobs on. The cluster should be able to run R scripts and also have the following packages installed and available:
-   data.table
-   iterators
-   mpi
-   doMPI
-   foreach
-   doParallel
-   coda
-   parallel
-   dplyr
-   ggplot2
-   jsonlite

### Data files
The below file can be placed in the data directory. It is available in https://zenodo.org/record/8354838
-   `inf_for_sbv.RDS` - This contains an RDS file with a timeseries of simulated primary infections used for input in the simulation-based validation for reinfections. 

### File system setup
For each data-generation scenario described in the paper, a directory exists in `sbv` names `method_%_analysis` where % corresponds to the respective scenario. 

### Additional utils
The following utils are generated in additional to the utils being generated for the overall project: 
- Parameter generation files: the files to generate the parameter combinations for each scenario can be found in `sbv/paramameter_generation/` where each scenario from 1 to 5 has a file `create_parameter_files_m%.R` associated with it.
    - The output of this is saved in `sbv/method_%_analysis/parameters.RData` and 
 `sbv/method_%_analysis/parameters_pscale1.RData`.
- `code/utils_generation/combine_file_methods.R`: This creates methods that are used when combining the raw results after the simulation-based validation.
    - The output of this is saved in `utils/cleanup_methods.RData`
- `code/utils_generation/observe_prob_functions.R`: This creates the functions used when generating the data for Scenario E where observation probability is varied. 
    - The output of this is saved in `utils/observe_prob_functions.RData`
- `code/utils_generation/generate_data.R`: This creates the functions used to simulate infections for each scenario using the provided timeseries of primary infections. 
    - The output of this is saved in `utils/generate_data.RData`
- `code/utils_generation/settings.R`: This provides a list of everything that must be loaded for the SBV file to run (which get loaded with the `lapply` function)
  
Note: running `make sbv` OR `make l2_sbv` will create all the necessary utils & parameter files for this scenario

### Configuration
For each Scenario 1 to 5, an example configuration file is provided as it was run in the simulation-based validation study (`sbv/method_%_analysis/m%_config_general.json`). To run the analysis, this must be copied and the '.example' must be removed from the filename. 

### Running pipeline
After creating the utils and the parameter files (this can be done with `make utils_sbv`) and copying the configuration JSON file, the pipeline can be run by running the file: 

`sbv/method_%_analysis/method_%_arrayjob.R`

Running this will complete the entire process for the respective seed and parameter combination
- By default, the first parameter combination in the `parameters.RData`. 
- The seed is specified in the method's config file (`sbv/method_%_analysis/m%_config_general.json`)
- Alternatively, you can the script with arguments
   `Rscript sbv/method_%_analysis/method_%_arrayjob.R {i} {seed}` which will override the set seed and i arguments.

The output was saved in: `sbv/raw_output/m%/{SEED}` as RDS files for each value of i. 

**Note**: The raw results were processed with the functions `combineRawResults` & `combineRawResultsOld` and saved as a dataframe for each frame in `sbv/method_%_analysis/output/final_output_data` for further analysis. 

**Note:** In the simulation-based validation done in the paper, files named `sbv/method_%_analysis/method_%_arrayjob_fit_through.R` were run to exclude parameter combinations according to the specified exclusion criteria. 

### Processing of results
In this repository, the results obtained in the analysis are saved and provided as CSV files in `sbv/method_%_analysis/results/` for each scenario. To use the below scripts, the CSV files must be imported as RDS files and stored in `sbv/method_%_analysis/output/final_output_data`. 

To create dataframes that can be used for further analysis: 
- `sbv/method_%_analysis/process_data_m%.R`: This will create dataframes that summarises the results obtained for each scenario. The dataframes are stored in  `sbv/method_%_analysis/output`
- Examples of outputs provided are:
     - `all_data.RDS`: This is a dataframe all the results from each seed combined in one RDS file. 
     - `all_data_excluded.RDS`: This is a dataframe all the results from each seed combined in one RDS file after excluding results according to the excluding criterion. 
     - `specificity_matrix.RDS`: This is a specificity matrix with the specificity for each parameter combination (where the scale is 1) 
     - `summarised_results.RDS`: This is the summarised results (median of clusters, etc.) after excluding results with the proportion of runs that converged. 
     - `summarised_results_all.RDS`: This is the summarised results (median of clusters, etc.) before excluding results
     - `summarised_results_med_con.RDS`: This is the summarised results (median of clusters, etc.) after excluding results with the median of the convergence.
 

The output from this can be **visualised** by running `sbv/method_%_analysis/plots_m%.R` and the respective plots are saved in `sbv/method_%_analysis/plots`.

### Additional plots and data scripts
The following R files provided in `sbv/final_plot_scripts` were used to generate plots for the paper: 
- `cluster_plot.R`: creates a plot showing the timing of cluster of 5 points above during the projection interval for the different data-generation scenarios
- `proportion_plot.R`: creates a plot showing the proportion during the projection interval for the different data-generation scenarios
- `data_plots_for_paper.R`: creates plots to show simluated data for the different scenarios.

The following data script is available in `sbv/method_%_analysis/`
- `check_results_complete_m%.R`:  used to confirm that all the parameter combines in the parameter.RData file of the method is present in a given dataframe, and if not, prints the missing runs. 

The output of this is saved in the `sbv/plots` directory and `output/paper_plots` directory. 

## Simulation-based validation for third infections
The simulation-based validation done for the third infections in Chapter 3 is very similar to the simulation based validation for reinfections in the previous section.

### Data files
The below file can be placed in the data directory. It is available in https://zenodo.org/record/8354838
-   `inf_for_sbv_third.RDS` - This contains an RDS file with a timeseries of simulated primary infections used for input in the simulation-based validation for third infections. 

### Additional utils
The following utils are generated in additional to the utils being generated for the overall project: 
- Parameter generation files: the file to generate the parameter combinations can be found in `sbv/paramameter_generation/create_parameter_files_third_infections.R`
    - The output of this is saved as
         - `sbv/third infections/parameters.RData` and 
         - `sbv/third infections/parameters_increase.RData`.
- (similar to reinfections) `code/utils_generation/combine_file_methods.R`: This creates methods that are used when combining the raw results after the simulation-based validation.
    - The output of this is saved in `utils/cleanup_methods.RData`
- `code/utils_generation/generate_data.R`: This creates the functions used to simulate infections for each scenario using the provided timeseries of primary infections. 
    - The output of this is saved in `utils/generate_data.RData`
    - In the third infections, the function `generate_data_third_increase` `and generate_data_third` are used to create the simulated third infection data. 
- (similar to reinfections) `code/utils_generation/settings.R`: This provides a list of everything that must be loaded for the SBV file to run (which get loaded with the `lapply` function)

Note: running `make sbv` OR `make l2_sbv` will create all the necessary utils & parameter files for this scenario

### Configuration
An example configuration file is provided as it was run in the simulation-based validation study (`sbv/third_infections/third_array_job.pbs.example`). To run the analysis, this must be copied and the '.example' must be removed from the filename. 

Important fields specified in this JSON file (in addition to the reinfections simulation-based validation): 
- `wave_split`: The date on which the first scale value gets introduced in the third infection hazard coefficient (represents the Omicron wave)
- `wave_split_2`: The date on which an additional scale value gets introduced in the third infection hazard coefficient

### Running pipeline
After creating the utils and the parameter files (this can be done with `make make l2_sbv` which will make the relevant functions for the additional lambda parameter) and copying the configuration JSON file, the pipeline can be run by running either:  
- `sbv/third_infections/third_infections_l2.R`: This will run the parameter combinations in parameters.RData which don't have the additional increase in reinfection risk after `wave_split_2`
-  `sbv/third_infections/third_infections_l2_increase.R`: This will run the parameter combinations in `parameters_increase.RData` which  have the additional increase in reinfection risk after `wave_split_2`

Running this will complete the entire process for the respective seed and parameter combination
- By default, the first parameter combination in the `parameters.RData`. 
- The seed is specified in the method's config file (`sbv/third_infections/third_array_job.pbs`)
- Alternatively, you can the script with arguments
   `Rscript {R file_name} {i} {seed}` which will override the set seed and i arguments.

The output was saved in: `sbv/raw_output/ml2third/{SEED}` **AND** `sbv/raw_output/ml2third_increase/{SEED}` respecitvely, as RDS files for each value of i. 

**Note**: The raw results were processed with the functions `combineRawResults` & `combineRawResultsOld` and saved as a dataframe for each frame in `sbv/third_infections/output/third_l2`  **AND** `sbv/third_infections/output/third_increase_p2` respectively for further analysis. 

### Processing of results
In this repository, the results obtained in the analysis are saved and provided as CSV files in `sbv/third_infections/results/` for each scenario. To use the below scripts, the CSV files must be imported as RDS files and stored in `sbv/third_infections/output/third_l2` AND `sbv/third_infections/output/third_increase_p2` respectively. 

To create dataframes that can be used for further analysis: 
- `sbv/third_infections/process_data_third.R`: This will create dataframes that summarises the results obtained for each scenario. The dataframes are stored in  `sbv/third_infections/output`
- Examples of outputs provided are:
     - `all_data.RDS`: This is a dataframe all the results from each seed combined in one RDS file (for the fixed second scale value). 
     - `all_data_excluded.RDS`: This is a dataframe all the results from each seed combined in one RDS file after excluding results according to the excluding criterion (for the fixed second scale value).  
     - `specificity_matrix.RDS`: This is a specificity matrix with the specificity for each parameter combination (where the scales are 1). A CSV copy of this is also saved as `specificity_matrix.csv` 
     - `summarised_results.RDS`: This is the summarised results (median of clusters, etc.) after excluding results with the proportion of runs that converged (for the fixed second scale value). 
     - `summarised_results_all.RDS`: This is the summarised results (median of clusters, etc.) before excluding results
     - `all_data_inc.RDS`: This is a dataframe all the results from each seed combined in one RDS file (for the increased second scale value). 
     - `all_data_excluded_inc.RDS`: This is a dataframe all the results from each seed combined in one RDS file after excluding results according to the excluding criterion (for the fixed second scale value).  
     - `summarised_results_inc.RDS`: This is the summarised results (median of clusters, etc.) after excluding results with the proportion of runs that converged (for the fixed second scale value). 
     - `summarised_results_all_inc.RDS`: This is the summarised results (median of clusters, etc.) before excluding results

The output from this can be **visualised** by running `sbv/third_infections/third_infections_plots.R` and the respective plots are saved in `sbv/third_infections/plots`.


## Additional information about repository
### CHPC Cluster 
The simulation-based validation for this study was done using an HPC cluster. The job files for each scenario is provided as `sbv/method_%_analysis/method_%_arrayjob.pbs.example` and `sbv/third_infectionss/l2_third_array_job.pbs.example` . The job files uses array jobs to run the simulation-based validation for each seed and each value of i (specified in the array job). 

It is also possible to run the extended method on a cluster using `lambda.pbs.example` in the main directory. 

To ensure the fluent running of the scripts on the cluster, a directory "oe_files" must be created manually (for the output and error files to be saved in) 

**The authors gratefully acknowledge the Centre for High Performance Computing (CHPC), South Africa, for providing computational resources to this research project.**
