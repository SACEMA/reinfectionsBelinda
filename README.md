# reinfectionsBelinda
Repository for Belinda's MSc work on for simulation-based validation on a catalytic model by Pulliam et al. 

The work in this repositoy consists of two parts:
1.  A pipeline to run the extended model to detect increases in the risk of an n<sup>th</sup> infection. 
2.  Simluation-based validation on the catalytic model

If you have questions or comments, please contact the repository maintainer, Belinda Lombard, at belindalombard@sun.ac.za.

## Software requirements 
-    R - a statistical programming language
-    R Studio (recommended)- a user interface for R

R packages required are indicated seperately in the two sections

## General pipeline files
The following files are applicable to the entire project and contained in the main directory: 
-   LICENSE - license information
-   README.md - this current file.
-   .gitignore - files and file types that the version control should ignore. 

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
-   `Makefile`: full pipeline via GNU Make (requires use of Unix-like command line); see https://www.gnu.org/software/make/ for more information
-   `config_general.json.example`:  configuration file used for manuscript preparation when fitting third infections and not adding an fitting a reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._
-   `config_general_l2.json.example`: configuration file used for manuscript preparation when fitting third infections and adding an fitting reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._

### Data files
The below file is in the `data` directory: 
-   `ts_data.csv` - national daily time series of newly detected putative primary infections (cnt), suspected second infections (reinf), suspected third infections (third), and suspected fourth infections (fourth) by specimen receipt date (date). This dataset was obtained from https://zenodo.org/record/7426515.
Scripts that prepare the data for the model will place the outputs in this directory.

### Code files
#### Data preparation scripts
The following file can be found in `code/run` is used to prepare the `ts_data.csv` file and save an output RDS file for further processing in the `data` directory:
-  `1_prep_ts_data` - creates an RDS file with time series data (used in analysis / plotting scripts)

The output is ignored by the version control system (with the `.gitignore` file). 

#### Utility scripts
The following files are in the `code/utils_generation` directory and saves necessary functions for the model to run inside the `utils` directory:
-   `wave_defs.R`: utility functions for defining wave periods
-   `fit_functions.R`:  utility functions for likelihood calculations (to calculate the expected n<sup>th</sup> infections & the associated likelihoods)
-   `mcmc_general.R`: creates the functions that will run during the fitting period (MCMC Sampler and the associated functions). This is for when only Lambda and Kappa are fitted.
-    `mcmc_general_l2.R`: creates the functions that will run during the fitting period (MCMC Sampler and the associated functions). This is for when Lambda and Kappa and an additional Lambda parameter, Lambda2 are fitted.
-    `plotting_fxns.R`: creates the functions needed to create associated plots

 The `utils` directory is ignored by the version control system (with the `.gitignore` file). 

#### Analysis scripts
Before running the analysis, the number of reinfection that needs to applied to the model needs to be specified in the `config_general.json` file by changing the `infection` field. For second infections, use "second". For third infections, use "third". For infections above three infections, use the accociated number (e.g. fourth infections are 4). \

The following scripts that can be found in `code/run` should be run in the order specified below (after running the data preperation files & the utils generation files): 
1. `2_run_mcmc.R` OR `2_run_mcmc_without_lambda2.R`: This will implement the MCMC fitting.
    - `2_run_mcmc`: file for fitting Kappa, Lambda and Lambda2.
    - `2_run_mcmc_without_lambda2.R`: file for fitting Kappa, Lambda.
1. `3_sim_null.R` OR `3_sim_null_without_lambda2.R`: simulates projections from the null model using a simplified simulation approach
     - `3_sim_null.R`: file for simulation when Kappa and Lambda and Lambda2 were fitted.
     - `3_sim_null_without_lambda2.R`: file for simulation when Kappa and Lambda were fitted.

Visualisation scripts: 
1. `convergence_plot.R` OR `convergence_plot_without_lambda2.R`: creates plot of convergence diagnostics using output of the MCMC fitting procedure.
     - `convergence_plot.R`: file for plot when Kappa and Lambda and Lambda2 were fitted. This was used in the paper to create *** when using the file `output/3_posterior_90_null_correctdata.RData` to create the plots. 
     - `convergence_plot_without_lambda2.R`: file for plot when Kappa and Lambda were fitted. This was used in the paper to create *** when using the file `output/3_posterior_90_null_correctdata.RData` to create the plots. This was used in the paper to create *** when using the file `output/3_posterior_90_null_l2_correctdata.RData` 
1. `3_sim_plot.R`: creates plot of observed data with model fits and projections.
1. `ts_plot_third.R`: creates plot to show timeseries of observed first, second and third infections in addition to the number of people eligible for second & third infections.
1. `third_infections_plot_paper`: creates simulations shown in paper for when lambda2 was not fitted and when lambda2 was fitted in addition to lambda and kappa for third infections. The data here are output from the MCMC fitting: `output/3_sim_90_null_correctdata.RDS` and `3_sim_90_null_l2_correctdata.RDS`. 

## Simulation-based validation for reinfections
Code to run the simulation-based validation presented in *** are provided. 

### Software requirements
-    R Packages must be installed (versions indicates the versions used for manuscript preperation):
     -  `data.table`: 
     -  `iterators`: 
     -  `foreach`: 
     -  `doParallel`: 
     -  `coda`: 
     -  `parallel`: 
     -  `dplyr`: 
     -  `ggplot2: 
-  _(Optional: this is for if you want to run the simulation-based validation for high computing)_ 
 CHPC cluster or a cluster that you can run PBS jobs on. The cluster should be able to run R scripts and also have the following packages installed and available (versions indicate versions used on cluster for manuscript preperation):
    -   `data.table`:
    -   `iterators`:
    -   `mpi`: 
    -   `doMPI`: 
    -   `foreach`: 
    -   `doParallel`: 
    -   `coda`: 
    -   `parallel`: 
    -   `dplyr`: 
    -   `ggplot2`: 
    -   `jsonlite`: 


