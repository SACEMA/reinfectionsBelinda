# reinfectionsBelinda
Repository for Belinda's MSc work on for simulation-based validation on a catalytic model by Pulliam et al. 

The work in this repositoy consists of two parts:
1. Simluation-based validation on the catalytic model
2. An pipeline to run the extended model to detect increases in the risk of an n<sup>th</sup> infection. 

## Simulation-based validation for reinfections
Code to run the simulation-based validation presented in *** are provided. 

### Setup
-    R Packages must be installed:
     -  `data.table`, `iterators`, `foreach`, `doParallel`, `coda`, `parallel`, `dplyr`, `ggplot2`
-  _(Optional: this is for if you want to run the simulation-based validation for high computing)_ 
 CHPC cluster or a cluster that you can run PBS jobs on. The cluster should be able to run R scripts and also have the following packages installed and available:
    -   `data.table`, `iterators`, `Rmpi`, `doMPI`, `foreach`, `doParallel`, `coda`, `parallel`, `dplyr`, `ggplot2`, `jsonlite`


## Run a method analysis


### Input
