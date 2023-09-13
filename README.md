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

### Pipeline files
These files located are in the main directory: 
-   `Makefile`: full pipeline via GNU Make (requires use of Unix-like command line); see https://www.gnu.org/software/make/ for more information
-   `config_general.json.example`:  configuration file used for manuscript preparation when fitting third infections and not adding an fitting a reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._
-   `config_general_l2.json.example`: configuration file used for manuscript preparation when fitting third infections and adding an fitting reinfection hazard coefficient. _copy this file and rename it as 'config_general.json' to use with the model._

        
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


