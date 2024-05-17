This file contains detailed information about the content of each script.

In order to reproduce the results, one should need to run, in the following folder: 1) the script 'run-simulation/05-exec.simulation.R; 2) all the scripts in the simulation-post-processing folder, in the orded of the numbers (01 first and 02 second); 3) all scripts in the analysis-and-figures folder (the order in which the script within this folder are run is irrelevant and will lead to the exact same results)

Reproducibility of all results is ensured by using seeds (including one seed at each iteration (which is itself rando, based on a global seed set in 'run-simulation/05-exec-simulation.R), and one random seed for each bayesian model)

## 'run-simulation'. 

Contains code required to rerun the simulation. Running the script '05.exec-simulation.R' is sufficient to start the simulation, provided you adjust the cluster configuration to your PC's configuration. An explanation of how to do this is included in the script itself. 

The idea of the script is that we create a grid, where each row corresponds to one iteration and the value of the columns determine the simulation conditions for that row. Then, when we run the simulation: 1) data is estimate according to the function in '01.gen-dat-function.R'; 2) models are estimated on that data using '0.2.estimation-functions.R', and managing errors thorugh '03-error-handlers.R'; 3) storing one file per iteration with model results and other information of interest, as determined in '0.4-sim-functions.R'.

Concretely, the folder contains the following subfolders:
  - 0.1.gen-dat-function.R: It includes one function to generate the data according to the parameters that define each simulation condition.
  - 0.2.estimation-functions.R: Includes two functions to estimated models for data created using the data-generating function. The first function returns model results of the Standard SCM using the R package Synth. The second function returns model results of both Bayesian SCM, using the package cmdstanR (which internally calls Stan).
  - 0.3.error-handlers.R: Includes three functions, used to handle error from estimating the Standard SCM, either of the Bayes SCM, or in generating the data or any other funcion (results_error_handler.R). The main goal of these functions is to prevent the simulation from crashing down in the event of an error, not to store specific informative error messages. They are to be used in combination with tryCatch.
  - 0.4.sim-functions.R: It incldudes one function used to run the simulation. This function is to be used in combination with a grid of all iterations, described in the next point. The function inputs a row number and then retrieves the simulation conditions in the grid from that row_number, estimates each of the models, and saves all relevant results (messages, digagnostics, conterfactual, conterfactual estimates, weights, and MAP estimates). Then, it saves the results of each model in a folder (with folder name specified as an argument). It saves one file per model. It estimates all the bayesian models specificated under 'models'.
  - 0.5.exec-simulation.R: Running this script will start the simulation. First, it sources all previous 4 scripts, load required libraries, and set a seed. Then, it creates a grid of simulation condition. Each row in that grid corresponds to one iteration, and the columns indicate the parameters for each iteration - for parameters defined in the main text. It also includes the creation of random seeds for each iteration. In that way, we ensure that we can reproduce every individual iteration without the need to first reproduce all the previous iterations. Seeds are also included to estimate the Bayesian models.

## 'simulation-post-processing'

It includes two scripts, designed to input each of the unstructured individual files (one per iteration), and convert them into 7 stuctured dataframes that contain relevant results for all conditions. Contains the following scripts:

   - 0.1.process-file-function.R: Contains a function that takes each of the created files and stores all the relevant information, in a structured format. Depending on the output specified as an argument, it returns: the simulation conditions for that file, the weights, the counterfactual estimates, the value of the covariates, and the message and diagnostics. 

   - 0.2.create-processed-files.R: Creates the structured dataframes detailed below.
	- Conditions.Rds: Contains information about the simulation conditions for each iteration and model file. Each row corresponds to one iteration, and each column is the value of the simulation condition.
	- Covariates.Rds: Contains information about the covariates values for each iteration and model file.
Each row corresponds to one iteration, and each volumn represents covariate values.
	- Effects.Rds: Each row corresponds to one time point. Colums correspond to the different true and estimated counterfactuals, including credible intervals for Bayesian models, as well as mean and median point estimates.
	- mess_and_diag.Rds: Contains the value of messages and diagnostics for each iteration.
It is a list, where each element corresponds to one iteration, and the elements inside it are every message and diagnostic returned for that iteration.
	- possibly-synth.R: Contains information about for which iterations a convex combination of untreated units can reproduce the treated unit, i.e., for which iterations a synthetic control is possible. Each row is one iteration, and the columns indicate whether a simulation value was possible or not.
	- pretreatment_outcomes.Rds: Contains information about the pretreatment outcomes in each condition.
Each row corresponds to one preintervention time-point.
	- weights.Rds: Contains information about the unit weights in each condition. Each row correspond to a unit, and each column corresponds to the estimate according to different bayesian models (including credible intervals and different point estimates), or the Standard SCM.
All files contain a column 'file_name', used to refer to the file_name where the information was retrieved from, and used to merge different dataframes. The files for which there was an error are excluded.

## 'stan'

Contains two stan files corresponding to each Bayesian model. 'horseshoe.stan' is the Horseshoe Priors BSCM. 'horseshoe_simplex.stan' is the Horseshoe Simplex Priors BSCM. Each file has a also the compiled application version created by Stan to estimate the model.

## analysis-and-figures

This folder contains scripts to analyze the simulation results. All the analysis are based on one of the data folders in sim-output/processed_results. The scripts are the following.

  -0.1. load-data.R. An script that loads all the dataframes into sim-putput/processed_results into R (to simplify other scripts, anda avoid repetitions)
  -0.2. analysis-functions.R. Includes three function. One function computes the differences between observed and estimated counterfactuals. This is later used to compute bias and RMSE for different conditions. The other two functions are helpers to label the grids or titles of ggplot2 objects, for each condition
  -0.3.count-number-possible-solutions.R: Short script that counts the sample size of each simulation condition
  -0.4.abadie_et_al_not_replicable.qmd. This .qmd document documents our failure to replicate the results in Abadie et al using the package Synth. It displays how the results are based on a non-convergent result.
The subfolder 'figures' includes all scripts that create figures:
  - 01.plot-bias-rmse-twoconditions.R: includes the R code needed to reproduce figure 3 and 4.
  - 02.plots-bias-rmse-allconditions.R: includes the R code needed to reproduce figures 3 and 4, in the form of a facet, for all conditions.
  - 03.replicate_individuals_iterations.R: includes the R code needed to reproduce figures 2 and 3 of the manuscript
  - 04. replicate_california_example.R: includes the R code needed to reproduce figures 7, 8, 9 and 10 of the manuscript
  - 0.5.plot-weights-distribution.R: includes the R code needed to reproduce figure 11 of the manuscript, as well as to reproduce that figure across all conditions
  - 0.6.plot-coverage: include the R code needed to estimate and plot coverage across all conditions.
  - 0.7.plot-figure1: includes the R code needed to reproduce figure1
  - 0.8.plots-weights-one-iteration.R: includes the R code needed to reproduce figure 6
  
## 
