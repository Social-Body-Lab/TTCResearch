This repository contains all the scripts and data needed to reproduce the analyses presented in the manuscript. 

To replicate the analyses, please follow these steps:

  1. First, use RStudio to run the `ten_tors_CFA_PCA_analyses.R` script from the `analysis_code` directory. This script requires the `TTC_data_anonymised.csv` dataset and uses confirmatory factor analyses (CFA) and principal component analyses (PCA) to create the outcome variables used in the final analysis. This script outputs a dataset, `TTC_data_CFA_PCA_variables.csv`, which contains the newly created factors and components, as well as the raw variables that make them up. 

  2. Second, again using RStudio, run the `ten_tors_main_analyses.R` script from the `analysis_code` directory. This script requires the `TTC_data_CFA_PCA_variables.csv` dataset created in Step 1. The script outputs all of the statistics and plots presented in the manuscript and supplementary online materials (SOM), as well as the assumption checks for each model with statistically significant findings. Plots from the manuscript and SOM are outputted to the `../outputs/main_analysis_plots/` directory, and assumption checks (including assumption check plots) are outputted to the `../outputs/assumption_checks/` directory (each assumption check directory corresponds to the name of the model in the `ten_tors_main_analyses.R` script that it is checking). To produce (in Microsoft Word) the model summary tables presented in the SOM, uncomment the lines using the `make_MS_Word_model_summary_table` function. 

The `analysis_code` directory should be in the same directory as the `data` directory and an empty version of the `outputs` directory (the `outputs` directory currently contains the outputs generated by the `ten_tors_main_analyses.R` script - running the `ten_tors_main_analyses.R` script will reproduce these outputs). 
