# Bayes_forecast_WG
A working group using Bayesian techniques to develop ecological forecasts

This branch has been tagged as a code publication for Lofton et al. 20XX,
"Using near-term forecasts and uncertainty partitioning to improve predictions of low-frequency cyanobacterial events"

# Instructions to run analysis:
1. Download or clone the Github repository to your local machine.
2. Run all Rscripts in the 1_Get_data folder to access all data associated with the analysis, published in the Environmental Data Initiative repository.
3. Sequentially run all Rscripts in the following folders (2,3,4,etc.). A guide to folders is provided below.

Note that for some scripts, particularly those for data visualization, the user will have to set a local working directory for plot output.
This is generally indicated at the top of the relevant scripts.

# Guide to file structure:
(see readmes within each folder for further details)

00_Data_files: folder that will be populated with analysis-ready formatted data files derived from data publications housed at the Environmental Data Initiative (EDI) when the user runs scripts in 1_Get_data

0_Function_library: contains Rscripts of functions that will be sourced during subsequent modeling and analysis Rscripts

1_Get_data: contains Rscripts to pull published data from EDI and format for covariate correlation analysis; formatted data are written to 00_Data_files

2_Covariate_correlation_analysis: contains Rscript and output from initial covariate correlation analysis to determine which drivers of G. echinulata density to include in Bayesian state-space models

3_Format_data_for_Bayes_models: Rscript to pull formatted data files from 00_Data_files and re-format data for input into Bayesian state-space models; re-formatted files are written to 00_Data_files

4.1_JAGS_models: contains scripts for all Bayesian state-space models tested during analysis

4.2_Calibrate_Bayesian_models: contains Rscript to calibrate all Bayesian state-space models using G. echinulata and driver data from 2009-2014

4.3_Hindcasting: contains Rscript to run G. echinulata hindcasts for all Bayesian state-space models from 2015-2016 using hindcasted driver data

4.4_Uncertainty_partitioning: contains Rscript to run uncertainty partitioning for all G. echinulata hindcasts from all models

5_Model_output: folder will be populated with model output from both the calibration and hindcasting phases of analysis as user runs scripts in 4.2_Calibrate_Bayesian_models, 4.3_Hindcasting, and 4.4_Uncertainty_partitioning

6_Output_analysis: contains Rscripts to analyze model output (in 5_Model_output) from both the calibration and hindcasting phases of analysis

7_Model ensemble: contains Rscript for an unweighted model ensemble hindcasting exercise based on hindcast output from individual Bayesian state-space models

8_Visualization: contains Rscripts to generate output visualization and manuscript figures from both the calibration and hindcasting phases of analysis

9_Manuscript: contains pdf file of manuscript, including tables, figures, and supplemental materials
