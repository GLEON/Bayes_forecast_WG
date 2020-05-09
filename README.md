# Bayes_forecast_WG
A working group using Bayesian techniques to develop ecological forecasts

This branch has been tagged as a code publication for Lofton et al. 2020,
"Process error dominates near-term forecast uncertainty of oligotrophic lake cyanobacterial density"

Guide to file structure:
(see readmes within each folder for further details)

00_Data_files: contains analysis-ready formatted data files derived from data publications housed at the Environmental Data Initiative (EDI)

0_Function_library: contains Rscripts of functions that will be sourced during subsequent modeling and analysis Rscripts

1_Get_data: contains Rscripts to pull published data from EDI and format for covariate correlation analysis; formatted data are written to 00_Data_files

2_Covariate_correlation_analysis: contains Rscript and output from initial covariate correlation analysis to determine which drivers of G. echinulata density to include in Bayesian state-space models

3_Format_data_for_Bayes_models: Rscript to pull formatted data files from 00_Data_files and re-format data for input into Bayesian state-space models; re-formatted files are written to 00_Data_files

4.1_JAGS_models: contains scripts for all Bayesian state-space models tested during analysis

4.2_Calibrate_Bayesian_models: contains Rscript to calibrate all Bayesian state-space models using G. echinulata and driver data from 2009-2014

4.3_Hindcasting: contains Rscript to run G. echinulata hindcasts for all Bayesian state-space models from 2015-2016 using hindcasted driver data

5_Model_output: contains model output from both the calibration and hindcasting phases of analysis

6_Output_analysis: contains Rscripts to analyze model output from both the calibration and hindcasting phases of analysis

7_Model ensemble: contains Rscript and output files for an unweighted model ensemble hindcasting exercise based on hindcast output from individual Bayesian state-space models

8_Visualization: contains Rscripts to generate output visualization and manuscript figures from both the calibration and hindcasting phases of analysis

9_Manuscript: contains pdf file of manuscript, including tables, figures, and supplemental materials