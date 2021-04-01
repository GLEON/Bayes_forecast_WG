0_Function_library

Contains helper functions that must be sourced to run analysis

Guide to functions:

create_forecast_id: creates an identifier for use in subsequent metadata each time a hindcast is generated

hindcasting_get_covar_hindcasts: generates physical covariate hindcasts for input into forecasting algorithm to produce G. echinulata hindcasts

hindcasting_get_data: pulls and formats appropriate data files for each Bayesian state-space model for hindcasts from 2015-2016

hindcasts_get_data_known_covars: pulls and formats appropriate data files for each Bayesian state-space model for hindcasts from 2015-2016 for comparative hindcasting runs where covariates are known

hindcasts_get_known_covars: formats driver data files for G. echinulata hindcasts using observed driver data to compare with hindcasts generated using hindcasted driver data

hindcasting_get_params: uses posterior output of calibrated Bayesian state-space models updated with assimilated data to generate parameter input for hindcasts containing different combinations of uncertainty sources

hindcasting_run_hindcast: runs a hindcast of G. echinulata from 1-4 weeks into the future for any Bayesian state-space model

hindcasting_run_hindcast_known_covars: runs a hindcast of G. echinulata from 1-4 weeks into the future for any Bayesian state-space model using observed covariates for comparison with hindcasts generated using hindcasted covariates

metadata_functions: generates NetCDF metadata and an EML file for each NetCDF hindcast file for publication

model_calibration_get_data: pulls and formats appropriate data files for each Bayesian state-space model for calibration from 2009-2014

model_calibration_plots: plotting code for Bayesian state-space model calibration visualization 

model_calibration_plug_n_play: assembles JAGS input for each Bayesian state-space model

model_hindcasting_plug_n_play: assembles JAGS input for each Bayesian state-space model using newly-assimilated data each week for 2015-2016

output_analysis_assessment_metrics: calculates a suite of assessment metrics on model hindcast output

uncertainty_partitioning_make_varmat: generates a matrix from hindcast output where each row represents a set of hindcasts with different forms of uncertainty included

