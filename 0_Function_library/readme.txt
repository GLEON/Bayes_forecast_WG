0_Function_library

Contains helper functions that must be sourced to run analysis

Guide to functions:

model_calibration_get_data: pulls and formats appropriate data files for each Bayesian state-space model for calibration from 2009-2014

model_calibration_plug_n_play: assembles JAGS input for each Bayesian state-space model

model_calibration_plots: plotting code for Bayesian state-space model calibration visualization 

hindcasting_get_data: pulls and formats appropriate data files for each Bayesian state-space model for hindcasts from 2015-2016

hindcasting_get_params: uses posterior output of calibrated Bayesian state-space models updated with assimilated data to generate parameter input for hindcasts containing different combinations of uncertainty sources

hindcasting_run_hindcast: runs a hindcast of G. echinulata from 1-4 weeks into the future for any Bayesian state-space model

model_hindcasting_plug_n_play: assembles JAGS input for each Bayesian state-space model using newly-assimilated data each week for 2015-2016

uncertainty_partitioning_make_varmat: generates a matrix from hindcast output where each row represents a set of hindcasts with different forms of uncertainty included

output_analysis_assessment_metrics: calculates a suite of assessment metrics on model hindcast output