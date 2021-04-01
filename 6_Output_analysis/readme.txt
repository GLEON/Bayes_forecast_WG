6_Output_analysis

Scripts to calculate calibration and validation metrics from 5_Model_output for model assessment

Guide to files and folders:

6.1_Predictive_intervals: contains .csvs of calculated 95% predictive intervals (all forms of latent state uncertainty + observation error) for all models at each forecast horizon (one-week, two-week, three-week, four-week)

6.2_Predictive_intervals_known_covars: contains .csvs of calculated 95% predictive intervals (all forms of latent state uncertainty + observation error) for all models at each forecast horizon (one-week, two-week, three-week, four-week) for hindcasts generated using observed drivers


6A_Predictive_intervals: script to calculate 95% predictive intervals for all hindcasts

6B_Hindcast_output_analysis: script to calculate various metrics to assess hindcast model performance for each model and forecast horizon. Metrics are detailed in script annotation. One output file is written for each forecast horizon and this file contains assessment metrics for all models at that forecast horizon. Output files are:
hindcast_output_analysis_wk_1
hindcast_output_analysis_wk_2
hindcast_output_analysis_wk_3
hindcast_output_analysis_wk_4

6C_Uncertainty_partitioning_output_analysis: script to calculate mean, minimum, and maximum contributions of each type of uncertainty to total forecast uncertainty for all models. One output file is written for each forecast horizon and that file contains summaries of the contributions of each uncertainty source for all models at that forecast horizon. Output files are:
uncertainty_partitioning_output_analysis_wk_1
uncertainty_partitioning_output_analysis_wk_2
uncertainty_partitioning_output_analysis_wk_3
uncertainty_partitioning_output_analysis_wk_4

6D_Observation_uncertainty_analysis: script to calculate the magnitude of observation uncertainty to hindcasts over time

6E_Check_uncertainty_partitioning_modality: plots distributions of the relative contributions of each uncertainty type over time

6F_Predictive_intervals_known_covars: script to calculate 95% predictive intervals for all hindcasts generated using observed drivers

6G_Hindcast_output_analysis_known_covars: script to calculate various metrics to assess hindcast model performance for each model and forecast horizon for hindcasts generated using observed drivers. Metrics are detailed in script annotation. One output file is written for each forecast horizon and this file contains assessment metrics for all models at that forecast horizon. Output files are:
hindcast_output_analysis_known_covars_wk_1
hindcast_output_analysis_known_covars_wk_2
hindcast_output_analysis_known_covars_wk_3
hindcast_output_analysis_known_covars_wk_4
