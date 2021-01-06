## create NetCDFs
# JAZ

library(tidyverse)
source('0_Function_library/metadata_functions.R')

# combining all forecast issue times and valid times into a single NetCDF file for publishing on EDI

csv_output_dir = 'edi.18.3/Gechinulata_hindcasts/Gechinulata_hindcasts/' # location of where Jake downloaded all hindcasts

files = list.files(csv_output_dir)

models = unique(sub('\\_.*','',files))
uncertainty = unique(sub('.*?_', '', files) %>% sub('\\_.*', '', .))
forecast_issue_times = as.Date(unique(sub('.*\\_', '', files) %>% sub('.csv', '', .)))

n_issue_times = length(forecast_issue_times) # there should be n_issue_date files for every model / uncertainty combination
n_mc = 7500 # number of monte carlo draws
forecast_project_id = 'GLEON_Bayes_forecast_WG_Gloeo_uncertainty_partition_20200909'

for(m in models){
  # need to get the different types of uncertainty for given model
  uncertainty_types = sub('.*?_', '', files[grep(dates[1], files[grep(m, files)])]) %>% sub('\\_.*', '', .)
  for(u in uncertainty_types){
    # create a NetCDF for model + uncertainty type for hindcast output
    forecast_iteration_ids = uuid::UUIDgenerate(n = n_issue_times)

    for(i in 1:length(forecast_issue_times)){
      # read in csv and store in NetCDF
      cur_issue_time = forecast_issue_times[i]
      cur_csv_file = files[grep(cur_issue_time, files[grep(u, files[grep(m, files)])])]
      cur_hindcast = read.csv(file.path(csv_output_dir, cur_csv_file), stringsAsFactor = F)

      hindcast_df <- select_if(cur_hindcast, ~sum(!is.na(.)) > 0)  %>% # getting rid of NA columns
        as_tibble()

      n_valid_times <- ncol(hindcast_df) # number of forecast weeks
      # forecast issue times are at week_0 and week_1-4 are x weeks in the future
      valid_times <- seq.Date(cur_issue_time + 7,
                              cur_issue_time + (n_valid_times * 7),
                              by = 'week')


    }
    # create metadata output for NetCDF

  }
}


