## create NetCDFs
# JAZ


library(tidyverse)
source('0_Function_library/metadata_functions.R')

# combining all forecast issue times and valid times into a single NetCDF file for publishing on EDI

csv_output_dir = 'edi.18.3/Gechinulata_hindcasts/Gechinulata_hindcasts/' # location of where Jake downloaded all hindcasts
nc_output_dir = '5_Model_output/5.4_nc_files'

files = list.files(csv_output_dir)

models = unique(sub('\\_.*','',files))
# uncertainty = unique(sub('.*?_', '', files) %>% sub('\\_.*', '', .))
forecast_issue_times = as.Date(unique(sub('.*\\_', '', files) %>% sub('.csv', '', .)))

n_issue_times = length(forecast_issue_times) # there should be n_issue_date files for every model / uncertainty combination
n_mc = 7500 # number of monte carlo draws
n_valid_times = 4 # making forecasts 4 weeks into the future; used for setting dimensions of netcdf
lon = -72.0304 # degrees east ****(need actual lon/lat of sampling location) ****
lat = 43.4307 # degrees north
depth = 1 # depth of prediction in meters
forecast_project_id = 'GLEON_Bayes_forecast_WG_Gloeo_uncertainty_partition_20200909'

for(m in models){
  # need to get the different types of uncertainty for given model
  model_files = files[grep(m, files)]
  uncertainty_types = sub('.*?_', '', model_files[grep(forecast_issue_times[1], model_files)]) %>% sub('\\_.*', '', .)
  for(u in uncertainty_types){
    # create a NetCDF for model + uncertainty type for hindcast output
    forecast_iteration_id = uuid::UUIDgenerate()
    model_name = paste(m, u, sep = '_')
    print(sprintf('Storing hindcasts for %s', model_name))
    nc_name_out = file.path(nc_output_dir, paste0(model_name, '.nc'))

    nc_create_hindcast_out(lon = lon,
                           lat = lat,
                           depth = depth,
                           n_mc = n_mc,
                           forecast_issue_times = forecast_issue_times,
                           n_valid_times = n_valid_times,
                           forecast_iteration_id = forecast_iteration_id,
                           forecast_project_id = forecast_project_id,
                           model_name = model_name,
                           nc_name_out = nc_name_out)
    for(i in 1:n_issue_times){
      # read in csv and store in newly created NetCDF
      cur_issue_time = forecast_issue_times[i]
      print(sprintf('  issue time: %s', cur_issue_time))

      cur_csv_file = paste(m, u, paste0(cur_issue_time,'.csv'), sep = '_')
      cur_hindcast = read.csv(file.path(csv_output_dir, cur_csv_file), stringsAsFactor = F)

      # we want dimensions the same as netCDF [issue_time, valid_times, lon, lat, depth, ens]
      hindcast_array = array(dim= c(1,n_valid_times, 1, 1, 1, n_mc))
      hindcast_array[1,,1,1,1,] = t(cur_hindcast)

      da_array = array(data = 0, dim = c(1, n_valid_times)) # Records whether or not observational data were used to constrain the system state or parameters at that point in time; weeks 1-4 should all be 0, week 0 had DA and would be 1 but we aren't storing week 0.
      forecast_flag_array = array(data = 0, dim = c(1, n_valid_times)) # Records whether or not that point in time was a forecast (1) or a hindcast (0); If the actual observed drivers/covariates over a period were used as inputs, that would be a hindcast.

      # store hindcast array in netcdf
      nc_hindcast_put(var_name = "Gloeo_abundance",
                      issue_time = cur_issue_time,
                      forecast_issue_times = forecast_issue_times,
                      values = hindcast_array,
                      nc_name_out = nc_name_out)
      nc_hindcast_put(var_name = "forecast",
                      issue_time = cur_issue_time,
                      forecast_issue_times = forecast_issue_times,
                      values = forecast_flag_array,
                      nc_name_out = nc_name_out)
      nc_hindcast_put(var_name = "data_assimilation",
                      issue_time = cur_issue_time,
                      forecast_issue_times = forecast_issue_times,
                      values = da_array,
                      nc_name_out = nc_name_out)
    }
    # create metadata output for NetCDF
    eml_name_out = file.path(nc_output_dir, paste0(model_name, '.eml.xml'))
    print('  creating eml file')

    create_hindcast_eml(model_out_nc_file = nc_name_out,
                        eml_file_name = eml_name_out,
                        model = m,
                        uncertainty = u)

  }
}



# example of how to retrieve variables from netcdf files and return as tibble
## return all issue times, valid times, and ensemble members
out = nc_hindcast_get(nc_file = '5_Model_output/5.4_nc_files/AR_IC.nc',
                      var_name = 'Gloeo_abundance') # should be like >1,000,000 rows
## return the first two issue times, all valid times, and all ensemble members
out = nc_hindcast_get(nc_file = '5_Model_output/5.4_nc_files/AR_IC.nc',
                      var_name = 'Gloeo_abundance',
                      forecast_issue_times = as.Date(c('2015-05-14','2015-05-21')))
## return the first two issue times, last week valid time, and all ensemble members
out = nc_hindcast_get(nc_file = '5_Model_output/5.4_nc_files/AR_IC.nc',
                      var_name = 'Gloeo_abundance',
                      forecast_issue_times = as.Date(c('2015-05-14','2015-05-21')),
                      forecast_valid_times = 28) # days since issue time
## return the first two issue times, last week valid time, and random draw of ensemble members
out = nc_hindcast_get(nc_file = '5_Model_output/5.4_nc_files/AR_IC.nc',
                      var_name = 'Gloeo_abundance',
                      forecast_issue_times = as.Date(c('2015-05-14','2015-05-21')),
                      forecast_valid_times = 28, # days since issue time
                      ens = sample(seq(1,n_mc,1), size = 200, replace = F)) # 200 random samples of ensemble output


