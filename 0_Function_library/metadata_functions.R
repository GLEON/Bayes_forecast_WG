
# notes on EFI forecast standards

# see document here: https://docs.google.com/document/d/1Xxyt-1LFHCE-CBwF_qad2sWHMHJoZVJdrOyK4ydjeR0/edit
# Following the Climate and Forecast (CF) convention, the order of dimensions for all three formats is T, Z, Y, X, E where T is time, Z, Y, and X are spatial dimensions, and E is ensemble member. In general forecasts issued at different dates or times should be stored in separate files, and thus the time dimension is the time being predicted. If multiple forecasts are placed within a single file then the issue time is the first time dimension and then the time being predicted is second.

library(ncdf4)
library(tidyverse)
library(EML)
library(emld)
library(uuid)
library(EFIstandards)

#' Function to organize model output arrays into netcdf
#' @param n_mc number of draws / ensembles
#' @param forecast_issue_time datetime when forecast was issued
#' @param forecast_iteration_id unique to each forecast generated
#' @param forecast_project_id id that is unique to the collection of forecasts
#' @param model_name model name used to forecast Gloeo abundance
#' @param nc_name_out name of the ncdf file
#'
nc_create_hindcast_out = function(lon,
                                  lat,
                                  depth,
                                  n_mc,
                                  forecast_issue_times,
                                  n_valid_times,
                                  forecast_iteration_id,
                                  forecast_project_id,
                                  model_name,
                                  nc_name_out,
                                  overwrite = T){

  #Set dimensions
  issue_times <- as.integer(forecast_issue_times - forecast_issue_times[1]) # days
  valid_times <- as.integer(seq(1, n_valid_times, 1) * 7) # days
  depth <- as.numeric(depth)
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  ens <- as.integer(seq(1, n_mc, 1))
  # Observation error flag [OPTIONAL]
  #   Name: obs_flag
  #   Format: integer
  #   Description: records whether the output for a variable reflects a prediction of a latent variable or whether observation error had been included in the prediction. The default is to assume that the observation error is present (i.e. if the ensemble quantiles would produce a predictive interval) and if all forecast variables include observation error this flag is optional. This flag is required if observation error is absent (i.e. ensemble quantiles would represent a confidence interval) or if a file includes a mix of latent and observable variables. This is particularly true if the same variable name exists in both CI and PI forms. Therefore, it is fine for variables in a file to vary in whether they have an obs_flag dimension or not.


  issue_time_dim <- ncdim_def("issue_time",
                              units = 'days',
                              longname = sprintf('days since %s', forecast_issue_times[1]),
                              vals = issue_times)
  valid_time_dim <- ncdim_def("valid_time",
                              units = 'days',
                              longname = 'days since forecast issue time',
                              vals = valid_times)
  depth_dim <- ncdim_def('depth',
                         units = 'meters',
                         longname = 'depth from lake surface',
                         vals = depth)
  lon_dim <- ncdim_def('lon',
                       units = 'degrees_east',
                       longname = 'longitude',
                       vals = lon)
  lat_dim <- ncdim_def('lat',
                       units = 'degrees_north',
                       longname = 'latitude',
                       vals = lat)
  ens_dim <- ncdim_def("ens",
                       units = "",
                       vals = ens,
                       longname = 'monte carlo draw / ensemble member')

  dim_nchar <- ncdim_def("nchar",
                         units = "",
                         vals = 1:nchar(as.character(forecast_issue_times[1])),
                         create_dimvar = FALSE)

  ## quick check that units are valid
  udunits2::ud.is.parseable(issue_time_dim$units)
  udunits2::ud.is.parseable(valid_time_dim$units)
  udunits2::ud.is.parseable(depth_dim$units)
  udunits2::ud.is.parseable(lon_dim$units)
  udunits2::ud.is.parseable(lat_dim$units)
  udunits2::ud.is.parseable(ens_dim$units)

  #Define the metadata about our variables
  fillvalue <- 1e32

  def_list <- list()
  def_list[[1]] <- ncvar_def(name =  'Gloeo_abundance',
                             units = 'log(colonies L-1)',
                             dim = list(issue_time_dim, valid_time_dim, depth_dim, lon_dim, lat_dim, ens_dim),
                             missval = fillvalue,
                             longname = 'G. echinulata total log(colonies L-1)',
                             prec = 'float')
  def_list[[2]] <- ncvar_def(name =  "forecast",
                             units = "integer",
                             dim = list(issue_time_dim, valid_time_dim),
                             missval = fillvalue,
                             longname = 'EFI standard forecast code. 0 = hindcast, 1 = forecast',
                             prec="single")
  def_list[[3]] <- ncvar_def(name =  'data_assimilation',
                             units = 'integer',
                             dim = list(issue_time_dim, valid_time_dim),
                             missval = fillvalue,
                             longname = 'EFI standard data assimilation code. 0 = no DA, 1 = DA at timestep',
                             prec = 'single')
  def_list[[4]] <- ncvar_def(name = 'forecast_issue_time',
                             units = 'datetime',
                             dim = list(dim_nchar, issue_time_dim),
                             longname = 'Forecast issue time',
                             prec = 'char')

  #create the netCDF file
  if(file.exists(nc_name_out)){
    if(overwrite){
      file.remove(nc_name_out)
      ncout <- nc_create(nc_name_out, def_list, force_v4 = T)
    }else{stop('cannot overwrite nc output file')}
  }else{ncout <- nc_create(nc_name_out, def_list, force_v4 = T)}

  # put in issue times
  ncvar_put(nc = ncout,
            varid = def_list[[4]],
            vals = forecast_issue_times)

  #Global file metadata
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_project_id",
            attval = as.character(forecast_project_id),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_iteration_id",
            attval = as.character(forecast_iteration_id),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_model_id",
            attval = as.character(model_name),
            prec =  "text")
  nc_close(ncout)
}


#' insert hindcast variables
#'
nc_hindcast_put = function(var_name, # variable name to put
                           issue_time,
                           forecast_issue_times,
                           values,
                           nc_name_out){

  ncout <- nc_open(nc_name_out, write = T)

  cur_var = ncout$var[[var_name]]
  varsize = cur_var$varsize
  # Gleoe output dims [issue_time_dim, valid_time_dim, depth_dim, lon_dim, lat_dim, ens_dim]; position of dimensions following:
  issue_time_pos = 1
  valid_time_pos = 2
  depth_pos = 3
  lon_pos = 4
  lat_pos = 5
  ens_pos = 6

  n_dims = cur_var$ndims

  cur_issue_time = which(issue_time == forecast_issue_times)

  # n_valid_times = varsize[valid_time_pos]
  # n_mc = varsize[ens_pos]

  start = rep(1, n_dims)
  start[issue_time_pos] = cur_issue_time

  count = varsize
  count[issue_time_pos] = 1 # adding only one issue time step

  ncvar_put(nc = ncout,
            varid = var_name,
            vals = values,
            start = start,
            count = count)

  nc_close(ncout)
}


#' function for returning hindcasted variables; returns tibble
#'
nc_hindcast_get = function(nc_file,
                           var_name,
                           forecast_issue_times = NULL,
                           forecast_valid_times = NULL,
                           lon = NULL,
                           lat = NULL,
                           depth = NULL,
                           ens = NULL){

  nc = nc_open(nc_file)

  # Gleoe output dims [issue_time_dim, valid_time_dim, depth_dim, lon_dim, lat_dim, ens_dim]; position of dimensions following:
  issue_time_pos = 1
  valid_time_pos = 2
  depth_pos = 3
  lon_pos = 4
  lat_pos = 5
  ens_pos = 6

  cur_var = nc$var[[var_name]]
  varsize = cur_var$varsize
  all_issue_times = as.Date(ncvar_get(nc, varid = 'forecast_issue_time')) # all forecast issue times
  all_valid_times = cur_var$dim[[valid_time_pos]]$vals
  all_depth = cur_var$dim[[depth_pos]]$vals
  all_lon = cur_var$dim[[lon_pos]]$vals
  all_lat = cur_var$dim[[lat_pos]]$vals
  all_ens = cur_var$dim[[ens_pos]]$vals

  n_dims = cur_var$ndims

  # return all values, and then filter
  all_out = ncvar_get(nc = nc, varid = var_name) %>% array(., dim = varsize) %>%
    reshape2::melt(varnames = c('forecast_issue_time',
                                'forecast_valid_time',
                                'depth', 'lon','lat', 'ensemble')) %>%
    mutate(forecast_issue_time = all_issue_times[forecast_issue_time],
           forecast_valid_time = forecast_issue_time + as.difftime(all_valid_times[forecast_valid_time], units = 'days'),
           depth = all_depth[depth],
           lon = all_lon[lon],
           lat = all_lat[lat],
           ensemble = all_ens[ensemble]) %>%
    rename(!!var_name := value) %>%
    as_tibble()

  add_time = function(x, y){x + as.difftime(y, units = 'days')}

  if(!is.null(forecast_issue_times)){
    cur_issue_times = as.Date(forecast_issue_times)
  }else{cur_issue_times = as.Date(all_issue_times)} # return all dates if NULL
  if(!is.null(forecast_valid_times)){
    cur_valid_times = lapply(cur_issue_times, add_time, y = forecast_valid_times) %>% do.call('c',.) %>% unique()
  }else{cur_valid_times = lapply(cur_issue_times, add_time, y = all_valid_times) %>% do.call('c',.) %>% unique()}
  if(!is.null(depth)){
    cur_depth = as.integer(depth)
  }else{cur_depth = as.integer(all_depth)}
  if(!is.null(lon)){
    cur_lon = as.numeric(lon)
  }else{cur_lon = as.numeric(all_lon)}
  if(!is.null(lat)){
    cur_lat = as.numeric(lat)
  }else{cur_lat = as.numeric(all_lat)}
  if(!is.null(ens)){
    cur_ens = as.integer(ens)
  }else{cur_ens = as.integer(all_ens)}

  out = dplyr::filter(all_out,
                      forecast_issue_time %in% cur_issue_times,
                      forecast_valid_time %in% cur_valid_times,
                      depth %in% cur_depth,
                      lon %in% cur_lon,
                      lat %in% cur_lat,
                      ensemble %in% cur_ens)

  nc_close(nc)

  return(out)
}







#
#' @param model_out_nc_file netcdf file for model output values and other associated data
#' @param eml_file_name name of file to create
#' @param model model used in the hindcasts
#' @param uncertainty type of uncertainty combination used in the hindcast

create_hindcast_eml = function(model_out_nc_file,
                               eml_file_name,
                               model,
                               uncertainty){

  model_out <- nc_open(model_out_nc_file)

  dates <- as.Date(ncvar_get(nc = model_out, varid = 'forecast_issue_time'))

  n_mc <- length(ncvar_get(nc = model_out, varid = 'ens'))

  model_metadata = get_model_metadata(model = model,
                                      uncertainty = uncertainty,
                                      n_mc = n_mc)

  pubDate <- "2021-02-10 16:30:00 EST" # change this to the revisions of the model

  forecast_project_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_project_id')$value

  forecast_iteration_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_iteration_id')$value

  forecast_model_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_model_id')$value

  nc_close(model_out)

  attributes <- tibble::tribble(
    ~attributeName, ~attributeDefinition, ~unit, ~formatString, ~numberType, ~definition,
    "forecast_issue_time",    "[dimension]{time}",    "date",     "YYYY-MM-DD", "numberType", 'date when forecast was issued',
    "forecast_valid_time",    "[dimension]{time}",    "date",     "YYYY-MM-DD", "numberType", 'date when forecast was valid',
    "depth",   "[dimension]{depth in lake}",     "meter",       NA,   "real",    NA,
    "lon", "[dimension]{longitude}", "degrees_east", NA, "real", NA,
    "lat", "[dimension]{latitude}", "degrees_north", NA, "real", NA,
    "ensemble",      "[dimension]{index of ensemble member}",   "dimensionless",    NA,    "integer", NA,
    "Gloeo_abundance",     "[variable]{G. echinulata abundance}", "log(colonies L-1)", NA,  "real", NA,
    "forecast",      "[flag]{whether time step used forecasted covariates}", "dimensionless",    NA, "integer",    NA,
    "data_assimilation", "[flag]{whether time step assimilated data}", "dimensionless",  NA, "integer",    NA
  )
  attrList <- set_attributes(attributes,
                             col_classes = c("Date",
                                             "Date",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric"))
  physical <- set_physical(model_out_nc_file)

  dataTable <- eml$dataTable(
    entityName = model_out_nc_file,
    entityDescription = 'Lake Sunapee G. echinulata forecasts',
    physical = physical,
    attributeList = attrList)

  author <- list(individualName = list(givenName = "Mary",
                                       surName = "Lofton"),
                 electronicMailAddress = "melofton@vt.edu",
                 id = 'orcid.org/0000-0003-3270-1330',
                 individualName = list(givenName = "Jacob",
                                       surName = "Zwart"),
                 electronicMailAddress = "jzwart@usgs.gov",
                 id = 'orcid.org/0000-0002-3870-405X')  ## Jake I'm not sure this syntax looks right - help!


  coverage <- EML::set_coverage(begin = lubridate::as_datetime(dates[1]),
                                end = lubridate::as_datetime(tail((dates)[1])),
                                geographicDescription = "Lake Sunapee",
                                west = -72.0831,
                                east = -72.0304,
                                north = 43.4307,
                                south = 43.3217,
                                altitudeMaximum = 333.3,
                                altitudeMinimum = 333.3,
                                altitudeUnits = 'meter',
                                sci_names = 'Gloeotrichia echinulata')

  keywordSet <- list(
    list(
      keywordThesaurus = "EFI controlled vocabulary",
      keyword = list("forecast",
                     "ecosystem",
                     "timeseries",
                     "water",
                     "HABs",
                     "algae")
    ))


  ### methods details here ###
  methods = list(methods = 'See Lofton et al. XXXX url:') ## need to add in methods details; could point to paper?
  ###########

  dataset = eml$dataset(title = 'Lake Sunapee G. echinulata forecasts',
                        creator = author,
                        contact = list(references = 'orcid.org/0000-0003-3270-1330'), # check if Mary's ORCID
                        pubDate = pubDate,
                        intellectualRights = 'https://spdx.org/licenses/CC-BY-4.0.html',
                        #Usage and licensing information. <intellectualRights> can be text, but we recommend providing the URL of a standard license, e.g. https://opensource.org/licenses/MIT
                        # <licensed> is more detailed and consists of the following subtags
                        # <licenseName>  e.g. Creative Commons Attribution 4.0 International
                        # <url> e.g. https://spdx.org/licenses/CC-BY-4.0.html
                        # <identifier> e.g. CC-BY-4.0

                        abstract = 'EML metadata for G. echinulata forecasts',
                        dataTable = dataTable,
                        keywordSet = keywordSet,
                        coverage = coverage,
                        methods = methods)

  # EFI forecast-specific metadata
  additionalMetadata = eml$additionalMetadata(
    metadata = list(
      forecast = list(
        ## Basic elements
        timestep = "7 days", ## should always be 1 week for this project
        forecast_horizon = '28 days',
        forecast_iteration_id = forecast_iteration_id,
        forecast_project_id = forecast_project_id,
        metadata_standard_version = "v0.3",
        model_description = list(
          name = forecast_model_id,
          type = "process-based",
          repository = "github.com/GLEON/Bayes_forecast_WG/tree/eco_apps_release/4.1_JAGS_models" # update when code is released; could also make this specific to each model file (e.g. AR.R, precip.R, etc..)
        ),
        ## UNCERTAINTY CLASSES
        initial_conditions = list(
          status = model_metadata$ic_status,
          complexity = model_metadata$ic_complexity,
          propagation = list(type = model_metadata$ic_propagation_type,
                             size = model_metadata$ic_propagation_size),
          assimilation = list(type = model_metadata$ic_assimilation_type,
                              reference = model_metadata$ic_assimilation_ref,
                              complexity = model_metadata$ic_assimilation_complexity)
        ),
        parameters = list(
          status = model_metadata$param_status,
          complexity = model_metadata$param_complexity,
          propagation = list(type = model_metadata$param_propagation_type,
                             size = model_metadata$param_propagation_size),
          assimilation = list(type = model_metadata$param_assimilation_type,
                              reference = model_metadata$param_assimilation_ref,
                              complexity = model_metadata$param_assimilation_complexity)
        ),
        drivers = list(
          status = model_metadata$driver_status,
          complexity = model_metadata$driver_complexity,
          propagation = list(type = model_metadata$driver_propagation_type,
                             size = model_metadata$driver_propagation_size),
          assimilation = list(type = model_metadata$driver_assimilation_type,
                              reference = model_metadata$driver_assimilation_ref,
                              complexity = model_metadata$driver_assimilation_complexity)
        ),
        random_effects = list(
          status = 'absent',
          complexity = NA,
          propagation = list(type = NA,
                             size = NA),
          assimilation = list(type = NA,
                              reference = NA,
                              complexity = NA)
        ),
        process_error = list(
          status = model_metadata$process_status,
          complexity = list(complexity = model_metadata$process_complexity,
                            covariance = model_metadata$process_complexity_cov,
                            localization = model_metadata$process_complexity_loc),
          propagation = list(type = model_metadata$process_propagation_type,
                             size = model_metadata$process_propagation_size),
          assimilation = list(type = model_metadata$process_assimilation_type,
                              reference = model_metadata$process_assimilation_ref,
                              complexity = model_metadata$process_assimilation_complexity)
        ),
        obs_error = list(
          status = model_metadata$obs_status,
          complexity = list(complexity = model_metadata$obs_complexity,
                            covariance = model_metadata$obs_complexity_cov,
                            localization = model_metadata$obs_complexity_loc),
          propagation = list(type = model_metadata$obs_propagation_type,
                             size = model_metadata$obs_propagation_size),
          assimilation = list(type = model_metadata$obs_assimilation_type,
                              reference = model_metadata$obs_assimilation_ref,
                              complexity = model_metadata$obs_assimilation_complexity)
        )
      )
    )
  )

  eml_out <- eml$eml(dataset = dataset,
                     additionalMetadata = additionalMetadata,
                     packageId = forecast_project_id,
                     system = "uuid")

  ### validate the EML
  eml_validate(eml_out)

  ## check that the EML is also a valid EFI forecast
  # EFIstandards::forecast_validator(eml_out) # commenting for now because I don't know where the error for https:/ is coming from

  write_eml(eml_out, file = eml_file_name)
}




## function for returning model metadata for eml file

get_model_metadata = function(model,
                              uncertainty,
                              n_mc){

  #' @param initial_conditions does the model consider uncertainty in initial conditions; Possible values:           # Possible values: absent, present, data_driven, propagates, assimilates
  #' @param ic_complexity number of state variables in the model
  #' @param parameters does the model consider parameter uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
  #' @param param_complexity number of estimated parameters / coefficients
  #' @param random_effects does the model consider random effects uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
  #' @param random_complexity number of random effect terms, which should be equivalent to the number of random effect variances estimated
  #' @param drivers does the model consider driver uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
  #' @param driver_complexity Number of different driver variables or covariates in a model
  #' @param process_error does the model consider process error uncertainty; Possible values: no, contains, data_driven, propagates, assimilates; additional subtags needed (covariance and localization)
  #' @param process_complexity dimension of the error covariance matrix. So if we had a n x n covariance matrix, n is the value entered for <complexity>. Typically n should match the dimensionality of the initial_conditions unless there are state variables where process error is not being estimated or propagated.

  if(model == 'RW'){
    param_complexity = 0
    param_status = 'absent'
    driver_complexity = 0
    driver_status = 'absent'
  }else if(model %in% c('BiasRW','AC')){
    param_complexity = 1
    param_status = 'data_driven'
    driver_complexity = 0
    driver_status = 'absent'
  }else if(model == 'BaseDLM'){
    param_complexity = 2
    param_status = 'data_driven'
    driver_complexity = 0
    driver_status = 'absent'
  }else if(model %in% c('MinWaterTemp','MinWaterTempLag', 'SchmidtLag', 'WaterTempMA','WindDir')){ # all single covariates
    param_complexity = 3
    param_status = 'data_driven'
    driver_complexity = 1
    driver_status = 'data_driven'
  }else if(model %in% c('GDD')){
    param_complexity = 4
    param_status = 'data_driven'
    driver_complexity = 1
    driver_status = 'data_driven'
  }else if(model %in% c('SchmidtAndWind', 'TempAndWind')){
    param_complexity = 4
    param_status = 'data_driven'
    driver_complexity = 2
    driver_status = 'data_driven'
  }else if(model %in% c('WindAndGDD')){
    param_complexity = 5
    param_status = 'data_driven'
    driver_complexity = 2
    driver_status = 'data_driven'
  }

  if(uncertainty == 'IC'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = param_status
    param_complexity = param_complexity
    param_propagation_type = NA
    param_propagation_size = NA
    param_assimilation_type = NA
    param_assimilation_ref = NA
    param_assimilation_complexity = NA
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'absent'
    process_complexity = NA
    process_complexity_cov = NA
    process_complexity_loc = NA
    process_propagation_type = NA
    process_propagation_size = NA
    process_assimilation_type = NA
    process_assimilation_ref = NA
    process_assimilation_complexity = NA
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  if(uncertainty == 'IC.Pa'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'absent'
    process_complexity = NA
    process_complexity_cov = NA
    process_complexity_loc = NA
    process_propagation_type = NA
    process_propagation_size = NA
    process_assimilation_type = NA
    process_assimilation_ref = NA
    process_assimilation_complexity = NA
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  if(uncertainty == 'IC.Pa.D'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = 'assimilates'
    driver_complexity = driver_complexity
    driver_propagation_type = 'ensemble'
    driver_propagation_size = n_mc
    driver_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    driver_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    driver_assimilation_complexity = driver_complexity
    # process_error
    process_status = 'absent'
    process_complexity = NA
    process_complexity_cov = NA
    process_complexity_loc = NA
    process_propagation_type = NA
    process_propagation_size = NA
    process_assimilation_type = NA
    process_assimilation_ref = NA
    process_assimilation_complexity = NA
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  if(uncertainty == 'IC.Pa.P'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  if(uncertainty == 'IC.Pa.P.O'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'data_driven'
    obs_complexity = 1
    obs_complexity_cov = TRUE
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  if(uncertainty == 'IC.Pa.D.P.O'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = 'assimilates'
    driver_complexity = driver_complexity
    driver_propagation_type = 'ensemble'
    driver_propagation_size = n_mc
    driver_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    driver_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    driver_assimilation_complexity = driver_complexity
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'data_driven'
    obs_complexity = 1
    obs_complexity_cov = TRUE
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }
  if(uncertainty == 'IC.Pa.D.P'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = 'assimilates'
    driver_complexity = driver_complexity
    driver_propagation_type = 'ensemble'
    driver_propagation_size = n_mc
    driver_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    driver_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    driver_assimilation_complexity = driver_complexity
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }
  if(uncertainty == 'IC.Pa.D'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = 'assimilates'
    param_complexity = param_complexity
    param_propagation_type = 'ensemble'
    param_propagation_size = n_mc
    param_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    param_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    param_assimilation_complexity = param_complexity
    # drivers
    driver_status = 'assimilates'
    driver_complexity = driver_complexity
    driver_propagation_type = 'ensemble'
    driver_propagation_size = n_mc
    driver_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    driver_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    driver_assimilation_complexity = driver_complexity
    # process_error
    process_status = 'absent'
    process_complexity = NA
    process_complexity_cov = NA
    process_complexity_loc = NA
    process_propagation_type = NA
    process_propagation_size = NA
    process_assimilation_type = NA
    process_assimilation_ref = NA
    process_assimilation_complexity = NA
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }
  if(uncertainty == 'IC.P.O'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = param_status
    param_complexity = param_complexity
    param_propagation_type = NA
    param_propagation_size = NA
    param_assimilation_type = NA
    param_assimilation_ref = NA
    param_assimilation_complexity = NA
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'data_driven'
    obs_complexity = 1
    obs_complexity_cov = TRUE
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }
  if(uncertainty == 'IC.P'){
    # initial conditions
    ic_status = 'assimilates'
    ic_complexity = 1 # number of state variables estimated (only Gloeo)
    ic_propagation_type = 'ensemble'
    ic_propagation_size = n_mc
    ic_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    ic_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    ic_assimilation_complexity = 1
    # parameters
    param_status = param_status
    param_complexity = param_complexity
    param_propagation_type = NA
    param_propagation_size = NA
    param_assimilation_type = NA
    param_assimilation_ref = NA
    param_assimilation_complexity = NA
    # drivers
    driver_status = driver_status
    driver_complexity = driver_complexity
    driver_propagation_type = NA
    driver_propagation_size = NA
    driver_assimilation_type = NA
    driver_assimilation_ref = NA
    driver_assimilation_complexity = NA
    # process_error
    process_status = 'assimilates'
    process_complexity = 1
    process_complexity_cov = TRUE
    process_complexity_loc = NA
    process_propagation_type = 'ensemble'
    process_propagation_size = n_mc
    process_assimilation_type = 'Recursive Bayesian Inference' # iterative Bayes
    process_assimilation_ref = 'Lofton et al. XXXX url:' ### need to add url / DOI here
    process_assimilation_complexity = 1
    # observation error
    obs_status = 'absent'
    obs_complexity = NA
    obs_complexity_cov = NA
    obs_complexity_loc = NA
    obs_propagation_type = NA # does not propagate cuz obs error
    obs_propagation_size = NA
    obs_assimilation_type = NA
    obs_assimilation_ref = NA
    obs_assimilation_complexity = NA
  }

  return(list(ic_status = ic_status,
              ic_complexity = ic_complexity,
              ic_propagation_type = ic_propagation_type,
              ic_propagation_size = ic_propagation_size,
              ic_assimilation_type = ic_assimilation_type,
              ic_assimilation_ref = ic_assimilation_ref,
              ic_assimilation_complexity = ic_assimilation_complexity,
              param_status = param_status,
              param_complexity = param_complexity,
              param_propagation_type = param_propagation_type,
              param_propagation_size = param_propagation_size,
              param_assimilation_type = param_assimilation_type,
              param_assimilation_ref = param_assimilation_ref,
              param_assimilation_complexity = param_assimilation_complexity,
              driver_status = driver_status,
              driver_complexity = driver_complexity,
              driver_propagation_type = driver_propagation_type,
              driver_propagation_size = driver_propagation_size,
              driver_assimilation_type = driver_assimilation_type,
              driver_assimilation_ref = driver_assimilation_ref,
              driver_assimilation_complexity = driver_assimilation_complexity,
              process_status = process_status,
              process_complexity = process_complexity,
              process_complexity_cov = process_complexity_cov,
              process_complexity_loc = process_complexity_loc,
              process_propagation_type = process_propagation_type,
              process_propagation_size = process_propagation_size,
              process_assimilation_type = process_assimilation_type,
              process_assimilation_ref = process_assimilation_ref,
              process_assimilation_complexity = process_assimilation_complexity,
              obs_status = obs_status,
              obs_complexity = obs_complexity,
              obs_complexity_cov = obs_complexity_cov,
              obs_complexity_loc = obs_complexity_loc,
              obs_propagation_type = obs_propagation_type,
              obs_propagation_size = obs_propagation_size,
              obs_assimilation_type = obs_assimilation_type,
              obs_assimilation_ref = obs_assimilation_ref,
              obs_assimilation_complexity = obs_assimilation_complexity))

}





