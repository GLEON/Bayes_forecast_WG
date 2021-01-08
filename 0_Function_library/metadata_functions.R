
# notes on EFI forecast standards

# see document here: https://docs.google.com/document/d/1rEV2qI1u4l3gjzy8-x3gjFChsd5SIWTUL6aWhPWYdr8/edit
# Following the Climate and Forecast (CF) convention, the order of dimensions for all three formats is T, Z, Y, X, E where T is time, Z, Y, and X are spatial dimensions, and E is ensemble member. In general forecasts issued at different dates or times should be stored in separate files, and thus the time dimension is the time being predicted. If multiple forecasts are placed within a single file then the issue time is the first time dimension and then the time being predicted is second.



# practice data #############
# hindcast_matrix = read.csv('edi.18.3/Gechinulata_hindcasts/Gechinulata_hindcasts/AR_IC.Pa.P.O_2015-05-14.csv', stringsAsFactor = F)
# Nmc = 7500
# forecast_issue_time = as.Date('2015-05-07')
# forecast_iteration_id = uuid::UUIDgenerate()
# forecast_project_id = 'GLEON_Bayes_forecast_WG_Gloeo_uncertainty_partition_20200909'
# model_name = 'AR_IC'
# nc_name_out = 'AR_IC.Pa.P.O_2015-05-14.nc'
#
# library(ncdf4)
# library(tidyverse)
# library(EML)
# library(emld)
# library(uuid)
# library(EFIstandards)
######

#' Function to organize model output arrays into netcdf
#' @param hindcast_matrix matrix of hindcast output; currently forecasting out 4 weeks
#' @param Nmc number of draws / ensembles
#' @param forecast_issue_time datetime when forecast was issued
#' @param forecast_iteration_id unique to each forecast generated
#' @param forecast_project_id id that is unique to the collection of forecasts
#' @param model_name model name used to forecast Gloeo abundance
#' @param nc_name_out name of the ncdf file
#'
matrix_to_ncdf = function(hindcast_matrix,
                          Nmc,
                          forecast_issue_time,
                          forecast_iteration_id,
                          forecast_project_id,
                          model_name,
                          nc_name_out){

  hindcast_df <- select_if(hindcast_matrix, ~sum(!is.na(.)) > 0)  %>% # getting rid of NA columns
    as_tibble()

  n_valid_times <- ncol(hindcast_df) # number of forecast weeks
  # need to check if forecast issue times and week_1 are the same or week_1 7 days in the future ###
  valid_times <- seq.Date(forecast_issue_time + 7,
                          forecast_issue_time + (n_valid_times * 7),
                          by = 'week')

  # flag for if any observations were assimilated for each date, 0 for no data assimilated, 1 for data assimilated; not sure if there were data assimilated in hindcasts at first time point or not; I'm thinking no, but need to confirm with Mary
  data_assimilation <- hindcast_df %>% mutate_all(~ifelse(!is.na(.), 0, NA))

  #Set dimensions
  ens <- as.integer(seq(1, Nmc, 1))
  timestep <- as.integer(seq(1, n_valid_times, 1))

  ens_dim <- ncdim_def("ens",
                       units = "",
                       vals = ens,
                       longname = 'monte carlo draw / ensemble member')
  time_dim <- ncdim_def("timestep",
                        units = '1 week',
                        longname = 'forecast valid timestep',
                        vals = timestep)

  dim_nchar <- ncdim_def("nchar",
                         units = "",
                         vals = 1:nchar(as.character(valid_times[1])),
                         create_dimvar = FALSE)

  ## quick check that units are valid
  udunits2::ud.is.parseable(ens_dim$units)
  udunits2::ud.is.parseable(time_dim$units)
  udunits2::ud.is.parseable(dim_nchar$units)

  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  def_list[[1]] <- ncvar_def(name = 'time',
                             units = 'datetime',
                             dim = list(dim_nchar, time_dim),
                             longname = 'forecast valid time',
                             prec = 'char')

  # could add in Z, Y, X variables here too (Z would be depth (1 m ?), Y would be longitude of sampling location, X would be latitude of sampling location)
  def_list[[2]] <- ncvar_def(name =  'Gloeo_abundance',
                             units = 'colonies L-1', # is this correct unit and/or is there better udunit?
                             dim = list(time_dim, ens_dim),
                             missval = fillvalue,
                             longname = 'G. echinulata total colonies L-1',
                             prec = 'float')

  def_list[[3]] <- ncvar_def(name =  'data_assimilation',
                             units = 'logical',
                             dim = list(time_dim, ens_dim),
                             missval = fillvalue,
                             longname = '1 = data assimilation used in timestep',
                             prec = 'single')

  ncout <- nc_create(nc_name_out, def_list, force_v4 = T)

  ncvar_put(nc = ncout,
            varid = def_list[[1]],
            vals = valid_times)
  ncvar_put(nc = ncout,
            varid = def_list[[2]],
            vals = as.matrix(hindcast_df))
  ncvar_put(nc = ncout,
            varid = def_list[[3]],
            vals = as.matrix(data_assimilation))

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
            attname = "forecast_issue_time",
            attval = as.character(forecast_issue_time),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "model_name",
            attval = as.character(model_name),
            prec =  "text")
  nc_close(ncout)
}

### practice data ################
model_out_nc_file = 'AR_IC.Pa.P.O_2015-05-14.nc'
eml_file_name = 'AR_IC.Pa.P.O_2015-05-14.eml.xml'
initial_conditions = 'propagates'
ic_complexity = 1
parameters = 'contains'
param_complexity = 1
random_effects = 'no'
random_complexity = NA
drivers = 'no'
driver_complexity = NA
process_error = 'propagates'
process_complexity = 1
covariance = TRUE
localization = FALSE
###########

#
#' @param model_out_nc_file netcdf file for model output values and other associated data
#' @param eml_file_name name of file to create
#' @param initial_conditions does the model consider uncertainty in initial conditions; Possible values: no, contains, data_driven, propagates, assimilates
#' @param ic_complexity number of state variables in the model
#' @param parameters does the model consider parameter uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
#' @param param_complexity number of estimated parameters / coefficients
#' @param random_effects does the model conisder random effects uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
#' @param random_complexity number of random effect terms, which should be equivalent to the number of random effect variances estimated
#' @param drivers does the model consider driver uncertainty; Possible values: no, contains, data_driven, propagates, assimilates
#' @param driver_complexity Number of different driver variables or covariates in a model
#' @param process_error does the model consider process error uncertainty; Possible values: no, contains, data_driven, propagates, assimilates; additional subtags needed (covariance and localization)
#' @param process_complexity dimension of the error covariance matrix. So if we had a n x n covariance matrix, n is the value entered for <complexity>. Typically n should match the dimensionality of the initial_conditions unless there are state variables where process error is not being estimated or propagated.
#' @param covariance if the model contains process error, does the model use covariance matrix ; T = full covariance matrix, F = diagonal only
#' @param localization if the model containes process error, is there any localization approached used for the covariance matrix; e.g. by distance.
#'
create_forecast_eml = function(model_out_nc_file,
                               eml_file_name,
                               initial_conditions,
                               ic_complexity = NA,
                               parameters,
                               param_complexity = NA,
                               random_effects,
                               random_complexity = NA,
                               drivers,
                               driver_complexity = NA,
                               process_error,
                               process_complexity = NA,
                               covariance = NA,
                               localization = NA){

  model_out <- nc_open(model_out_nc_file)

  dates <- ncvar_get(nc = model_out, varid = 'time')

  Nmc <- length(ncvar_get(nc = model_out, varid = 'ens'))

  forecast_issue_time <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_issue_time')$value

  forecast_project_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_project_id')$value

  forecast_iteration_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_iteration_id')$value

  model_name <- ncatt_get(nc = model_out, varid = 0, attname = 'model_name')$value

  nc_close(model_out)

  attributes <- tibble::tribble(
    ~attributeName, ~attributeDefinition, ~unit, ~formatString, ~numberType, ~definition,
    "valid_date",          "forecast valid date",    "date",     "YYYY-MM-DD", "numberType", NA,
    "ensemble",      "index of ensemble member",   "dimensionless",    NA,    "integer", NA,
    "Gloeo abundance",     "G. echinulata abundance", "colonies L-1", NA,  "real", NA,
    "forecast_issue_time",     "time that forecast was created", NA, "YYYY-MM-DD",  NA, NA,
    "data_assimilation",     "flag whether time step included data assimilation", "dimensionless", NA, "integer", NA,
    "forecast_id",     "ID for specific forecast iteration", NA, NA,  NA, "forecast id",
    "forecast_project_id",     "ID for forecasting project", NA, NA,  NA, "project id"
  )
  attrList <- set_attributes(attributes,
                             col_classes = c("Date",
                                             "numeric",
                                             "numeric",
                                             "Date",
                                             "numeric",
                                             "character",
                                             "character"))
  physical <- set_physical(model_out_nc_file)

  dataTable <- eml$dataTable(
    entityName = model_out_nc_file,
    entityDescription = 'Lake Sunapee G. echinulata forecasts',
    physical = physical,
    attributeList = attrList)

  author <- list(individualName = list(givenName = "Mary",
                                       surName = "Lofton"),
                 electronicMailAddress = "melofton@vt.edu",
                 id = 'https://orcid.org/0000-0003-3270-1330') # is this Mary's ORCID?

  coverage <- EML::set_coverage(begin = lubridate::as_datetime(dates[1]),
                                end = lubridate::as_datetime(tail((dates)[1])),
                                geographicDescription = "Lake Sunapee",
                                west = -72.0831,
                                east = -72.0304,  # need to check if this is correct boundaries and altitude
                                north = 43.4307,
                                south = 43.3217,
                                altitudeMaximum = 333.3,
                                altitudeMinimum = 333.3,
                                altitudeUnits = 'meter')

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
                        contact = list(references = 'https://orcid.org/0000-0003-3270-1330'), # check if Mary's ORCID
                        pubDate = forecast_issue_time,
                        intellectualRights = '?', # not sure what to put here; would publisher go here? EDI policies? somehting else?
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
        timestep = "1 week", ## should always be 1 week for this project
        forecast_horizon = sprintf("%s days", length(dates) * 7),
        forecast_issue_time = forecast_issue_time,
        forecast_iteration_id = forecast_iteration_id,
        forecast_project_id = forecast_project_id,
        metadata_standard_version = "0.2",
        model_description = list(
          name = model_name,
          type = "process-based",
          repository = "https://github.com/GLEON/Bayes_forecast_WG/tree/eco_apps_release/4.1_JAGS_models" # update when code is released; could also make this specific to each model file (e.g. AR.R, precip.R, etc..)
        ),
        ## UNCERTAINTY CLASSES
        initial_conditions = list(
          # Possible values: no, contains, data_driven, propagates, assimilates
          uncertainty = initial_conditions,
          propagation = list(type = ifelse(initial_conditions == 'propagates',
                                           'ensemble', # this project is all ensemble forecasts
                                           NA),
                             size = ifelse(initial_conditions == 'propagates',
                                           Nmc, NA)), # required if ensemble
          # Number of parameters / dimensionality
          complexity = ic_complexity
        ),
        parameters = list(
          uncertainty = parameters,
          propagation = list(type = ifelse(parameters == 'propagates',
                                           'ensemble', # this project is all ensemble forecasts
                                           NA),
                             size = ifelse(parameters == 'propagates',
                                           Nmc, NA)), # required if ensemble
          complexity = param_complexity
        ),
        random_effects = list(
          uncertainty = random_effects,
          propagation = list(type = ifelse(random_effects == 'propagates',
                                           'ensemble', # this project is all ensemble forecasts
                                           NA),
                             size = ifelse(random_effects == 'propagates',
                                           Nmc, NA)), # required if ensemble
          complexity = random_complexity
        ),
        process_error = list(
          uncertainty = process_error,
          propagation = list(type = ifelse(process_error == 'propagates',
                                           'ensemble', # this project is all ensemble forecasts
                                           NA),
                             size = ifelse(process_error == 'propagates',
                                           Nmc, NA)), # required if ensemble
          complexity = process_complexity,
          covariance = covariance,
          localization = localization
        ),
        drivers = list(
          uncertainty = drivers,
          propagation = list(type = ifelse(drivers == 'propagates',
                                           'ensemble', # this project is all ensemble forecasts
                                           NA),
                             size = ifelse(drivers == 'propagates',
                                           Nmc, NA)), # required if ensemble
          complexity = driver_complexity
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
  EFIstandards::forecast_validator(eml_out)

  write_eml(eml_out, file = eml_file_name)
}

