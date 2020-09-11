
# notes on EFI forecast standards

# see document here: https://docs.google.com/document/d/1rEV2qI1u4l3gjzy8-x3gjFChsd5SIWTUL6aWhPWYdr8/edit
# Following the Climate and Forecast (CF) convention, the order of dimensions for all three formats is T, Z, Y, X, E where T is time, Z, Y, and X are spatial dimensions, and E is ensemble member. In general forecasts issued at different dates or times should be stored in separate files, and thus the time dimension is the time being predicted. If multiple forecasts are placed within a single file then the issue time is the first time dimension and then the time being predicted is second.



# practice data
hindcast_matrix = read.csv('edi.18.3/Gechinulata_hindcasts/Gechinulata_hindcasts/AR_IC.Pa.P.O_2015-05-14.csv', stringsAsFactor = F)
Nmc = 7500
forecast_issue_time = as.Date('2015-05-07')
forecast_id = uuid::UUIDgenerate()
forecast_project_id = 'GLEON_Bayes_forecast_WG_Gloeo_uncertainty_partition_20200909'
nc_name_out = 'AR_IC.Pa.P.O_2015-05-14.nc'

library(ncdf4)
library(tidyverse)

#' Function to organize model output arrays into netcdf
#' @param hindcast_matrix matrix of hindcast output; currently forecasting out 4 weeks
#' @param Nmc number of draws / ensembles
#' @param forecast_issue_time datetime when forecast was issued
#' @param forecast_id unique to each forecast generated
#' @param forecast_project_id id that is unique to the collection of forecasts
#' @param nc_name_out name of the ncdf file
#'
matrix_to_ncdf = function(hindcast_matrix,
                          Nmc,
                          forecast_issue_time,
                          forecast_id,
                          forecast_project_id,
                          nc_name_out){

  hindcast_df <- select_if(hindcast_matrix, ~sum(!is.na(.)) > 0)  %>% # getting rid of NA columns
    as_tibble()

  n_valid_times <- ncol(hindcast_df) # number of forecast weeks
  # need to check if forecast issue times and week_1 are the same or week_1 7 days in the future
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

  # could add in Z, Y, X variables here too (Z would be depth (1 m), Y would be longitude, X would be latitude)
  def_list[[2]] <- ncvar_def(name =  'Gloeo_abundance',
                             units = 'colonies L-1',
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
            attval = as.character(forecast_id),
            prec =  "text")
  ncatt_put(nc = ncout,
            varid = 0,
            attname = "forecast_issue_time",
            attval = as.character(forecast_issue_time),
            prec =  "text")
  nc_close(ncout)

}

# practice data
model_out_nc_file = 'AR_IC.Pa.P.O_2015-05-14.nc'
eml_file_name = 'AR_IC.Pa.P.O_2015-05-14.eml.xml'
#
create_forecast_eml = function(model_out_nc_file,
                               eml_file_name){

  model_out <- nc_open(model_out_nc_file)

  dates <- ncvar_get(nc = model_out, varid = 'time')

  forecast_issue_time <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_issue_time')$value

  forecast_project_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_project_id')$value

  forecast_id <- ncatt_get(nc = model_out, varid = 0, attname = 'forecast_iteration_id')$value

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
                             col_classes = c("Date", "numeric",
                                             "numeric", "Date",
                                             "numeric", "character", "character"))
  physical <- set_physical(model_out_nc_file)

  dataTable <- eml$dataTable(
    entityName = model_out_nc_file,
    entityDescription = 'Lake Sunapee G. echinulata forecasts',
    physical = physical,
    attributeList = attrList)

  me <- list(individualName = list(givenName = "Mary",
                                   surName = "Lofton"),
             electronicMailAddress = "melofton@vt.edu",
             id = 'https://orcid.org/0000-0003-3270-1330')

  coverage <- EML::set_coverage(begin = lubridate::as_datetime(dates[1]),
                                end = lubridate::as_datetime(tail((dates)[1])),
                                geographicDescription = "Lake Sunapee",
                                west = -72.0836, east = -72.0314,  # could read in geo boundary and set this with function
                                north = 43.4323, south = 43.3238,
                                altitudeMaximum = 333, altitudeMinimum = 333, altitudeUnits = 'meter')

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
  methods = list(methods = 'methods go here')
  ###########

  dataset = eml$dataset(title = 'Lake Sunapee G. echinulata forecasts',
                        creator = me,
                        contact = list(references = 'https://orcid.org/0000-0003-3270-1330'),
                        pubDate = forecast_issue_time,
                        intellectualRights = '?',
                        abstract = 'EML metadata for G. echinulata forecasts',
                        dataTable = dataTable,
                        keywordSet = keywordSet,
                        coverage = coverage,
                        methods = methods)

  eml_out <- eml$eml(dataset = dataset,
                     packageId = forecast_project_id,
                     system = "uuid")

  ### validate the EML
  eml_validate(eml_out)

  write_eml(eml_out, file = eml_file_name)
}

