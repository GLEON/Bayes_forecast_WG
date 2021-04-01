1_Get_data

Scripts to pull data from EDI data publications, re-format data, and calculate summary statistics for input to covariate correlation analysis

Guide to Rscripts:

1_Get_buoy_par: pulls data from the following publication

LSPA, K.C. Weathers, and B.G. Steele. 2020. High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019 ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/698e9ffb0cdcda81ecf7188bff54445e. Accessed 2020-05-09.

and calculates summary statistics for photosynthetically active radiation (PAR) data collected at Site 3 on Lake Sunapee using the Global Lake Ecological Observatory Network (GLEON) and Lake Sunapee Protective Association (LSPA) buoy

1_Get_buoy_wind: pulls data from the following publication

LSPA, K.C. Weathers, and B.G. Steele. 2020. High-Frequency Weather Data at Lake Sunapee, New Hampshire, USA, 2007-2019 ver 3. Environmental Data Initiative. https://doi.org/10.6073/pasta/698e9ffb0cdcda81ecf7188bff54445e. Accessed 2020-05-09.

and calculates summary statistics for wind data collected at Site 3 on Lake Sunapee using the Global Lake Ecological Observatory Network (GLEON) and Lake Sunapee Protective Association (LSPA) buoy

1_Get_gloeo_data: pulls data from the following publication

Cottingham, K.L., C.C. Carey, and K.C. Weathers. 2020. Gloeotrichia echinulata density at four nearshore sites in Lake Sunapee, NH, USA from 2005-2016 ver 2. Environmental Data Initiative. https://doi.org/10.6073/pasta/b6f418436088b14666a02467797ff1ad. Accessed 2020-05-09.

and re-formats G. echinulata density data for input to covariate correlation analysis

1_Get_NLDAS_solar_radiation: pulls data from the following staged data publication

Lofton, M.E., J.A. Brentrup, W.S. Beck, J.A. Zwart, R. Bhattacharya, L.S. Brighenti, S.H. Burnet, I.M. McCullough, B.G. Steele, C.C. Carey, K.L. Cottingham, M.C. Dietze, H.A. Ewing, K.C. Weathers, and S.L. LaDeau. 2021. Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016 ver 5. Environmental Data Initiative. https://doi.org/DOI_PLACE_HOLDER (Accessed 2021-04-01).

and calculates summary statistics for solar radiation data downloaded for Lake Sunapee from the North American Land Data Assimilation System (https://ldas.gsfc.nasa.gov/nldas)

1_Get_onset_watertemp: pulls data from the following publication

Cottingham, K.L., C.C. Carey, and K.C. Weathers. 2020. High-frequency temperature data from four near-shore sites, Lake Sunapee, NH, USA, 2006-2018 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/3e325757f0e981d91cd297f257f05f55 (Accessed 2021-04-01).

and calculates summary statistics for water temperature data collected at nearshore Sites 1 and 2 on Lake Sunapee using Onset water temperature loggers

1_Get_PRISM_precip: pulls data from the following staged data publication

Lofton, M.E., J.A. Brentrup, W.S. Beck, J.A. Zwart, R. Bhattacharya, L.S. Brighenti, S.H. Burnet, I.M. McCullough, B.G. Steele, C.C. Carey, K.L. Cottingham, M.C. Dietze, H.A. Ewing, K.C. Weathers, and S.L. LaDeau. 2021. Lake Sunapee Gloeotrichia echinulata density near-term hindcasts from 2015-2016 and meteorological model driver data, including shortwave radiation and precipitation from 2009-2016 ver 5. Environmental Data Initiative. https://doi.org/DOI_PLACE_HOLDER (Accessed 2021-04-01).

and calculates summary statistics for precipitation daily summaries downloaded for Lake Sunapee from the Parameter-elevation Relationships on Independent Slopes Model (PRISM) model (http://www.prism.oregonstate.edu)

1_Get_schmidt_stability: pulls data from the following publication

Richardson, D.C., C.C. Carey, K.C. Weathers, D.A. Bruesewitz, LSPA, and B.G. Steele. 2020. High Frequency Meteorological, Drift-Corrected Dissolved Oxygen, and Thermistor Temperature Data - Lake Sunapee Buoy, NH, USA, 2007 – 2013 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/846a36bf6fd704e508511e5f8a2ab3b5. Accessed 2020-05-09.

and calculates Schimdt stability and summary statistics thereof using a string of temperature loggers deployed at Site 3 on Lake Sunapee using the Global Lake Ecological Observatory Network (GLEON) and Lake Sunapee Protective Association (LSPA) buoy

Sunapee_LMP_GLEON_gloeositesv3.R: pulls data from the following DOI_PLACE_HOLDER

Bethel G Steele, Kathleen C Weathers, & Lake Sunapee Protective Association. (2021). Quality controlled in situ data from multiple locations in Lake Sunapee, NH, USA from the Lake Sunapee Protective Association's Long-term Monitoring Program, 1986-2020 (Version v2020.1) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.4652076

and calculates mean total phosphorus and Secchi depth in the surface water of Lake Sunapee