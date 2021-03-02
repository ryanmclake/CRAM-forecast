# set your WD paths for CRAM FLARE

lake_directory <- "/home/ryan333/CRAM-forecast"
qaqc_data_location <- file.path(lake_directory, "qaqc_data")
data_location <- "/home/ryan333/CRAM-forecast"
noaa_data_location <- "/home/ryan333/CRAM-forecast/NOAA_data/noaa/NOAAGEFS_1hr"
if(!dir.exists(qaqc_data_location)){dir.create(qaqc_data_location)}


# See configured YAML file for CRAMPTON --> Different from FCR!
config <- yaml::read_yaml(file.path(lake_directory,"data_processing", "observation_processing.yml"))

config$data_location <- data_location
config$noaa_data_location <- noaa_data_location

if(!file.exists(file.path(config$data_location, config$realtime_met_station_location))){
  stop("Missing temperature data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$realtime_temp_location))){
  stop("Missing met station data GitHub repo")
}
if(!file.exists(file.path(config$noaa_data_location, config$noaa_location))){
  stop("Missing NOAA forecast GitHub repo")
}


setwd(file.path(config$data_location, config$realtime_met_station_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$realtime_temp_location))
system(paste0("git pull"))

setwd(file.path(config$noaa_data_location, config$noaa_location))
system(paste0("git pull"))

