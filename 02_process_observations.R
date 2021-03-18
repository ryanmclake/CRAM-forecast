config <- yaml::read_yaml(file.path(lake_directory,"data_processing","observation_processing.yml"))

config$data_location <- "./data"
config$qaqc_data_location <- "./qaqc_data"


library(tidyverse)
library(lubridate)

source(file.path(lake_directory, "data_processing/R/met_qaqc.R"))
source(file.path(lake_directory, "data_processing/R/temp_qaqc.R"))
source(file.path(lake_directory, "data_processing/R/in_situ_qaqc.R"))

if(is.null(config$met_file)){
  met_qaqc(realtime_file = file.path(config$data_location, config$met_raw_obs_fname[1]),
           qaqc_file = file.path(config$data_location, config$met_raw_obs_fname[2]),
           cleaned_met_file_dir = config$qaqc_data_location,
           input_file_tz = "CST6CDT",
           local_tzone = config$local_tzone)
}else{
  file.copy(file.path(config$data_location,config$met_file), cleaned_met_file, overwrite = TRUE)
}

cleaned_observations_file_long <- paste0(config$qaqc_data_location,"/observations_postQAQC_long.csv")

if(is.null(config$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config$data_location,config$insitu_obs_fname),
               data_location = config$data_location,
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name_code = config$lake_name_code,
               config = config)
}else{
  file.copy(file.path(config$data_location,config$combined_obs_file), cleaned_met_file_dir, overwrite = TRUE)
}
