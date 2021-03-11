in_situ_qaqc <- function(insitu_obs_fname,
                         data_location,
                         cleaned_observations_file_long,
                         lake_name_code,
                         config){
  
  print("QAQC Bouy Temperature")
  
  d <- temp_qaqc(realtime_file = insitu_obs_fname[1],
                          qaqc_file = insitu_obs_fname[2],
                          input_file_tz = "CST6CDT",
                          focal_depths,
                          local_tzone = config$local_tzone,
                          config = config)
}
