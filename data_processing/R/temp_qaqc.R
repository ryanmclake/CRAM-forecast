temp_qaqc <- function(realtime_file,
                               qaqc_file,
                               maintenance_file,
                               input_file_tz,
                               focal_depths,
                               local_tzone,
                               config){

  d <- readr::read_csv(realtime_file)
  
 
  d_therm <- d %>% dplyr::rename(timestamp = timestamp,
                                 depth = depth,
                                 value = value) %>%
    dplyr::mutate(variable = "temperature",
                  method = "thermistor",
                  value = ifelse(is.nan(value), NA, value))
  
  d <- d_therm %>% mutate(depth = as.numeric(depth))
  
  write_csv(d, paste0(config$qaqc_data_location,"/observations_postQAQC_long.csv"))
}
