met_qaqc <- function(realtime_file,
                     qaqc_file,
                     cleaned_met_file_dir,
                     input_file_tz,
                     local_tzone){
  
  if(is.na(qaqc_file)){
    d1 <- readr::read_csv(realtime_file)
    d1$time <- lubridate::force_tz(d1$time, tzone = "CST6CDT") #input_file_tz
    
    d2 <- readr::read_csv(qaqc_file)
    d2$time <- lubridate::force_tz(d2$time, tzone = "CST6CDT") #input_file_tz

    d1 <- data.frame(time = d1$time, ShortWave = d1$surface_downwelling_shortwave_flux_in_air, LongWave = d1$surface_downwelling_longwave_flux_in_air, AirTemp = d1$air_temperature, RelHum = d1$specific_humidity, WindSpeed = d1$wind_speed, Rain = d1$precipitation_flux, pressure = d1$air_pressure)
    d2 <- data.frame(time = d2$time, ShortWave = d2$surface_downwelling_shortwave_flux_in_air, LongWave = d2$surface_downwelling_longwave_flux_in_air, AirTemp = d2$air_temperature, RelHum = d2$specific_humidity, WindSpeed = d2$wind_speed, Rain = d2$precipitation_flux, pressure = d2$air_pressure)
    
    d1 <- d1[which(d1$time > d2$time[nrow(d2)] | d1$time < d2$time[1]), ]
    
    d <- rbind(d2, d1)
    
  }else{
    
    d1 <- readr::read_csv(realtime_file)
    d1$time <- lubridate::force_tz(d1$time, tzone = "CST6CDT") #input_file_tz
    
    d <- d1
  }
  
  
  wshgt <- 3
  roughlength <- 0.000114
  maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
  minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
  
  d <- d %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                           relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                           relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                           relative_humidity = specific_humidity / 100,
                           air_temperature = air_temperature + 273.15,
                           surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                           wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                           wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
    filter(is.na(time) == FALSE)
  
  
  d$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = d$relative_humidity,
                                                  T = d$air_temperature,
                                                  press = d$air_pressure)
  
  d <- d %>%select(time, 
                   air_temperature, 
                   air_pressure, 
                   relative_humidity, 
                   surface_downwelling_longwave_flux_in_air, 
                   surface_downwelling_shortwave_flux_in_air, 
                   precipitation_flux, specific_humidity, 
                   wind_speed)
  
  cf_var_names1 <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air",
                     "surface_downwelling_shortwave_flux_in_air", "precipitation_flux","specific_humidity","wind_speed")
  
  cf_var_units1 <- c("K", "Pa", "1", "Wm-2", "Wm-2", "kgm-2s-1", "1", "ms-1")  #Negative numbers indicate negative exponents
  
  d <- d %>%
    tidyr::drop_na()
  
  
  model_name <- "observed-met"
  site <- "cram"
  lat <- 46.209675
  lon <- 360-89.473688
  start_time <- dplyr::first((d$time))
  end_time <- dplyr::last((d$time))
  cf_units <- cf_var_units1
  
  identifier <- paste(model_name, site,sep="_")
  
  fname <- paste0(identifier,".nc")
  
  output_file <- file.path(cleaned_met_file_dir, fname)
  
  start_time <- min(d$time)
  end_time <- max(d$time)
  
  data <- d %>%
    dplyr::select(-time)
  
  diff_time <- as.numeric(difftime(d$time, d$time[1], units = "hours"))
  
  cf_var_names <- names(data)
  
  time_dim <- ncdf4::ncdim_def(name="time",
                               units = paste("hours since", format(start_time, "%Y-%m-%d %H:%M")),
                               diff_time, #GEFS forecast starts 6 hours from start time
                               create_dimvar = TRUE)
  lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
  lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", lon, create_dimvar = TRUE)
  
  dimensions_list <- list(time_dim, lat_dim, lon_dim)
  
  nc_var_list <- list()
  for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
    nc_var_list[[i]] <- ncdf4::ncvar_def(cf_var_names[i], cf_units[i], dimensions_list, missval=NaN)
  }
  
  nc_flptr <- ncdf4::nc_create(output_file, nc_var_list, verbose = FALSE, )
  
  #For each variable associated with that ensemble
  for (j in 1:ncol(data)) {
    # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
    ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], unlist(data[,j]))
  }
  
  ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
  
}
