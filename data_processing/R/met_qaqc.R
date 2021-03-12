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
    
    d <- readr::read_csv(realtime_file)
  }
  
  d <- as.data.frame(d)
  
  
  # Fill in missing data (Used Amelia II instead of NLDAS)
  # This way all data are solely derived from the NEON data platforms
  
  #ShortWave
  amelia.sw <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
  sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
    select(time, ShortWave)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
  
  #LongWave
  amelia.lw <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
  lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
    select(time, LongWave)%>%
    group_by(time)%>%
    summarise_all(funs(mean))
  plot(lw_imputations$time, lw_imputations$LongWave)
  
  #AirTemp
  amelia.at <- amelia(d, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
  at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
    select(time, AirTemp)%>%
    group_by(time)%>%
    summarise_all(funs(mean))
  plot(at_imputations$time, at_imputations$AirTemp)
  
  #Himidity
  amelia.rh <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
  rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
    select(time, RelHum)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
  plot(rh_imputations$time, rh_imputations$RelHum)
  
  #Rain
  amelia.pr <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain")
  pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
    select(time, Rain)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(Rain = ifelse(Rain <= 0, 0, Rain))
  plot(pr_imputations$time, pr_imputations$Rain)
  
  #WindSpeed
  amelia.ws <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed")
  ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
    select(time, WindSpeed)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
  plot(ws_imputations$time, ws_imputations$WindSpeed)
  
  #Pressure
  amelia.p <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure")
  p_imputations <- bind_rows(unclass(amelia.p$imputations), .id = "m") %>%
    select(time, Pressure)%>%
    group_by(time)%>%
    summarise_all(funs(mean))
  plot(p_imputations$time, p_imputations$Pressure)
  
  
  imputed <- left_join(sw_imputations, lw_imputations, by = "time")%>%
    left_join(., at_imputations, by = "time")%>%
    left_join(., rh_imputations, by = "time")%>%
    left_join(., pr_imputations, by = "time")%>%
    left_join(., ws_imputations, by = "time")%>%
    left_join(., p_imputations, by = "time")
  
  
  met_new <- left_join(d, imputed, by = "time")
  
  d <- met_new %>%
    mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
    mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
    mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
    mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
    mutate(WindSpeed.x = ifelse(is.na(WindSpeed.x), WindSpeed.y, WindSpeed.x))%>%
    mutate(Rain.x = ifelse(is.na(Rain.x), Rain.y, Rain.x))%>%
    mutate(Pressure.x = ifelse(is.na(Pressure.x), Pressure.y, Pressure.x))%>%
    select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed.x, Rain.x, Pressure.x)%>%
    rename(surface_downwelling_shortwave_flux_in_air = ShortWave.x, 
           surface_downwelling_longwave_flux_in_air = LongWave.x, 
           air_temperature = AirTemp.x, 
           specific_humidity = RelHum.x, 
           wind_speed = WindSpeed.x, 
           precipitation_flux = Rain.x, 
           air_pressure = Pressure.x)
  
  
  d$time <- lubridate::force_tz(d$time, tzone = "CST6CDT") #input_file_tz
  
  
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
