download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "./duckdb_r_src.tar.gz")
install.packages("duckdb_r_src.tar.gz", repo = NULL)

remotes::install_github("cboettig/neonstore")
remotes::install_github("eco4cast/EFIstandards", force = T)
remotes::install_github("rqthomas/flare", force = T)

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, VIM, naniar, missMDA, Amelia, mice, FactoMineR, broom)

# -----------------------------------------------------------------------------------------------------------------

#' Find and set the neonstore directory
neonstore::neon_dir()
Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore_cram")
neonstore::neon_dir()

# Lake and tower met station download
met_products = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001", "DP1.00006.001", "DP1.00001.001", "DP1.00004.001")

# Download newest met products
neonstore::neon_download(product = met_products, site = c("CRAM","UNDE"))


# Store the NEON met data products
neonstore::neon_store("SECPRE_30min-expanded")
neonstore::neon_store("2DWSD_30min-expanded")
neonstore::neon_store("SLRNR_30min-expanded")
neonstore::neon_store("SAAT_30min-expanded")
neonstore::neon_store("RH_30min-expanded")
neonstore::neon_store("BP_30min-basic")

# Meteorological target data
# ----------------------------------------------------------------------------------------

# Airtemp
airtemp <- neonstore::neon_table(table = "SAAT_30min-expanded", site = "CRAM") %>%
  select(endDateTime, tempSingleMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Radiation
radiation <- neonstore::neon_table(table = "SLRNR_30min-expanded", site = "CRAM") %>%
  select(endDateTime, inSWMean, inLWMean) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("inSWMean", "inLWMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Humidity
humidity <- neonstore::neon_table(table = "RH_30min-expanded", site = "CRAM") %>% 
  select(endDateTime, RHMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Precipitation
precip  <- neonstore::neon_table(table = "SECPRE_30min-expanded", site = "UNDE") %>%
  select(endDateTime, secPrecipBulk) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Wind Speed
windspeed <- neonstore::neon_table(table = "2DWSD_30min-expanded", site = "CRAM")%>%  
  select(endDateTime, windSpeedMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Pressure
pressure <- neonstore::neon_table(table = "BP_30min-basic", site = "CRAM") %>%
  select(endDateTime, staPresMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("staPresMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)


met_target <- full_join(radiation, airtemp, by = "time")%>%
  full_join(., humidity, by = "time")%>%
  full_join(., windspeed, by = "time")%>%
  full_join(., precip, by = "time")%>%
  full_join(., pressure, by = "time")%>%
  rename(ShortWave = inSWMean, LongWave = inLWMean, AirTemp = tempSingleMean,
         RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk, Pressure = staPresMean)%>%
  mutate(Rain = Rain*0.024)%>%
  mutate(Pressure = Pressure*1000)%>%
  mutate(ShortWave = ifelse(ShortWave<=0,0,ShortWave))%>%
  filter(time >= "2018-08-06")
met_target <- as.data.frame(met_target)


# Fill in missing data (Used Amelia II instead of ImputeTS)

#ShortWave
amelia.sw <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
plot(sw_imputations$time, sw_imputations$ShortWave)

#LongWave
amelia.lw <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))
plot(lw_imputations$time, lw_imputations$LongWave)

#AirTemp
amelia.at <- amelia(met_target, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))
plot(at_imputations$time, at_imputations$AirTemp)

#Himidity
amelia.rh <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
plot(rh_imputations$time, rh_imputations$RelHum)

#Rain
amelia.pr <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain")
pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
  select(time, Rain)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(Rain = ifelse(Rain <= 0, 0, Rain))
plot(pr_imputations$time, pr_imputations$Rain)

#WindSpeed
amelia.ws <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed")
ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
  select(time, WindSpeed)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
plot(ws_imputations$time, ws_imputations$WindSpeed)

#Pressure
amelia.p <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure")
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
  

met_new <- left_join(met_target, imputed, by = "time")

met_qaqc <- met_new %>%
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

cf_var_names1 <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air",
                   "surface_downwelling_shortwave_flux_in_air", "precipitation_flux","specific_humidity","wind_speed")

cf_var_units1 <- c("K", "Pa", "1", "Wm-2", "Wm-2", "kgm-2s-1", "1", "ms-1")  #Negative numbers indicate negative exponents

model_name <- "observed-met"
site <- "cram"
lat <- 46.209675
lon <- 360-89.473688
start_time <- dplyr::first((met_qaqc$time))
end_time <- dplyr::last((met_qaqc$time))
cf_units <- cf_var_units1


identifier <- paste(model_name, site,sep="_")

fname <- paste0(identifier,".nc")

output_file <- file.path(config$qaqc_data_location, fname)

start_time <- min(met_qaqc$time)
end_time <- max(met_qaqc$time)

data <- met_qaqc %>%
  dplyr::select(-time)

diff_time <- as.numeric(difftime(met_qaqc$time, met_qaqc$time[1], units = "hours"))

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








# Lake water temperature
buoy_products = c("DP1.20264.001")

# Download newest buoy  products
neonstore::neon_download(product = buoy_products, site = "CRAM")

# Store the NEON buoy data products
neonstore::neon_store("TSD_30_min-expanded")

# Water temperature by depth
# ----------------------------------------------------------------------------------------
water_temp <- neonstore::neon_table(table = "TSD_30_min-expanded", site = "CRAM")%>% 
  select(endDateTime, thermistorDepth, tsdWaterTempMean) %>%
  arrange(endDateTime, thermistorDepth)%>%
  rename(Depth = thermistorDepth)%>%
  rename(temp = tsdWaterTempMean)%>%
  mutate(variable = "watertemperature")

