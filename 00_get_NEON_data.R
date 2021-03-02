download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "./duckdb_r_src.tar.gz")
install.packages("duckdb_r_src.tar.gz", repo = NULL)


remotes::install_github("cboettig/neonstore", force = T)


if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, VIM, naniar, missMDA, Amelia, mice, FactoMineR, broom)

# -----------------------------------------------------------------------------------------------------------------

#' Find and set the neonstore directory
neonstore::neon_dir()
Sys.setenv("NEONSTORE_HOME" = "/groups/rqthomas_lab/neonstore_cram")
neonstore::neon_dir()

# Lake and tower met station download
met_products = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001", "DP1.00006.001", "DP1.00001.001")

# Download newest met products
neonstore::neon_download(product = met_products, site = c("CRAM","UNDE"))

# Store the NEON met data products
neonstore::neon_store("SECPRE_30min-expanded")
neonstore::neon_store("2DWSD_30min-expanded")
neonstore::neon_store("SLRNR_30min-expanded")
neonstore::neon_store("SAAT_30min-expanded")
neonstore::neon_store("RH_30min-expanded")

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


met_target <- full_join(radiation, airtemp, by = "time")%>%
  full_join(., humidity, by = "time")%>%
  full_join(., windspeed, by = "time")%>%
  full_join(., precip, by = "time")%>%
  rename(ShortWave = inSWMean, LongWave = inLWMean, AirTemp = tempSingleMean,
         RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk)%>%
  mutate(Rain = Rain*0.024)%>%
  mutate(ShortWave = ifelse(ShortWave<=0,0,ShortWave))%>%
  filter(time >= "2018-08-06")
met_target <- as.data.frame(met_target)


# Fill in missing data

#ShortWave
amelia.sw <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave")
sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
  select(time, ShortWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

#LongWave
amelia.lw <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave")
lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
  select(time, LongWave)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

#AirTemp
amelia.at <- amelia(met_target, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp")
at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
  select(time, AirTemp)%>%
  group_by(time)%>%
  summarise_all(funs(mean))

#Himidity
amelia.rh <- amelia(met_target, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum")
rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
  select(time, RelHum)%>%
  group_by(time)%>%
  summarise_all(funs(mean))%>%
  mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

imputed <- left_join(sw_imputations, lw_imputations, by = "time")%>%
  left_join(., at_imputations, by = "time")%>%
  left_join(., rh_imputations, by = "time")

met_new <- left_join(met_target, imputed, by = "time")

met_qaqc <- met_new %>%
  mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
  mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
  mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
  mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
  select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed, Rain)%>%
  rename(ShortWave = ShortWave.x, LongWave = LongWave.x, AirTemp = AirTemp.x, RelHum = RelHum.x)


write_csv(met_qaqc, "./qaqc_data/cram_met_obs.csv")



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

