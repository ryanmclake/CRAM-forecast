#download.file("https://github.com/cwida/duckdb/releases/download/master-builds/duckdb_r_src.tar.gz", destfile = "./duckdb_r_src.tar.gz")
#install.packages("duckdb_r_src.tar.gz", repo = NULL)

remotes::install_github("cboettig/neonstore", force = T)
remotes::install_github("eco4cast/EFIstandards", force = T)
remotes::install_github("FLARE-forecast/flare", force = T)
devtools::install_github("rqthomas/noaaGEFSpoint", force = T)

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, VIM, naniar, missMDA, Amelia, mice, FactoMineR, broom, aws.s3)

# Set up the directories
local_directory <- file.path(getwd(), "data", "NOAA_data")
lake_directory <- getwd()
noaa_data_location <- file.path(getwd(),"data","NOAA_data","noaa","NOAAGEFS_1hr")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
files_noaa <- list.files(noaa_data_location)
date = seq(from = lubridate::as_date("2020-09-25"), to = lubridate::as_date(Sys.Date()-2), by = "days")
cycle = c("00")
siteID_noaa = "CRAM"
siteID_neon = c("CRAM","UNDE")

source(file.path(lake_directory, "data_download/NOAA_downloads.R"))

if(last(files_noaa) == as.Date(Sys.Date()-1)){
  "You have all the NOAA data my friend"
}else{
  for(i in 1:length(date)){
    for(g in 1:length(cycle)){
      download_noaa_files_s3(siteID = siteID_noaa,
                             date = date[i], 
                             cycle = cycle[g], 
                             local_directory <- local_directory)
    }
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NEON DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

neonstore::neon_dir()
Sys.setenv("NEONSTORE_HOME" = "C:/Users/Owner/Desktop/CRAM-forecast/data/neonstore/")

# Lake and tower met station download
met_products = c("DP1.00098.001", "DP1.00002.001", "DP1.00023.001", "DP1.00006.001", "DP1.00001.001", "DP1.00004.001")

# Download newest met products
neonstore::neon_download(product = met_products, site = siteID_neon)

# Store the NEON met data products
neonstore::neon_store("SECPRE_30min-basic")
neonstore::neon_store("2DWSD_30min-basic")
neonstore::neon_store("SLRNR_30min-basic")
neonstore::neon_store("SAAT_30min-basic")
neonstore::neon_store("RH_30min-basic")
neonstore::neon_store("BP_30min-basic")


# Airtemp
airtemp <- neonstore::neon_table(table = "SAAT_30min-basic", site = "CRAM") %>%
  select(endDateTime, tempSingleMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Radiation
radiation <- neonstore::neon_table(table = "SLRNR_30min-basic", site = "CRAM") %>%
  select(endDateTime, inSWMean, inLWMean) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("inSWMean", "inLWMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Humidity
humidity <- neonstore::neon_table(table = "RH_30min-basic", site = "CRAM") %>% 
  select(endDateTime, RHMean)%>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Precipitation
precip  <- neonstore::neon_table(table = "SECPRE_30min-basic", site = "UNDE") %>%
  select(endDateTime, secPrecipBulk) %>%
  mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
  select(-endDateTime)%>%
  group_by(time) %>%
  summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
  arrange(time)%>%
  mutate(time = time - 6*3600)

# Wind Speed
windspeed <- neonstore::neon_table(table = "2DWSD_30min-basic", site = "CRAM")%>%  
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

write_csv(met_target, "./data/met_data_w_gaps.csv")


# Lake water temperature
buoy_products = c("DP1.20264.001")

# Download newest buoy  products
neonstore::neon_download(product = buoy_products, site = "CRAM")

# Store the NEON buoy data products
neonstore::neon_store("TSD_30_min-basic")

# Water temperature by depth
# ----------------------------------------------------------------------------------------
water_temp <- neonstore::neon_table(table = "TSD_30_min-basic", site = "CRAM")%>% 
  select(endDateTime, thermistorDepth, tsdWaterTempMean) %>%
  arrange(endDateTime, thermistorDepth)%>%
  rename(depth = thermistorDepth)%>%
  rename(value = tsdWaterTempMean)%>%
  rename(timestamp = endDateTime)%>%
  mutate(variable = "temperature",
         method = "thermistor",
         value = ifelse(is.nan(value), NA, value))%>%
  select(timestamp, depth, value, variable, method)

write_csv(water_temp, "./data/temp_data.csv")