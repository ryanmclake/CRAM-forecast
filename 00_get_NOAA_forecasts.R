

download_noaa_files_s3 <- function(siteID, date, cycle, local_directory){
  
  Sys.setenv("AWS_DEFAULT_REGION" = "data",
             "AWS_S3_ENDPOINT" = "ecoforecast.org")
  
  object <- aws.s3::get_bucket("drivers", prefix=paste0("noaa/NOAAGEFS_1hr/",siteID,"/",date,"/",cycle))
  
  for(i in 1:length(object)){
    aws.s3::save_object(object[[i]], bucket = "drivers", file = file.path(local_directory, object[[i]]$Key))
  }
}
date = seq(from = as.Date("2021-03-01"), to = as.Date(Sys.Date()), by = "days")
cycle = c("00")

for(i in 1:length(date)){
  for(g in 1:length(cycle)){
  download_noaa_files_s3(siteID = "CRAM", 
                         date = date[i], 
                         cycle = cycle[g], 
                         local_directory <- "C:/Users/Owner/Documents/CRAM-forecast/NOAA_data/")

}
}

