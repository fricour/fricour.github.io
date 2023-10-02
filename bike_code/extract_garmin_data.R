library(FITfileR)
library(tidyverse)
library(dplyr)

# list fit files from Garmin Connect (downloaded through https://github.com/tcgoetz/GarminDB)
path_to_fit_files <- '~/HealthData/FitFiles/Activities/'
fit_files <- list.files(path_to_fit_files, pattern = "*.fit", full.names = TRUE)

# function that extracts fit files data (see https://msmith.de/FITfileR/articles/FITfileR.html)
extract_garmin_data <- function(file){
  
  # read file
  f <- readFitFile(file)
  
  # get file id
  #f_id <- file_id(f)$serial_number
  f_id <- unlist(stringr::str_split(file, "/"))
  f_id <- unlist(stringr::str_split(f_id[length(f_id)], '_'))[1]  
    
  print(f_id)
  
  # get activity type
  activity_type <- try(getMessagesByType(f, "sport")$sport[1])
  
  # check activity type
  if(class(activity_type) == 'try-error'){
    activity_type <- 'unknown'
  }
  
  data <- records(f) |> 
      bind_rows() |>
      mutate(id = f_id, activity_type = activity_type) |>
      arrange(timestamp())
  
  return(data)
}

# check error ('id')
# which(stringr::str_detect(fit_files, '2463451310') == TRUE)

# test
#toto <- map_dfr(fit_files[80:90], extract_garmin_data)

# extract all data
# all_activities <- map_dfr(fit_files, extract_garmin_data)
# vroom::vroom_write(all_activities, 'data/all_activities_270723.csv')
