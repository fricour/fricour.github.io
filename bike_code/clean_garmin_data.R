library(tidyverse)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(leafgl)

# read all activities
all_activities <- vroom::vroom('data/all_activities_270723.csv')

# keep cycling activities and hiking activities
cycling <- all_activities |> filter(activity_type == 'cycling' | activity_type == 'unknown') |>
  select(time = timestamp, lat = position_lat, lon = position_long, altitude, temperature, id) |>
  drop_na(lat) |>
  drop_na(lon)

hiking <- all_activities |> filter(activity_type == 'hiking') |>
  select(time = timestamp, lat = position_lat, lon = position_long, altitude, temperature, id) |>
  drop_na(lat) |>
  drop_na(lon)

# clean hiking data from outliers 
cycling <- cycling |> filter(lat < 179 | lon < 179)
hiking <- hiking |> filter(lat < 179 | lon < 179)

# # Create a new leaflet map
# map <- leaflet() |> addTiles()
# 
# # Loop through the dataframe to add polylines for each id
# for (unique_id in unique(cycling$id)) {
#   polyline_data <- cycling[cycling$id == unique_id,]
#   polyline_data <- polyline_data |> arrange(time)
#   map <- addPolylines(
#     map,
#     lng = polyline_data$lon,
#     lat = polyline_data$lat,
#     group = as.character(unique_id)  # Use the id as the group name
#   )
# }

# Create LineString objects for each 'id'
lines_list <- cycling %>% 
  #filter(time < "2018-06-01") %>%
  group_by(id) %>%
  arrange(time) %>% # needed to avoid issues with trajectories (occurred due to (sometimes) several records in a single fit file)
  summarize(geometry = st_sfc(st_linestring(cbind(lon, lat))))

# Convert the summarized data to an sf dataframe with spatial lines
lines_sf <- st_as_sf(lines_list)

# save lines_sf
st_write(lines_sf, "data/cycling.shp")
st_write(lines_sf, "data/cycling.geojson")

# leaflet() %>%
#   addProviderTiles(provider = providers$CartoDB.Positron) %>%
#   fitBounds(lng1 = 0, lat1 = 40, lng2 = 10, lat2 = 50) %>%
#   addGlPolylines(data = toto)

