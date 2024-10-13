library(tidyverse)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(leafgl)

# read all activities
all_activities <- vroom::vroom('data/all_activities_13102024.csv')

# compute distance for each ride
cycling_stats <- all_activities |>
  filter(activity_type == 'cycling' | activity_type == 'unknown') |>
  group_by(id) |>
  summarise(full_distance = max(distance)/1000) # m -> km
  
print(sum(cycling_stats$full_distance))
  
# compute yearly distance
yearly_stats <- all_activities |>
  filter(activity_type == 'cycling' | activity_type == 'unknown') |>
  mutate(year = lubridate::year(timestamp)) |>
  group_by(id) |>
  summarise(full_distance = max(distance)/1000, year = unique(year)) |>
  ungroup() |>
  group_by(year) |>
  summarise(yearly_distance = sum(full_distance))

data.table::fwrite(yearly_stats, "data/yearly_distance_cycling.csv")
#print(sum(cycling_id_to_keep$full_distance))

min_km <- 50
id_to_keep <- cycling_stats |>
  filter(full_distance >= 50)
  
# keep cycling activities and hiking activities
cycling <- all_activities |> filter(activity_type == 'cycling' | activity_type == 'unknown') |>
  filter(id %in% id_to_keep$id) |>
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
map <- leaflet() |> addTiles()

# Loop through the dataframe to add polylines for each id
for (unique_id in unique(cycling$id)) {
  polyline_data <- cycling[cycling$id == unique_id,]
  polyline_data <- polyline_data |> arrange(time)
  map <- addPolylines(
    map,
    lng = polyline_data$lon,
    lat = polyline_data$lat,
    group = as.character(unique_id)  # Use the id as the group name
  )
}

# Create LineString objects for each 'id'
lines_list <- cycling |>
  group_by(id) |>
  arrange(time) |> # needed to avoid issues with trajectories (occurred due to (sometimes) several records in a single fit file)
  summarize(geometry = st_sfc(st_linestring(cbind(lon, lat))))

# Convert the summarized data to an sf dataframe with spatial lines
lines_sf <- st_as_sf(lines_list)

# save lines_sf
st_write(lines_sf, "data/cycling.geojson")
