# testing tidytransit and loading octranspo GTFS data
# for ONs data updates
# Avg. # of public transit stops within 600m walking distance
# Avg. # of public transit routes stopping within 600m walking distance
# Number of public transit stops
# Number of public transit stops per 1000 people
# Number of public transit stops per km2

# ORIGINAL DATA FORMAT:
# polygon_attribute category3
# <chr>             <chr>
# 1 transit_numS      Avg. # of public transit stops within 600m
# 2 transit_numR      Avg. # of public transit routes stopping within 600m
# 3 transit_count     Number of public transit stops
# 4 transit_count1000 Number of public transit stops per 1000 people
# 5 transit_countsqkm Number of public transit stops per km2


library(tidyverse)
library(tidytransit)
library(leaflet)

##############################
# Set up OCTranspo data ----

gtfs <- tidytransit::read_gtfs("data/google_transit.zip")

# get stops and put in NAD format

stops <- gtfs$stops %>%
  rename(lat = stop_lat, lon = stop_lon)

stops_nad <- stops %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  sf::st_transform(crs = 32189) %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  bind_cols(dplyr::select(stops, stop_id)) %>%
  left_join(select(stops, stop_id, lat, lon),  by = "stop_id")

# get dissemination blocks (DBs) and put in NAD format

# load shapefiles from windows. This gives DB centroids within Ottawa
ott_dbs_shp <- sf::read_sf("/mnt/c/Users/chris/Documents/large_shapefiles/ottawa_dbs_and_centroids/ottawa_db_centroids_32189.shp")

# want wgs84 and nad. nad lets us calculate metres distance, wgs84 for valhalla
ott_dbs_wgs84 <- sf::st_transform(ott_dbs_shp, crs = "WGS84") %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  rename(lon = X, lat = Y) %>%
  bind_cols(dplyr::select(sf::st_set_geometry(ott_dbs_shp, NULL), DBUID))

ott_dbs_nad <- sf::st_coordinates(ott_dbs_shp) %>%
  as_tibble() %>%
  bind_cols(dplyr::select(sf::st_set_geometry(ott_dbs_shp, NULL), DBUID)) %>%
  left_join(ott_dbs_wgs84, by = "DBUID")



# for each db, find octranspo stops within 1 km.

# get stops close to each db for walk distance calculations
# only takes a minute or two
dbs_stops <- ott_dbs_nad %>%
  mutate(stops_close = purrr::map2(X, Y, function(dbX, dbY){
    dplyr::mutate(stops_nad,
                  xdiff = X - dbX,
                  ydiff = Y - dbY,
                  dist = sqrt(xdiff^2 + ydiff ^2)) %>%
      dplyr::filter(dist < 1000)

  })
  )



####################-
# Walkability calcs using Valhalla ----
# we only run the analysis if the file doesn't exist. Note you need a running
# Valhalla instance to do this!
if (!file.exists("data/db_octranspostops_within_600m.csv")) {
i = 1
results_dbuids <- character(length = nrow(db_stops))
results_numstops <- integer(length = nrow(db_stops))
# get number of walkable stops from each db using a silly loop
for (i in 1:nrow(dbs_stops)){
  message(i)
  db <- dbs_stops[i,]

  db_stops <- db$stops_close[[1]]

  if (nrow(db_stops) > 0){
  distances <- valhallr::od_table(froms = db, from_id_col = "DBUID",
                                  tos = db_stops, to_id_col = "stop_id",
                                  costing = "pedestrian", verbose = TRUE,
                                  hostname = "localhost", port = "8002")

  close_stops <- distances %>%
    filter(distance < 0.6) %>%
    nrow()
  } else {
    close_stops <- 0
  }


  results_dbuids[[i]] <- db$DBUID
  results_numstops[[i]] <- close_stops
} # end of silly loop

db_stops_within_600m <- dplyr::tibble(DBUID = results_dbuids,
                         num_stops = results_numstops)

# save to file in case anything goes wrong!!!
db_stops_within_600m %>%
  write_csv("data/db_octranspostops_within_600m.csv")
} else {
db_stops_within_600m <- read_csv("data/db_octranspostops_within_600m.csv")
}


#############################################-
# Avg. # of public transit stops within 600m walking distance----
## group dbs into neighbourhoods, find avg # of stops within walking distance within hoods
num_stops_within_600m_walk <- db_stops_within_600m %>%
  mutate(DBUID = as.character(DBUID)) %>%
  left_join(onsr::db_to_ons_data, by = "DBUID") %>%
  group_by(ONS_ID) %>%
  summarise(value = mean(num_stops)) %>%
  mutate(measure= "transit_numS") %>%
  left_join(sf::st_set_geometry(onsr::ons_shp, NULL), by = "ONS_ID") %>%
  mutate(value = replace_na(value, 0))

num_stops_within_600m_walk_ott <- db_stops_within_600m %>%
  mutate(DBUID = as.character(DBUID)) %>%
  left_join(onsr::db_to_ons_data, by = "DBUID") %>%
  #group_by(ONS_ID) %>%
  summarise(value = mean(num_stops)) %>%
  mutate(measure= "transit_numS", .before = 1) %>%
  mutate(ONS_ID = 0, .before = 1) %>%
  mutate(Name = "Ottawa", Name_FR = "Ottawa")

num_stops_within_600m_walk <- num_stops_within_600m_walk_ott %>%
  bind_rows(num_stops_within_600m_walk) %>%
  write_csv("outputs/num_stops_within_600m_walk.csv")


#############################################-
# Avg. # of public transit routes stopping within 600m walking distance----


# get routes stopping at each stop in nested tibble
stop_routes <- gtfs$stop_times %>%
  left_join(gtfs$trips, by = "trip_id") %>%
  group_by(stop_id) %>%
  distinct(route_id) %>%
  nest(routes = route_id )


# get # routes stopping at all stops
stop_numroutes <- gtfs$stop_times %>%
  left_join(gtfs$trips, by = "trip_id") %>%
  group_by(stop_id) %>%
  distinct(route_id) %>%
  count(sort = TRUE)


temp <- dbs_stops %>%
  select(DBUID, stops_close) %>%
  unnest(stops_close) %>%
  select(DBUID, stop_id) %>%
  left_join(stop_routes, by = "stop_id")  %>%
  unnest(routes) #%>%
  #group_by(DBUID, route_id) #

dbs_num_routes <- temp %>%
  distinct(DBUID, route_id) %>%
  group_by(DBUID) %>%
  summarise(num_routes = n(), .groups = "drop")

num_routes_within_600m_walk <- onsr::db_to_ons_data %>%
  left_join(dbs_num_routes, by = "DBUID")  %>%
  mutate(num_routes = replace_na(num_routes, 0)) %>%
  group_by(ONS_ID) %>%
  summarise(value = mean(num_routes)) %>%
  mutate(measure= "transit_numR") %>%
  left_join(sf::st_set_geometry(onsr::ons_shp, NULL), by = "ONS_ID")

num_routes_within_600m_walk_ott <- onsr::db_to_ons_data %>%
  left_join(dbs_num_routes, by = "DBUID")  %>%
  mutate(num_routes = replace_na(num_routes, 0)) %>%
  #group_by(ONS_ID) %>%
  summarise(value = mean(num_routes)) %>%
  mutate(measure= "transit_numR", .before = 1) %>%
  mutate(ONS_ID = 0, .before = 1) %>%
  mutate(Name = "Ottawa", Name_FR = "Ottawa")

num_routes_within_600m_walk <- num_routes_within_600m_walk_ott %>%
  bind_rows(num_routes_within_600m_walk) %>%
  write_csv("outputs/num_routes_within_600m_walk.csv")


################################-
# Number of public transit stops within neighbourhood boundaries----

num_octranspo_stops <- stops %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  onsr::get_pts_neighbourhood(pgon = onsr::ons_shp) %>%
  sf::st_set_geometry(NULL) %>%
  group_by(ONS_ID, Name) %>%
  count(name = "value") %>%
  mutate(measure= "transit_count")

ott_shp <- sf::st_union(onsr::ons_shp)

num_octranspo_stops_ott <- stops %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
  sf::st_filter(ott_shp) %>%
  sf::st_set_geometry(NULL) %>%
  count(name = "value") %>%
  mutate(ONS_ID = 0, .before = 1) %>%
  mutate(Name = "Ottawa", Name_FR = "Ottawa") %>%
  mutate(measure= "transit_count", .before = 1)



num_octranspo_stops <- num_octranspo_stops_ott %>%
  bind_rows(num_octranspo_stops) %>%
  write_csv("outputs/num_octranspo_stops.csv")

#########################-
# Number of public transit stops per 1000 people----

ons_data <- onsr::get_ons_data()

pop2016 <- ons_data %>%
  filter(polygon_attribute == "pop2016") %>%
  select(ONS_ID, pop2016 = value) %>%
  mutate(ONS_ID = as.integer(ONS_ID))

octranspo_stops_per_100_residents <- num_octranspo_stops %>%
  left_join(pop2016, by = "ONS_ID") %>%
  mutate(value= value/pop2016 * 1000) %>%
  mutate(measure = "transit_count1000") %>%
  select(-pop2016)

octranspo_stops_per_100_residents %>%
  write_csv("outputs/octranspo_stops_per_100_residents.csv")

#####################################-
# Number of public transit stops per km2----

area <- ons_data %>%
  filter(polygon_attribute == "area") %>%
  select(ONS_ID, area = value) %>%
  mutate(ONS_ID = as.integer(ONS_ID))

octranspo_stops_per_km2 <- num_octranspo_stops %>%
  left_join(area, by = "ONS_ID") %>%
  mutate(value= value/area) %>%
  mutate(measure = "transit_countsqkm") %>%
  select(-area)

octranspo_stops_per_km2 %>%
  write_csv("outputs/octranspo_stops_per_km2.csv")


###################### -
# COMBINE ALL----

wide_data <- list.files(path = "outputs") %>%
  paste0("outputs/", .) %>%
  purrr::map(readr::read_csv) %>%
  enframe() %>%
  unnest(value) %>%
select(-name) %>%
  select(-Name, -Name_FR) %>%
  pivot_wider(names_from = ONS_ID, values_from = value, values_fill = 0)

######################### -
# Save final wide data----
wide_data %>%
  mutate(across(where(is.numeric), round, digits = 2)) %>%
  write_csv(sprintf("output_final/ons_transpo_update-%s.csv", Sys.Date()))
