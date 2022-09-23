# testing tidytransit and loading octranspo GTFS data
# for ONs data updates
# Avg. # of public transit stops within 600m walking distance
# Avg. # of public transit routes stopping within 600m walking distance
# Number of public transit stops
# Number of public transit stops per 1000 people
# Number of public transit stops per km2

library(tidyverse)
library(tidytransit)
library(leaflet)

gtfs <-tidytransit::read_gtfs("data/google_transit.zip")

class(gtfs)

# extract shapes, add back route info for colours
shp <- tidytransit::gtfs_as_sf(gtfs) %>%
  tidytransit::get_route_geometry() %>%
  left_join(gtfs$routes) %>%
  mutate(route_color = paste0("#", route_color))

# map o train
shp[1,] %>% leaflet() %>% addTiles() %>% addPolylines()

# map all routes
# shp %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolylines(color = ~ route_color, label = ~ route_id)


# get stops

stops <- gtfs$stops%>%
  rename(lat = stop_lat, lon = stop_lon)

stops_shp <- stops %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84", remove = FALSE)

stops %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(label = ~stop_name)

# stops per hood testing
z <- stops_shp %>%
  onsr::get_pts_neighbourhood()

z %>%
  group_by(ONS_ID, Name) %>%
  count(sort = TRUE)

# stittsville has most??
# it's big i guess?
z %>%
  filter(ONS_ID == "951") %>%
  ggplot() + geom_sf()

# est industrial has second most??
# ah east industrial is very large and includes hurdman station, many stops there
z %>%
  filter(ONS_ID == "917") %>%
  ggplot() + geom_sf()

#########
# try valhallr
# set up and run server
#docker run -dt --name valhalla_gis-ops -p 8002:8002 -v C:/Users/chris/Documents/large_shapefiles/osm_ontario:/custom_files ghcr.io/gis-ops/docker-valhalla/valhalla

test <- valhallr::route(from = stops[1,], to = stops[2,])

valhallr::map_trip(test)
valhallr::print_trip(test)



# which routes stop at which stops?
# get result for very first stop, sussex/rideau fall
gtfs$stop_times %>% filter(stop_id %in% gtfs$stops[1,]$stop_id) %>% left_join(gtfs$trips, by = "trip_id") %>% distinct(route_id)

# get # routes stopping at all stops
gtfs$stop_times %>%
  left_join(gtfs$trips, by = "trip_id") %>%
  group_by(stop_id) %>%
  distinct(route_id) %>%
  count(sort = TRUE)


# try distance simply
# THIS WORKS!
# NOTE: map length is greater than valhalla length because valhalla "snaps" to
# the road network, map length includes going off to the side to go up on the
# curb where the bus stop actually is
stops_shp_nad <- stops_shp %>%
  sf::st_transform(crs = 32189)

sf::st_distance(stops_shp_nad[1,], stops_shp_nad[2,])

sf::st_distance(stops_shp[1,], stops_shp[2,])

# quesiton: is NAd in metres? can we do distance with simple euclid?

a <- stops_shp_nad[1,]
b <- stops_shp_nad[2,]

bench::mark(sqrt((a$lat - b$lat)^2 + (a$lon-b$lon)^2))
bench::mark(sf::st_distance(stops_shp_nad[1,], stops_shp_nad[2,]))

stops_shp_nad %>%
  mutate(tempdist = sqrt((lat - a$lat)^2 +(lon - a$lon)^2 ) * 100000) %>%
  filter(tempdist < 1000)

# it's pretty fast to do spatial filtering with dplyr
bench::mark(stops_shp_nad %>%
              mutate(tempdist = sqrt((lat - a$lat)^2 +(lon - a$lon)^2 ) * 100000) %>%
              filter(tempdist < 1000)
)

# NO!! thats working with lat/lon still, try this
stops_nad <- stops_shp_nad
tempnad <- sf::st_coordinates(stops_nad)
stops_nad <- sf::st_set_geometry(stops_nad, NULL)
stops_nad$nadlat <- tempnad[,1]
stops_nad$nadlon <- tempnad[,2]

a <- stops_nad[1,]
stops_nad %>%
  mutate(tempdist = sqrt((nadlat - a$nadlat)^2 + (nadlon-a$nadlon)^2))

bench::mark(stops_nad %>%
              mutate(tempdist = sqrt((nadlat - a$nadlat)^2 + (nadlon-a$nadlon)^2)))

# base R...
bench::mark(sqrt((a$nadlat - stops_nad$nadlat)^2 + (a$nadlon - stops_nad$nadlon)^2))

bench::mark(sqrt((a$nadlat - stops_nad$nadlat)^2 + (a$nadlon - stops_nad$nadlon)^2) < 1000)


# THIS IS REAL FAST, 6772 itr/sec
bench::mark(stops_nad[sqrt((a$nadlat - stops_nad$nadlat)^2 + (a$nadlon - stops_nad$nadlon)^2) < 1000, ])

# CONCLusiiON: FOR FILTERING STOPS WITHIN DISTANCE, CONVERT TO NAD, DO EUCLIDEAN DISTANCE AND FILTER


# try buffering?
stops_shp[1,] %>%
  sf::st_buffer(600) %>%
  # sf::st_transform(crs = "WGS84") %>%
  #sf::st_simplify() %>%
  leaflet() %>% addTiles() %>% addPolygons()

# count how many stops within 600m of this stop, practicing for dbs
stops_shp[1,] %>%
  sf::st_buffer(600) %>%
  sf::st_filter(x=stops_shp, y=.) %>%
  count()

# not fast: this takes 70ms, ~15 itr/second.
bench::mark(stops_shp[1,] %>%
              sf::st_buffer(600) %>%
              sf::st_filter(x=stops_shp, y=.) %>%
              count()
)

bench::mark(stops_shp_nad[1,] %>%
              sf::st_buffer(600) %>%
              sf::st_filter(x=stops_shp_nad, y=.) %>%
              count()
)


# it's consistently faster for wgs84!!
bench::mark(stops_shp[1,] %>%
              sf::st_buffer(600) )

bench::mark(stops_shp_nad[1,] %>%
              sf::st_buffer(600) )



# load shapefiles from windows?
ott_dbs_shp <- sf::read_sf("/mnt/c/Users/chris/Documents/large_shapefiles/ottawa_dbs_and_centroids/ottawa_db_centroids_32189.shp")

ott_dbs_nad <-  dplyr::bind_cols(ott_dbs_shp, sf::st_coordinates(ott_dbs_shp)) %>%
  dplyr::rename(lat = X, lon = Y) %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::select(DBUID, lat, lon)





results <- dplyr::tribble(~DBUID , ~num_stops , ~stops, ~threshold )

threshold <- 700
i=1
for ( i in 1: nrow(ott_dbs_nad)){

  if (i %% 100 == 0) message(paste0(i, "        \r"), appendLF = FALSE)

  db <- ott_dbs_nad[i,]

  # way faster with base R: 44.9us (20600itr/s) vs 5.17ms (190itr/sec)
  # num_stops <- sum(sqrt((stops_nad$nadlat- db$lat)^2 + (stops_nad$nadlon - db$lon)^2)< threshold)
  #
  #
  # results[i,]$DBUID <- db$DBUID
  # results[i,]$num_stops <- num_stops
  # results[i,]$threshold <- threshold

  stops_index <- sqrt((stops_nad$nadlat- db$lat)^2 + (stops_nad$nadlon - db$lon)^2) < threshold
  stops <- stops_nad[stops_index,]$stop_id

  num_stops <- length(stops)# sum(stops_index)


  results[i,]$DBUID <- db$DBUID
  results[i,]$stops <- list(stops)
  results[i,]$num_stops <- num_stops
  results[i,]$threshold <- threshold
}
results


bench::mark(sum(sqrt((stops_nad$nadlat- db$lat)^2 + (stops_nad$nadlon - db$lon)^2)< threshold))



bench::mark(
 assign = { results[i,]$DBUID <- db$DBUID
  results[i,]$num_stops <- num_stops
  results[i,]$threshold <- threshold}
)

bench::mark(loop_dplyr = {
  if (i %% 100 == 0) message(paste0(i, "        \r"), appendLF = FALSE)

  db <- ott_dbs_nad[i,]

  stops_close <- stops_nad %>%
    dplyr::mutate(dist = sqrt((nadlat - db$lat)^2 + (nadlon - db$lon)^2)) %>%
    dplyr::filter(dist < threshold)

  num_stops <- nrow(stops_close)

  results[i,]$DBUID <- db$DBUID
  results[i,]$num_stops <- num_stops
  results[i,]$threshold <- threshold
  },
  loop_baser = {
    if (i %% 100 == 0) message(paste0(i, "        \r"), appendLF = FALSE)

    db <- ott_dbs_nad[i,]

    # stops_close <- stops_nad %>%
    #   dplyr::mutate(dist = sqrt((nadlat - db$lat)^2 + (nadlon - db$lon)^2)) %>%
    #   dplyr::filter(dist < threshold)
    #
    # num_stops <- nrow(stops_close)

    # way faster with base R: 44.9us (20600itr/s) vs 5.17ms (190itr/sec)

    stops_index <- sqrt((stops_nad$nadlat- db$lat)^2 + (stops_nad$nadlon - db$lon)^2) < threshold
    stops <- stops_nad[stops_index,]

    num_stops <- sum(stops_index)


    results[i,]$DBUID <- db$DBUID
    results[i,]$stops <- list(stops)
    results[i,]$num_stops <- num_stops
    results[i,]$threshold <- threshold
  })

bench::mark
