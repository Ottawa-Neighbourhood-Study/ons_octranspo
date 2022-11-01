library(targets)
library(tarchetypes)

# tell targets which packages we need to load to do the analysis
tar_option_set(packages = c("tidyverse",
                            "tidytransit",
                            "leaflet",
                            "sf"
))


list(
  #################################### -
  ## LOAD DATA ----
  tar_target(ons_shp, onsr::ons_shp),
  tar_target(ons_shp_50mbuffer,
             sf::st_buffer(sf::st_transform(ons_shp, crs = 32189), 50)),
  tar_target(ons_data, onsr::get_ons_data()),
  tar_target(gtfs_file,"data/google_transit.zip", format = "file"),
  tar_target(gtfs, tidytransit::read_gtfs(gtfs_file)),
  tar_target(stops, dplyr::rename(gtfs$stops, lat = stop_lat, lon = stop_lon)),
  tar_target(stops_nad, {
    stops %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = "WGS84") %>%
      sf::st_transform(crs = 32189) %>%
      sf::st_coordinates() %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(dplyr::select(stops, stop_id)) %>%
      dplyr::left_join(select(stops, stop_id, lat, lon),  by = "stop_id")
  }),
  tar_target(stops_nad_shp, sf::st_as_sf(stops_nad, coords = c("X","Y"), crs = 32189)),

  # load shapefiles from windows. This gives DB centroids within Ottawa
  tar_target(ott_dbs_shp, sf::read_sf("/mnt/c/Users/chris/Documents/large_shapefiles/ottawa_dbs_and_centroids/ottawa_db_centroids_32189.shp")),
  tar_target(ott_dbs_shp_whole, sf::read_sf("/mnt/c/Users/chris/Documents/large_shapefiles/ottawa_dbs_and_centroids/ottawa_dbs.shp") %>%
               sf::st_transform(crs = 32189)),
  tar_target(ott_dbs_buffer, sf::st_buffer(ott_dbs_shp_whole, dist = 1000)),
  # want wgs84 and nad. nad lets us calculate metres distance, wgs84 for valhalla
  tar_target(ott_dbs_wgs84, {
    sf::st_transform(ott_dbs_shp, crs = "WGS84") %>%
      sf::st_coordinates() %>%
      tibble::as_tibble() %>%
      dplyr::rename(lon = X, lat = Y) %>%
      dplyr::bind_cols(dplyr::select(sf::st_set_geometry(ott_dbs_shp, NULL), DBUID))
  }),

  tar_target(ott_dbs_nad, {
    sf::st_coordinates(ott_dbs_shp) %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(dplyr::select(sf::st_set_geometry(ott_dbs_shp, NULL), DBUID)) %>%
      dplyr::left_join(ott_dbs_wgs84, by = "DBUID")
  }),


  ####################### -
  # DISTANCE ANALYSIS ----
  # for each db, find octranspo stops within 1 km.
  # doing this with a loop, faster to just get it done
  tar_target(dbs_stops_close_buffer, {
    results <- list()
    # for each db
    for (i in 1:nrow(ott_dbs_buffer)) {
      message(i)
      db <- ott_dbs_buffer[i,] %>% dplyr::select(DBUID)

      intersects <- sf::st_set_geometry(sf::st_filter(stops_nad_shp, db), NULL)
      intersects$DBUID <- db$DBUID

      results[[i]] <- intersects
    }

    tibble::enframe(results) %>%
      tidyr::unnest(value) %>%
      dplyr::select(-name)


  }),

  tar_target(dbs_stops_walkdist, {

    dbs_stops <- tidyr::nest(dbs_stops_close_buffer, stops_close = -DBUID) %>%
      dplyr::left_join(ott_dbs_nad, by = "DBUID")

    results <- vector(mode = "list", length = nrow(dbs_stops))

    # get number of walkable stops from each db using a silly loop

    for (i in 1:nrow(dbs_stops)){
      message(i)
      db <- dbs_stops[i,]

      db_from <- dplyr::select(db, lon, lat, DBUID)
      db_stops <- db$stops_close[[1]]

      # only do calcluations if there are any stops
      if (nrow(db_stops) > 0){
        distances <- valhallr::od_table(froms = db_from, from_id_col = "DBUID",
                                        tos = db_stops, to_id_col = "stop_id",
                                        costing = "pedestrian", verbose = TRUE,
                                        hostname = "localhost", port = "8002")

        results[[i]] <- distances

      } # end if there are any stops

    } # end of silly loop

    results %>%
      tibble::enframe() %>%
      tidyr::unnest(value) %>%
      dplyr::select(-name)

  }),


  #############################################-
  # Avg. # of public transit stops within 600m walking distance----
  ## group dbs into neighbourhoods, find avg # of stops within walking distance within hoods
  tar_target( transit_numS, {
    num_db_stops <- dbs_stops_walkdist %>%
      dplyr::filter(distance < 0.6) %>%
      dplyr::group_by(DBUID) %>%
      dplyr::count() %>%
      dplyr::left_join(onsr::db_to_ons_data, ., by = "DBUID") %>%
      dplyr::mutate(n = tidyr::replace_na(n, 0)) %>%
      dplyr::left_join(dplyr::mutate(onsr::ottawa_db_pops_2016, DBUID = as.character(DBUID)), by = "DBUID")

    hoods <- num_db_stops%>%
      dplyr::group_by(ONS_ID) %>%
      dplyr::summarise(value = sum(n * db_pop_2016) / sum(db_pop_2016)) %>%
      dplyr::left_join(sf::st_set_geometry(onsr::ons_shp, NULL), .,  by = "ONS_ID") %>%
      dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
      dplyr::mutate(measure = "transit_numS", .before = 1)

    ottawa <- num_db_stops %>%
      dplyr::summarise(value = sum(n * db_pop_2016) / sum(db_pop_2016)) %>%
      dplyr::mutate(measure= "transit_numS", .before = 1) %>%
      dplyr::mutate(ONS_ID = 0, .before = 1) %>%
      dplyr::mutate(Name = "Ottawa", Name_FR = "Ottawa")

    transit_numS <- dplyr::bind_rows(ottawa, hoods)

    transit_numS
  }),

  #############################################-
  # Avg. # of public transit routes stopping within 600m walking distance----
  tar_target(transit_numR, {
    #
    # # get routes stopping at each stop in nested tibble
    stop_routes <- gtfs$stop_times %>%
      dplyr::left_join(gtfs$trips, by = "trip_id") %>%
      dplyr::group_by(stop_id) %>%
      dplyr::distinct(route_id) %>%
      tidyr::nest(routes = route_id )


    # get # routes stopping at all stops
    stop_numroutes <- gtfs$stop_times %>%
      dplyr::left_join(gtfs$trips, by = "trip_id") %>%
      dplyr::group_by(stop_id) %>%
      dplyr::distinct(route_id) %>%
      dplyr::count(sort = TRUE)


    dbs_num_routes <- dbs_stops_walkdist %>%
      dplyr::filter(distance < 0.6)  %>%
      dplyr::select(DBUID, stop_id) %>%
      dplyr::left_join(stop_routes, by = "stop_id")  %>%
      tidyr::unnest(routes) %>%
      dplyr::distinct(DBUID, route_id) %>%
      dplyr::group_by(DBUID) %>%
      dplyr::count(name = "num_routes") %>%
      dplyr::ungroup()


    hoods <- onsr::db_to_ons_data %>%
      dplyr::left_join(dplyr::mutate(onsr::ottawa_db_pops_2016, DBUID = as.character(DBUID)), by = "DBUID") %>%
      dplyr::left_join(dbs_num_routes, by = "DBUID")  %>%
      dplyr::mutate(num_routes = tidyr::replace_na(num_routes, 0)) %>%
      dplyr::group_by(ONS_ID) %>%
      dplyr::summarise(value = sum(num_routes * db_pop_2016) / sum(db_pop_2016)) %>%
      #dplyr::summarise(value = mean(num_routes)) %>%
      dplyr::left_join(sf::st_set_geometry(onsr::ons_shp, NULL), .,  by = "ONS_ID") %>%
      dplyr::mutate(measure= "transit_numR")

    ottawa <- onsr::db_to_ons_data %>%
      dplyr::left_join(dplyr::mutate(onsr::ottawa_db_pops_2016, DBUID = as.character(DBUID)), by = "DBUID") %>%
      dplyr::left_join(dbs_num_routes, by = "DBUID")  %>%
      dplyr::mutate(num_routes = tidyr::replace_na(num_routes, 0)) %>%
      #group_by(ONS_ID) %>%
      dplyr::summarise(value = sum(num_routes * db_pop_2016) / sum(db_pop_2016)) %>%
      dplyr::mutate(measure= "transit_numR", .before = 1) %>%
      dplyr::mutate(ONS_ID = 0, .before = 1) %>%
      dplyr::mutate(Name = "Ottawa", Name_FR = "Ottawa")

    num_routes_within_600m_walk <- dplyr::bind_rows(ottawa, hoods)

    num_routes_within_600m_walk
  }),


  ################################-
  # Number of public transit stops within neighbourhood boundaries----
  tar_target(transit_count, {

    stops <- gtfs$stops

    hoods <- stops %>%
      sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = "WGS84") %>%
      sf::st_transform(crs = 32189) %>%
      onsr::get_pts_neighbourhood(pgon = ons_shp_50mbuffer) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::group_by(ONS_ID, Name) %>%
      dplyr::count(name = "value") %>%
      dplyr::left_join(sf::st_set_geometry(onsr::ons_shp, NULL), ., by = c("ONS_ID", "Name")) %>%
      dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
      dplyr::mutate(measure= "transit_count", .before = 1)

    ott_shp <- sf::st_union(ons_shp_50mbuffer)

    ottawa <- stops %>%
      sf::st_as_sf(coords = c("stop_lon", "stop_lat"), crs = "WGS84") %>%
      sf::st_transform(crs = 32189) %>%
      sf::st_filter(ons_shp_50mbuffer) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::count(name = "value") %>%
      dplyr::mutate(ONS_ID = 0, .before = 1) %>%
      dplyr::mutate(Name = "Ottawa", Name_FR = "Ottawa") %>%
      dplyr::mutate(measure= "transit_count", .before = 1)



    transit_count <- dplyr::bind_rows(ottawa, hoods)
  }),


  #########################-
  # Number of public transit stops per 1000 people----
  tar_target(transit_count1000, {


    pop2016 <- ons_data %>%
      filter(polygon_attribute == "pop2016") %>%
      select(ONS_ID, pop2016 = value) %>%
      mutate(ONS_ID = as.integer(ONS_ID))

    transit_count1000 <- transit_count %>%
      dplyr::left_join(pop2016, by = "ONS_ID") %>%
      dplyr::mutate(value= value/pop2016 * 1000) %>%
      dplyr::mutate(measure = "transit_count1000") %>%
      dplyr::select(-pop2016)

    transit_count1000
  }),


  #####################################-
  # Number of public transit stops per km2----
  tar_target(transit_countsqkm, {

    area <- ons_data %>%
      dplyr::filter(polygon_attribute == "area") %>%
      dplyr::select(ONS_ID, area = value) %>%
      dplyr::mutate(ONS_ID = as.integer(ONS_ID))

    transit_countsqkm <- transit_count %>%
      dplyr::left_join(area, by = "ONS_ID") %>%
      dplyr::mutate(value= value/area) %>%
      dplyr::mutate(measure = "transit_countsqkm") %>%
      dplyr::select(-area)

    transit_countsqkm
  }),

  tar_target(all_outputs_long,
             dplyr::bind_rows(transit_numS, transit_numR, transit_count, transit_count1000, transit_countsqkm) ),

  tar_target(all_outputs_wide, {
    test <- all_outputs_long %>%
      dplyr::select(polygon_attribute = measure,
                    value,
                    ONS_ID) %>%
      dplyr::arrange(ONS_ID) %>%
      tidyr::pivot_wider(names_from = ONS_ID, values_from = value)

             }),

  targets::tar_target(compare_last_time,{
    ons_data %>%
      dplyr::filter(polygon_attribute %in% c(unique(all_outputs_long$measure))) %>%
      dplyr::select(measure = polygon_attribute, ONS_ID, value_old = value) %>%
      dplyr::mutate(ONS_ID = as.numeric(ONS_ID)) %>%
      dplyr::left_join(all_outputs_long, by = c("ONS_ID", "measure") )%>%
      dplyr::mutate(value_diff = value-value_old,
                    value_diff_pct = value_diff/value_old)

  }),

  targets::tar_target(save_results,{
    readr::write_csv(all_outputs_long, sprintf("output_final/octranspo_results_long-%s.csv", Sys.Date()))
    readr::write_csv(all_outputs_wide, sprintf("output_final/octranspo_results_wide-%s.csv", Sys.Date()))
    readr::write_csv(compare_last_time, sprintf("output_final/octranspo_diffs-%s.csv", Sys.Date()))
  }),


  NULL

)
