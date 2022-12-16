#testing gtfs route geometry intersection

z <- tidytransit::get_route_geometry(tidytransit::gtfs_as_sf(gtfs))

bb_xmin = -75.71
bb_xmax = -75.66
bb_ymin = 45.40
bb_ymax = 45.43

z %>%
  ggplot() +
  ggspatial::annotation_map_tile() +
  geom_sf(aes(colour = route_id)) +
  theme(legend.position = "none") +

  coord_sf(xlim = c(xmin = bb_xmin,
                    xmax = bb_xmax),
           ylim = c(ymin = bb_ymin,
                    ymax = bb_ymax))


bbox_polygon <- sf::st_polygon(list(rbind(c(bb_xmin,bb_ymin), c(bb_xmax,bb_ymin), c(bb_xmax,bb_ymax), c(bb_xmin,bb_ymax), c(bb_xmin,bb_ymin)))) %>%
  sf::st_geometry() %>%
  sf::st_as_sf(crs = "WGS84")


z %>%
  ggplot() +
  ggspatial::annotation_map_tile(zoomin = -1) +
  geom_sf() +
  geom_sf(data = bbox_polygon, fill = NA, colour = "blue", size = 3) +
  labs(title = "All 188 OCTranspo routes",
       subtitle = "March 2020")

z %>%
  sf::st_filter(bbox_polygon) %>%
  ggplot() +
  ggspatial::annotation_map_tile(zoomin = -1) +
  geom_sf() +
  geom_sf(data = bbox_polygon, fill = NA, colour = "blue", size = 3) +
  labs(title = "50 OCTranspo routes intersection downtown area (blue)",
       subtitle = "March 2020")


z %>%
  filter(!sf::st_intersects(., bbox_polygon, sparse = FALSE)) %>%
  ggplot() + geom_sf() +geom_sf(data = bbox_polygon, colour = "blue")

sf::st_crop(z, xmin = bb_xmin,
            xmax = bb_xmax,
            ymin = bb_ymin,
            ymax = bb_ymax) %>%
  ggplot() +
  ggspatial::annotation_map_tile() +
  geom_sf(aes(colour = route_id)) +
  theme(legend.position = "none") #+

  coord_sf(xlim = c(xmin = bb_xmin,
                    xmax = bb_xmax),
           ylim = c(ymin = bb_ymin,
                    ymax = bb_ymax))
