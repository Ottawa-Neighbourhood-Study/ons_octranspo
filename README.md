
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ottawa Neighbourhood Study OCTranspo data refresh

This project refreshes the Ottawa Neighbourhood Study’s (ONS) data about
OCTranspo availability in Ottawa’s neighbourhoods, using ONS Gen2
neighbourhoods.

It uses the R package **targets**, which enables a reproducible
scientific workflow. As written it requires access to a local Valhalla
instance (e.g. through the gisops/valhalla Docker image).

Transit data is from the [OCTranspo official GTFS
feed](https://www.octranspo.com/en/plan-your-trip/travel-tools/developers/),
updated as of November 5, 2022.

Final results are saved in the `output_final` folder in several formats.

# Variable definitions

| polygon_attribute | Name                                                                   | Method of calculation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|-------------------|------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| transit_numS      | Avg. \# of public transit stops within 600m                            | Transit stops are collected from OCTranspo’s GTFS feed. Walk distances are calculated from each dissemination block (DB) to all transit stops within a 1km radius, and for each DB the number of stops within 600m is counted. DB-level results are averaged up to neighbourhood-level results using population-weighting (2016 census results) and assigned to the neighbourhoods they maximally overlap.                                                                                                                               |
| transit_numR      | Avg. \# of public transit routes stopping within 600m walking distance | Transit stops are collected from OCTranspo’s GTFS feed. Walk distances are calculated from each dissemination block (DB) to all transit stops within a 1km radius, and for each DB the number of unique routes within 600m is counted. For clarity, if bus route 88 has five stops within 600m of a given DB, these five stops will only count as one route. DB-level results are averaged up to neighbourhood-level results using population-weighting (2016 census results) and assigned to the neighbourhoods they maximally overlap. |
| transit_count     | Number of public transit stops                                         | Number of transit stops within each neighbourhood plus a 50m buffer.                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| transit_count1000 | Number of public transit stops per 1000 people                         | Number of transit stops within each neighbourhood plus a 50m buffer, per 1000 neighbourhood residents.                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| transit_countsqkm | Number of public transit stops per km2                                 | Number of transit stops within each neighbourhood plus a 50m buffer, per square kilometer.                                                                                                                                                                                                                                                                                                                                                                                                                                               |
