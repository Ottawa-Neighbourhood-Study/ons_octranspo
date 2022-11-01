
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ottawa Neighbourhood Study OCTranspo data refresh

This project refreshes the Ottawa Neighbourhood Study’s (ONS) data about
OCTranspo availability in Ottawa’s neighbourhoods, using ONS Gen2
neighbourhoods.

It uses the R package **targets**, which enables a reproducible
scientific workflow. As written it requires access to a local Valhalla
instance (e.g. through the gisops/valhalla Docker image).

Transit data is from the [OCTranspo official GTFS
feed](https://www.octranspo.com/en/plan-your-trip/travel-tools/developers/),
updated as of August 22, 2022.

Final results are saved in the `output_final` folder in several formats.
