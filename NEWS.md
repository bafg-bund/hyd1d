# hyd1d

* add the Elbe estuary to the gauging_station_data table in the pg database
* use `station_int` within waterLevelFlys3Seq to initialize the returned wldf

# hyd1d 0.4.4

* Import validated gauging data for 2022.
* Fix SSL error within `getPegelonlineW` for R < 4.2.0.

# hyd1d 0.4.3

* Remove the package 'ROracle' from suggests.
* Remove the package 'RCurl' from imports.

# hyd1d 0.4.2

* Added a `NEWS.md` file to track changes to the package.
* Silence failure of df.gauging_data-download and update during package loading.
* Shorten package title.
* Changes to incorporate availability on CRAN.
* Move  README figure to `man/figures`.
* Adapt the newly required `bibentry` synthax in `inst/CITATION`.
