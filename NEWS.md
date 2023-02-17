# palaeoverse (development version)

* Added get_timescale_data and removed deeptime dependency
* Updated font colors in GTS2012 and GTS2020 based on background luminance

# palaeoverse 1.1.1

* palaeoverse now requires deeptime (>= 1.0.0)
* Added additional test to catch when Zenodo is offline (palaeorotate)
* Added a coords argument to the tax_range_space function to control whether coordinates are returned
* palaeorotate documentation updated to reflect updates to GPlates Web Service (#74)
* MULLER2022 plate rotation model made available in palaeorotate (#74)
* model argument in palaeorotate now allows multiple models to be called - palaeogeographic uncertainty is now calculated on the requested models
* max distance calculations in palaeorotate's uncertainty functionality updated to use geosphere::distGeo

# palaeoverse 1.1.0
Updates

* Added package-level documentation function
* bin_time now accepts user-defined probability function (#20)
* lat_bins now accepts user-defined latitudinal range (#23)
* bin_lat now has functionality for handling boundary occurrences (#23)
* tax_unique now supports arbitrary higher taxonomic levels (#52)
* time_bins now supports user input dataframes (#19)
* tax_unique now supports returning the unique names appended to the input dataframe (#51)

Minor bug fixes

* Updated palaeorotate reconstruction files to use an hexagonal equal-area grid (#59)
* Added error handling for when GPlates and Zenodo is offline
* Added default value for the round argument in palaeorotate
* Resolved issue with chunk processing in palaeorotate
* Fixed the behavior of time_bins(scale = "GTS2012") (#62)
* Fixed tests when suggested packages were not installed (#64)
* Removed dependence of tests on divDyn and deeptime (#64)
* Fixed tax_range_time example (#60)
* Fixed look_up issue when handling pre-Cambrian occurrences (#63)
* tax_unique now retains homonyms from different higher taxonomic groups (#50)
* Fixed axis_geo for phylogenies (#68)

# palaeoverse 1.0.0
First full release of the palaeoverse R package.
The package provides functions to help streamline analyses and improve code readability and reproducibility.
