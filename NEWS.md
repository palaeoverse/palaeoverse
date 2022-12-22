# palaeoverse development version
Updates

* Added package-level documentation function
* lat_bins now accepts user-defined latitudinal range (#23)
* bin_lat now has functionality for handling boundary occurrences (#23)
* tax_unique now supports arbitrary higher taxonomic levels (#52)

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
