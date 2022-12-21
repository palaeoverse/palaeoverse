# palaeoverse development version
Updates

* Added package-level documentation function
* bin_time now accepts user-defined probability weights

Minor bug fixes

* Updated palaeorotate reconstruction files to use an hexagonal equal-area grid.
* Added error handling for when GPlates and Zenodo is offline.
* Added default value for the round argument in palaeorotate. 
* Resolved issue with chunk processing in palaeorotate.
* Fixed the behavior of time_bins(scale = "GTS2012") (#62)
* Fixed tests when suggested packages were not installed (#64)
* Removed dependence of tests on divDyn and deeptime
* Fixed tax_range_time example (#60)
* Fixed look_up issue when handling pre-Cambrian occurrences.

# palaeoverse 1.0.0
First full release of the palaeoverse R package.
The package provides functions to help streamline analyses and improve code readability and reproducibility.
