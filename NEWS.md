# palaeoverse 1.4.0

* Several updates to palaeorotate
  * Mantle reference frame models removed
  * TorsvikCocks2017 model added
  * Added updated reconstruction files (1 myr resolution)
* Fixed point plotting bug in tax_range_strat
* Added lat_bins_area (#114)
* Replaced lat_bins with lat_bins_degrees
* Made all functions tibble-friendly (#118)

# palaeoverse 1.3.0

* Added plot customisability to tax_range_time (#99)
* Update palaeorotate to handle updates with GPlates API (#112)
* New function to create stratigraphic range plots, tax_range_strat (#97)
* Added age arguments (max_ma/min_ma) to bin_time (#106)
* Added warning to bin_time (#104)
* Improved link accessibility (#88)
* Fix superfluous columns in palaeorotate (#110)

# palaeoverse 1.2.1

* Fixed handling of multiple models in palaeorotate (#92)
* Fixed custom ticks behavior for axis_geo_phylo
* Added ability to use custom bins for tax_expand_time (#94)

# palaeoverse 1.2.0

* time_bins updated to enable users to call geological time scales from Macrostrat
* Fixed minor documentation error in tax_range_time
* Fixed unnecessary column output in palaeorotate (#78)
* Fixed binding issues with the "point" method in palaeorotate for some specific datasets (#78)
* Removed deeptime dependency
* Updated font colors in GTS2012 and GTS2020 based on background luminance
* Fixed multi-model call in palaeorotate for the "point" method which did not return all requested model coordinates (#82)
* Fixed an erroneous entry in interval_key (#84)
* Added two tutorial vignettes (#31)
* Fixed a tax_range_space bug for custom column names

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
