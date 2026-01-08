# Package index

## Data cleaning and preparing

Functions for cleaning and preparing occurrence or taxonomic data for
downstream analyses.

- [`tax_check()`](https://palaeoverse.palaeoverse.org/reference/tax_check.md)
  : Taxonomic spell check
- [`phylo_check()`](https://palaeoverse.palaeoverse.org/reference/phylo_check.md)
  : Check phylogeny tip names
- [`tax_unique()`](https://palaeoverse.palaeoverse.org/reference/tax_unique.md)
  : Filter occurrences to unique taxa
- [`palaeorotate()`](https://palaeoverse.palaeoverse.org/reference/palaeorotate.md)
  : Palaeorotate fossil occurrences
- [`look_up()`](https://palaeoverse.palaeoverse.org/reference/look_up.md)
  : Look up geological intervals and assign geological stages
- [`tax_expand_lat()`](https://palaeoverse.palaeoverse.org/reference/tax_expand_lat.md)
  : Generate pseudo-occurrences from latitudinal range data
- [`tax_expand_time()`](https://palaeoverse.palaeoverse.org/reference/tax_expand_time.md)
  : Generate pseudo-occurrences from temporal range data

## Data binning

Functions for binning occurrence data.

- [`bin_lat()`](https://palaeoverse.palaeoverse.org/reference/bin_lat.md)
  : Assign fossil occurrences to latitudinal bins
- [`bin_space()`](https://palaeoverse.palaeoverse.org/reference/bin_space.md)
  : Assign fossil occurrences to spatial bins
- [`bin_time()`](https://palaeoverse.palaeoverse.org/reference/bin_time.md)
  : Assign fossil occurrences to time bins

## Data summarisation

Functions to summarise occurrence-level data.

- [`tax_range_space()`](https://palaeoverse.palaeoverse.org/reference/tax_range_space.md)
  : Calculate the geographic range of fossil taxa
- [`tax_range_time()`](https://palaeoverse.palaeoverse.org/reference/tax_range_time.md)
  : Calculate the temporal range of fossil taxa

## Data visualization

Functions to aid in the visualisation of data and analysis results.

- [`axis_geo()`](https://palaeoverse.palaeoverse.org/reference/axis_geo.md)
  [`axis_geo_phylo()`](https://palaeoverse.palaeoverse.org/reference/axis_geo.md)
  : Add an axis with a geological timescale
- [`tax_range_strat()`](https://palaeoverse.palaeoverse.org/reference/tax_range_strat.md)
  : Generate a stratigraphic section plot

## Auxiliary functions

Functions to support further functionality.

- [`lat_bins()`](https://palaeoverse.palaeoverse.org/reference/lat_bins.md)
  **\[deprecated\]** : Generate equal-width latitudinal bins
- [`lat_bins_area()`](https://palaeoverse.palaeoverse.org/reference/lat_bins_area.md)
  : Generate equal-area latitudinal bins
- [`lat_bins_degrees()`](https://palaeoverse.palaeoverse.org/reference/lat_bins_degrees.md)
  : Generate equal-width latitudinal bins
- [`time_bins()`](https://palaeoverse.palaeoverse.org/reference/time_bins.md)
  : Generate time bins
- [`group_apply()`](https://palaeoverse.palaeoverse.org/reference/group_apply.md)
  : Apply a function over grouping(s) of data

## Built-in data

palaeoverse comes with a selection of built-in datasets that are used in
examples to illustrate various use cases.

- [`GTS2012`](https://palaeoverse.palaeoverse.org/reference/GTS2012.md)
  : Geological Timescale 2012
- [`GTS2020`](https://palaeoverse.palaeoverse.org/reference/GTS2020.md)
  : Geological Timescale 2020
- [`interval_key`](https://palaeoverse.palaeoverse.org/reference/interval_key.md)
  : Example dataset: Interval key for the look_up function
- [`reefs`](https://palaeoverse.palaeoverse.org/reference/reefs.md) :
  Example dataset: Phanerozoic reefs from the PaleoReefs Database
- [`tetrapods`](https://palaeoverse.palaeoverse.org/reference/tetrapods.md)
  : Example dataset: Early tetrapod data from the Paleobiology Database
