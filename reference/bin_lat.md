# Assign fossil occurrences to latitudinal bins

A function to assign fossil occurrences to user-specified latitudinal
bins.

## Usage

``` r
bin_lat(occdf, bins, lat = "lat", boundary = FALSE)
```

## Arguments

- occdf:

  `dataframe`. A dataframe of the fossil occurrences you wish to bin.
  This dataframe should contain a column with the latitudinal
  coordinates of occurrence data.

- bins:

  `dataframe`. A dataframe of the bins that you wish to allocate fossil
  occurrences to, such as that returned by
  [`lat_bins()`](https://palaeoverse.palaeoverse.org/reference/lat_bins.md).
  This dataframe must contain at least the following named columns:
  "bin", "max" and "min".

- lat:

  `character`. The name of the column you wish to be treated as the
  input latitude (e.g. "lat" or "p_lat"). This column should contain
  numerical values. Defaults to "lat".

- boundary:

  `logical`. If `TRUE`, occurrences falling on the boundaries of
  latitudinal bins will be duplicated and assigned to both bins. If
  `FALSE`, occurrences will be binned into the upper bin only (i.e.
  highest row number).

## Value

A dataframe of the original input `occdf` with appended columns
containing respective latitudinal bin information.

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Sofia Galvan

## Examples

``` r
# Load occurrence data
occdf <- tetrapods
# Generate latitudinal bins
bins <- lat_bins_degrees(size = 10)
# Bin data
occdf <- bin_lat(occdf = occdf, bins = bins, lat = "lat")
#> Presence of occurrences falling on boundaries detected. 
#> Occurrences assigned to upper bin.
```
