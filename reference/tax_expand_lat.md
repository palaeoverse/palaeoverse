# Generate pseudo-occurrences from latitudinal range data

A function to generate pseudo-occurrences for taxa based on latitudinal
ranges (e.g. the output of the 'lat' method in
[`tax_range_space`](https://palaeoverse.palaeoverse.org/reference/tax_range_space.md)).
While the resulting pseudo-occurrences should not be treated as
equivalent to actual occurrence data (e.g. like that from the
Paleobiology Database), such pseudo-occurrences may be useful for
performing statistical analyses where the row representing a taxon must
be replicated for each latitudinal bin through which the taxon ranges.

## Usage

``` r
tax_expand_lat(taxdf, bins, max_lat = "max_lat", min_lat = "min_lat")
```

## Arguments

- taxdf:

  `dataframe`. A dataframe of taxa (such as the output of the 'lat'
  method in
  [`tax_range_space`](https://palaeoverse.palaeoverse.org/reference/tax_range_space.md))
  with columns containing latitudinal range data (maximum and minimum
  latitude). Column names are assumed to be "max_lat" and "min_lat", but
  may be updated via the `max_lat` and `min_lat` arguments. Each row
  should represent a unique taxon. Additional columns may be included
  (e.g. taxon names, additional taxonomy, etc) and will be included in
  the returned `data.frame`.

- bins:

  `dataframe`. A dataframe of the bins that you wish to allocate fossil
  occurrences to, such as that returned by
  [`lat_bins`](https://palaeoverse.palaeoverse.org/reference/lat_bins.md).
  This dataframe must contain at least the following named columns:
  "bin", "max" and "min".

- max_lat:

  `character`. The name of the column you wish to be treated as the
  maximum latitude of the latitudinal range (e.g. "max_lat").

- min_lat:

  `character`. The name of the column you wish to be treated as the
  minimum latitude of the latitudinal range (e.g. "min_lat").

## Value

A `dataframe` where each row represents a latitudinal bin which a taxon
ranges through. The columns are identical to those in the user-supplied
data with additional columns included to identify bins. Output will be
returned in the order of supplied bins.

## Developer(s)

Lewis A. Jones & William Gearty

## Reviewer(s)

Christopher D. Dean

## Examples

``` r
bins <- lat_bins_degrees()
taxdf <- data.frame(name = c("A", "B", "C"),
                    max_lat = c(60, 20, -10),
                    min_lat = c(20, -40, -60))
ex <- tax_expand_lat(taxdf = taxdf,
                     bins = bins,
                     max_lat = "max_lat",
                     min_lat = "min_lat")
```
