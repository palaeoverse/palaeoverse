# Generate equal-area latitudinal bins

A function to generate approximately equal-area latitudinal bins for a
user-specified number of bins and latitudinal range. This approach is
based on calculating the curved surface area of spherical segments
bounded by two parallel discs.

## Usage

``` r
lat_bins_area(n = 12, min = -90, max = 90, r = 6371, plot = FALSE)
```

## Arguments

- n:

  `numeric`. A single numeric value defining the number of equal-area
  latitudinal bins to split the latitudinal range into (as defined by
  `min` and `max`).

- min:

  `numeric`. A single numeric value defining the lower limit of the
  latitudinal range (defaults to -90).

- max:

  `numeric`. A single numeric value defining the upper limit of the
  latitudinal range (defaults to 90).

- r:

  `numeric`. The radius of the Earth in kilometres. Defaults to the
  volumetric mean radius of the Earth (6371 km). Other user-specified
  `r` values are accepted (e.g. equatorial radius 6378 km).

- plot:

  `logical`. Should a plot of the latitudinal bins be generated? If
  `TRUE`, a plot is generated. Defaults to `FALSE`.

## Value

A `data.frame` of user-defined number of latitudinal bins. The
`data.frame` contains the following columns: bin (bin number), min
(minimum latitude of the bin), mid (midpoint latitude of the bin), max
(maximum latitude of the bin), area (the area of the bin in km²),
area_prop (the proportional area of the bin across all bins).

## Developer(s)

Lewis A. Jones & Kilian Eichenseer

## Reviewer(s)

Kilian Eichenseer & Bethany Allen

## See also

For bins with unequal area, but equal latitudinal range, see
[lat_bins_degrees](https://palaeoverse.palaeoverse.org/reference/lat_bins_degrees.md).

## Examples

``` r
# Generate 12 latitudinal bins
bins <- lat_bins_area(n = 12)
# Generate latitudinal bins for just the (sub-)tropics
bins <- lat_bins_area(n = 6, min = -30, max = 30)
# Generate latitudinal bins and a plot
bins <- lat_bins_area(n = 24, plot = TRUE)
```
