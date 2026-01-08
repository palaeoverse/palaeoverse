# Generate equal-width latitudinal bins

A function to generate latitudinal bins of a given size for a
user-defined latitudinal range. If the desired size of the bins is not
compatible with the defined latitudinal range, bin size can be updated
to the nearest integer which is divisible into this range.

## Usage

``` r
lat_bins_degrees(size = 10, min = -90, max = 90, fit = FALSE, plot = FALSE)
```

## Arguments

- size:

  `numeric`. A single numeric value defining the width of the
  latitudinal bins. This value must be more than 0, and less than or
  equal to 90 (defaults to 10).

- min:

  `numeric`. A single numeric value defining the lower limit of the
  latitudinal range (defaults to -90).

- max:

  `numeric`. A single numeric value defining the upper limit of the
  latitudinal range (defaults to 90).

- fit:

  `logical`. Should bin size be checked to ensure that the entire
  latitudinal range is covered? If `fit = TRUE`, bin size is set to the
  nearest integer which is divisible by the user-input range. If
  `fit = FALSE`, and bin size is not divisible into the range, the upper
  part of the latitudinal range will be missing.

- plot:

  `logical`. Should a plot of the latitudinal bins be generated? If
  `TRUE`, a plot is generated. Defaults to `FALSE`.

## Value

A `dataframe` of latitudinal bins of user-defined size. The `data.frame`
contains the following columns: bin (bin number), min (minimum latitude
of the bin), mid (midpoint latitude of the bin), max (maximum latitude
of the bin).

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Bethany Allen

## See also

For equal-area latitudinal bins, see
[lat_bins_area](https://palaeoverse.palaeoverse.org/reference/lat_bins_area.md).

## Examples

``` r
# Generate 20 degrees latitudinal bins
bins <- lat_bins_degrees(size = 20)

# Generate latitudinal bins with closest fit to 13 degrees
bins <- lat_bins_degrees(size = 13, fit = TRUE)
#> Bin size set to 12 degrees to fit latitudinal range.

# Generate latitudinal bins for defined latitudinal range
bins <- lat_bins_degrees(size = 10, min = -50, max = 50)
```
