# Generate equal-width latitudinal bins

A function to generate latitudinal bins of a given size for a
user-defined latitudinal range. If the desired size of the bins is not
compatible with the defined latitudinal range, bin size can be updated
to the nearest integer which is divisible into this range.

## Usage

``` r
lat_bins_degrees(
  size = 10,
  min = -90,
  max = 90,
  fit = FALSE,
  plot = deprecated()
)

# S3 method for class 'palaeoverse_lat_bins_degrees'
plot(
  x,
  ...,
  col = c("#01665e", "#80cdc1"),
  xlab = "Longitude (°)",
  ylab = "Latitude (°)"
)
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

  **\[deprecated\]** Use
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the output
  of this function instead.

- x:

  `data.frame`. An object of class `"palaeoverse_lat_bins_degrees"`
  created by `lat_bins_degrees()`.

- ...:

  Extra arguments passed to
  [`plot()`](https://rdrr.io/r/base/plot.html). The following arguments
  are already set internally and must not be specified here: `type`,
  `xlim`, `ylim`.

- col:

  `character`. A character vector of length 2 indicating the colours to
  use for the bins.

- xlab:

  `character`. The x-axis title.

- ylab:

  `character`. The y-axis title.

## Value

A `data.frame` of latitudinal bins of user-defined size. The
`data.frame` contains the following columns: bin (bin number), min
(minimum latitude of the bin), mid (midpoint latitude of the bin), max
(maximum latitude of the bin).

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Bethany Allen

## See also

For equal-area latitudinal bins, see
[lat_bins_area](https://palaeoverse.palaeoverse.org/dev/reference/lat_bins_area.md).

## Examples

``` r
# Generate 25 degrees latitudinal bins
lat_bins_degrees(size = 25)
#>   bin min   mid max
#> 1   1  60  72.5  85
#> 2   2  35  47.5  60
#> 3   3  10  22.5  35
#> 4   4 -15  -2.5  10
#> 5   5 -40 -27.5 -15
#> 6   6 -65 -52.5 -40
#> 7   7 -90 -77.5 -65

# Generate latitudinal bins with closest fit to 13 degrees
lat_bins_degrees(size = 13, fit = TRUE)
#> Bin size set to 12 degrees to fit latitudinal range.
#>    bin min mid max
#> 1    1  78  84  90
#> 2    2  66  72  78
#> 3    3  54  60  66
#> 4    4  42  48  54
#> 5    5  30  36  42
#> 6    6  18  24  30
#> 7    7   6  12  18
#> 8    8  -6   0   6
#> 9    9 -18 -12  -6
#> 10  10 -30 -24 -18
#> 11  11 -42 -36 -30
#> 12  12 -54 -48 -42
#> 13  13 -66 -60 -54
#> 14  14 -78 -72 -66
#> 15  15 -90 -84 -78

# Generate latitudinal bins for defined latitudinal range
lat_bins_degrees(size = 10, min = -50, max = 50)
#>    bin min mid max
#> 1    1  40  45  50
#> 2    2  30  35  40
#> 3    3  20  25  30
#> 4    4  10  15  20
#> 5    5   0   5  10
#> 6    6 -10  -5   0
#> 7    7 -20 -15 -10
#> 8    8 -30 -25 -20
#> 9    9 -40 -35 -30
#> 10  10 -50 -45 -40

# Plot latitudinal bins
plot(lat_bins_degrees(size = 20))
```
