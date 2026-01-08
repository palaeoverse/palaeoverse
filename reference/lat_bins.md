# Generate equal-width latitudinal bins

**\[deprecated\]**

`lat_bins()` was renamed to
[`lat_bins_degrees()`](https://palaeoverse.palaeoverse.org/reference/lat_bins_degrees.md)
to be consistent with `lat_bins_area().`

## Usage

``` r
lat_bins(size = 10, min = -90, max = 90, fit = FALSE, plot = FALSE)
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
