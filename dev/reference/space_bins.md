# Generate H3 spatial bins

A function to generate spatial hexagonal equal-area grid cells (i.e.
bins) using the [H3 discrete global grid system](https://h3geo.org/).
These can then be used in
[`bin_space()`](https://palaeoverse.palaeoverse.org/dev/reference/bin_space.md)
to assign fossil occurrences (or localities) to spatial bins.

## Usage

``` r
space_bins(resolution, spacing, area)
```

## Arguments

- resolution:

  `numeric`. The desired H3 resolution. Must be a whole number between 0
  and 15. Cannot be used if `spacing` or `area` is used.

- spacing:

  `numeric`. The desired spacing between the centre of adjacent cells.
  This value should be provided in kilometres. Cannot be used if
  `resolution` or `area` is used.

- area:

  `numeric`. The desired area of each cell. This value should be
  provided in square kilometres. Cannot be used if `resolution` or
  `spacing` is used.

## Value

An object of class `palaeoverse_space_bins`. This object can be passed
to the `bins` argument in
[`bin_space()`](https://palaeoverse.palaeoverse.org/dev/reference/bin_space.md)
to assign occurrences (or any set of coordinates) to these bins. Under
the hood, this is an `sfc_POLYGON` with CRS EPSG:4326 (WGS 1984). This
object has an attribute `h3_resolution` that is currently used
internally by
[`bin_space()`](https://palaeoverse.palaeoverse.org/dev/reference/bin_space.md)
to name columns automatically, but may change in future releases.

## Details

The [H3 discrete global grid system](https://h3geo.org/) uses fixed
resolutions, which can be directly set by passing an H3 resolution level
(0-15) to `resolution`. Alternatively, resolution can be inferred from a
target `spacing` or `area`. `spacing` is defined as the distance between
the centroids of adjacent cells; the function compares the input spacing
to the average centroid distance at each H3 resolution and selects the
closest match. Likewise, a target `area` is compared to the average cell
area at each resolution, and the nearest resolution is used.

The table below shows, for each H3 resolution, the number of cells
covering the globe, their average area (source:
<https://h3geo.org/docs/core-library/restable>), and the average spacing
between the centroids of adjacent cells. This information is also
available via
[h3jsr::h3_info_table](https://obrl-soil.github.io/h3jsr/reference/h3_info_table.html).

|            |                     |                       |                      |
|------------|---------------------|-----------------------|----------------------|
| Resolution | Number of cells     | Average area (sq. km) | Average spacing (km) |
| 0          | 122                 | 4,357,449.42          | 1,918.61             |
| 1          | 842                 | 609,788.44            | 725.17               |
| 2          | 5,882               | 86,801.78             | 274.09               |
| 3          | 41,162              | 12,393.43             | 103.60               |
| 4          | 288,122             | 1,770.35              | 39.16                |
| 5          | 2,016,842           | 252.90                | 14.80                |
| 6          | 14,117,882          | 36.13                 | 5.59                 |
| 7          | 98,825,162          | 5.16                  | 2.11                 |
| 8          | 691,776,122         | 0.737                 | 0.799                |
| 9          | 4,842,432,842       | 0.105                 | 0.302                |
| 10         | 33,897,029,882      | 0.0150                | 0.114                |
| 11         | 237,279,209,162     | 0.00215               | 0.0431               |
| 12         | 1,660,954,464,122   | 0.000307              | 0.0163               |
| 13         | 11,626,681,248,842  | 0.0000439             | 0.00617              |
| 14         | 81,386,768,741,882  | 0.00000627            | 0.00234              |
| 15         | 569,707,381,193,162 | 0.000000895           | 0.000883             |

## See also

[`bin_space()`](https://palaeoverse.palaeoverse.org/dev/reference/bin_space.md)

## Examples

``` r
space_bins(resolution = 1)
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> Geometry set for 842 features 
#> Geometry type: eoverse_space_bins
#> Dimension:     XY
#> Bounding box:  xmin: -179.7613 ymin: -87.80824 xmax: 179.9534 ymax: 87.82362
#> Geodetic CRS:  WGS 84
#> First 5 geometries:
#> POLYGON ((40.0114 75.01232, 55.65377 76.66744, ...
#> POLYGON ((13.88006 80.34595, 33.23853 83.59755,...
#> POLYGON ((67.04817 73.26747, 82.21912 72.97478,...
#> POLYGON ((61.13368 80.94452, 89.55466 80.74027,...
#> POLYGON ((31.83128 68.92996, 41.0975 70.96472, ...

space_bins(spacing = 1000)
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> Geometry set for 842 features 
#> Geometry type: eoverse_space_bins
#> Dimension:     XY
#> Bounding box:  xmin: -179.7613 ymin: -87.80824 xmax: 179.9534 ymax: 87.82362
#> Geodetic CRS:  WGS 84
#> First 5 geometries:
#> POLYGON ((40.0114 75.01232, 55.65377 76.66744, ...
#> POLYGON ((13.88006 80.34595, 33.23853 83.59755,...
#> POLYGON ((67.04817 73.26747, 82.21912 72.97478,...
#> POLYGON ((61.13368 80.94452, 89.55466 80.74027,...
#> POLYGON ((31.83128 68.92996, 41.0975 70.96472, ...

space_bins(area = 100000)
#> ℹ H3 resolution: 2
#> ℹ Average spacing between adjacent cells: 274.09 km.
#> ℹ Average cell area: 86,801.78 sq. km.
#> Geometry set for 5882 features 
#> Geometry type: eoverse_space_bins
#> Dimension:     XY
#> Bounding box:  xmin: -179.9789 ymin: -89.07415 xmax: 179.9829 ymax: 89.07415
#> Geodetic CRS:  WGS 84
#> First 5 geometries:
#> POLYGON ((36.42872 77.68575, 43.86177 78.00915,...
#> POLYGON ((30.78838 80.33975, 40.19373 80.81642,...
#> POLYGON ((48.58085 76.68007, 55.65377 76.66744,...
#> POLYGON ((46.68072 79.52276, 55.75077 79.55833,...
#> POLYGON ((29.20747 75.65094, 35.20921 76.15221,...
```
