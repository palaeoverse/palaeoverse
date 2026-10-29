# Assign fossil occurrences to spatial bins

A function to assign fossil occurrences (or any set of coordinates) to
spatial bins (i.e. cells) using a hexagonal equal-area grid. This
function is related to
[`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md),
which creates the spatial bins required by the `bins` argument.

## Usage

``` r
bin_space(
  data,
  bins,
  lng = "lng",
  lat = "lat",
  plot = deprecated(),
  spacing,
  sub_grid,
  return
)

# S3 method for class 'palaeoverse_bin_space'
plot(
  x,
  ...,
  col = NULL,
  centres = list(c(lat = 0, lon = 0), c(lat = 0, lon = 180), c(lat = 23, lon = -102)),
  subtitles = NULL,
  first_layer
)
```

## Arguments

- data:

  `data.frame`. A dataframe of the fossil occurrences (or any set of
  coordinates) you wish to bin. This dataframe should contain the
  decimal degree coordinates of your occurrences, and they should be of
  class `numeric`.

- bins:

  `sfc_POLYGON`. An object of class `palaeoverse_space_bins` created by
  [`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md),
  corresponding to the bins that you wish to allocate fossil occurrences
  to.

- lng:

  `character`. The name of the column you wish to be treated as the
  input longitude (e.g. "lng" or "p_lng").

- lat:

  `character`. The name of the column you wish to be treated as the
  input latitude (e.g. "lat" or "p_lat").

- plot:

  **\[deprecated\]** Use
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the output
  of this function instead.

- spacing, sub_grid, return:

  **\[deprecated\]** These arguments are no longer used and don't have a
  replacement. Pass the output of
  [`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md)
  to the `bins` argument instead.

- x:

  `data.frame`. An object of class `"palaeoverse_bin_space"` created by
  `bin_space()`.

- ...:

  Extra arguments passed to
  [`plot()`](https://rdrr.io/r/base/plot.html). The following argument
  is already set internally and must not be specified here: `add`.

- col:

  `character`. Colour(s) of the occupied cells. Must be of length 1 (in
  which case all resolutions have the same colour) or of length equal to
  the number of resolutions in `x`, ordered from the coarsest to the
  finest resolution. By default, colours are taken in order from the
  colour-blind friendly Okabe-Ito palette (see
  [`grDevices::palette.colors()`](https://rdrr.io/r/grDevices/palette.html)).

- centres:

  `list`. Views to display (at most four). These must be either:

  - `c(lat = , lon = )`: the whole globe centred on this point;

  - `c(xmin = , xmax = , ymin = , ymax = )`: a bounding box (in
    longitude for `x` and latitude for `y`), centred on its midpoint.
    `xmax` must be greater than `xmin`, and `ymax` must be greater than
    `ymin`.

  By default, three globes are centred on longitude 0, longitude 180,
  and -102.

- subtitles:

  `character`. Titles of each globe, in the same order as `centres`.
  Must be `NULL` (default, no subtitles) or of length equal to the
  number of elements in `centres`.

- first_layer:

  `sf`, `sfc`, or `sfg`. An object from the package `sf` (e.g. lines,
  polygons, or points) indicating the first layer to be printed on each
  globe. Polygons (such as continents) are filled in dark grey to
  distinguish them from oceans. If unspecified (default), it plots the
  modern coastlines with
  `sf::st_geometry(rnaturalearthdata::coastline110)`. Set
  `first_layer = NULL` to remove this layer.

## Value

A `data.frame` with the original input data and three new columns:

- `cell_id_{h3_resolution}`: the ID of the H3 cell that contains the
  row's coordinates

- `cell_centroid_lat_{h3_resolution}`: the latitude of the H3 cell's
  centroid

- `cell_centroid_lng_{h3_resolution}`: the longitude of the H3 cell's
  centroid

In these three column names, `{h3_resolution}` corresponds to the H3
resolution that is used. For instance, using
`bins = space_bins(spacing = 250)` results in an H3 resolution equal to
2, so the column name for the cell ID will be `"cell_id_2"`.

Note: calling `bin_space()` several times can overwrite existing cell
columns if `bins` uses the same resolution as existing columns.

## Details

This function assigns fossil occurrence data into equal-area grid cells
using discrete hexagonal grids via the
[`h3jsr`](https://obrl-soil.github.io/h3jsr/reference/h3jsr-package.html)
package. This package relies on [Uber's H3](https://h3geo.org/docs/)
library, a geospatial indexing system that partitions the world into
hexagonal cells. In H3, 16 different resolutions are available (see the
'Details' section of
[`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md)).

Note: prior to implementation, coordinate reference system (CRS) for
input data is defined as EPSG:4326 (World Geodetic System 1984). The
user should transform their data accordingly if this is not appropriate.
If you are unfamiliar with working with geographic data, we highly
recommend checking out [Geocomputation with
R](https://r.geocompx.org/index.html).

The base grid uses the coarsest resolution among the `cell_ID_*`
columns, and the occupied cells of every resolution are drawn on top of
it. The world is shown as one or several globes (see `centres`) using an
orthographic projection, which avoids distorting cells that cross the
antimeridian or cover the poles. Present-day coastlines are drawn as the
first layer, which requires the package `rnaturalearthdata`.

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Bethany Allen & Kilian Eichenseer

## See also

[`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md)

## Examples

``` r
# Get internal data
data("reefs")

# Reduce data for plotting
data <- reefs[1:250, ]

# Bin data using a hexagonal equal-area grid
ex1 <- bin_space(data = data, bins = space_bins(spacing = 500))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
plot(ex1)


# Make filled hexagons slightly transparent
plot(ex1, col = adjustcolor("#E69F00", alpha.f = 0.5))


# Bin data using a hexagonal equal-area grid and sub-grid
ex2 <- data |>
  bin_space(bins = space_bins(spacing = 1000)) |>
  bin_space(bins = space_bins(spacing = 250))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> ℹ H3 resolution: 2
#> ℹ Average spacing between adjacent cells: 274.09 km.
#> ℹ Average cell area: 86,801.78 sq. km.

plot(ex2, main = "Reefs", subtitles = c("Africa", "Pacific", "Americas"))


# EXAMPLE: rarefy
# Load data
data <- tetrapods[1:250, ]

# Assign to spatial bins and sub-bins
data <- data |>
  bin_space(bins = space_bins(spacing = 1000)) |>
  bin_space(bins = space_bins(spacing = 250))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> ℹ H3 resolution: 2
#> ℹ Average spacing between adjacent cells: 274.09 km.
#> ℹ Average cell area: 86,801.78 sq. km.

# Get unique bins
bins <- unique(data$cell_ID_1)

# n reps
n <- 10

# Rarefy data across sub-grid grid cells
# Returns a list with each element a bin with respective mean genus richness
df <- lapply(bins, function(x) {
  # subset data for respective grid cell
  tmp <- data[which(data$cell_ID_1 == x), ]

  # Which sub-grid cells are there within this bin?
  sub_bin <- unique(tmp$cell_ID_2)

  # Sample 1 sub-grid cell n times
  s <- sample(sub_bin, size = n, replace = TRUE)

  # Count the number of unique genera within each sub_grid cell for each rep
  counts <- sapply(s, function(i) {
    # Number of unique genera within each sample
    length(unique(tmp[which(tmp$cell_ID_2 == i), ]$genus))
  })

  # Mean richness across subsamples
  mean(counts)
})
df
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 1.5
#> 
#> [[3]]
#> [1] 15.8
#> 
#> [[4]]
#> [1] 1
#> 
#> [[5]]
#> [1] 1
#> 
#> [[6]]
#> [1] 4
#> 
#> [[7]]
#> [1] 3.5
#> 
#> [[8]]
#> [1] 3.6
#> 
#> [[9]]
#> [1] 1
#> 
#> [[10]]
#> [1] 2
#> 
#> [[11]]
#> [1] 1
#> 
#> [[12]]
#> [1] 1
#> 
#> [[13]]
#> [1] 1
#> 
```
