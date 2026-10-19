# Assign fossil occurrences to spatial bins

A function to assign fossil occurrences (or any set of coordinates) to
spatial bins (i.e. cells) using a hexagonal equal-area grid. This
function is related to
[`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md),
which creates the spatial bins required by the `bins` argument.

## Usage

``` r
bin_space(
  occdf,
  bins,
  lng = "lng",
  lat = "lat",
  plot = FALSE,
  spacing,
  sub_grid,
  return
)
```

## Arguments

- occdf:

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

  `logical`. Should the occupied cells of the equal-area grid be
  plotted?

- spacing, sub_grid, return:

  **\[deprecated\]** These arguments are no longer used and don't have a
  replacement. Pass the output of
  [`space_bins()`](https://palaeoverse.palaeoverse.org/dev/reference/space_bins.md)
  to the `bins` argument instead.

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
occdf <- reefs[1:250, ]

# Bin data using a hexagonal equal-area grid
ex1 <- bin_space(occdf = occdf, bins = space_bins(spacing = 500))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.

# Bin data using a hexagonal equal-area grid and sub-grid
ex2 <- occdf |>
  bin_space(bins = space_bins(spacing = 1000)) |>
  bin_space(bins = space_bins(spacing = 250))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> ℹ H3 resolution: 2
#> ℹ Average spacing between adjacent cells: 274.09 km.
#> ℹ Average cell area: 86,801.78 sq. km.

# EXAMPLE: rarefy
# Load data
occdf <- tetrapods[1:250, ]

# Assign to spatial bins and sub-bins
occdf <- occdf |>
  bin_space(bins = space_bins(spacing = 1000)) |>
  bin_space(bins = space_bins(spacing = 250))
#> ℹ H3 resolution: 1
#> ℹ Average spacing between adjacent cells: 725.17 km.
#> ℹ Average cell area: 609,788.4 sq. km.
#> ℹ H3 resolution: 2
#> ℹ Average spacing between adjacent cells: 274.09 km.
#> ℹ Average cell area: 86,801.78 sq. km.

# Get unique bins
bins <- unique(occdf$cell_ID_1)

# n reps
n <- 10

# Rarefy data across sub-grid grid cells
# Returns a list with each element a bin with respective mean genus richness
df <- lapply(bins, function(x) {
  # subset occdf for respective grid cell
  tmp <- occdf[which(occdf$cell_ID_1 == x), ]

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
