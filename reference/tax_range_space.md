# Calculate the geographic range of fossil taxa

A function to calculate the geographic range of fossil taxa from
occurrence data. The function can calculate geographic range in four
ways: convex hull, latitudinal range, maximum Great Circle Distance, and
the number of occupied equal-area hexagonal grid cells.

## Usage

``` r
tax_range_space(
  occdf,
  name = "genus",
  lng = "lng",
  lat = "lat",
  method = "lat",
  spacing = 100,
  coords = FALSE
)
```

## Arguments

- occdf:

  `dataframe`. A dataframe of fossil occurrences. This dataframe should
  contain at least three columns: names of taxa, longitude and latitude
  (see `name`, `lng`, and `lat` arguments).

- name:

  `character`. The name of the column you wish to be treated as the
  input names (e.g. "species" or "genus"). NA data should be removed
  prior to function call.

- lng:

  `character`. The name of the column you wish to be treated as the
  input longitude (e.g. "lng" or "p_lng"). NA data should be removed
  prior to function call.

- lat:

  `character`. The name of the column you wish to be treated as the
  input latitude (e.g. "lat" or "p_lat"). NA data should be removed
  prior to function call.

- method:

  `character`. How should geographic range be calculated for each taxon
  in `occdf`? Four options exist in this function: "con", "lat", "gcd",
  and "occ". See Details for a description of each.

- spacing:

  `numeric`. The desired spacing (in km) between the center of adjacent
  grid cells. Only required if the `method` argument is set to "occ".
  The default is 100.

- coords:

  `logical`. Should the output coordinates be returned for the "con" and
  "gcd" `method`?

## Value

A `dataframe` with method-specific columns:

- For the "con" method, a `dataframe` with each unique taxa (`taxon`)
  and taxon ID (`taxon_id`) by convex hull coordinate (`lng` & `lat`)
  combination, and area (`area`) in km² is returned.

- For the "lat" method, a `dataframe` with unique taxa (`taxon`), taxon
  ID (`taxon_id`), maximum latitude of occurrence (`max_lat`), minimum
  latitude of occurrence (`min_lat`), and latitudinal range
  (`range_lat`) is returned.

- For the "gcd" method, a `dataframe` with each unique taxa (`taxon`)
  and taxon ID (`taxon_id`) by coordinate combination (`lng` & `lat`) of
  the two most distant points, and the Great Circle Distance (`gcd`)
  between these points in km is returned.

- For the "occ" method, a `dataframe` with unique taxa (`taxon`), taxon
  ID (`taxon_id`), the number of occupied cells (`n_cells`), proportion
  of occupied cells from all occupied by occurrences
  (`proportional_occ`), and the spacing between cells (`spacing`) in km
  is returned. Note: the number of occupied cells and proportion of
  occupied cells is highly dependent on the user-defined `spacing.` For
  the "con", "lat" and "gcd" method, values of zero indicate that the
  respective taxon is a singleton (i.e. represented by only one
  occurrence).

## Details

Four commonly applied approaches (Darroch et al. 2020) are available
using the `tax_range_space` function for calculating ranges:

- Convex hull: the "con" method calculates the geographic range of taxa
  using a convex hull for each taxon in `occdf`, and calculates the area
  of the convex hull (in km²) using
  [`geosphere::areaPolygon()`](https://rdrr.io/pkg/geosphere/man/area.html).
  The convex hull method works by creating a polygon that encompasses
  all occurrence points of the taxon.

- Latitudinal: the "lat" method calculates the palaeolatitudinal range
  of a taxon. It does so for each taxon in `occdf` by finding their
  maximum and minimum latitudinal occurrence (from input `lat`). The
  palaeolatitudinal range of each taxon is also calculated (i.e. the
  difference between the minimum and maximum latitude).

- Maximum Great Circle Distance: the "gcd" method calculates the maximum
  Great Circle Distance between occurrences for each taxon in `occdf`.
  It does so using
  [`geosphere::distHaversine()`](https://rdrr.io/pkg/geosphere/man/distHaversine.html).
  This function calculates Great Circle Distance using the Haversine
  method with the radius of the Earth set to the 6378.137 km. Great
  Circle Distance represents the shortest distance between two points on
  the surface of a sphere. This is different from Euclidean Distance,
  which represents the distance between two points on a plane.

- Occupied cells: the "occ" method calculates the number and proportion
  of occupied equal-area grid cells. It does so using discrete hexagonal
  grids via the
  [`h3jsr`](https://obrl-soil.github.io/h3jsr/reference/h3jsr-package.html)
  package. This package relies on [Uber's H3](https://h3geo.org/docs/)
  library, a geospatial indexing system that partitions the world into
  hexagonal cells. In H3, 16 different resolutions are available ([see
  here](https://h3geo.org/docs/core-library/restable/)). In the
  implementation of the `tax_range_space()` function, the resolution is
  defined by the user-input `spacing` which represents the distance
  between the centroid of adjacent cells. Using this distance, the
  function identifies which resolution is most similar to the input
  `spacing`, and uses this resolution.

## Reference(s)

Darroch, S. A., Casey, M. M., Antell, G. S., Sweeney, A., & Saupe, E. E.
(2020). High preservation potential of paleogeographic range size
distributions in deep time. The American Naturalist, 196(4), 454-471.

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Bethany Allen & Christopher D. Dean

## Examples

``` r
# Grab internal data
occdf <- tetrapods[1:100, ]
# Remove NAs
occdf <- subset(occdf, !is.na(genus))
# Convex hull
ex1 <- tax_range_space(occdf = occdf, name = "genus", method = "con")
# Latitudinal range
ex2 <- tax_range_space(occdf = occdf, name = "genus", method = "lat")
# Great Circle Distance
ex3 <- tax_range_space(occdf = occdf, name = "genus", method = "gcd")
# Occupied grid cells
ex4 <- tax_range_space(occdf = occdf, name = "genus",
                       method = "occ", spacing = 500)
# Convex hull with coordinates
ex5 <- tax_range_space(occdf = occdf, name = "genus", method = "con",
coords = TRUE)
```
