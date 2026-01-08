# Palaeorotate fossil occurrences

A function to estimate palaeocoordinates for fossil occurrence data
(i.e. reconstruct the geographic distribution of organisms' remains at
time of deposition). Each occurrence is assigned palaeocoordinates based
on its current geographic position and age estimate.

## Usage

``` r
palaeorotate(
  occdf,
  lng = "lng",
  lat = "lat",
  age = "age",
  model = "MERDITH2021",
  method = "point",
  uncertainty = TRUE,
  round = 3
)
```

## Arguments

- occdf:

  `data.frame`. Fossil occurrences to be palaeogeographically
  reconstructed. `occdf` should contain columns with longitudinal and
  latitudinal coordinates, as well as age estimates. The age of rotation
  should be supplied in millions of years before present.

- lng:

  `character`. The name of the column you wish to be treated as
  longitude (defaults to "lng").

- lat:

  `character`. The name of the column you wish to be treated as latitude
  (defaults to "lat").

- age:

  `character`. The name of the column you wish to be treated as the age
  for rotation (defaults to "age").

- model:

  `character`. The name(s) of the Global Plate Model(s) to be used to
  reconstruct palaeocoordinates. See details for available models.

- method:

  `character`. Method used to calculate palaeocoordinates for fossil
  occurrences. Either "grid" to use reconstruction files, or "point"
  (default) to use the GPlates API service. See details section for
  specific details.

- uncertainty:

  `logical`. Should the uncertainty in palaeogeographic reconstructions
  be returned? If set to TRUE (default), the palaeolatitudinal range and
  maximum geographic distance (in km) between output palaeocoordinates
  are calculated. This argument is only relevant if more than one Global
  Plate Model is specified in `model`.

- round:

  `numeric`. Numeric value indicating the number of decimal places
  `lng`, `lat` and `age` should be rounded to. This functionality is
  only relevant for the "point" `method`. Rounding can speed up
  palaeorotation by reducing the number of unique coordinate pairs.
  Defaults to a value of 3. If no rounding is desired, set this value to
  `NULL`.

## Value

A `data.frame` containing the original input occurrence `data.frame` and
the reconstructed coordinates (i.e. "p_lng", "p_lat"). The "grid"
`method` also returns the age of rotation ("rot_age") and the reference
coordinates rotated ("rot_lng" and "rot_lat"). If only one model is
requested, a column containing the rotation model used ("rot_model") is
also appended. Otherwise, the name of each model is appended to the name
of each column containing palaeocoordinates (e.g. "p_lng_GOLONKA"). If
`uncertainty` is set to `TRUE`, the palaeolatitudinal range
("range_p_lat") and the maximum geographic distance ("max_dist") in km
between palaeocoordinates will also be returned (the latter calculated
via [`distGeo`](https://rdrr.io/pkg/geosphere/man/distGeo.html)).

## Details

This function can estimate palaeocoordinates using two different
approaches (`method`):

- Reconstruction files: The "grid" `method` uses reconstruction files
  from Jones & Domeier (2024) to spatiotemporally link present-day
  geographic coordinates and age estimates with a discrete global grid
  rotated at one million-year time steps throughout the Phanerozoic
  (540–0 Ma). Here, resolution 3 (~119 km spacing) of the reconstruction
  files is used. All files, and the process used to generate them, are
  available and documented in Jones & Domeier (2024). If fine-scale
  spatial analyses are being conducted, use of the "point" `method` (see
  GPlates API below) may be preferred (particularly if occurrences are
  close to plate boundaries). When using the "grid" `method`,
  coordinates within the same grid cell will be assigned equivalent
  palaeocoordinates due to spatial aggregation. However, this approach
  enables efficient estimation of the past distribution of fossil
  occurrences. Note: each reconstruction file is ~45 MB in size.

- GPlates API: The "point" `method` uses the [GPlates Web
  Service](https://gwsdoc.gplates.org) to reconstruct palaeocoordinates
  for point data. The use of this `method` is slower than the "grid"
  `method` if many unique time intervals exist in your dataset. However,
  it provides palaeocoordinates with higher precision.

Available models and timespan for each `method`:

- "MERDITH2021" (Merdith et al., 2021)

  - 0–1000 Ma (point)

  - 0–540 Ma (grid)

- "TorsvikCocks2017" (Torsvik and Cocks, 2016)

  - 0–540 Ma (point/grid)

- "PALEOMAP" (Scotese, 2016)

  - 0–1100 Ma (point)

  - 0–540 Ma (grid)

- "MATTHEWS2016_pmag_ref" (Matthews et al., 2016)

  - 0–410 Ma (grid/point)

- "GOLONKA" (Wright et al., 2013)

  - 0–540 Ma (grid/point)

## References

- Jones, L.A., Domeier, M. A Phanerozoic gridded dataset for
  palaeogeographic reconstructions. Sci Data 11, 710 (2024).
  [doi:10.1038/s41597-024-03468-w](https://doi.org/10.1038/s41597-024-03468-w)
  .

- Matthews, K.J., Maloney, K.T., Zahirovic, S., Williams, S.E., Seton,
  M., and Müller, R.D. (2016). Global plate boundary evolution and
  kinematics since the late Paleozoic. Global and Planetary Change, 146,
  226-250.
  [doi:10.1016/j.gloplacha.2016.10.002](https://doi.org/10.1016/j.gloplacha.2016.10.002)
  .

- Merdith, A., Williams, S.E., Collins, A.S., Tetley, M.G., Mulder,
  J.A., Blades, M.L., Young, A., Armistead, S.E., Cannon, J., Zahirovic,
  S., Müller. R.D. (2021). Extending full-plate tectonic models into
  deep time: Linking the Neoproterozoic and the Phanerozoic.
  Earth-Science Reviews, 214(103477).
  [doi:10.1016/j.earscirev.2020.103477](https://doi.org/10.1016/j.earscirev.2020.103477)
  .

- Scotese, C., & Wright, N. M. (2018). PALEOMAP Paleodigital Elevation
  Models (PaleoDEMs) for the Phanerozoic. [PALEOMAP
  Project](https://www.earthbyte.org/paleodem-resource-scotese-and-wright-2018/).

- Torsvik, T. H. & Cocks, L. R. M. Earth History and Palaeogeography.
  Cambridge University Press, 2016.

- Wright, N., Zahirovic, S., Müller, R. D., & Seton, M. (2013). Towards
  community-driven paleogeographic reconstructions: integrating
  open-access paleogeographic and paleobiology data with plate
  tectonics. Biogeosciences, 10(3), 1529-1541.
  [doi:10.5194/bg-10-1529-2013](https://doi.org/10.5194/bg-10-1529-2013)
  .

See [GPlates documentation](https://gwsdoc.gplates.org/reconstruction)
for additional information and details.

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Kilian Eichenseer, Lucas Buffan & Will Gearty

## Examples

``` r
if (FALSE) { # \dontrun{
#Generic example with a few occurrences
occdf <- data.frame(lng = c(2, -103, -66),
                lat = c(46, 35, -7),
                age = c(88, 125, 200))

#Calculate palaeocoordinates using reconstruction files
ex1 <- palaeorotate(occdf = occdf, method = "grid")

#Calculate palaeocoordinates using the GPlates API
ex2 <- palaeorotate(occdf = occdf, method = "point")

#Calculate uncertainity in palaeocoordinates from models
ex3 <- palaeorotate(occdf = occdf,
                    method = "grid",
                    model = c("MERDITH2021",
                              "GOLONKA",
                              "PALEOMAP"),
                    uncertainty = TRUE)

#Now with some real fossil occurrence data!

#Grab some data from the Paleobiology Database
data(tetrapods)

#Assign midpoint age of fossil occurrence data for reconstruction
tetrapods$age <- (tetrapods$max_ma + tetrapods$min_ma)/2

#Rotate the data
ex3 <- palaeorotate(occdf = tetrapods)

#Calculate uncertainity in palaeocoordinates from models
ex4 <- palaeorotate(occdf = tetrapods,
                    model = c("MERDITH2021",
                              "GOLONKA",
                              "PALEOMAP"),
                    uncertainty = TRUE)
} # }
```
