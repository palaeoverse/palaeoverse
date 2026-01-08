# Assign fossil occurrences to time bins

A function to assign fossil occurrences to specified time bins based on
different approaches commonly applied in palaeobiology.

## Usage

``` r
bin_time(
  occdf,
  min_ma = "min_ma",
  max_ma = "max_ma",
  bins,
  method = "mid",
  reps = 100,
  fun = dunif,
  ...
)
```

## Arguments

- occdf:

  `dataframe`. A dataframe of the fossil occurrences you wish to bin.
  This dataframe should contain at least two columns with `numeric`
  values: maximum age of occurrence and minimum age of occurrence (see
  `max_ma`, `min_ma`). If required, `numeric` ages can be generated from
  interval names via the
  [`look_up()`](https://palaeoverse.palaeoverse.org/reference/look_up.md)
  function.

- min_ma:

  `character`. The name of the column you wish to be treated as the
  minimum age for `occdf` and `bins`, e.g. "min_ma" (default).

- max_ma:

  `character`. The name of the column you wish to be treated as the
  maximum age for `occdf` and `bins`, e.g. "max_ma" (default).

- bins:

  `dataframe`. A dataframe of the bins that you wish to allocate fossil
  occurrences to such as that returned by
  [`time_bins()`](https://palaeoverse.palaeoverse.org/reference/time_bins.md).
  This dataframe must contain at least the following named columns:
  "bin" and those specified to `max_ma` (default: "max_ma") and `min_ma`
  (default: "min_ma"). Columns `max_ma` and `min_ma` must be `numeric`
  values.

- method:

  `character`. The method desired for binning fossil occurrences.
  Currently, five methods exist in this function: "mid", "majority",
  "all", "random", and "point". See Details for a description of each.

- reps:

  `numeric`. A non-negative `numeric` specifying the number of
  replications for sampling. This argument is only useful in the case of
  the "random" or "point" method being specified in the `method`
  argument. Defaults to 100.

- fun:

  `function`. A probability density function from the stats package such
  as [dunif](https://rdrr.io/r/stats/Uniform.html) or
  [dnorm](https://rdrr.io/r/stats/Normal.html). This argument is only
  useful if the "point" method is specified in the `method` argument.

- ...:

  Additional arguments available in the called function (`fun`). These
  arguments may be required for function arguments without default
  values, or if you wish to overwrite the default argument value (see
  example). `x` input values are generated internally based on the age
  range of the fossil occurrence and should not be manually provided.
  Note that `x` input values range between 0 and 1, and function
  arguments should therefore be scaled to be within these bounds.

## Value

For methods "mid", "majority" and "all", a `dataframe` of the original
input `occdf` with the following appended columns is returned:
occurrence id (`id`), number of bins that the occurrence age range
covers (`n_bins`), bin assignment (`bin_assignment`), and bin midpoint
(`bin_midpoint`). In the case of the "majority" method, an additional
column of the majority percentage overlap (`overlap_percentage`) is also
appended. For the "random" and "point" method, a `list` is returned (of
length reps) with each element a copy of the `occdf` and appended
columns (random: `bin_assignment` and `bin_midpoint`; point:
`bin_assignment` and `point_estimates`).

## Details

Five approaches (methods) exist in the `bin_time()` function for
assigning occurrences to time bins:

- Midpoint: The "mid" method is the simplest approach and uses the
  midpoint of the fossil occurrence age range to bin the occurrence.

- Majority: The "majority" method bins an occurrence into the bin which
  it most overlaps with. As part of this implementation, the majority
  percentage overlap of the occurrence is also calculated and returned
  as an additional column in `occdf`. If desired, these percentages can
  be used to further filter an occurrence dataset.

- All: The "all" method bins an occurrence into every bin its age range
  covers. For occurrences with age ranges of more than one bin, the
  occurrence row is duplicated. Each occurrence is assigned an ID in the
  column `occdf$id` so that duplicates can be tracked. Additionally,
  `occdf$n_bins` records the number of bins each occurrence appears
  within.

- Random: The "random" method randomly samples X amount of bins (with
  replacement) from the bins that the fossil occurrence age range covers
  with equal probability regardless of bin length. The `reps` argument
  determines the number of times the sample process is repeated. All
  replications are stored as individual elements within the returned
  list with an appended `bin_assignment` and `bin_midpoint` column to
  the original input `occdf`. If desired, users can easily bind this
  list using `do.call(rbind, x)`.

- Point: The "point" method randomly samples X (`reps`) amount of point
  age estimates from the age range of the fossil occurrence. Sampling
  follows a user-input probability density function such as
  [dnorm](https://rdrr.io/r/stats/Normal.html) (see example 5). Users
  should also provide any additional arguments for the probability
  density function (see `...`). However, `x` (vector of quantiles)
  values should not be provided as these values are input from the age
  range of each occurrence. These values range between 0 and 1, and
  therefore function arguments should be scaled to be within these
  bounds. The `reps` argument determines the number of times the sample
  process is repeated. All replications are stored as individual
  elements within the returned list with an appended `bin_assignment`
  and `point_estimates` column to the original input `occdf`. If
  desired, users can easily bind this list using `do.call(rbind, x)`.

## Developer(s)

Christopher D. Dean & Lewis A. Jones

## Reviewer(s)

William Gearty

## Examples

``` r
#Grab internal tetrapod data
occdf <- tetrapods[1:100, ]
bins <- time_bins()

#Assign via midpoint age of fossil occurrence data
ex1 <- bin_time(occdf = occdf, bins = bins, method = "mid")
#> Warning: One or more occurrences have a midpoint age equivalent to a bin boundary. Binning skipped for these occurrences. Hint: `which(is.na(occdf$bin_assignment))`.

#Assign to all bins that age range covers
ex2 <- bin_time(occdf = occdf, bins = bins, method = "all")

#Assign via majority overlap based on fossil occurrence age range
ex3 <- bin_time(occdf = occdf, bins = bins, method = "majority")

#Assign randomly to overlapping bins based on fossil occurrence age range
ex4 <- bin_time(occdf = occdf, bins = bins, method = "random", reps = 5)

#Assign point estimates following a normal distribution
ex5 <- bin_time(occdf = occdf, bins = bins, method = "point", reps = 5,
                fun = dnorm, mean = 0.5, sd = 0.25)
```
