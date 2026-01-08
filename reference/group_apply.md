# Apply a function over grouping(s) of data

A function to apply `palaeoverse` functionality across subsets (groups)
of data, delineated using one or more variables. Functions which receive
a `data.frame` as input (e.g. `nrow`, `ncol`, `lengths`, `unique`) may
also be used.

## Usage

``` r
group_apply(occdf, group, fun, ...)
```

## Arguments

- occdf:

  `dataframe`. A dataframe of fossil occurrences or taxa, as relevant to
  the desired function. This dataframe must contain the grouping
  variables and the necessary variables for the function you wish to
  call (see function-specific documentation for required columns).

- group:

  `character`. A vector of column names, specifying the desired
  subgroups (e.g. "collection_no", "stage_bin"). Supplying more than one
  grouping variable will produce an output containing subgroups for each
  unique combination of values.

- fun:

  `function`. The function you wish to apply to `occdf`. See details for
  compatible functions.

- ...:

  Additional arguments available in the called function. These arguments
  may be required for function arguments without default values, or if
  you wish to overwrite the default argument value (see examples).

## Value

A `data.frame` of the outputs from the selected function, with prepended
column(s) indicating the user-defined groups. If a single vector is
returned via the called function, it will be transformed to a
`data.frame` with the column name equal to the input function.

## Details

`group_apply` applies functions to subgroups of data within a supplied
dataset, enabling the separate analysis of occurrences or taxa from
different time intervals, spatial regions, or trait values. The function
serves as a wrapper around `palaeoverse` functions. Other functions
which can be applied to a `data.frame` (e.g. `nrow`, `ncol`, `lengths`,
`unique`) may also be used.  
  
All `palaeoverse` functions which require a dataframe input can be used
in conjunction with the `group_apply` function. However, this is
unnecessary for many functions (e.g.
[`bin_time`](https://palaeoverse.palaeoverse.org/reference/bin_time.md))
as groups do not need to be partitioned before binning. This list
provides users with `palaeoverse` functions that might be interesting to
apply across group(s):

- [`tax_unique`](https://palaeoverse.palaeoverse.org/reference/tax_unique.md):
  return the number of unique taxa per grouping variable.

- [`tax_range_time`](https://palaeoverse.palaeoverse.org/reference/tax_range_time.md):
  return the temporal range of taxa per grouping variable.

- [`tax_range_space`](https://palaeoverse.palaeoverse.org/reference/tax_range_space.md):
  return the geographic range of taxa per grouping variable.

- [`tax_check`](https://palaeoverse.palaeoverse.org/reference/tax_check.md):
  return potential spelling variations of the same taxon per grouping
  variable. Note: `verbose` needs to be set to FALSE.

## Developer(s)

Lewis A. Jones & William Gearty

## Reviewer(s)

Kilian Eichenseer & Bethany Allen

## Examples

``` r
# Examples
# Get tetrapods data
occdf <- tetrapods[1:100, ]
# Remove NA data
occdf <- subset(occdf, !is.na(genus))
# Count number of occurrences from each country
ex1 <- group_apply(occdf = occdf, group = "cc", fun = nrow)
# Unique genera per collection with group_apply and input arguments
ex2 <- group_apply(occdf = occdf,
                   group = "collection_no",
                   fun = tax_unique,
                   genus = "genus",
                   family = "family",
                   order = "order",
                   class = "class",
                   resolution = "genus")
# Use multiple variables (number of occurrences per collection and formation)
ex3 <- group_apply(occdf = occdf,
                   group = c("collection_no", "formation"),
                   fun = nrow)
# Compute counts of occurrences per latitudinal bin
# Set up lat bins
bins <- lat_bins_degrees()
# bin occurrences
occdf <- bin_lat(occdf = occdf, bins = bins)
# Calculate number of occurrences per bin
ex4 <- group_apply(occdf = occdf, group = "lat_bin", fun = nrow)
```
