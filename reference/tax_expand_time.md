# Generate pseudo-occurrences from temporal range data

A function to generate interval-level pseudo-occurrences for taxa based
on temporal ranges (e.g. the output of
[`tax_range_time`](https://palaeoverse.palaeoverse.org/reference/tax_range_time.md)).
While the resulting pseudo-occurrences should not be treated as
equivalent to actual occurrence data (e.g. like that from the
Paleobiology Database), such pseudo-occurrences may be useful for
performing statistical analyses where the row representing a taxon must
be replicated for each interval through which the taxon persisted.

## Usage

``` r
tax_expand_time(
  taxdf,
  max_ma = "max_ma",
  min_ma = "min_ma",
  bins = NULL,
  scale = "GTS2020",
  rank = "stage",
  ext_orig = TRUE
)
```

## Arguments

- taxdf:

  `dataframe`. A dataframe of taxa (such as that produced by
  [`tax_range_time`](https://palaeoverse.palaeoverse.org/reference/tax_range_time.md))
  with columns for the maximum and minimum ages (FADs and LADs). Each
  row should represent a unique taxon. Additional columns may be
  included (e.g. taxon names, additional taxonomy, etc) and will be
  included in the returned `data.frame`. If required, `numeric` ages can
  be generated from interval names via the
  [`look_up`](https://palaeoverse.palaeoverse.org/reference/look_up.md)
  function.

- max_ma:

  `character`. The name of the column you wish to be treated as the
  maximum limit (FADs) of the age range (e.g. "max_ma").

- min_ma:

  `character`. The name of the column you wish to be treated as the
  minimum limit (LADs) of the age range (e.g. "min_ma").

- bins:

  `dataframe`. A dataframe of the bins that you wish to allocate
  pseudo-occurrences to such as that returned by
  [`time_bins()`](https://palaeoverse.palaeoverse.org/reference/time_bins.md).
  This dataframe must contain at least the following named columns:
  "bin", "max_ma" and "min_ma". Columns "max_ma" and "min_ma" must be
  `numeric` values.

- scale:

  `character`. Specify the desired geological timescale to be used,
  either "GTS2020" or "GTS2012". Passed to
  [`time_bins()`](https://palaeoverse.palaeoverse.org/reference/time_bins.md)
  if `bins` is not specified.

- rank:

  `character`. Specify the desired stratigraphic rank. Choose from:
  "stage", "epoch", "period", "era", and "eon". Passed to
  [`time_bins()`](https://palaeoverse.palaeoverse.org/reference/time_bins.md)
  if `bins` is not specified.

- ext_orig:

  `logical`. Should two additional columns be added to identify the
  intervals in which taxa originated and went extinct?

## Value

A `dataframe` where each row represents an interval during which a taxon
in the original user-supplied data persisted. The columns are identical
to those in the user-supplied data with additional columns included to
identify the intervals. If `ext_orig` is `TRUE`, two additional columns
are added to identify in which intervals taxa originated and went
extinct.

## Developer(s)

William Gearty & Lewis A. Jones

## Reviewer(s)

Lewis A. Jones

## Examples

``` r
taxdf <- data.frame(name = c("A", "B", "C"),
                    max_ma = c(150, 60, 30),
                    min_ma = c(110, 20, 0))
ex <- tax_expand_time(taxdf)

bins <- time_bins(scale = "GTS2012", rank = "stage")
ex2 <- tax_expand_time(taxdf, bins = bins)
```
