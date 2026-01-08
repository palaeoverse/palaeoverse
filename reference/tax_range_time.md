# Calculate the temporal range of fossil taxa

A function to calculate the temporal range of fossil taxa from
occurrence data.

## Usage

``` r
tax_range_time(
  occdf,
  name = "genus",
  min_ma = "min_ma",
  max_ma = "max_ma",
  group = NULL,
  by = "FAD",
  plot = FALSE,
  plot_args = NULL,
  intervals = "periods"
)
```

## Arguments

- occdf:

  `dataframe`. A dataframe of fossil occurrences containing at least
  three columns: names of taxa, minimum age and maximum age (see `name`,
  `min_ma`, and `max_ma` arguments). These ages should constrain the age
  range of the fossil occurrence and are assumed to be in millions of
  years before present.

- name:

  `character`. The name of the column you wish to be treated as the
  input names, e.g. "genus" (default).

- min_ma:

  `character`. The name of the column you wish to be treated as the
  minimum limit of the age range, e.g. "min_ma" (default).

- max_ma:

  `character`. The name of the column you wish to be treated as the
  maximum limit of the age range, e.g. "max_ma" (default).

- group:

  `character`. The name of the column you wish to be treated as the
  grouping variable, e.g. "family". If not supplied, all taxa are
  treated as a single group.

- by:

  `character`. How should the output be sorted? Either: "FAD"
  (first-appearance date; default), "LAD" (last-appearance data), or
  "name" (alphabetically by taxon names).

- plot:

  `logical`. Should a plot of the ranges be generated?

- plot_args:

  `list`. A list of optional arguments relevant to plotting. See Details
  for options.

- intervals:

  `character`. The time interval information used to plot the x-axis:
  either A) a `character` string indicating a rank of intervals from the
  built-in
  [`GTS2020`](https://palaeoverse.palaeoverse.org/reference/GTS2020.md), B)
  a `character` string indicating a `data.frame` hosted by
  [Macrostrat](https://macrostrat.org) (see
  [`time_bins`](https://palaeoverse.palaeoverse.org/reference/time_bins.md)),
  or C) a custom `data.frame` of time interval boundaries (see
  [axis_geo](https://palaeoverse.palaeoverse.org/reference/axis_geo.md)
  Details). A list of strings or data.frames can be supplied to add
  multiple time scales to the same side of the plot (see
  [axis_geo](https://palaeoverse.palaeoverse.org/reference/axis_geo.md)
  Details). Defaults to "periods".

## Value

A `data.frame` containing the following columns: unique taxa (`taxon`),
taxon ID (`taxon_id`), first appearance of taxon (`max_ma`), last
appearance of taxon (`min_ma`), duration of temporal range
(`range_myr`), and number of occurrences per taxon (`n_occ`) is
returned.

## Details

The temporal range(s) of taxa are calculated by extracting all unique
taxa (`name` column) from the input `occdf`, and checking their first
and last appearance. The temporal duration of each taxon is also
calculated. If the input data columns contain NAs, these must be removed
prior to function call. A plot of the temporal range of each taxon is
also returned if `plot = TRUE`. Customisable argument options (i.e.
[`graphics::par()`](https://rdrr.io/r/graphics/par.html)) to pass to
`plot_args` as a list (and their defaults) for plotting include:

- xlab = "Time (Ma)"

- ylab = "Taxon ID"

- col = "black"

- bg = "black"

- pch = 20

- cex = 1

- lty = 1

- lwd = 1

Note: this function provides output based solely on the user input data.
The true duration of a taxon is likely confounded by uncertainty in
dating occurrences, and incomplete sampling and preservation.

## Developer(s)

Lewis A. Jones

## Reviewer(s)

Bethany Allen, Christopher D. Dean & Kilian Eichenseer

## Examples

``` r
# Grab internal data
occdf <- tetrapods
# Remove NAs
occdf <- subset(occdf, !is.na(order) & order != "NO_ORDER_SPECIFIED")
# Temporal range
ex <- tax_range_time(occdf = occdf, name = "order", plot = TRUE)

# Temporal range ordered by class
# Update margins for plotting
par(mar = c(8, 5, 6, 6))
ex <- tax_range_time(occdf = occdf, name = "order", group = "class",
                     plot = TRUE)

# Customise appearance
ex <- tax_range_time(occdf = occdf, name = "order", group = "class",
                     plot = TRUE,
                     plot_args = list(ylab = "Orders",
                                      pch = 21, col = "black", bg = "blue",
                                      lty = 2),
                     intervals = list("periods", "eras"))

# Control plotting order of groups
occdf$class <- factor(x = occdf$class,
                      levels = c("Reptilia", "Osteichthyes"))
ex <- tax_range_time(occdf = occdf, name = "order",
                     group = "class", plot = TRUE)
```
