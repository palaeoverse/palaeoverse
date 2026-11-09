# Calculate the stratigraphic ranges of fossil taxa

A function to calculate the stratigraphic ranges of fossil taxa from
occurrence data.

## Usage

``` r
tax_range_strat(
  data,
  name = "genus",
  level = "bed",
  group = NULL,
  certainty = NULL,
  by = "FAD",
  plot_args = deprecated(),
  x_args = deprecated(),
  y_args = deprecated()
)

# S3 method for class 'palaeoverse_tax_range_strat'
plot(
  x,
  ...,
  axis = NULL,
  main = "Stratigraphic range of taxa",
  xlab = "Taxon",
  ylab = "Bed number",
  col.group = c("grey85", "grey95")
)
```

## Arguments

- data:

  `dataframe`. A dataframe of fossil occurrences containing at least two
  columns: names of taxa, and their stratigraphic position (see `name`
  and `level` arguments).

- name:

  `character`. The name of the column you wish to be treated as the
  input names, e.g. "genus" (default).

- level:

  `character`. The name of the column you wish to be treated as the
  stratigraphic levels associated with each occurrence, e.g. "bed"
  (default) or "height". Stratigraphic levels must be `numeric`.

- group:

  `character`. The name of the column you wish to be treated as the
  grouping variable, e.g. "family". If not supplied, all taxa are
  treated as a single group.

- certainty:

  `character`. The name of the column you wish to be treated as the
  information on whether an identification is certain (1) or uncertain
  (0). By default (`certainty = NULL`), no column name is provided, and
  all occurrences are assumed to be certain. In the plot, certain
  occurrences will be plotted with a black circle and joined with solid
  lines, while uncertain occurrences will be plotted with a white circle
  and joined with dashed lines.

- by:

  `character`. How should the output be sorted? Either: "FAD" (first
  appearance; default), "LAD" (last appearance), or "name"
  (alphabetically by taxon names).

- plot_args, x_args, y_args:

  **\[deprecated\]** These arguments have been removed. Use
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the output
  of this function instead.

- x:

  `data.frame`. An object of class `"palaeoverse_tax_range_strat"`
  created by `tax_range_strat()`.

- ...:

  Extra [graphical parameters](https://rdrr.io/r/graphics/par.html) (see
  Details). The following arguments are already set internally and must
  not be specified here: `y`, `axes`, `type`, `xaxs`.

- axis:

  A function call drawing an axis, such as
  [`axis_geo()`](https://palaeoverse.palaeoverse.org/dev/reference/axis_geo.md)
  or [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html). If
  `NULL` (default), the y-axis has a tick at each sampled stratigraphic
  level.

- main:

  `character`. The plot title.

- xlab:

  `character`. The x-axis title.

- ylab:

  `character`. The y-axis title.

- col.group:

  `character`. The background colours to use when `x` has groups. Must
  be either a single colour used for all groups, or of the same length
  as the number of unique groups. If `NULL`, the group backgrounds are
  not coloured.

## Value

A `data.frame` containing the following columns: taxon ID (`ID`), unique
taxa (`taxon`), lowest stratigraphic level of taxon (`min_bin`), and
highest stratigraphic level of taxon (`max_bin`). If `certainty` is
supplied, the lowest and highest stratigraphic levels of the certain
occurrences of each taxon (`min_bin_certain` and `max_bin_certain`) are
also returned.

This `data.frame` has attributes starting with
`palaeoverse_tax_range_strat_*` that are for internal use only.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) invisibly
returns a list with the graphical parameters `mar` and `usr` used to
draw the plot (see
[`graphics::par()`](https://rdrr.io/r/graphics/par.html)).

## Details

The graphical parameters are reset when
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) exits. See
'Examples' to know how to add components to the plot.

The styling of the points and line segments can be adjusted by supplying
named arguments to
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). `col` (segment
and point color), `lwd` (segment width), `pch` (point symbol), `bg`
(background point color for some values of `pch`), `lty` (segment line
type), and `cex` (point size) are supported.

If `x` contains information on certainty, these arguments may be vectors
of length two, in which case the first value of the vector will be used
for the "certain" points and segments, and the second value of the
vector will be used for the "uncertain" points and segments. If only a
single value is supplied, it will be used for both.

## Developer(s)

Bethany Allen, William Gearty, Lewis A. Jones & Alexander Dunhill

## Reviewer(s)

William Gearty & Lewis A. Jones

## Examples

``` r
# Load tetrapod dataset
data(tetrapods)
# Sample tetrapod occurrences
tetrapod_names <- tetrapods$accepted_name[1:50]
# Simulate bed numbers
beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
# Simulate certainty values
certainty_sampled <- sample(x = 0:1, size = 50, replace = TRUE)
# Combine into data frame
data <- data.frame(taxon = tetrapod_names,
                    bed = beds_sampled,
                    certainty = certainty_sampled)

# Plot stratigraphic ranges
ex <- tax_range_strat(data, name = "taxon")
plot(ex)

ex <- tax_range_strat(data, name = "taxon", certainty = "certainty")
plot(ex, ylab = "Stratigraphic height (m)")


# Plot stratigraphic ranges with more labelling
ex <- tax_range_strat(data, name = "taxon", certainty = "certainty",
                      by = "name")
eras_custom <- data.frame(name = c("Mesozoic", "Cenozoic"),
                          max_age = c(0.5, 3.5),
                          min_age = c(3.5, 10.5),
                          color = c("#67C5CA", "#F2F91D"))
plot(ex, main = "Section A", ylab = "Stratigraphic height (m)",
     axis = axis_geo(side = 4, intervals = eras_custom, tick_labels = FALSE))


# Update margins for plotting
par(mar = c(5, 6, 6, 2))
# Pull class data
data$class <- tetrapods$class[1:50]
# Group stratigraphic ranges by class
ex <- tax_range_strat(data, name = "taxon", group = "class",
                      certainty = "certainty", by = "name")
plot(ex, main = "Section A", ylab = "Stratigraphic height (m)")

```
