# Look up geological intervals and assign geological stages

A function that uses interval names to assign either [international
geological
stages](https://stratigraphy.org/ICSchart/ChronostratChart2022-02.pdf)
and numeric ages from the International Commission on Stratigraphy
(ICS), or user-defined intervals, to fossil occurrences.

## Usage

``` r
look_up(
  occdf,
  early_interval = "early_interval",
  late_interval = "late_interval",
  int_key = FALSE,
  assign_with_GTS = "GTS2020",
  return_unassigned = FALSE
)
```

## Arguments

- occdf:

  `data.frame`. A dataframe of fossil occurrences or other geological
  data, with columns of class `character` specifying the earliest and
  the latest possible interval associated with each occurrence.

- early_interval:

  `character`. Name of the column in `occdf` that contains the earliest
  interval from which the occurrences are from. Defaults to
  "early_interval".

- late_interval:

  `character`. Name of the column in `occdf` that contains the latest
  interval from which the occurrences are from. Defaults to
  "late_interval".

- int_key:

  `data.frame`. A dataframe linking interval names to international
  geological stage names from the ICS, or other, user-defined intervals.
  This dataframe should contain the following named columns containing
  `character` values:  

  - `interval_name` contains the names to be matched from `occdf`  

  - `early_stage` contains the names of the earliest stages
    corresponding to the intervals  

  - `late_stage` contains the latest stage corresponding to the
    intervals  

  Optionally, named `numeric` columns provide maximum and minimum ages
  for the intervals:  

  - `max_ma`

  - `min_ma`

  If set to `FALSE` (default), stages and numerical ages can be assigned
  based on one of the GTS tables (see below).

- assign_with_GTS:

  `character` or `FALSE`. Allows intervals to be searched in the
  `GTS2020` (default) or the `GTS2012` table. Set to `FALSE` to disable.

- return_unassigned:

  `logical`. Return interval names which could not be assigned, instead
  of the dataframe with assignments. Defaults to `FALSE`.

## Value

A `dataframe` of the original input `data` with the following appended
columns is returned: `early_stage` and `late_stage`, corresponding to
the earliest and latest international geological stage which could be
assigned to the occurrences based on the given interval names.
`interval_max_ma` and `interval_min_ma` return maximum and minimum
interval ages if provided in the interval key, or if they can be fetched
from GTS2012 or GTS2020. A column `interval_mid_ma` is appended to
provide the midpoint ages of the intervals.

## Details

If `int_key` is set to `FALSE` (default), this function can be used to
assign numerical ages solely based on stages from a GTS table, and to
assign stages based on GTS interval names.

Instead of geological stages, the user can supply any names in the
`early_stage` and `late_stage` column of `int_key`. `assign_with_GTS`
should then be set to `FALSE`.

An exemplary `int_key` has been included within this package
([`interval_key`](https://palaeoverse.palaeoverse.org/reference/interval_key.md)).
This key works well for assigning geological stages to many of the
intervals from the [Paleobiology Database](https://paleobiodb.org) and
the [PaleoReefs Database](https://www.paleo-reefs.pal.uni-erlangen.de/).
`palaeoverse` cannot guarantee that all of the stage assignments with
the exemplary key are accurate. The table corresponding to this key can
be loaded with
[`palaeoverse::interval_key`](https://palaeoverse.palaeoverse.org/reference/interval_key.md).

## Developer(s)

Kilian Eichenseer & William Gearty

## Reviewer(s)

Lewis A. Jones & Christopher D. Dean

## Examples

``` r
## Just use GTS2020 (default):
# create exemplary dataframe
taxdf <- data.frame(name = c("A", "B", "C"),
early_interval = c("Maastrichtian", "Campanian", "Sinemurian"),
late_interval = c("Maastrichtian", "Campanian", "Bartonian"))
# assign stages and numerical ages
taxdf <- look_up(taxdf)

## Use exemplary int_key
# Get internal reef data
occdf <- reefs
 # assign stages and numerical ages
occdf <- look_up(occdf,
                early_interval = "interval",
                late_interval = "interval",
                int_key = interval_key)

## Use exemplary int_key and return unassigned
# Get internal tetrapod data
occdf <- tetrapods
# assign stages and numerical ages
occdf <- look_up(occdf, int_key = palaeoverse::interval_key)
#> Warning: `NA`, `""` or `" "` entries from `late_interval` have been
#>             filled in with the corresponding `early_interval` entries
#> Warning: The following intervals could not be matched with intervals from int_key
#>       or GTS: Early Triassic, Late Pennsylvanian, Vokhmian
# return unassigned intervals
unassigned <- look_up(occdf, int_key = palaeoverse::interval_key,
                      return_unassigned = TRUE)
#> Warning: `NA`, `""` or `" "` entries from `late_interval` have been
#>             filled in with the corresponding `early_interval` entries

## Use own key and GTS2012:
# create example data
occdf <- data.frame(
  stage = c("any Permian", "first Permian stage",
            "any Permian", "Roadian"))
# create example key
interval_key <- data.frame(
  interval_name = c("any Permian", "first Permian stage"),
  early_stage = c("Asselian", "Asselian"),
  late_stage = c("Changhsingian", "Asselian"))
# assign stages and numerical ages:
occdf <- look_up(occdf,
                 early_interval = "stage", late_interval = "stage",
                 int_key = interval_key, assign_with_GTS = "GTS2012")
```
