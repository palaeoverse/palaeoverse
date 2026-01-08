# Example dataset: Interval key for the look_up function

A table of geological intervals and the earliest and latest
corresponding international geological stages from the International
Commission on Stratigraphy (ICS). The table was compiled using regional
stratigraphies, the [GeoWhen
Database](https://timescalefoundation.org/resources/geowhen/), temporal
information from the [Paleobiology
Database](https://paleobiodb.org/classic/displaySearchStrataForm) and
the [Geological Timescale
2022](https://stratigraphy.org/ICSchart/ChronostratChart2022-10.pdf).
Some assignments were made with incomplete information on the
stratigraphic provenance of intervals. The assignments in this table
should be verified before research use. They are provided here as an
example of functionality only.

## Usage

``` r
interval_key
```

## Format

A data frame with 1323 rows and 3 variables:

- interval_name:

  Stratigraphic interval

- early_stage:

  Earliest (oldest) geological stage which overlaps with the interval

- late_stage:

  Latest (youngest) geological stage which overlaps with the interval

## Source

Compiled by Kilian Eichenseer and Lewis Jones for assigning geological
stages to ccurrences from the [Paleobiology
Database](https://paleobiodb.org) and the [PaleoReefs
Database](https://www.paleo-reefs.pal.uni-erlangen.de/).
