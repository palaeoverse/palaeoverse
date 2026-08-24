# Example dataset: Early tetrapod data from the Paleobiology Database

A dataset of tetrapod occurrences ranging from the Carboniferous through
to the Early Triassic, from the [Palaeobiology
Database](https://paleobiodb.org/). Dataset includes a range of
variables relevant to common palaeobiological analyses, relating to
identification, geography, environmental context, traits and more.
Additional information can be found
[here](https://paleobiodb.org/data1.2/). The downloaded data is
unaltered, with the exception of removing some superfluous variables,
and can be used to demonstrate how the functions in the palaeoverse
package might be applied.

## Usage

``` r
tetrapods
```

## Format

A data frame with 5270 rows and 32 variables:

- occurrence_no:

  Reference number given to the particular occurrence in the
  Paleobiology Database

- collection_no:

  Reference number given to the Paleobiology Database collection
  (locality) that the occurrence belongs to

- identified_name:

  Taxon name as it appears in the original publication, which may
  include expressions of uncertainty (e.g. "cf.", "aff.", "?") or
  novelty (e.g. "n. gen.", "n. sp.")

- identified_rank:

  The taxonomic rank, or resolution, of the identified name

- accepted_name:

  Taxon name once the identified name has passed through the
  Paleobiology Database's internal taxonomy, which collapses synonyms,
  amends binomials which have been altered (e.g. species moving to
  another genus) and updates taxa which are no longer valid (e.g.
  *nomina dubia*)

- accepted_rank:

  The taxonomic rank, or resolution, of the accepted name

- early_interval:

  The oldest (or only) time interval within which the occurrence is
  thought to have been deposited

- late_interval:

  The youngest time interval within which the occurrence is thought to
  have been deposited

- max_ma, min_ma:

  The age range given to the occurrence

- phylum, class, order, family, genus:

  The taxa (of decreasing taxonomic level) which the occurrence is
  identified as belonging to

- abund_value, abund_unit:

  The number (and units) of fossils attributed to the occurrence

- lng, lat:

  The modern-day longitude and latitude of the fossil locality

- collection_name:

  The name of the Paleobiology Database collection which the occurrence
  belongs to, typically a spatio-temporally restricted locality

- cc:

  The country (code) where the fossils were discovered

- formation, stratgroup, member:

  The geological units from which the fossils were collected

- zone:

  The biozone which the occurrence is attributed to

- lithology1:

  The main lithology of the beds in the section where the fossils were
  collected

- environment:

  The inferred environmental conditions in the place of deposition

- pres_mode:

  The mode of preservation of the fossils found in the collection (not
  necessarily of that specific occurrence), which will include
  information on whether they are body or trace fossils

- taxon_environment:

  The environment within which the taxon is thought to have lived,
  collated within the Paleobiology Database

- motility, life_habit, diet:

  Various types of trait data for the taxon, collated within the
  Paleobiology Database

## Source

Compiled by Bethany Allen, current version downloaded on 14th July 2022.
See item descriptions for details.

## References

Uhen MD et al. (2023). Paleobiology Database User Guide Version 1.0.
PaleoBios, 40 (11).
[doi:10.5070/P9401160531](https://doi.org/10.5070/P9401160531) .  
