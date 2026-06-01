# Classify the certainty of taxonomic identifications

Check whether a given taxonomic name is certain or uncertain by
screening for common substitutes, abbreviations, qualifiers, and
notations for denoting the certainty of taxonomic identifications (see
Details for screening values).

## Usage

``` r
tax_certainty(
  taxdf = NULL,
  name = NULL,
  terms = NULL,
  certainty = c(1, 0),
  append = TRUE
)
```

## Arguments

- taxdf:

  `data.frame`. A `data.frame` with a named column containing the
  taxonomic names to be checked.

- name:

  `character`. The column name of the taxonomic names you wish to check
  (e.g. "identified_name").

- terms:

  `list`. A named list of uncertainty terms to screen `name` for.
  Matched values will be classified as "uncertain". A pre-defined named
  list of terms is screened for by default (see Details). These terms
  can be ignored (e.g. `terms = list(species = NULL)`), or replaced
  through this argument (e.g. `terms = list(species = "sp1")`). Note,
  screened terms are not case-sensitive.

- certainty:

  `vector`. A vector of length two denoting how certainty should be
  coded. The first element of the vector denotes "certain" status
  (default: 1), while the second denotes "uncertain" status (default:
  0).

- append:

  `logical`. If `TRUE` (default), the returned object is a `data.frame`
  consisting of the input `taxdf` with a column denoting the taxonomic
  "certainty" appended. If `FALSE`, a two-column `data.frame` containing
  the input `name` and the taxonomic identification certainty status is
  returned.

## Value

When `append` is `TRUE`, the input `taxdf` with an appended "certainty"
column classifying each taxon (default). When `append` is `FALSE`, a
two-column `data.frame` with input `name` and 'certainty' column
classifying each taxon.

## Details

This function screens `name` for common substitutes, abbreviations,
qualifiers, and notations expressing uncertainty in taxonomic
identifications. When **any** of these notations are present, the
taxonomic name is considered uncertain, while in their absence, the
taxonomic name is considered certain. A pre-defined named list of terms
is screened for by default (i.e.
`list(subspecies = c("(?<!n\\. )ssp\\.", "(?<!n\\. )subsp\\."), ...)`),
with the following names and values:

- subspecies: ssp., subsp. (while ignoring n. ssp. and n. subsp.)

- species: sp., spp. (while ignoring n. sp. and n. spp.)

- genus: gen. (while ignoring n. gen. and n. gen.)

- family: fam. (while ignoring n. fam.)

- indeterminable: indeterminabilis, indeterminata, indet., ind.

- uncertain: incerta, ind., ?, "", ”

- confer: confer, cf., cfr., conf.

- dubia: dubia, sp. dub., nomen dubium

- incertae: incertae sedis, inc. sed.

- problematica: problematica

- informal: informal

- unavailable: NA

- trace: ex., exuvia, exuviae

- not_specified: NO_X_SPECIFIED, where X is any character string

Additional terms to screen for can be provided via the `terms` argument
via a named list (e.g. `terms = list(custom = "species1")`). In
addition, the pre-defined named list can be modified to omit, or update
certain terms (e.g. `terms = list(species = NULL)` or
`terms = list(genus = c("(?<!n\\. )gen\\.")`). Note, while this function
intends to minimise false positives (e.g. use of "sp." over "sp" to
avoid mid-name matches, ignoring "n. gen." (new genus) but flagging
"gen."), it is the responsibility of the user to understand the scale of
risk for screened terms with respect to the input data.

The pre-defined list is intended to be comprehensive, and is informed
by:

- Sigovini, M., Keppel, E., & Tagliapietra, D. (2016). Open Nomenclature
  in the biodiversity era. *Methods in Ecology and Evolution*, 7(10),
  1217-1225.
  [doi:10.1111/2041-210X.12594](https://doi.org/10.1111/2041-210X.12594)
  .

If you wish additional terms to be screened for by default, please raise
a [GitHub Issue](https://github.com/palaeoverse/palaeoverse/issues).

## Developer(s)

Lewis A. Jones, Bruna M. Farina

## Reviewer(s)

Lewis A. Jones, Bethany J. Allen, & William Gearty

## Examples

``` r
# Get internal data
data(tetrapods)
occdf <- tetrapods[1:100, ]
# Summarise taxonomic certainty
certainty <- tax_certainty(taxdf = occdf, name = "identified_name",
                           append = FALSE)
# Append uncertainty to dataframe
certainty <- tax_certainty(taxdf = occdf, name = "identified_name",
                           certainty = c("certain", "uncertain"),
                           append = TRUE)
# Turn off subspecies- and species-level screening terms (genus-level data)
certainty <- tax_certainty(taxdf = occdf, name = "identified_name",
                           terms = list(subspecies = NULL, species = NULL),
                           certainty = c("certain", "uncertain"),
                           append = FALSE)
```
