# Taxonomic spell check

A function to check for and count potential spelling variations of the
same taxon. Spelling variations are checked within alphabetical groups
(default), or within higher taxonomic groups if provided.

## Usage

``` r
tax_check(
  taxdf,
  name = "genus",
  group = NULL,
  dis = 0.05,
  start = 1,
  verbose = TRUE
)
```

## Arguments

- taxdf:

  `data.frame`. A dataframe with named columns containing taxon names
  (e.g. "species", "genus"). An optional column containing the groups
  (e.g. "family", "order") which taxon names belong to may also be
  provided (see `group` for details). NA values or empty strings in the
  name and group columns (i.e. "" and " ") are ignored.

- name:

  `character`. The column name of the taxon names you wish to check
  (e.g. "genus").

- group:

  `character`. The column name of the higher taxonomic assignments in
  `taxdf` you wish to group by. If `NULL` (default), name comparison
  will be conducted within alphabetical groups.

- dis:

  `numeric`. The dissimilarity threshold: a value greater than 0
  (completely dissimilar), and less than 1 (completely similar).
  Potential synonyms above this threshold are not returned. This value
  is set to 0.05 by default, but the user might wish to experiment with
  this value for their specific data.

- start:

  `numeric`. The number of shared characters at the beginning of
  potential synonyms that should match. Potential synonyms below this
  value will not be returned. By default this value is set to 1 (i.e.
  the first letter of synonyms must match).

- verbose:

  `logical`. Should the results of the non-letter character check be
  reported to the user? If `TRUE`, the result will only be reported if
  such characters are detected in the taxon names.

## Value

If verbose = `TRUE` (default), a `list` with three elements. The first
element in the list (synonyms) is a `data.frame` with each row reporting
a pair of potential synonyms. The first column "group" contains the
higher group in which they occur (alphabetical groupings if `group` is
not provided). The second column "greater" contains the most common
synonym in each pair. The third column "lesser" contains the least
common synonym in each pair. The third and fourth column
(`count_greater`, `count_lesser`) contain the respective counts of each
synonym in a pair. If no matches were found for the filtering arguments,
this element is `NULL` instead. The second element (`non_letter_name`)
is a vector of taxon names which contain non-letter characters, or
`NULL` if none were detected. The third element (non_letter_group) is a
vector of taxon groups which contain non-letter characters, or `NULL` if
none were detected. If verbose = `FALSE`, a `data.frame` as described
above is returned, or `NULL` if no matches were found.

## Details

When higher taxonomy is provided, but some entries are missing,
comparisons will still be made within alphabetical groups of taxa which
lack higher taxonomic affiliations. The function also performs a check
for non-letter characters which are not expected to be present in
correctly-formatted taxon names. This detection may be made available to
the user via the `verbose` argument. Comparisons are performed using the
Jaro dissimilarity metric via
[`stringdist::stringdistmatrix()`](https://rdrr.io/pkg/stringdist/man/stringdist.html).

As all string distance metrics rely on approximate string matching,
different metrics can produce different results. This function uses Jaro
distance as it was designed with short, typed strings in mind, but good
practice should include comparisons using multiple metrics, and
ultimately specific taxonomic vetting where possible. A more complete
implementation and workflow for cleaning taxonomic occurrence data is
available in the `fossilbrush` R package on CRAN.

## Reference

van der Loo, M. P. J. (2014). The stringdist package for approximate
string matching. The R Journal 6, 111-122.

## Developer(s)

Joseph T. Flannery-Sutherland & Lewis A. Jones

## Reviewer(s)

Lewis A. Jones, Kilian Eichenseer & Christopher D. Dean

## Examples

``` r
if (FALSE) { # \dontrun{
# load occurrence data
data("tetrapods")
# Check taxon names alphabetically
ex1 <- tax_check(taxdf = tetrapods, name = "genus", dis = 0.1)
# Check taxon names by group
ex2 <- tax_check(taxdf = tetrapods, name = "genus",
                 group = "family", dis = 0.1)
} # }
```
