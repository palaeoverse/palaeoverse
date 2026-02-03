#' Classify the certainty of taxonomic identifications
#'
#' Check whether a given taxonomic name is certain or uncertain by screening
#' for common substitutes, abbreviations, qualifiers, and notations for
#' denoting the certainty of taxonomic identifications (see Details for
#' screening values).
#'
#' @param taxdf \code{data.frame}. A dataframe with a named column containing
#'   the taxonomic names to be checked.
#' @param name \code{character}. The column name of the taxonomic names you
#'   wish to check (e.g. "identified_name").
#' @param terms \code{list}. A named list of uncertainty terms to screen
#'   `name` for. Matched values will be classified as "uncertain". A
#'   pre-defined named list of terms is screened for by default (see Details).
#'   These terms can be ignored, or replaced through this argument (e.g.
#'   `terms = list(species = "")`).
#' @param certainty \code{character}. A vector of length two denoting
#'   how certainty should be coded. The first element of the vector denotes
#'   "certain" status (default: 1), while the second denotes "uncertain"
#'   status (default: 0).
#' @param append \code{logical}. If \code{TRUE} (default), the returned object
#'   is a \code{data.frame} consisting of the input \code{taxdf} with a column
#'   denoting the taxonomic 'certainty' appended. If \code{FALSE}, a vector
#'   containing the taxonomic identification certainty status is returned.
#'
#' @return When `append` is \code{TRUE}, the input \code{taxdf} with an
#'   appended 'certainty' column classifying each taxon (default), or a vector
#'   if \code{append} is `FALSE`.
#'
#' @details This function screens `name` for common substitutes,
#'   abbreviations, qualifiers, and notations expressing uncertainty in
#'   taxonomic identifications. When **any** of these notations are present,
#'   the taxonomic name is considered uncertain, while in their absence, the
#'   taxonomic name is considered certain. A pre-defined named list of terms
#'   is screened for by default, with the follow names and values:
#'
#'   - subspecies: ssp., subsp.
#'   - species: sp., spp.
#'   - genus: gen.
#'   - family: fam.
#'   - indeterminable: indeterminabilis, indeterminata, indet., ind.
#'   - uncertain: incerta, ind., "", ''
#'   - confer: confer, cf., cfr., conf.
#'   - dubia: dubia, sp. dub., nomen dubium
#'   - incertae: incertae sedis, inc. sed.
#'   - problematica: problematica
#'   - informal: informal
#'   - unavailable: NA
#'   - trace: ex., exuvia, exuviae
#'   - not_specified: NO_X_SPECIFIED, where X is any character string
#'
#'   Additional terms to screen for can be provided via the `terms` argument
#'   via a named list (e.g. `terms = list(custom = "species1")`). In addition,
#'   the pre-defined named list can be modified to omit, or update certain
#'   terms (e.g. `terms = list(species = "")` or
#'   `terms = list(genus = "gen\\.", "genus")`).
#'
#'   The pre-defined list is intended to be comprehensive, and is informed by:
#'
#'   - Sigovini, M., Keppel, E., & Tagliapietra, D. (2016). Open Nomenclature in
#'   the biodiversity era. *Methods in Ecology and Evolution*, 7(10), 1217-1225.
#'   \doi{10.1111/2041-210X.12594}.
#'
#'   If you wish additional terms to be screened for by default, please
#'   raise a [GitHub Issue](https://github.com/palaeoverse/palaeoverse/issues).
#'
#' @section Developer(s):
#'     Lewis A. Jones, Bruna M. Farina
#' @section Reviewer(s):
#'     Lewis A. Jones, Bethany J. Allen, & William Gearty
#'
#' @examples
#' # Get internal data
#' data(tetrapods)
#' occdf <- tetrapods[1:100, ]
#' # Summarise taxonomic certainty
#' certainty <- tax_certainty(taxdf = occdf, name = "identified_name",
#'                            certainty = c("certain", "uncertain"),
#'                            append = TRUE)
#' certainty <- tax_certainty(taxdf = occdf, name = "genus",
#'                            terms = list(subspecies = "", species = ""),
#'                            certainty = c("certain", "uncertain"),
#'                            append = FALSE)
#' @export
tax_certainty <- function(taxdf = NULL, name = NULL, terms = NULL,
                          certainty = c(1, 0), append = TRUE) {
  # Error handling
  # Check taxdf is dataframe
  if (!is.data.frame(taxdf)) {
      stop("`taxdf` must be a data.frame.")
  }
  # Check for valid name input
  if (is.null(taxdf[[name]])) {
    stop("`names` is not a named column in `taxdf`.")
  }
  if (!is.character(taxdf[[name]])) {
    stop("`names` must be of class character.")
  }
  # Check for valid list input
  if (!is.list(terms) & !is.null(terms)) {
    stop("`terms` must be of class list or NULL.")
  }
  # Check for valid append input
  if (!is.logical(append)) {
    stop("`append` must be of class logical (TRUE/FALSE).")
  }
  # Create temporary taxdf to not replace original values
  taxdf$certainty <- taxdf[[name]]
  # Replace empty rows with NA
  taxdf$certainty <- gsub(pattern = "^$|^\\s+$",
                          replacement = NA_character_,
                          x = taxdf$certainty)
  # Terms to screen for
  screen <- list(subspecies = c("ssp\\.", "subsp\\."),
                 species = c("sp\\.", "spp\\."),
                 genus = c("gen\\."),
                 family = c("fam\\."),
                 indeterminable = c("indeterminabilis", "indeterminata",
                                    "indet\\.", "ind\\."),
                 uncertain = c("incerta", "?", "inc\\.",
                               "\\\"\\\"", "\\\'\\\'"),
                 confer = c("confer$", "cf\\.", "cfr\\.", "conf\\."),
                 dubia = c("dubia$","sp\\. dub\\.", "nomen dubium"),
                 incertae = c("incertae sedis", "inc\\. sed\\."),
                 problematica = c("problematica"),
                 informal = c("informal"),
                 unavailable = c("^NA$"),
                 trace = c("ex\\.", "exuvia", "exuviae"),
                 not_specified = c("NO_.*_SPECIFIED"))
  # Update pre-defined terms and/or include custom terms
  screen[names(terms)] <- terms
  # Identify taxonomic certainty
  matches <- lapply(screen, function(x) {
    sapply(x, grepl, taxdf$certainty, ignore.case = TRUE)
  })
  matches <- do.call(cbind, matches)
  # Calculate row sums (1 (or more) = match/uncertain, 0 = no match/certain)
  matches <- rowSums(matches)
  # Match user definitions
  # first element of `certainty` is certain (0 + 1), second uncertain (>1 + 1)
  matches <- matches + 1
  # Update values with more than 1 uncertainty match
  matches[matches >= 2] <- 2
  # Extract certainty
  classif <- certainty[matches]
  # Add to dataframe?
  if (append == TRUE) {
    taxdf$certainty <- classif
    return(taxdf)
  } else {
    return(classif)
  }
}
