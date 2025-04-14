#' Classify the certainty of taxonomic identifications
#'
#' Check whether a given taxonomic name is certain or uncertain by screening
#' for common substitutes, abbreviations, qualifiers, and notations for
#' denoting the certainty of taxonomic identifications (see Details for
#' screening values).
#'
#' @param taxdf \code{data.frame}. A dataframe with named columns
#'   containing taxonomic names to be checked (e.g. "species"). Names
#'   separated by underscores or spaces are allowed.
#' @param name \code{character}. The column name of the taxonomic names you
#'   wish to check (e.g. "genus").
#' @param rank \code{character}. Taxonomic rank to be considered when
#'   screening for certainty in taxonomic assignment (e.g. "species", "genus",
#'   "family", "order", "class").
#' @param certainty \code{character}. A vector of length two denoting
#'   how certainty should be coded. The first element of the vector denotes
#'   "certain" status (default: 1), while the second denotes "uncertain"
#'   status (default: 0).
#' @param add_df \code{logical}. If \code{TRUE} (default), a 'certainty'
#'   column is appended to the input \code{taxdf}. Otherwise, a vector of
#'   taxonomic identification certainty status is returned.
#'
#' @return The input \code{taxdf} with an appended 'certainty' column
#'   classifying each taxon (default), or a vector if \code{add_df} is
#'   `FALSE`.
#'
#' @details This function screens for common substitutes, abbreviations,
#'   qualifiers, and notations for denoting the certainty of taxonomic
#'   identifications.
#'
#'   Screened values when `rank` is "species":
#'   - "cf.", "aff.", "?", "indet.", "sp.", "ex gr.", "incertae sedis",
#'   "problematica", "informal", "NO_SPECIES_SPECIFIED", and "NA"/NA.
#'
#'   Screened values when `rank` is any higher taxonomic rank (e.g. "genus"):
#'   - "cf.", "aff.", "?", "incertae sedis", "problematica", "informal",
#'   "NO_`rank`_SPECIFIED", and "NA"/NA.
#'
#' @section Developer(s):
#'     Bruna M. Farina
#' @section Reviewer(s):
#'     Lewis A. Jones, Bethany J. Allen, & William Gearty
#'
#' @examples
#' # Get internal data
#' data(tetrapods)
#' # Summarise taxonomic certainty
#' certainty <- tax_certainty(taxdf = tetrapods, name = "accepted_name",
#'                            certainty = c("certain", "uncertain"),
#'                            rank = "species", add_df = TRUE)
#' @export
tax_certainty <- function(taxdf = NULL, name = NULL, rank = NULL,
                          certainty = c(1, 0),
                          add_df = TRUE) {
  # Error handling
  # Check taxdf is dataframe
  if (!is.data.frame(taxdf)) {
      stop("`taxdf` must be a data.frame")
  }
  # Check for valid name input
  if (is.null(taxdf[[name]]) ||
      !is.character(taxdf[[name]]) ||
      length(taxdf[[name]]) == 0) {
    stop("`names` must be a non-empty character vector and cannot be NULL")
  }
  # Check for valid rank input
  if (is.null(rank) ||
      !rank %in% c("species", "genus", "family", "order", "class")) {
    stop("Invalid `rank`. It must be either: 'species', 'genus', 'family',
         'order', or 'class'")
  }
  # Common PBDB entry (NO_RANK_SPECIFIED)
  not_specified <- paste0("NO_", toupper(rank), "_SPECIFIED")
  # Assign rank for handling
  rank <- ifelse(rank == "species", "species", "other")
  # Create temporary taxdf to not replace original values
  tmpdf <- taxdf
  # Replace empty rows with NA
  tmpdf[[name]] <- gsub(pattern = "^$|^\\s+$", replacement = "NA",
                        x = tmpdf[[name]])
  # Change NA to character
  tmpdf[[name]][which(is.na(tmpdf[[name]]))] <- "NA"
  # Define match patterns
  match_patterns <- switch(rank,
                          "species" = c("\\S+\\s+cf\\.\\s+\\S+",
                                        "\\S+\\s+aff\\.\\s+\\S+",
                                        "\\S+\\s*\\?\\s*\\S+", "sp\\.$",
                                        "informal","\\S+\\s+ex gr\\.\\s+\\S+",
                                        "indet\\.","incertae sedis",
                                        "problematica", "^NA$", not_specified),
                          "other" = c("^\\cf\\.\\s*\\S+", "^\\aff\\.\\s*\\S+",
                                      "^\\?\\S*", "incertae sedis",
                                      "problematica", "^NA$", not_specified))
  # Identify taxonomic certainty
  matches <- sapply(match_patterns, grepl, tmpdf[[name]], ignore.case = TRUE)
  # Calculate row sums (1 = match/uncertain, 0 = no match/certain)
  if (is.null(nrow(matches))) {
    matches <- sum(matches)
  } else {
    matches <- rowSums(as.matrix.data.frame(matches))
  }
  # Match user definitions
  # first element of `certainty` is certain (0 + 1), second uncertain (1 + 1)
  matches <- matches + 1
  classif <- certainty[matches]
  # Add to dataframe?
  if (add_df == TRUE) {
    taxdf$certainty <- classif
    return(taxdf)
  } else {
    return(classif)
  }
}
