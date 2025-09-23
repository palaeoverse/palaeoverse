#' Classify the certainty of taxonomic identifications
#'
#' Check whether a given taxonomic name is certain or uncertain by screening
#' for common substitutes, abbreviations, qualifiers, and notations for
#' denoting the certainty of taxonomic identifications (see Details for
#' screening values).
#'
#' @param taxdf \code{data.frame}. A dataframe with a named column
#'   containing the taxonomic names to be checked.
#' @param name \code{character}. The column name of the taxonomic names you
#'   wish to check (e.g. "genus"). Taxonomic names separated by underscores or
#'   spaces are allowed.
#' @param rank \code{character}. Taxonomic rank to be considered when
#'   screening for certainty in taxonomic assignment (e.g. "species", "genus",
#'   "family", "order", "class"). See details.
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
#'   taxonomic name is considered certain. Screened values differ between
#'   `rank` to prevent any undesired side effects. For example, if seeking
#'   genus-level certainty, screening for "sp." would result in incorrect
#'   classification depending on the structure of input data (i.e. genus sp.).
#'
#'   Screened values when `rank` is "species":
#'   - "cf.", "aff.", "?", "indet.", "sp.", "ex gr.", "incertae sedis",
#'   "problematica", "informal", "NO_.*_SPECIFIED", and "NA"/NA.
#'
#'   Screened values when `rank` is any higher taxonomic rank (e.g. "genus"):
#'   - "cf.", "aff.", "?", "incertae sedis", "problematica", "informal",
#'   "NO_.*_SPECIFIED", and "NA"/NA.
#'
#' @section Developer(s):
#'     Bruna M. Farina, Lewis A. Jones
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
#'                            rank = "species", append = TRUE)
#' certainty <- tax_certainty(taxdf = occdf, name = "genus",
#'                            certainty = c("certain", "uncertain"),
#'                            rank = "species", append = FALSE)
#' @export
tax_certainty <- function(taxdf = NULL, name = NULL, rank = NULL,
                          certainty = c(1, 0),
                          append = TRUE) {
  # Error handling
  # Check taxdf is dataframe
  if (!is.data.frame(taxdf)) {
      stop("`taxdf` must be a data.frame.")
  }
  # Check for valid name input
  if (is.null(taxdf[[name]])) {
    stop("`names` is not a named column in `taxdf`.")
  }
  if (length(taxdf[[name]]) == 0) {
    stop("`names` is of length 0.")
  }
  if (!is.character(taxdf[[name]])) {
    stop("`names` must be of class character.")
  }
  # Check for valid rank input
  if (is.null(rank) ||
      !rank %in% c("species", "genus", "family", "order", "class")) {
    stop("Invalid `rank`. It must be either: 'species', 'genus', 'family',
         'order', or 'class'.")
  }
  # Check for valid append input
  if (!is.logical(append)) {
    stop("`append` must be of class logical (TRUE/FALSE).")
  }
  # Common PBDB entry (NO_X_SPECIFIED)
  not_specified <- paste0("NO_.*_SPECIFIED")
  # Assign rank for handling
  rank <- ifelse(rank == "species", "species", "other")
  # Create temporary taxdf to not replace original values
  taxdf$certainty <- taxdf[[name]]
  # Replace empty rows with NA
  taxdf$certainty <- gsub(pattern = "^$|^\\s+$", replacement = "NA",
                          x = taxdf$certainty)
  # Change NA to character
  taxdf$certainty[which(is.na(taxdf$certainty))] <- "NA"
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
  matches <- sapply(match_patterns, grepl, taxdf$certainty, ignore.case = TRUE)
  # Calculate row sums (1 = match/uncertain, 0 = no match/certain)
  if (!is.matrix(matches)) {
    matches <- sum(matches)
  } else {
    matches <- rowSums(as.matrix.data.frame(matches))
  }
  # Match user definitions
  # first element of `certainty` is certain (0 + 1), second uncertain (1 + 1)
  matches <- matches + 1
  # Update values with more than 1 uncertainty match
  matches[matches > 2] <- 2
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
