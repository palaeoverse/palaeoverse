#' Classify the certainty of taxonomic identifications

#' Check whether a given taxon in a vector is certain or uncertain.
#'The function screens for "cf.", "aff.", "?", "indet.", "sp.", "ex gr.",
#'"incertae sedis", "problematica", and NA.
#'
#' @param taxon_df \code{data.frame}. A dataframe containing the taxa names
#' to be checked. It allows taxon names separated by underscore or space
#' @param name \code{character}. The column name of the taxon names
#' you wish to check
#' @param rank \code{character}. Taxonomic rank to be considered when screening
#' for certainty (species, genus, family, order, class). If no taxonomic rank
#' provided (e.g., unranked clade), the function will screen for all
#' previously mentioned qualifiers
#' @param certainty \code{character}. Vector of how to represent certainty and
#' uncertainty. Options are TRUE/FALSE, 0/1, or certain/uncertain (default)
#' @param add_df \code{logical}. If TRUE (default), it adds a "certainty" column
#' to the input dataframe
#'
#' @return Character vector classifying each species as "certain" or "uncertain"
#'
#' @section Developer(s):
#'     Bruna M. Farina
#' @section Reviewer(s):
#'     Lewis A. Jones
#'     Bethany J. Allen
#'
#' @examples
#' data(tetrapods)
#'
#' certainty <- tax_certainty(taxon_df = tetrapods name = "identified_name", rank = species,
#'                           add_df = TRUE)
#'
#' @export
tax_certainty <- function(taxon_df = NULL, name = NULL, rank = NULL,
                          certainty = c("certain", "uncertain"),
                          add_df = TRUE)


  if (!data.frame(taxon_df)) {
    stop("'taxon_df' must be a data.frame")
  }

if (is.null(taxon_df[,taxon_column]) ||
    !is.character(taxon_df[,taxon_column]) ||
    length(taxon_df[,taxon_column]) == 0) {
  stop("'names' must be a non-empty character vector and cannot be NULL")
}


if (rank == "species") {
  match_patterns <- c("\\S+\\s+\\cf\\.\\s+\\S+", "\\s+\\S+\\aff\\.\\s+\\S+",
                      "\\s+\\S+\\?\\s+\\S+", "sp\\.$", "informal", "ex gr\\.",
                      "indet\\.", "incertae sedis","problematica",  NA)
  classif  <- apply(sapply(match_patterns, grepl, taxon_df[,taxon_column]),
                    1, any)

} else if (rank == "genus") {
  match_patterns <- c("cf\\.\\s*\\S+", "aff\\.\\s*\\S+", "\\?\\s*\\S+", NA)
  classif <- apply(sapply(match_patterns, grepl, taxon_df[,taxon_column]),
                   1, any)
} else if (rank == "family" | "order" | "class") {
  match_patterns <- c("cf\\.\\s*\\S+", "aff\\.\\s*\\S+", "\\?\\s*\\S+",
                      "invertae sedis", "problematica", NA)
  classif <- apply(sapply(match_patterns, grepl, taxon_df[,taxon_column]),
                   1, any)
} else {
  match_patterns <- c("\\S+\\s+\\cf\\.\\s+\\S+", "cf\\.\\s*\\S+",
                      "\\s+\\S+\\aff\\.\\s+\\S+", "aff\\.\\s*\\S+",
                      "\\s+\\S+\\?\\s+\\S+","\\?\\s*\\S+",
                      "sp\\.$", "informal", "ex gr\\.", "indet\\.",
                      "incertae sedis","problematica",  NA)
  classif <- apply(sapply(match_patterns, grepl, taxon_df[,taxon_column]),
                   1, any)
}

if (certainty == c("certain", "uncertain")) {
  classif <- ifelse(classif, "certain", "uncertain")
} else if (certainty == c("TRUE", "FALSE")) {
  classif <- ifelse(classif, TRUE, FALSE)
} else if (certainty == c("0", "1")) {
  classif <- ifelse(classif, 0, 1)
}

if (add_df == TRUE) {
  taxon_dfdf['certainty'] <- certainty
} else {
  return(certainty)
}
