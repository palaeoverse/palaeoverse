#' Check if a taxon is certain or uncertain.
#'
#' Check whether a given taxon in a vector is certain or uncertain.
#' Binomial names (genus and species) are considered "certain",
#' whereas names containing only the genus,
#' or genus and not determined species is considered "uncertain".
#'
#' @param taxon_names Character vector containing the species names to be checked.
#' It allows taxon names separated by underscore or space.
#' @return Character vector classifying each species as certain ("1") or uncertain ("0").
#' @examples
#' sirenia_sp <- c("Metaxytherium indet.", "Felsinotherium_serresii", "Felsinotherium gervaisi",
#'                 "Potamosiren indet.", "Potamosiren", "Metaxytherium sp.")
#'
#' certainty <- taxon_uncert(sirenia_sp)
#'
#' @export
taxon_uncert <- function(taxon_names) {

  if (!is.character(taxon_names)) {
    stop("'taxon_names' should be a chracter vector")
  }
  if (length(taxon_names) == 0) {
    stop("'taxon_names' is empty")

  if (is.null(taxon_names)) {
    stop("'taxon_names' is NULL")
  }
  }
  if (any(is.na(taxon_names))) {
    stop("'taxon_names' should not contain NA values")
  }

  match_patterns <- c("cf.", "aff.", "\\?", "^[A-Za-z]*$", "indet.", "sp.")

  certainty <- apply(sapply(match_patterns, grepl, taxon_names), 1, any)

  certainty <- ifelse(certainty, "0", "1")

  return(certainty)
}

