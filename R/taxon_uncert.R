#' taxon_uncert
#'
#' Check whether each taxon in a vector is certain or uncertain.
#'
#' @param taxon_names Character vector containing the species names to be checked.
#' @return Return a character vector classifying each species as certain ("1") or uncertain ("0")
#' @examples
#' sirenia_sp <- c("Metaxytherium indet.", "Felsinotherium_serresii", "Felsinotherium gervaisi",
#'                 "Potamosiren indet.", "Potamosiren", "Metaxytherium sp.")
#'
#'  certainty <- taxon_uncert(sirenia_sp)
#'
#' @export
taxon_uncert <- function(taxon_names) {

  if (!is.character(taxon_names)) {
    return ("Input should be a chracter vector")
  }
  if (length(taxon_names) == 0) {
    return("Input is empty")
  }
  if (any(is.na(taxon_names))) {
    return("Input should not contain NAs")
  }

  match_patterns <- c("cf.", "aff.", "\\?", "^[A-Za-z]*$", "indet.", "sp.")

  certainty <- apply(sapply(match_patterns, grepl, taxon_names), 1, any)

  certainty <- ifelse(certainty, "0", "1")

  return(certainty)
}
