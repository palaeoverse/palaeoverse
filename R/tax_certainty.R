#' Classify the certainty of taxonomic identifications

#' Check whether a given taxon in a vector is certain or uncertain.
#'The function screens for 'cf.', 'aff.', '?', 'indet.', 'sp.', 'ex gr.',
#''incertae sedis', 'problematica', 'informal', and NA.
#'
#' @param taxa_df \code{data.frame}. A dataframe containing the taxa names
#' to be checked. It allows taxon names separated by underscore or space
#' @param name \code{character}. The column name of the taxon names
#' you wish to check
#' @param rank \code{character}. Taxonomic rank to be considered when screening
#' for certainty (species, genus, family, order, class).
#' @param certainty \code{character}. Vector of how to represent certainty and
#' uncertainty. Options are 'TRUE' (uncertain)/'FALSE' (certain),
#' '0' (certain)/'1' (uncertain), or 'certain'/'uncertain' (default)
#' @param add_df \code{logical}. If TRUE (default), it adds a 'certainty' column
#' to the input dataframe
#'
#' @return A character vector classifying each species as 'certain' or
#' 'uncertain', or a new column added to the input dataframe (default).
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
#' tetrapods_sp <- tetrapods[which(tetrapods$identified_rank == "species"),]
#'
#' certainty <- tax_certainty(taxa_df = tetrapods_sp, name = "identified_name",
#'                           certainty = c("certain", "uncertain"),
#'                           rank = "species", add_df = TRUE)
#'
#' @export
tax_certainty <- function(taxa_df = NULL, name = NULL, rank = NULL,
                          certainty = c("certain", "uncertain"),
                          add_df = TRUE) {

  if (!is.data.frame(taxa_df)) {
      stop("'taxon_df' must be a data.frame")
  }

  if (is.null(taxa_df[[name]]) ||
      !is.character(taxa_df[[name]]) ||
      length(taxa_df[[name]]) == 0) {
    stop("'names' must be a non-empty character vector and cannot be NULL")
  }

  if (!rank %in% c("species", "genus", "family", "order", "class")) {
    stop("Invalid 'rank'. It must be one of 'species', 'genus', 'family',
    'order', or 'class'")
  }

  if (is.null(rank)) {
    stop("Invalid 'rank'. It must be one of 'species', 'genus', 'family',
    'order', or 'class'")
  }
  if (!certainty[1]  %in% c("certain", "uncertain", "TRUE",
                          "FALSE", "0", "1")) {
    stop("Invalid 'certainty' option. Must be 'certain'/'uncertain',
        'TRUE'/'FALSE', or '0'/'1'")
  }

  rank <- ifelse(rank %in% c("genus", "family", "order", "class"), "other",
                 rank)

  taxa_df[[name]] <- gsub("^$|^\\s+$", NA, taxa_df[[name]])

  match_patterns <- switch(rank,
                          "species" = c("\\S+\\s+cf\\.\\s+\\S+",
                                        "\\S+\\s+aff\\.\\s+\\S+",
                                        "\\S+\\s*\\?\\s*\\S+", "sp\\.$",
                                        "informal","\\S+\\s+ex gr\\.\\s+\\S+",
                                        "indet\\.","incertae sedis",
                                        "problematica"),

                          "other" = c("cf\\.\\s*\\S+", "aff\\.\\s*\\S+",
                                      "\\?\\s*\\S+", "incertae sedis",
                                      "problematica"))


  if (rank == "species") {
    matches <- sapply(match_patterns, grepl, taxa_df[[name]], ignore.case = TRUE)

    if (is.vector(matches)) {
      matches <- matrix(matches, nrow = 1, byrow = TRUE)
    }
    na_entries <- is.na(taxa_df[[name]])
    classif <- rowSums(matches) > 0

  } else if (rank == "other") {
    matches <- sapply(match_patterns, grepl, taxa_df[[name]], ignore.case = TRUE)

    if (is.vector(matches)) {
      matches <- matrix(matches, nrow = 1, byrow = TRUE)
    }
    na_entries <- is.na(taxa_df[[name]])
    classif <- rowSums(matches) > 0

  }


  if ((certainty[1] == "certain" || certainty[1] =="uncertain")
      & (certainty[2] == "certain" || certainty[2] == "uncertain")) {
    classif <- ifelse(classif, "uncertain", "certain")

  } else if ((certainty[1] == "TRUE" || certainty[1] == "FALSE") &
             (certainty[2] == "TRUE" ||certainty[2] == "FALSE")) {
    classif <- ifelse(classif, TRUE, FALSE)

  } else if ((certainty[1] == "0" || certainty[1] == "1") &
             (certainty[2] == "0" || certainty[2] == "1")) {
    classif <- ifelse(classif, 1, 0)
  }

  if (add_df == TRUE) {
    taxa_df$certainty <- classif
    return(taxa_df)
  } else {
    return(certainty)
  }

}
