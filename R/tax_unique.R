#' Filter occurrences to unique taxa
#'
#' A function to filter a list of taxonomic occurrences to unique taxa of a
#' predefined resolution. Occurrences identified to a coarser taxonomic
#' resolution than the desired level are retained if they belong to a clade
#' which is not otherwise represented in the dataset (see details section for
#' further information). This has previously been described as "cryptic
#' diversity" (e.g. Mannion et al. 2011).
#'
#' @param occdf \code{dataframe}. A dataframe containing information on the
#' occurrences or taxa to filter.
#' @param binomial \code{character}. The name of the column in \code{occdf}
#' containing the genus and species names of the occurrences, either in the
#' form "genus species" or "genus_species".
#' @param species \code{character}. The name of the column in \code{occdf}
#' containing the species-level identifications (i.e. the specific epithet).
#' @param genus \code{character}. The name of the column in \code{occdf}
#' containing the genus-level identifications.
#' @param ... \code{character}. Other named arguments specifying columns of
#' higher levels of taxonomy (e.g. subfamily, order, superclass). The names of
#' the arguments will be the column names of the output, and the values of the
#' arguments correspond to the columns of \code{occdf}. The given order of the
#' arguments is the order in which they are filtered. Therefore, these arguments
#' must be in ascending order from lowest to highest taxonomic rank (see
#' examples below). At least one higher level of taxonomy must be specified.
#' @param name \code{character}. The name of the column in \code{occdf}
#' containing the taxonomic names at mixed taxonomic levels; the data column
#' "accepted_name" in a [Paleobiology Database](https://paleobiodb.org/#/)
#' occurrence dataframe is of this type.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences, either "species" (the default) or "genus".
#' @param orig \code{logical}. Should the original dataframe be returned with
#' the unique names appended as a new column?
#'
#' @return A \code{dataframe} of taxa, with each row corresponding to a unique
#' "species" or "genus" in the dataset (depending on the chosen resolution).
#' The dataframe will include the taxonomic information provided into the
#' function, as well as a column providing the 'unique' names of each taxon. If
#' \code{orig} is \code{TRUE}, the original dataframe (\code{occdf}) will be
#' returned with these 'unique' names appended as a new column. Occurrences that
#' do not represent unique taxa will have \code{NA} for their 'unique' names.
#'
#' @details Palaeobiologists usually count unique taxa by retaining only
#' unique occurrences identified to a given taxonomic resolution, however
#' this function retains occurrences identified to a coarser taxonomic
#' resolution which are not already represented within the dataset. For example,
#' consider the following set of occurrences:
#'
#' - *Albertosaurus sarcophagus*
#' - *Ankylosaurus* sp.
#' - Aves indet.
#' - Ceratopsidae indet.
#' - Hadrosauridae indet.
#' - *Ornithomimus* sp.
#' - *Tyrannosaurus rex*
#'
#' A filter for species-level identifications would reduce the species richness
#' to two. However, none of these clades are nested within one another, so each
#' of the indeterminately identified occurrences represents at least one species
#' not already represented in the dataset. This function is designed to deal
#' with such taxonomic data, and would retain all seven 'species' in this
#' example.
#'
#' Taxonomic information is supplied within a dataframe, in which columns
#' provide identifications at different taxonomic levels. Occurrence
#' data can be filtered to retain either unique species, or unique genera. If a
#' species-level filter is desired, the minimum input requires either (1)
#' `binomial`, (2) `species` and `genus`, or (3) `name` and `genus` columns to
#' be entered, as well as at least one column of a higher taxonomic level.
#' In a standard [Paleobiology Database](https://paleobiodb.org/#/)
#' occurrence dataframe, species names are only
#' captured in the 'accepted_name' column, so a species-level filter should use
#' '`genus` = "genus"' and '`name` = "accepted_name"' arguments. If a
#' genus-level filter is desired, the minimum input requires either (1)
#' `binomial` or (2) `genus` columns to be entered, as well as at least one
#' column of a higher taxonomic level.
#'
#' Missing data should be indicated with NAs, although the function can handle
#' common labels such as "NO_FAMILY_SPECIFIED" within Paleobiology Database
#' datasets.
#'
#' The function matches taxonomic names at increasingly higher taxonomic levels,
#' so homonyms may be falsely filtered out.
#'
#' @section References:
#'
#' Mannion, P. D., Upchurch, P., Carrano, M. T., and Barrett, P. M. (2011).
#' Testing the effect of the rock record on diversity: a multidisciplinary
#' approach to elucidating the generic richness of sauropodomorph dinosaurs
#' through time.
#' Biological Reviews, 86, 157-181. \doi{10.1111/j.1469-185X.2010.00139.x}.
#'
#' @section Developer(s):
#' Bethany Allen & William Gearty
#' @section Reviewer(s):
#' Lewis A. Jones & William Gearty
#'
#' @examples
#' #Retain unique species
#' occdf <- tetrapods[1:100, ]
#' species <- tax_unique(occdf = occdf, genus = "genus", family = "family",
#' order = "order", class = "class", name = "accepted_name")
#'
#' #Retain unique genera
#' genera <- tax_unique(occdf = occdf, genus = "genus", family = "family",
#' order = "order", class = "class", resolution = "genus")
#'
#' #Create dataframe from lists
#' occdf2 <- data.frame(species = c("rex", "aegyptiacus", NA), genus =
#' c("Tyrannosaurus", "Spinosaurus", NA), family = c("Tyrannosauridae",
#' "Spinosauridae", "Diplodocidae"))
#' dinosaur_species <- tax_unique(occdf = occdf2, species = "species", genus =
#' "genus", family = "family")
#'
#' #Retain unique genera per collection with group_apply
#' genera <- group_apply(occdf = occdf,
#'                      group = c("collection_no"),
#'                      fun = tax_unique,
#'                      genus = "genus",
#'                      family = "family",
#'                      order = "order",
#'                      class = "class",
#'                      resolution = "genus")
#'
#' @export
#'
tax_unique <- function(occdf = NULL, binomial = NULL, species = NULL,
                       genus = NULL, ..., name = NULL,
                       resolution = "species", orig = FALSE) {
  #Give errors for incorrect input
  if (is.null(occdf)) {
    stop("Must enter an `occdf` of occurrences or taxon names")
  }

  if (!is.data.frame(occdf)) {
    stop("`occdf` must be a data.frame")
  }

  #Check for column labels and rename them if present
  if (!is.null(binomial) && !(binomial %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `binomial`")
  }

  if (!is.null(species) && !(species %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `species`")
  }

  if (!is.null(genus) && !(genus %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `genus`")
  }

  higher_args <- list(...)
  higher_names <- names(higher_args)
  higher_cols <- unname(unlist(higher_args))
  if (length(higher_args) == 0) {
    stop("At least one higher taxonomic level must be supplied (e.g. `family`)")
  }
  for (level_label in higher_names) {
    col_name <- higher_args[[level_label]]
    if (!(col_name %in% colnames(occdf))) {
      stop(paste0("`occdf` does not contain column name provided to `",
                  level_label, "`"))
    }
    #Substitute labels used in PBDB downloads
    occdf[[col_name]] <-
      gsub("NO_FAMILY_SPECIFIED|NO_ORDER_SPECIFIED|NO_CLASS_SPECIFIED", NA,
           occdf[[col_name]])
    if (any(grepl("[[:punct:]]", occdf[[col_name]]))) {
      stop(paste0("`", level_label, "` column should not contain punctuation"))
    }
  }

  if (!is.null(name) && !(name %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `names`")
  }

  if (!is.null(genus) && any(grepl("[[:punct:]]", occdf[[genus]]))) {
    stop("`genus` column should not contain punctuation")
  }

  if (!is.null(species) && any(grepl("[[:punct:]]", occdf[[species]]))) {
    stop("`species` column should not contain punctuation")
  }

  if (!is.null(binomial) && any(grepl("[^[:alnum:][:space:]]",
                                       occdf[[binomial]]))) {
    stop("`binomial` column should not contain punctuation except spaces or
         underscores")
  }

  if (!is.null(name) && any(grepl("[^[:alnum:][:space:]]", occdf[[name]]))) {
    stop("`name` column should not contain punctuation except spaces or
         underscores")
  }

  if ((resolution == "species") && (is.null(binomial)) && (is.null(species))
      && (is.null(name))) {
    stop("Species names must be supplied by specifying `binomial`, `genus` and
    `species`, or `genus` and `name` columns to estimate richness at species
    level")
  }

  if ((resolution == "genus") && (is.null(binomial)) && (is.null(genus))) {
    stop("Genus names must be supplied by specifying `binomial` or `genus`
    columns to estimate richness at genus level")
  }

  if ((resolution != "species") && (resolution != "genus")) {
    stop("Resolution must be 'species' or 'genus'")
  }

  #Run function
  genus_species <- NULL
  occurrences <- occdf[, c(binomial, species, genus, higher_cols, name)]

  #Rename columns
  if (!is.null(species)) {
    colnames(occurrences)[colnames(occurrences) == species] <- "species"
  }
  if (!is.null(genus)) {
    colnames(occurrences)[colnames(occurrences) == genus] <- "genus"
  }
  if (!is.null(binomial)) {
    colnames(occurrences)[colnames(occurrences) == binomial] <- "binomial"
  }
  if (!is.null(name)) {
    colnames(occurrences)[colnames(occurrences) == name] <- "name"
  }
  colnames(occurrences)[match(higher_cols, colnames(occurrences))] <-
    higher_names

  #Change underscores in binomials to spaces
  if (!is.null(binomial)) {
    occurrences$binomial <- gsub("_", " ", occdf$binomial)
  }
  if (!is.null(name)) {
    occurrences$name <- gsub("_", " ", occurrences$name)
  }

  if (!is.null(name)) {
    #If name is not a binomial, replace with NA
    occurrences$name[grep(" ", occurrences$name, invert = TRUE)] <- NA

    #Rename column
    colnames(occurrences)[colnames(occurrences) == "name"] <-
      "genus_species"
  }

  if (!is.null(binomial)) {
    #Rename column
    colnames(occurrences)[colnames(occurrences) == "binomial"] <-
      "genus_species"

    #If no `genus` column is given, make one
    if (is.null(genus)) {
      occurrences$genus <- sub(" .*", "", occurrences$genus_species)
    }
  }

  if (!is.null(species)) {
    #Create genus_species column
    occurrences$genus_species <- paste(occurrences$genus, occurrences$species)
    occurrences <- subset(occurrences, select = -c(species))

    #If one or both of the values was NA, set to NA
    occurrences$genus_species[grep("NA", occurrences$genus_species)] <- NA
  }

  #Remove absolute repeats
  if (orig) {
    occurrences_with_dupes <- occurrences
    # List the rows of the original dataframe that correspond to each unique row
    # Hack to include NAs as groups
    occurrences_with_dupes[is.na(occurrences_with_dupes)] <- "thisisanNAvalue"
    occurrences_with_dupes$rows <- seq_len(nrow(occurrences_with_dupes))
    form <- as.formula(
      paste(". ~",
            paste(c(rev(higher_names), "genus", "genus_species"),
                  collapse = " + "))
      )
    occurrences <- aggregate(form, data = occurrences_with_dupes,
                             FUN = paste, collapse = ",")
    # Switch hack groups back to true NAs
    occurrences[occurrences == "thisisanNAvalue"] <- NA

  } else {
    occurrences <- unique(occurrences)
  }

  #Create an empty dataset to collect unique occurrences in
  to_retain <- data.frame()

  if (resolution == "species") {
    #Retain occurrences identified to species level and remove from dataframe
    to_retain <- rbind(to_retain, occurrences[!is.na(occurrences$genus_species),
                                              ])
    occurrences <- occurrences[is.na(occurrences$genus_species), ]

    #Retain occurrences identified to genus level and not already in dataset
    #The merge handles cases where the same genus name occurs in multiple
    #higher taxonomic groups
    to_retain <- merge(subset(occurrences,
                              subset = !is.na(genus),
                              select = -c(genus_species)),
                       to_retain,
                       by = c(rev(higher_names), "genus"), all = T)
    if (orig) {
      to_retain$rows <- ifelse(is.na(to_retain$rows.y),
                               to_retain$rows.x, to_retain$rows.y)
      to_retain$rows.x <- to_retain$rows.y <- NULL
    }
    occurrences <- occurrences[is.na(occurrences$genus), ]

  } else if (resolution == "genus") {
    #Remove genus_species column and remove genus repeats
    if (!is.null(binomial) || !is.null(name)) {
      occurrences <- subset(occurrences, select = -c(genus_species))
    } else if (!is.null(species)) {
      occurrences <- subset(occurrences, select = -c(genus_species, species))
    }

    occurrences <- unique(occurrences)

    #Retain genus identifications and remove from dataframe
    to_retain <- rbind(to_retain, occurrences[!is.na(occurrences$genus), ])
    occurrences <- occurrences[is.na(occurrences$genus), ]
  }

  #Retain occurrences identified to higher levels and not already in dataset
  for (col_name in higher_names) {
    to_retain <- rbind(to_retain,
                       occurrences[!(occurrences[[col_name]] %in%
                                       c(to_retain[[col_name]], NA)), ])
    occurrences <- occurrences[is.na(occurrences[[col_name]]), ]
  }

  #Produce column with unique taxon names
  if (resolution == "species") {
    to_retain$unique_name <- to_retain$genus_species
  } else {
    to_retain$unique_name <- NA
  }

  for (i in seq_len(nrow(to_retain))) {
    if (is.na(to_retain$unique_name[i])) {
      if (!is.na(to_retain$genus[i])) {
      to_retain$unique_name[i] <- paste(to_retain$genus[i], "sp.")
      } else {
        for (col_name in higher_names) {
          if (!is.na(to_retain[i, col_name])) {
            to_retain$unique_name[i] <- paste(to_retain[i, col_name], "indet.")
            break
          }
        }
      }
    }
  }

  #Reorder
  rows_col <- if (orig) "rows" else NULL
  if (resolution == "species") {
    to_retain <- to_retain[, c(rev(higher_names), "genus", "genus_species",
                               "unique_name", rows_col)]
  }
  if (resolution == "genus") {
    to_retain <- to_retain[, c(rev(higher_names), "genus", "unique_name",
                               rows_col)]
  }

  for (col_name in rev(higher_names)) {
    to_retain <- to_retain[order(to_retain[[col_name]]), ]
  }
  to_retain <- to_retain[order(to_retain$genus), ]
  if (resolution == "species") {
    to_retain <- to_retain[order(to_retain$genus_species), ]
  }

  row.names(to_retain) <- NULL
  return(to_retain)
}
