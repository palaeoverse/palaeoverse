#' Filter occurrences to unique taxa
#'
#' A function to filter a list of taxonomic occurrences to unique taxa of a
#' predefined resolution.
#'
#' @param occdf \code{dataframe}. A dataframe containing information on the
#' occurrences or taxa to filter.
#' @param binomial \code{character}. The name of the column in the dataframe
#' containing the genus and species names of the occurrences, either in the
#' form "genus species" or "genus_species".
#' @param species \code{character}. The name of the column in the dataframe
#' containing the species-level identifications.
#' @param genus \code{character}. The name of the column in the data frame
#' containing the genus-level identifications.
#' @param family \code{character}. The name of the column in the data frame
#' containing the family-level identifications.
#' @param order \code{character}. The name of the column in the data frame
#' containing the order-level identifications.
#' @param class \code{character}. The name of the column in the data frame
#' containing the class-level identifications.
#' @param names \code{character}. The name of the column in the data frame
#' containing the taxonomic names at mixed taxonomic levels; the data column
#' "accepted_names" in a Paleobiology Database download is of this type.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences, either species (the default) or genus.
#'
#' @return A \code{dataframe} of taxa, with each row corresponding to a unique
#' "species" or "genus" in the dataset (depending on the chosen resolution).
#' The dataframe will include the taxonomic information provided into the
#' function, as well as a column providing the 'unique' names of each taxon.
#'
#' @details In many cases palaeobiologists count unique taxa by retaining only
#' unique occurrences identified to the given taxonomic resolution, however
#' here we also retain occurrences identified to a coarser resolution which are
#' not already represented within the dataset.
#' If we take the following set of occurrences:
#' Albertosaurus sarcophagus
#' Ankylosaurus sp.
#' Aves indet.
#' Ceratopsidae indet.
#' Hadrosauridae indet.
#' Ornithominus sp.
#' Tyrannosaurus rex
#' then a filter for species-level identifications will reduce the species
#' diversity to two. However, none of these clades are nested within another, so
#' each of the indeterminately identified occurrences represents at least one
#' species not already represented in the dataset. This function is designed to
#' deal with such taxonomic data, and would retain all seven 'species' in this
#' dataset.
#' Taxonomic information is supplied within a dataframe, in which columns
#' provide identifications at different taxonomic levels. Occurrence
#' data can be filtered to retain either unique species or unique genera. If a
#' species-level filter is desired, the minimum input is for either 'binomial',
#' 'species' and 'genus', or 'names' and 'genus' columns to be entered, as well
#' as a 'family' column. In a standard Paleobiology Database dataframe, species
#' information is only captured in the 'accepted_name' column, so a species-
#' level filter should use 'genus = 'genus'' and 'names = 'accepted_name''
#' arguments.If a genus-level filter is desired, the minimum input is for
#' either 'binomial' or 'genus' columns to be entered, as well as a 'family'
#' column.
#' Missing data should be indicated with NAs, although the function can handle
#' the e.g. "NO_FAMILY_SPECIFIED" labels within Paleobiology Database datasets.
#' The function matches taxonomic names at increasingly higher taxonomic levels,
#' so homonyms may be falsely filtered out.
#'
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#' Lewis A. Jones and William Gearty
#'
#' @examples
#' #Retain unique species
#' tax_unique(occdf = tetrapods, genus = "genus", family = "family", order =
#' "order", class = "class", names = "accepted_name")
#'
#' #Retain unique genera
#' tax_unique(occdf = tetrapods, genus = "genus", family = "family", order =
#' "order", class = "class", resolution = "genus")
#'
#' #Create dataframe from lists
#' occdf <- data.frame(c("rex", "aegyptiacus", NA), c("Tyrannosaurus",
#' "Spinosaurus", NA), c("Tyrannosauridae", "Spinosauridae", "Diplodocidae"))
#' colnames(occdf) <- c("species", "genus", "family")
#' tax_unique(occdf = occdf, species = "species", genus = "genus", family =
#' "family")
#'
#' @export
#'
tax_unique <- function(occdf = NULL, binomial = NULL, species = NULL,
                       genus = NULL, family = NULL, order = NULL,
                       class = NULL, names = NULL, resolution = "species") {

#Give errors for incorrect input
  if (is.null(occdf)) {
    stop("Must enter an 'occdf' of occurrences or taxon names")
  }

  if (!is.data.frame(occdf)) {
    stop("occdf must be a data.frame")
  }

  #Check for column labels and rename them if present
  if (!is.null(binomial) && !(binomial %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'binomial'")
  }
  if (!is.null(binomial)) {
    colnames(occdf)[colnames(occdf) == binomial] <-
      "binomial"
  }

  if (!is.null(species) && !(species %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'species'")
  }
  if (!is.null(species)) {
    colnames(occdf)[colnames(occdf) == species] <-
      "species"
  }

  if (!is.null(genus) && !(genus %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'genus'")
  }
  if (!is.null(genus)) {
    colnames(occdf)[colnames(occdf) == genus] <-
      "genus"
  }

  if (is.null(family)) {
    stop("A column of family names must be supplied")
  }
  if (!(family %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'family'")
  }
  colnames(occdf)[colnames(occdf) == family] <- "family"

  if (!is.null(order) && !(order %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'order'")
  }
  if (!is.null(order)) {
    colnames(occdf)[colnames(occdf) == order] <-
      "order"
  }

  if (!is.null(class) && !(class %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'class'")
  }
  if (!is.null(class)) {
    colnames(occdf)[colnames(occdf) == class] <-
      "class"
  }

  if (!is.null(names) && !(names %in% colnames(occdf))) {
    stop("occdf does not contain column matching name for 'names'")
  }
  if (!is.null(names)) {
    colnames(occdf)[colnames(occdf) == names] <-
      "names"
  }

  #Substitute labels used in PBDB downloads
  occdf$family <- gsub("NO_FAMILY_SPECIFIED", NA, occdf$family)

  if (!is.null(order)) {
    occdf$order <- gsub("NO_ORDER_SPECIFIED", NA, occdf$order)
  }

  if (!is.null(class)) {
    occdf$class <- gsub("NO_CLASS_SPECIFIED", NA, occdf$class)
  }

  #Change underscores in binomials to spaces
  if (!is.null(binomial)) {
    occdf$binomial <- gsub("_", " ", occdf$binomial)
  }

  if ((!is.null(class) && any(grepl("[[:punct:]]", occdf$class))) ||
      (!is.null(order) && any(grepl("[[:punct:]]", occdf$order))) ||
      (!is.null(family) && any(grepl("[[:punct:]]", occdf$family))) ||
      (!is.null(genus) && any(grepl("[[:punct:]]", occdf$genus))) ||
      (!is.null(species) && any(grepl("[[:punct:]]", occdf$species))) ||
      (!is.null(binomial) && any(grepl("[^[:alnum:][:space:]]",
                                       occdf$binomial))) ||
      (!is.null(names) && any(grepl("[[:punct:]]", occdf$names)))) {
    stop("Name columns should not contain punctuation")
  }

  if ((resolution == "species") && (is.null(binomial)) && (is.null(species))
      && (is.null(names))) {
    stop("Species names must be supplied by specifying 'binomial', 'genus' and
    'species', or 'genus' and 'names' columns to estimate diversity at species
    level")
  }

  if ((resolution == "genus") && (is.null(binomial)) && (is.null(genus))
      && (is.null(names))) {
    stop("Genus names must be supplied by specifying 'binomial' or 'genus'
    columns to estimate diversity at genus level")
  }

  if ((resolution != "species") && (resolution != "genus")) {
    stop("Resolution must be 'species' or 'genus'")
  }

#Run function
  genus_species <- NULL
  occurrences <- subset(occdf, select = c(binomial, species, genus, family,
                                          order, class, names))

  if (!is.null(names)) {
    #If name is not a binomial, replace with NA
    occurrences$names[grep(" ", occurrences$names, invert = TRUE)] <- NA

    #Rename column
    colnames(occurrences)[colnames(occurrences) == "names"] <-
      "genus_species"
  }

  if (!is.null(binomial)) {
    #Rename column
    colnames(occurrences)[colnames(occurrences) == "binomial"] <-
      "genus_species"

    #If no 'genus' column is given, make one
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
  occurrences <- unique(occurrences)

  #Create an empty dataset to collect unique occurrences in
  to_retain <- data.frame()

  if (resolution == "species") {
    #Retain occurrences identified to species level and remove from dataframe
    to_retain <- rbind(to_retain, occurrences[!is.na(occurrences$genus_species),
                                              ])
    occurrences <- occurrences[is.na(occurrences$genus_species), ]

    #Retain occurrences identified to genus level and not already in dataset
    to_retain <- rbind(to_retain,
                       occurrences[(!occurrences$genus %in% c(to_retain$genus,
                                                              NA)), ])
    occurrences <- occurrences[is.na(occurrences$genus), ]
  } else

  if (resolution == "genus") {
    #Remove genus_species column and remove genus repeats
    if (!is.null(binomial) || !is.null(names)) {
    occurrences <- subset(occurrences, select = -c(genus_species))
    } else
      if (!is.null(species)) {
    occurrences <- subset(occurrences, select = -c(genus_species, species))
      }

    occurrences <- unique(occurrences)

    #Retain genus identifications and remove from dataframe
    to_retain <- rbind(to_retain, occurrences[!is.na(occurrences$genus), ])
    occurrences <- occurrences[is.na(occurrences$genus), ]
  }

  #Retain occurrences identified to family level and not already in dataset
  to_retain <- rbind(to_retain,
                     occurrences[!(occurrences$family %in% c(to_retain$family,
                                                             NA)), ])
  occurrences <- occurrences[is.na(occurrences$family), ]

  if (!is.null(order)) {
  #Retain occurrences identified to order level and not already in dataset
  to_retain <- rbind(to_retain,
                     occurrences[!(occurrences$order %in% c(to_retain$order,
                                                            NA)), ])
  occurrences <- occurrences[is.na(occurrences$order), ]
  }

  if (!is.null(class)) {
  #Retain occurrences identified to class level and not already in dataset
  to_retain <- rbind(to_retain,
                     occurrences[!(occurrences$class %in% c(to_retain$class,
                                                            NA)), ])
  occurrences <- occurrences[is.na(occurrences$class), ]
  }

  #Produce column with unique taxon names
  if (resolution == "species") {
    to_retain$unique_names <- to_retain$genus_species
  } else {
    to_retain$unique_names <- NA
  }

  for (i in 1:seq_len(nrow(to_retain))) {
    if (is.na(to_retain$unique_names[i])) {
      if (!is.na(to_retain$genus[i])) {
      to_retain$unique_names[i] <- paste(to_retain$genus[i], "sp.")
      } else if (!is.na(to_retain$family[i])) {
      to_retain$unique_names[i] <- paste(to_retain$family[i], "indet.")
      } else if (!is.na(to_retain$order[i])) {
      to_retain$unique_names[i] <- paste(to_retain$order[i], "indet.")
      } else if (!is.na(to_retain$class[i])) {
      to_retain$unique_names[i] <- paste(to_retain$class[i], "indet.")
      }
    }
  }

  #Reorder
  if (resolution == "species") {
    if (!is.null(class)) {
      to_retain <- to_retain[, c("class", "order", "family", "genus",
                               "genus_species", "unique_names")]
    } else
    if (!is.null(order)) {
      to_retain <- to_retain[, c("order", "family", "genus", "genus_species",
                                "unique_names")]
    } else {
      to_retain <- to_retain[, c("family", "genus", "genus_species",
                               "unique_names")]
    }
  }

  if (resolution == "genus") {
     if (!is.null(class)) {
      to_retain <- to_retain[, c("class", "order", "family", "genus",
                                 "unique_names")]
    } else
      if (!is.null(order)) {
        to_retain <- to_retain[, c("order", "family", "genus", "unique_names")]
    } else {
        to_retain <- to_retain[, c("family", "genus", "unique_names")]
      }
  }

  if (!is.null(class)) {
    to_retain <- to_retain[order(to_retain$class), ]
  }
  if (!is.null(order)) {
    to_retain <- to_retain[order(to_retain$order), ]
  }
  to_retain <- to_retain[order(to_retain$family), ]
  to_retain <- to_retain[order(to_retain$genus), ]
  if (resolution == "species") {
    to_retain <- to_retain[order(to_retain$genus_species), ]
  }

  return(to_retain)
}
