#' Filter occurrences to unique taxa
#'
#' A function to filter a list of taxonomic occurrences to unique taxa of a
#' predefined resolution.
#'
#' @param paleobioDB \code{dataframe}. A dataframe of taxonomic occurrences
#' downloaded directly from the Paleobiology Database. The dataframe must
#' include the following columns: class, order, family, genus, accepted_name.
#' @param species \code{character}. A vector of species names.
#' @param genus \code{character}. A vector of genus names.
#' @param family \code{character}. A vector of family names.
#' @param order \code{character}. A vector of order names.
#' @param class \code{character}. A vector of class names.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences, either species (the default) or genus.
#'
#' @return A \code{dataframe} of unique taxa, with row numbers corresponding to
#' the original dataset.
#'
#' @details In many cases palaeobiologists achieve count unique taxa by
#' retaining only unique occurrences identified to the given taxonomic
#' resolution, however here we also retain occurrences identified to a coarser
#' resolution which are not already represented within the dataset.
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
#' Data can be supplied in one of two forms, either a dataframe containing
#' standard occurrence data from the Paleobiology Database, or a series of
#' vectors which each describe a different taxonomic level for a set of
#' occurrences (the order of names in the vectors must correspond). At a
#' minimum, species, genus, and family vectors must be supplied. Occurrence
#' data can be filtered to retain either unique species or unique genera, and
#' can be defined as unique either across the whole dataset or within set
#' categories (e.g. temporal intervals, spatial bins). Missing data should be
#' indicated with NAs.
#'
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#' Lewis A. Jones and William Gearty
#'
#' @examples
#' #Retain unique species
#' tax_unique(paleobioDB = tetrapods)
#' tax_unique(species = c("rex", "aegyptiacus", NA),
#'            genus = c("Tyrannosaurus", "Spinosaurus", NA),
#'            family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae"))
#'
#' #Retain unique genera
#' tax_unique(paleobioDB = tetrapods, resolution = "genus")
#'
#' @export
#'
tax_unique <- function(paleobioDB = NULL, species = NULL, genus = NULL,
                       family = NULL, order = NULL, class = NULL,
                       resolution = "species") {

#Give errors for incorrect input
  if (is.null(paleobioDB) && is.null(genus)) {
    stop("Must enter either paleobioDB or individual taxonomic vectors")
  }

  if (!is.null(paleobioDB)) {

    if (!is.null(genus)) {
      stop("Must enter either paleobioDB or individual taxonomic vectors, not
           both")
    }

    if (!is.data.frame(paleobioDB)) {
      stop("paleobioDB must be a data.frame")
    }

    if (any(!(c("class", "order", "family", "genus", "accepted_name")
              %in% colnames(paleobioDB)))) {
    stop("paleobioDB must contain the following named columns: class, order,
    family, genus")
    }

    if ((resolution == "species") &&
        !("accepted_name" %in% colnames(paleobioDB))) {
      stop("paleobioDB must contain the accepted_name column to estimate
           diversity at species level")
    }

    paleobioDB$class <- gsub("NO_CLASS_SPECIFIED", NA, paleobioDB$class)
    paleobioDB$order <- gsub("NO_ORDER_SPECIFIED", NA, paleobioDB$order)
    paleobioDB$family <- gsub("NO_FAMILY_SPECIFIED", NA, paleobioDB$family)

    if ((any(grepl("[[:punct:]]", paleobioDB$class))) ||
        (any(grepl("[[:punct:]]", paleobioDB$order))) ||
        (any(grepl("[[:punct:]]", paleobioDB$family))) ||
        (any(grepl("[[:punct:]]", paleobioDB$genus))) ||
        (any(grepl("[[:punct:]]", paleobioDB$accepted_name)))) {
      stop("paleobioDB taxonomy columns should not contain punctuation")
    }
  }

  if ((!is.null(class) && !is.vector(class)) ||
      (!is.null(order) && !is.vector(order)) ||
      (!is.null(family) && !is.vector(family)) ||
      (!is.null(genus) && !is.vector(genus)) ||
      (!is.null(species) && !is.vector(species))) {
    stop("Taxononic information must be of class vector")
  }

  if (!is.null(genus)) {
    if ((!is.null(species) && (length(genus) != length(species))) ||
        length(genus) != length(family) ||
        (!is.null(order) && (length(genus) != length(order))) ||
        (!is.null(class) && (length(genus) != length(class)))) {
      stop("Taxononic vectors must all be the same length")
        }
  }

  if ((!is.null(class) && any(grepl("[[:punct:]]", class))) ||
      (!is.null(order) && any(grepl("[[:punct:]]", order))) ||
      (!is.null(family) && any(grepl("[[:punct:]]", family))) ||
      (!is.null(genus) && any(grepl("[[:punct:]]", genus))) ||
      (!is.null(species) && any(grepl("[[:punct:]]", species)))) {
    stop("Taxonomic vectors should not contain punctuation")
  }

  if (!is.null(genus) && (resolution == "species") && (is.null(species))) {
    stop("Vector of species names must be supplied to estimate diversity at
         species level")
  }

  if ((resolution != "species") && (resolution != "genus")) {
    stop("Resolution must be 'species' or 'genus'")
  }

#Run function
  genus_species <- NULL
  category <- NULL

  if (!is.null(paleobioDB)) {
    #Filter paleobioDB necessary columns
    occurrences <- paleobioDB[, c("class", "order", "family",
                                "genus", "accepted_name")]

    #If accepted name is not a binomial, replace with NA
    occurrences$accepted_name[grep(" ", occurrences$accepted_name,
                                   invert = TRUE)] <- NA

    #Rename column
    colnames(occurrences)[colnames(occurrences) == "accepted_name"] <-
      "genus_species"
  }

  if (is.null(paleobioDB)) {
    #Compile supplied columns into a data frame
    occurrences <- as.data.frame(cbind(class, order, family, genus, species))

    #Create genus_species column
    occurrences$genus_species <- paste(occurrences$genus, occurrences$species)

    #If one or both of the values was NA, set to NA
    occurrences$genus_species[grep("NA", occurrences$genus_species)] <- NA
  }

  #Remove absolute repeats
  occurrences <- unique(occurrences)

  #Create an empty dataset to collect unique occurrences in
  to_retain <- data.frame()

  if (resolution == "species") {
    #Retain occurrences identified to species level and remove from dataframe
    to_retain <- rbind(to_retain, occurrences[!is.na(occurrences$genus_species), ])
    occurrences <- occurrences[is.na(occurrences$genus_species), ]

    #Retain occurrences identified to genus level and not already in dataset
    to_retain <- rbind(to_retain,
                       occurrences[(!occurrences$genus %in% c(to_retain$genus, NA)),
                               ])
    occurrences <- occurrences[is.na(occurrences$genus), ]
  } else

  if (resolution == "genus") {
    #Remove genus_species column and remove genus repeats
    if (!is.null(paleobioDB)) {
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
                     occurrences[!(occurrences$family %in% c(to_retain$family, NA)),
                             ])
  occurrences <- occurrences[is.na(occurrences$family), ]

  if (!is.null(paleobioDB) || !is.null(order)) {
  #Retain occurrences identified to order level and not already in dataset
  to_retain <- rbind(to_retain,
                     occurrences[!(occurrences$order %in% c(to_retain$order, NA)), ])
  occurrences <- occurrences[is.na(occurrences$order), ]
  }

  if (!is.null(paleobioDB) || !is.null(class)) {
  #Retain occurrences identified to class level and not already in dataset
  to_retain <- rbind(to_retain,
                     occurrences[!(occurrences$class %in% c(to_retain$class, NA)), ])
  occurrences <- occurrences[is.na(occurrences$class), ]
  }

  #Reorder
  if (!is.null(paleobioDB) || !is.null(class)) {
    to_retain <- to_retain[order(to_retain$class), ]
  }
  if (!is.null(paleobioDB) || !is.null(order)) {
    to_retain <- to_retain[order(to_retain$order), ]
  }
  to_retain <- to_retain[order(to_retain$family), ]
  to_retain <- to_retain[order(to_retain$genus), ]
  if (resolution == "species") {
    to_retain <- to_retain[order(to_retain$genus_species), ]
  }

  return(to_retain)
}
