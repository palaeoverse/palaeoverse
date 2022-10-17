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
#' @param binomial \code{character}. The name of the column in the dataframe
#' containing the genus and species names of the occurrences, either in the
#' form "genus species" or "genus_species".
#' @param species \code{character}. The name of the column in the dataframe
#' containing the species-level identifications (i.e. the specific epithet).
#' @param genus \code{character}. The name of the column in the dataframe
#' containing the genus-level identifications.
#' @param family \code{character}. The name of the column in the dataframe
#' containing the family-level identifications.
#' @param order \code{character}. The name of the column in the dataframe
#' containing the order-level identifications.
#' @param class \code{character}. The name of the column in the dataframe
#' containing the class-level identifications.
#' @param name \code{character}. The name of the column in the dataframe
#' containing the taxonomic names at mixed taxonomic levels; the data column
#' "accepted_names" in a Paleobiology Database \url{https://paleobiodb.org/#/}
#' occurrence dataframe is of this type.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences, either "species" (the default) or "genus".
#'
#' @return A \code{dataframe} of taxa, with each row corresponding to a unique
#' "species" or "genus" in the dataset (depending on the chosen resolution).
#' The dataframe will include the taxonomic information provided into the
#' function, as well as a column providing the 'unique' names of each taxon.
#'
#' @details In many cases palaeobiologists count unique taxa by retaining only
#' unique occurrences identified to a given taxonomic resolution, however
#' this function retain occurrences identified to a coarser taxonomic resolution
#' which are not already represented within the dataset.
#'
#' If we take the following set of occurrences:
#'
#' - *Albertosaurus sarcophagus*
#' - *Ankylosaurus* sp.
#' - Aves indet.
#' - Ceratopsidae indet.
#' - Hadrosauridae indet.
#' - *Ornithomimus* sp.
#' - *Tyrannosaurus rex*
#'
#' a filter for species-level identifications would reduce the species richness
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
#' be entered, as well as a `family` column. In a standard Paleobiology Database
#' \url{https://paleobiodb.org/#/} occurrence dataframe, species names are only
#' captured in the 'accepted_name' column, so a species-level filter should use
#' '`genus` = "genus"' and '`name` = "accepted_name"' arguments. If a
#' genus-level filter is desired, the minimum input requires either (1)
#' `binomial` or (2) `genus` columns to be entered, as well as a `family`
#' column.
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
#' Bethany Allen
#' @section Reviewer(s):
#' Lewis A. Jones & William Gearty
#'
#' @examples
#' #Retain unique species
#' species <- tax_unique(occdf = tetrapods, genus = "genus", family = "family",
#' order = "order", class = "class", name = "accepted_name")
#'
#' #Retain unique genera
#' genera <- tax_unique(occdf = tetrapods, genus = "genus", family = "family",
#' order = "order", class = "class", resolution = "genus")
#'
#' #Create dataframe from lists
#' occdf <- data.frame(species = c("rex", "aegyptiacus", NA), genus =
#' c("Tyrannosaurus", "Spinosaurus", NA), family = c("Tyrannosauridae",
#' "Spinosauridae", "Diplodocidae"))
#' dinosaur_species <- tax_unique(occdf = occdf, species = "species", genus =
#' "genus", family = "family")
#'
#' #Retain unique genera per collection with group_apply
#' genera <- group_apply(occdf = tetrapods,
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
                       genus = NULL, family = NULL, order = NULL,
                       class = NULL, name = NULL, resolution = "species") {

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
  if (!is.null(binomial)) {
    colnames(occdf)[colnames(occdf) == binomial] <-
      "binomial"
  }

  if (!is.null(species) && !(species %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `species`")
  }
  if (!is.null(species)) {
    colnames(occdf)[colnames(occdf) == species] <-
      "species"
  }

  if (!is.null(genus) && !(genus %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `genus`")
  }
  if (!is.null(genus)) {
    colnames(occdf)[colnames(occdf) == genus] <-
      "genus"
  }

  if (is.null(family)) {
    stop("`family` argument is missing. Provide column name of family names.")
  }
  if (!(family %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `family`")
  }
  colnames(occdf)[colnames(occdf) == family] <- "family"

  if (!is.null(order) && !(order %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `order`")
  }
  if (!is.null(order)) {
    colnames(occdf)[colnames(occdf) == order] <-
      "order"
  }

  if (!is.null(class) && !(class %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `class`")
  }
  if (!is.null(class)) {
    colnames(occdf)[colnames(occdf) == class] <-
      "class"
  }

  if (!is.null(name) && !(name %in% colnames(occdf))) {
    stop("`occdf` does not contain column name provided to `names`")
  }
  if (!is.null(name)) {
    colnames(occdf)[colnames(occdf) == name] <-
      "name"
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
  if (!is.null(name)) {
    occdf$name <- gsub("_", " ", occdf$name)
  }

  if (!is.null(class) && any(grepl("[[:punct:]]", occdf$class))) {
    stop("`class` column should not contain punctuation")
  }

  if (!is.null(order) && any(grepl("[[:punct:]]", occdf$order))) {
    stop("`order` column should not contain punctuation")
  }

  if (!is.null(family) && any(grepl("[[:punct:]]", occdf$family))) {
    stop("`family` column should not contain punctuation")
  }

  if (!is.null(genus) && any(grepl("[[:punct:]]", occdf$genus))) {
    stop("`genus` column should not contain punctuation")
  }

  if (!is.null(species) && any(grepl("[[:punct:]]", occdf$species))) {
    stop("`species` column should not contain punctuation")
  }

  if (!is.null(binomial) && any(grepl("[^[:alnum:][:space:]]",
                                       occdf$binomial))) {
    stop("`binomial` column should not contain punctuation except spaces or
         underscores")
  }

  if (!is.null(name) && any(grepl("[^[:alnum:][:space:]]", occdf$name))) {
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
  occurrences <- subset(occdf, select = c(binomial, species, genus, family,
                                          order, class, name))

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
    to_retain$unique_name <- to_retain$genus_species
  } else {
    to_retain$unique_name <- NA
  }

  for (i in seq_len(nrow(to_retain))) {
    if (is.na(to_retain$unique_name[i])) {
      if (!is.na(to_retain$genus[i])) {
      to_retain$unique_name[i] <- paste(to_retain$genus[i], "sp.")
      } else if (!is.na(to_retain$family[i])) {
      to_retain$unique_name[i] <- paste(to_retain$family[i], "indet.")
      } else if (!is.na(to_retain$order[i])) {
      to_retain$unique_name[i] <- paste(to_retain$order[i], "indet.")
      } else if (!is.na(to_retain$class[i])) {
      to_retain$unique_name[i] <- paste(to_retain$class[i], "indet.")
      }
    }
  }

  #Reorder
  if (resolution == "species") {
    if (!is.null(class)) {
      to_retain <- to_retain[, c("class", "order", "family", "genus",
                               "genus_species", "unique_name")]
    } else if (!is.null(order)) {
      to_retain <- to_retain[, c("order", "family", "genus", "genus_species",
                                "unique_name")]
    } else {
      to_retain <- to_retain[, c("family", "genus", "genus_species",
                               "unique_name")]
    }
  }

  if (resolution == "genus") {
     if (!is.null(class)) {
      to_retain <- to_retain[, c("class", "order", "family", "genus",
                                 "unique_name")]
    } else if (!is.null(order)) {
        to_retain <- to_retain[, c("order", "family", "genus", "unique_name")]
    } else {
        to_retain <- to_retain[, c("family", "genus", "unique_name")]
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

  row.names(to_retain) <- NULL
  return(to_retain)
}
