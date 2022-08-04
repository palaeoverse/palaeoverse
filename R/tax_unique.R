#' Filter occurrences to unique taxa
#'
#' A function to filter a list of taxonomic occurrences to unique taxa of a
#' predefined resolution. In many cases palaeobiologists achieve this by
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
#' @param paleobioDB \code{dataframe}. A dataframe of taxonomic occurrences
#' downloaded directly from the Paleobiology Database. The dataframe must
#' include the following columns: class, order, family, genus, accepted_name.
#' @param species \code{character}. A vector of species names.
#' @param genus \code{character}. A vector of genus names.
#' @param family \code{character}. A vector of family names.
#' @param order \code{character}. A vector of order names.
#' @param class \code{character}. A vector of class names.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences, either species (the default) or genera.
#' @param by \code{numeric, character}. A category within which to determine
#' unique taxa. When using \code{paleobioDB}, this should specify the
#' column within the dataframe to use as the category. When using individual
#' taxonomic vectors, this should be an additional vector of numbers or
#' character-based labels designating which category each occurrence belongs to.
#'
#' @return A \code{dataframe} of unique taxa, with row numbers corresponding to
#' the original dataset.
#'
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#'
#' @examples
#' #Retain unique species
#' tax_unique(paleobioDB = tetrapods)
#' tax_unique(species = c("rex", "aegyptiacus", NA),
#'            genus = c("Tyrannosaurus", "Spinosaurus", NA),
#'            family = c("Tyrannosauridae", "Spinosauridae", "Diplodocidae"))
#'
#' #Retain unique genera
#' tax_unique(paleobioDB = tetrapods, resolution = "genera")
#'
#' #Retain unique species within each country
#' tax_unique(paleobioDB = tetrapods, by = "cc")
#'
#' @export
#'
#For testing
tax_unique <- function(paleobioDB = NULL, species = NULL, genus = NULL,
                       family = NULL, order = NULL, class = NULL,
                       resolution = "species", by = NULL) {

#Give errors for incorrect input
  if (is.null(paleobioDB) & is.null(species)) {
    stop("Must enter either paleobioDB or individual taxonomic vectors")
  }

  if (!is.null(paleobioDB) & !is.null(species)) {
    stop("Must enter either paleobioDB or individual taxonomic vectors, not
         both")
  }

  if (!is.null(paleobioDB) & !is.data.frame(paleobioDB)) {
    stop("paleobioDB must be a data frame")
  }

  if (!is.null(paleobioDB)) {
    if (!"class" %in% colnames(paleobioDB) |
       !"order" %in% colnames(paleobioDB) |
       !"family" %in% colnames(paleobioDB) |
       !"genus" %in% colnames(paleobioDB) |
       !"accepted_name" %in% colnames(paleobioDB)) {
    stop("paleobioDB must contain the following columns: class, order, family,
         genus, accepted_name")
    }
  }

  if (!is.null(paleobioDB)) {
    paleobioDB$class <- gsub("NO_CLASS_SPECIFIED", NA, paleobioDB$class)
    paleobioDB$order <- gsub("NO_ORDER_SPECIFIED", NA, paleobioDB$order)
    paleobioDB$family <- gsub("NO_FAMILY_SPECIFIED", NA, paleobioDB$family)
  }

  if (!is.null(paleobioDB) &
      (any(grepl("[[:punct:]]", paleobioDB$class))) |
      (any(grepl("[[:punct:]]", paleobioDB$order))) |
      (any(grepl("[[:punct:]]", paleobioDB$family))) |
      (any(grepl("[[:punct:]]", paleobioDB$genus))) |
      (any(grepl("[[:punct:]]", paleobioDB$accepted_name)))) {
    stop("paleobioDB taxonomy columns should not contain punctuation")
  }

  if ((!is.null(class) & (!is.vector(class))) |
      (!is.null(order) & (!is.vector(order))) |
      (!is.null(family) & (!is.vector(family))) |
      (!is.null(genus) & (!is.vector(genus))) |
      (!is.null(species) & (!is.vector(species)))) {
    stop("Taxononic information must be in a vector")
  }

  if ((!is.null(class) & any(grepl("[[:punct:]]", class))) |
      (!is.null(order) & any(grepl("[[:punct:]]", order))) |
      (!is.null(family) & any(grepl("[[:punct:]]", family))) |
      (!is.null(genus) & any(grepl("[[:punct:]]", genus))) |
      (!is.null(species) & any(grepl("[[:punct:]]", species)))) {
    stop("Taxonomy vectors should not contain punctuation")
  }

  if ((resolution != "species") & (resolution != "genera")){
    stop("Resolution must be species or genera")
  }

  if (!is.null(by) & !is.null(paleobioDB)) {
    if (!by %in% colnames(paleobioDB)) {
    stop("by must be a column name within the paleobioDB data frame")
    }
  }

  if (!is.null(by) & !is.null(species) & length(by) != length(species)) {
    stop("When using taxonomic vectors, by should also be a vector of the same
         length as the other vectors")
  }

#Run function

  if (!is.null(paleobioDB)) {
    #Filter paleobioDB necessary columns
    occurrences <- paleobioDB[, c("class", "order", "family",
                                "genus", "accepted_name")]

    #If a category is provided, add it as a column
    if(!is.null(by)){
      occurrences$category <- paleobioDB[[by]]
    } else {
      occurrences$category <- 1
    }

    #If accepted name is not a binomial, replace with NA
    occurrences$accepted_name[grep(" ", occurrences$accepted_name,
                                   invert = TRUE)] <- NA

    #Rename column
    colnames(occurrences)[colnames(occurrences) == "accepted_name"] <-
      "genus_species"
  } else

  if (is.null(paleobioDB)) {
    #Compile supplied columns into a data frame
    occurrences <- as.data.frame(cbind(class, order, family, genus, species))

    #Create genus_species column
    occurrences$genus_species <- paste(occurrences$genus, occurrences$species)

    #If one or both of the values was NA, set to NA
    occurrences$genus_species[grep("NA", occurrences$genus_species)] <- NA

    #If a category is provided, add it as a column
    if(!is.null(by)){
      occurrences$category <- by
    } else {
      occurrences$category <- 1
    }
  }

  #Remove absolute repeats
  occurrences <- unique(occurrences)

  #Create a list of unique categories
  uniq_cats <- unique(occurrences$category)

  #Create an empty dataset to collect unique occurrences in
  new_dataset <- data.frame()

  #Loop through each collection
  for (i in 1:(length(uniq_cats))) {

    #Filter to a single category
    one_cat <- occurrences[which(occurrences[,"category"] == uniq_cats[i]),]
    to_retain <- data.frame()

    if (resolution == "species") {
      #Retain occurrences identified to species level and remove from dataframe
      to_retain <- rbind(to_retain, one_cat[!is.na(one_cat$genus_species),])
      one_cat <- one_cat[is.na(one_cat$genus_species),]

      #Retain occurrences identified to genus level and not already in dataset
      to_retain <- rbind(to_retain,
                         one_cat[(!one_cat$genus %in% c(to_retain$genus, NA)),])
      one_cat <- one_cat[is.na(one_cat$genus),]} else

    if (resolution == "genera") {
      #Remove genus_species column and remove genus repeats
      one_cat <- subset(one_cat, select = -genus_species)
      one_cat <- unique(one_cat)

      #Retain genus identifications and remove from dataframe
      to_retain <- rbind(to_retain, one_cat[!is.na(one_cat$genus),])
      one_cat <- one_cat[is.na(one_cat$genus),]
    }

    #Retain occurrences identified to family level and not already in dataset
    to_retain <- rbind(to_retain,
                       one_cat[!(one_cat$family %in% c(to_retain$family, NA)),])
    one_cat <- one_cat[is.na(one_cat$family),]

    if (!is.null(paleobioDB) | !is.null(order)){
    #Retain occurrences identified to order level and not already in dataset
    to_retain <- rbind(to_retain,
                       one_cat[!(one_cat$order %in% c(to_retain$order, NA)),])
    one_cat <- one_cat[is.na(one_cat$order),]}

    if (!is.null(paleobioDB) | !is.null(class)){
    #Retain occurrences identified to class level and not already in dataset
    to_retain <- rbind(to_retain,
                       one_cat[!(one_cat$class %in% c(to_retain$class, NA)),])
    one_cat <- one_cat[is.na(one_cat$class),]}

    #Reorder
    if (!is.null(paleobioDB) | !is.null(class)){
      to_retain <- to_retain[order(to_retain$class),]}
    if (!is.null(paleobioDB) | !is.null(order)){
      to_retain <- to_retain[order(to_retain$order),]}
    to_retain <- to_retain[order(to_retain$family),]
    to_retain <- to_retain[order(to_retain$genus),]
    if (resolution == "species") {
      to_retain <- to_retain[order(to_retain$genus_species),]}

    #Add occurrences to retain to the new dataset
    new_dataset <- rbind(new_dataset, to_retain)
  }

  return(new_dataset)
}
