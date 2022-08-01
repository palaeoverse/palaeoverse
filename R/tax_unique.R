#' Filter occurrences to unique taxa
#'
#' A function to filter a list of taxonomic occurrences to unique taxa of a
#' predefined resolution. In its simplest form, this involves retaining all
#' unique identifications within the dataset of the given taxonomic resolution,
#' however here we also retain occurrences identified to a coarser resolution
#' which are not already represented within the dataset.
#' Need to give an example here!
#'
#' @param paleobioDB \code{dataframe}. A data frame of taxonomic occurrences
#' downloaded directly from the Paleobiology Database.
#' @param species \code{character}. A vector of species names.
#' @param genus \code{character}. A vector of genus names.
#' @param family \code{character}. A vector of family names.
#' @param order \code{character}. A vector of order names.
#' @param class \code{character}. A vector of class names.
#' @param resolution \code{character}. The taxonomic resolution at which to
#' identify unique occurrences. At present, you can choose between species
#' (the default) or genera.
#' @param by \code{numeric, character}. A category within which to determine
#' unique taxa. For example, this might be a designation of temporal intervals
#' or spatial bins. When using \code{paleobioDB}, this should specify the
#' column within the data frame to use as the category. When using individual
#' taxonomic vectors, this should be an additional vector of numbers or
#' character-based labels designating which category each occurrence belongs to.
#'
#' @return A \code{dataframe} of unique taxa, with the vector or row numbers
#' they correspond to in the original inputs.
#'
#' @section Developer(s):
#' Bethany Allen
#' @section Reviewer(s):
#'
#' @examples
#' #Describe an example here
#' implement(example)
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

  if (!is.null(paleobioDB) &
      (!"class" %in% colnames(paleobioDB)) |
      (!"order" %in% colnames(paleobioDB)) |
      (!"family" %in% colnames(paleobioDB)) |
      (!"genus" %in% colnames(paleobioDB)) |
      (!"accepted_name" %in% colnames(paleobioDB))) {
    stop("paleobioDB must contain the following columns: class, order, family,
         genus, accepted_name")
  }

  if ((!is.null(species) & (!is.vector(species))) |
      (!is.null(genus) & (!is.vector(genus))) |
      (!is.null(family) & (!is.vector(family))) |
      (!is.null(order) & (!is.vector(order))) |
      (!is.null(class) & (!is.vector(class)))) {
    stop("Taxononic information must be in a vector")
  }

  if (!is.null(by) & !is.null(paleobioDB) & !by %in% colnames(paleobioDB)) {
    stop("by must be a column name within the paleobioDB data frame")
  }

  if (!is.null(by) & !is.null(species) & !is.vector(by)) {
    stop("When using taxonomic vectors, by must also be a vector")
  }

#Run function

  if (!is.null(paleobioDB)) {
    #Filter paleobioDB necessary columns
    occurrences <- paleobioDB[c("class", "order", "family",
                             "genus", "accepted_name")]

    #If a category is provided, add it as a column
    if(!is.null(by)){
      occurrences$category <- paleobioDB[[by]]
    } else {
      occurrences$category <- 1
    }

    #If accepted name is not a binomial, replace with NA
    occurrences[grep(" ", occurrences$accepted_name, invert = TRUE),
              "accepted_name"] <- NA
    colnames(occurrences)[colnames(occurrences) == "accepted_name"] <-
      "genus_species"
  } else

  if (is.null(paleobioDB)) {
    #Compile supplied columns into a data frame
    occurrences <- cbind(class, order, family, genus, species)

    #Create genus_species column
    occurrences$genus_species <- paste(occurrences$genus, occurrences$family)
    #If one or both of the values was NA, set to NA
    occurrences$genus_species[grep("NA", occurrences$genus_species),
                "accepted_name"] <- NA

    #If a category is provided, add it as a column
    if(!is.null(by)){
      occurrences$category <- by
    } else {
      occurrences$category <- 1
    }
  }

  #Add row numbers for later reference
  occurrences$row_no <- 1:nrow(occurrences)

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
      #Retain occurrences identified to species level and remove from the data frame
      to_retain <- rbind(to_retain, one_cat[!is.na(one_cat$genus_species),])
      one_cat <- one_cat[is.na(one_cat$genus_species),]

      #Retain occurrences identified to genus level and not already in the dataset
      to_retain <- rbind(to_retain, one_cat[!(one_cat$genus %in% to_retain$genus),])
      one_cat <- one_cat[is.na(one_cat$genus),]} else

    if (resolution == "genera") {
      #Remove genus_species
      one_cat <- subset(one_cat, select = -genus_species)

      #Remove repeats and retain
      to_retain <- unique(one_cat)
    }

    #Retain occurrences identified to family level and not already in the dataset
    to_retain <- rbind(to_retain, one_cat[!(one_cat$family %in% to_retain$family),])
    one_cat <- one_cat[is.na(one_cat$family),]

    if (!is.null(order)){
    #Retain occurrences identified to order level and not already in the dataset
    to_retain <- rbind(to_retain, one_cat[!(one_cat$order %in% to_retain$order),])
    one_cat <- one_cat[is.na(one_cat$order),]}

    if (!is.null(class)){
    #Retain occurrences identified to class level and not already in the dataset
    to_retain <- rbind(to_retain, one_cat[!(one_cat$class %in% to_retain$class),])
    one_cat <- one_cat[is.na(one_cat$class),]}

    #Add occurrences to retain to the new dataset
    new_dataset <- rbind(new_dataset, to_retain)
  }

  #Sort by row number in original dataset
  new_dataset <- new_dataset[order(new_dataset$row_no),]

  return(new_dataset)
}
