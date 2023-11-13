#' Generate stratigraphic section plot
#'
#' A function to plot the stratigraphic range of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences containing
#' at least two columns: names of taxa, and their stratigraphic position
#' (see `name` and `level` arguments).
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names, e.g. "genus" (default).
#' @param level \code{character}. The name of the column you wish to be treated
#' as the stratigraphic levels associated with each occurrence, e.g. "bed"
#' (default) or "height".
#' @param certainty \code{character}. The name of the column you wish to be
#' treated as the information on whether an identification is certain ("1") or
#' uncertain ("0"). In the plot, any occurrence labelled as certain will be
#' plotted with a black circle, while any occurrence labelled as uncertain will
#' be plotted with a white circle.
#' @param by \code{character}. How should the output be sorted?
#' Either: "FAD" (first appearance; default), "LAD" (last appearance), or
#' "name" (alphabetically by taxon names).
#' @param label \code{character}. The title given to the plot, e.g. "Section A"
#' (default).
#'
#' @return A \code{plot} showing the stratigraphic ranges of taxa in a section,
#' with levels at which the taxon was sampled indicated with a point.
#'
#' @section Developer(s):
#' Bethany Allen & Alexander Dunhill
#' @section Reviewer(s):
#' William Gearty
#'
#' @examples
#' # Plot stratigraphic ranges
#' tax_range_strat()
#'
#' @export
tax_range_strat <- function (occdf, name = "taxon", level = "bed",
                             certainty = "certainty", by = "FAD",
                             label = "Section A")
{
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (!is.numeric(occdf[, level])) {
    stop("`level` must be of class numeric.")
  }

  if (any(c(name, level) %in% colnames(occdf) == FALSE)) {
    stop("Either `name` or `level` is not a named column in\n`occdf`")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, level]))) {
    stop("The `level` column contains NA values")
  }

  if (!by %in% c("name", "FAD", "LAD")) {
    stop("`by` must be either \"FAD\", \"LAD\", or \"name\"")
  }

  #List and order unique taxa
  unique_taxa <- unique(occdf[, name])
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #Create object to hold information
  ranges <- data.frame(taxon = unique_taxa, min_bin = NA, max_bin = NA)

  #Populate nested list
  for (i in seq_along(unique_taxa)) {
    occ_filter <- occdf[(occdf[, name] == unique_taxa[i]),]
    ranges[i,2] <- min(occ_filter[level])
    ranges[i,3] <- max(occ_filter[level])
  }

  #Reorder lists
  if (by == "LAD") {
    ranges <- ranges[order(ranges$max_bin), ]
  }

  if (by == "FAD") {
    ranges <- ranges[order(ranges$min_bin), ]
  }

  #Add ID numbers
  ranges$ID <- c(1:length(unique_taxa))
  labels <- ranges[, c(1,4)]
  occdf <- merge(occdf, labels, by.x = name, by.y = "taxon")

  #Split certain and uncertain occurrences
  certain <- occdf[(occdf[, certainty] == 1), ]
  uncertain <- occdf[(occdf[, certainty] == 0), ]

  #Create plot
  plot(x = NA, y = NA,
       xlim = c(1, length(unique_taxa)),
       ylim = c(min(ranges$min_bin), max(ranges$max_bin)),
       axes = FALSE,
       xlab = "Taxon",
       ylab = "Stratigraphic height (m)",
       main = label)
  segments(y0 = ranges$min_bin, y1 = ranges$max_bin,
           x0 = ranges$ID,
           col = "black")
  points(y = certain$height, x = certain$ID, pch = 19,
         col = "black")
  points(y = uncertain$height, x = uncertain$ID, pch = 1,
         col = "black")
  axis(2, unique(occdf$height))
  axis(1, ranges$taxon, at = c(1:max(ranges$ID)), las = 2)
  box()

  return()
}

# to test
occdf <- read.csv("data/stratrange_test_data.csv")
tax_range_strat(occdf = occdf)
