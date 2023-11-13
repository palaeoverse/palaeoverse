#' Generate stratigraphic section plot
#'
#' A function to generate a stratigraphic section plot.
#'
#' @param size \code{numeric}. A single numeric value defining the width of the
#' latitudinal bins. This value must be more than 0, and less than or equal to
#' 90 (defaults to 10).
#' @return A \code{dataframe} of latitudinal bins of user-defined size.
#' @importFrom graphics polygon abline title
#' @section Developer(s):
#' Bethany Allen & Alexander Dunhill
#' @section Reviewer(s):
#' William Gearty
#' @export
#' @examples
#' # Generate 20 degrees latitudinal bins
#' bins <- lat_bins(size = 20)
tax_range_strat <- function (occdf, name = "genus", section = "Section",
                             certainty = "certainty", height = "height",
                             by = "FAD")
{
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (!is.numeric(occdf[, height])) {
    stop("`height` must be of class numeric.")
  }

  if (any(c(name, height) %in% colnames(occdf) == FALSE)) {
    stop("Either `name` or `height` is not a named column in\n`occdf`")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, min_bin]))) {
    stop("The `height` column contains NA values")
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
    ranges[i,2] <- min(occ_filter$height)
    ranges[i,3] <- max(occ_filter$height)
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
  occdf <- merge(occdf, labels, by.x = "genus", by.y = "taxon")

  #Split certain and uncertain occurrences
  certain <- occdf[(occdf[, certainty] == 1), ]
  uncertain <- occdf[(occdf[, certainty] == 0), ]

  #Create plot
  plot(x = NA, y = NA,
       xlim = c(1, length(unique_taxa)),
       ylim = c(min(ranges$min_bin), max(ranges$max_bin)),
       axes = FALSE,
       xlab = "Taxa",
       ylab = "Stratigraphic height (m)",
       main = section)
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
