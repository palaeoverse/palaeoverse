#' Generate stratigraphic section plot
#'
#' A function to plot the stratigraphic ranges of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences containing
#'   at least two columns: names of taxa, and their stratigraphic position (see
#'   `name` and `level` arguments).
#' @param name \code{character}. The name of the column you wish to be treated
#'   as the input names, e.g. "taxon" (default).
#' @param level \code{character}. The name of the column you wish to be treated
#'   as the stratigraphic levels associated with each occurrence, e.g. "bed"
#'   (default) or "height".
#' @param certainty \code{character}. The name of the column you wish to be
#'   treated as the information on whether an identification is certain ("1") or
#'   uncertain ("0"). By default (\code{certainty = NULL}), no column name is
#'   provided, and all occurrences are assumed to be certain. In the plot, any
#'   occurrence labelled as certain will be plotted with a black circle, while
#'   any occurrence labelled as uncertain will be plotted with a white circle.
#' @param by \code{character}. How should the output be sorted? Either: "FAD"
#'   (first appearance; default), "LAD" (last appearance), or "name"
#'   (alphabetically by taxon names).
#' @param xlab \code{character}. The x-axis label (over taxa) passed directly to
#'   \code{plot}.
#' @param ylab \code{character}. The y-axis label (over strata) passed directly
#'   to \code{plot}.
#' @param plot_args A list of optional arguments that are passed directly to
#'   [graphics::plot()].
#' @param x_args A list of optional arguments that are passed directly to
#'   [axis()] when generating the x-axis.
#' @param y_args A list of optional arguments that are passed directly to
#'   [axis()] when generating the x-axis.
#'
#' @return Invisibly returns a data.frame of the calculated taxonomic
#'   stratigraphic ranges.
#'
#'   Function is usually used for its side effect, which is to create a plot
#'   showing the stratigraphic ranges of taxa in a section, with levels at which
#'   the taxon was sampled indicated with a point.
#'
#' @section Developer(s): Bethany Allen & Alexander Dunhill
#' @section Reviewer(s): William Gearty & Lewis A. Jones
#' @importFrom graphics axis par segments plot points box
#'
#' @examples
#' #Load tetrapod dataset
#' data(tetrapods)
#' # Sample tetrapod occurrences
#' tetrapod_names <- tetrapods$accepted_name[1:50]
#' # Simulate bed numbers
#' beds_sampled <- sample.int(n = 10, size = 50, replace = TRUE)
#' # Simulate certainty values
#' certainty_sampled <- sample(x = 0:1, size = 50, replace = TRUE)
#' #Combine into data frame
#' occdf <- data.frame(taxon = tetrapod_names, bed = beds_sampled,
#' certainty = certainty_sampled)
#' # Plot stratigraphic ranges
#' par(mar = c(12, 5, 2, 2))
#' plot_section(occdf, ylab = "Stratigraphic height (m)")
#' plot_section(occdf, certainty = "certainty",
#'              ylab = "Stratigraphic height (m)")
#' plot_section(occdf, certainty = "certainty", by = "LAD",
#'              ylab = "Bed number")
#' plot_section(occdf, certainty = "certainty", by = "name",
#'              ylab = "Bed number")
#' plot_section(occdf, certainty = "certainty", ylab = "Bed number",
#'              main = "Section A")
#'
#' @export
plot_section <- function(occdf, name = "taxon", level = "bed",
                         certainty = NULL, by = "FAD",
                         xlab = "", ylab = "", plot_args = list(),
                         x_args = list(font = 3, las = 2), y_args = list()) {

  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (!is.numeric(occdf[, level])) {
    stop("`level` must be of class numeric.")
  }

  if (any(c(name, level) %in% colnames(occdf) == FALSE)) {
    stop("Either `name` or `level` is not a named column in `occdf`")
  }

  if (!is.null(certainty)) {
    if (!is.character(certainty)) {
      stop("`certainty` must either be of class character or NULL")
    }
    if (certainty %in% colnames(occdf) == FALSE) {
      stop("`certainty` is not a named column in `occdf`")
    }
    if (any(is.na(occdf[, certainty]))) {
      stop("The `certainty` column contains NA values")
    }
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
  if (is.null(certainty)) {
    ranges <- data.frame(taxon = unique_taxa, min_bin = NA, max_bin = NA)
  } else {
    ranges <- data.frame(taxon = unique_taxa, min_bin = NA,
                         max_bin = NA, min_bin_certain = NA,
                         max_bin_certain = NA)
  }

  #Populate nested list
  for (i in seq_along(unique_taxa)) {
    occ_filter <- occdf[(occdf[, name] == unique_taxa[i]), ]
    ranges[i, 2] <- min(occ_filter[level])
    ranges[i, 3] <- max(occ_filter[level])
    #If uncertainty is used, fill second set of columns for certain IDs
    if (!is.null(certainty)) {
      occ_filter <- occ_filter[(occ_filter[, certainty] == 1), ]
      if (nrow(occ_filter) == 0) {
        occ_filter[1, ] <- NA
      }
      ranges[i, 4] <- min(occ_filter[level])
      ranges[i, 5] <- max(occ_filter[level])
    }
  }

  #Reorder lists
  if (by == "LAD") {
    ranges <- ranges[order(ranges$min_bin), ]
    ranges <- ranges[order(ranges$max_bin), ]
  }

  if (by == "FAD") {
    ranges <- ranges[order(ranges$max_bin), ]
    ranges <- ranges[order(ranges$min_bin), ]
  }

  #Add ID numbers
  ranges$ID <- seq_along(unique_taxa)
  labels <- ranges[, c("taxon", "ID")]
  occdf <- merge(occdf, labels, by.x = name, by.y = "taxon")

  #Obtain uncertain occurrences
  if (!is.null(certainty)) {
    uncertain <- occdf[(occdf[, certainty] == 0), ]
  }

  #Create plot
  dump <- c("x", "y", "xlim", "ylim", "axes", "xlab", "ylab")
  if (any(names(plot_args) %in% dump)) {
    plot_args <- plot_args[-which(names(plot_args) %in% dump)]
  }
  do.call(plot, args =
            c(list(x = NA, y = NA,
                   xlim = c(1, length(unique_taxa)),
                   ylim = c(min(ranges$min_bin), max(ranges$max_bin)),
                   axes = FALSE,
                   xlab = xlab,
                   ylab = ylab),
              plot_args))
  if (is.null(certainty)) {
    segments(y0 = ranges$min_bin, y1 = ranges$max_bin,
             x0 = ranges$ID,
             col = "black", lwd = 1.5)
  } else {
    segments(y0 = ranges$min_bin, y1 = ranges$max_bin,
             x0 = ranges$ID,
             col = "black", lty = 2)
    segments(y0 = ranges$min_bin_certain, y1 = ranges$max_bin_certain,
             x0 = ranges$ID,
             col = "black", lwd = 1.5)
  }
  points(y = occdf$bed, x = occdf$ID, pch = 19,
         col = "black")
  if (!is.null(certainty)){
    points(y = uncertain$bed, x = uncertain$ID, pch = 21,
           col = "black", bg = "white")
  }
  # plot y-axis
  dump <- c("side", "at")
  if (any(names(y_args) %in% dump)) {
    y_args <- y_args[-which(names(y_args) %in% dump)]
  }
  do.call(axis, args = c(list(side = 2, at = unique(occdf$bed)), y_args))
  # plot x-axis
  dump <- c("side", "at", "labels")
  if (any(names(x_args) %in% dump)) {
    x_args <- x_args[-which(names(x_args) %in% dump)]
  }
  do.call(axis, args = c(list(side = 1, at = ranges$ID, labels = ranges$taxon),
                         x_args))
  box()
  invisible(ranges)
}
