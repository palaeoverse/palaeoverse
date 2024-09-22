#' Generate equal-area latitudinal bins
#'
#' A function to generate approximately equal-area latitudinal bins for a
#' user-specified given number of bins and latitudinal range. This approach
#' is based on calculating the curved surface area of spherical segments
#' bounded by two parallel disks.
#'
#' @param n \code{numeric}. A single numeric value defining the number of
#'   approximately equal-area latitudinal bins to split the latitudinal
#'   range by (as defined by `min` and `max`).
#' @param min \code{numeric}. A single numeric value defining the lower limit
#'   of the latitudinal range (defaults to -90).
#' @param max \code{numeric}. A single numeric value defining the upper limit
#'   of the latitudinal range (defaults to 90).
#' @param r \code{numeric}. The radius of the Earth in metres. Defaults to the
#'  the mean radius of the Earth (6371008.7714).
#' @param plot \code{logical}. Should a plot of the latitudinal bins be
#'   generated?
#' @return A \code{data.frame} of user-defined number of latitudinal bins.
#' @importFrom graphics polygon abline title
#' @section Developer(s):
#'   Lewis A. Jones
#' @section Reviewer(s):
#'   TBC
#' @export
#' @examples
#' # Generate 12 latitudinal bins
#' bins <- lat_bins_equal(n = 12)
#' # Generate latitudinal bins for just the (sub-)tropics
#' bins <- lat_bins_equal(n = 6, min = -30, max = 30)
#' # Generate latitudinal bins and a plot
#' bins <- lat_bins_equal(n = 24, plot = TRUE)
lat_bins_equal <- function(n = 12,
                           min = -90, max = 90, r = 6371008.7714,
                           plot = FALSE) {
  # Error handling
  if (!is.numeric(n)) {
    stop("`n` should be a numeric.")
  }

  if (max > 90 || max < -90) {
    stop("`max` should be less than 90 and more than -90.")
  }

  if (min > 90 || min < -90) {
    stop("`min` should be less than 90 and more than -90.")
  }

  if (min > max) {
    stop("`min` should be less than `max`.")
  }

  if (!is.logical(plot)) {
    stop("`plot` should be logical (TRUE/FALSE).")
  }

  if (!is.numeric(r)) {
    stop("`r` should be a numeric.")
  }

  # Internal function for calculating area between two disks
  bounded_surface_area <- function(x, r = r) {
    min <- min(x) * (pi / 180)
    max <- max(x) * (pi / 180)
    a <- (2 * pi) * (r^2) * abs(sin(max) - sin(min))
    return(a)
  }
  # Calculate Earth area
  #earth_area <- bounded_surface_area(x = c(-90, 90), r = r)
  # Calculate area between parallel disks (min/max limits)
  limits_area <- bounded_surface_area(x = c(min, max), r = r)
  # What is the area of each bin?
  bin_area <- limits_area / n
  # Specify number of steps (higher number of steps, increased equality)
  vals <- seq(min, max, by = 0.1)
  # Set starting index
  indx <- 1
  # Create empty df for populating
  bins <- data.frame(bin = 1:n, max = rep(NA, n), mid = rep(NA, n),
                     min = rep(NA, n), area = rep(NA, n))
  # Run across desired number of bins
  for (i in seq_len(n)) {
    # Set area value to 0
    a <- 0
    # Record starting index
    start_indx <- indx
    while (a < bin_area) {
      # Increase index
      indx <- indx + 1
      # Extract latitudes
      lat <- c(vals[indx-1], vals[indx])
      # If NAs are present, index has gone beyond vals length
      if (indx > length(vals)) {
        indx <- indx - 1
        break
      }
      # Calculate area
      a <- a + bounded_surface_area(x = lat, r = r)
    }
    # Record final index
    end_indx <- indx
    # Assign breaks
    bins$max[i] <- vals[end_indx]
    bins$min[i] <- vals[start_indx]
    # Assign area value
    bins$area[i] <- a
  }
  # Order bins
  bins <- bins[order(bins$min, decreasing = TRUE), ]
  # Update bin numbers
  bins$bin <- 1:nrow(bins)
  # Drop row names
  row.names(bins) <- NULL
  # Add area proportions
  bins$area_prop <- bins$area / sum(bins$area)
  # Add mid bin
  bins$mid <- (bins$max + bins$min) / 2
  #plot latitudinal bins
  if (plot == TRUE) {
    plot(1, type = "n", xlim = c(-180, 180),
         ylim = c(min(bins$min), max(bins$max)),
         xlab = "Longitude (\u00B0)", ylab = "Latitude (\u00B0)")
    cols <- rep(c("#01665e", "#80cdc1"), nrow(bins))
    for (i in seq_len(nrow(bins))){
      polygon(x = c(-180, -180, 180, 180),
              y = c(bins$min[i], bins$max[i], bins$max[i], bins$min[i]),
              col = cols[i],
              border = "black")
    }
  }
  # Return bins
  return(bins)
}
