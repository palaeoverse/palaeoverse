#' Generate equal-area latitudinal bins
#'
#' A function to generate approximately equal-area latitudinal bins for a
#' user-specified number of bins and latitudinal range. This approach is
#' based on calculating the curved surface area of spherical segments bounded
#' by two parallel discs.
#'
#' @param n \code{numeric}. A single numeric value defining the number of
#'   equal-area latitudinal bins to split the latitudinal range into (as
#'   defined by `min` and `max`).
#' @param min \code{numeric}. A single numeric value defining the lower limit
#'   of the latitudinal range (defaults to -90).
#' @param max \code{numeric}. A single numeric value defining the upper limit
#'   of the latitudinal range (defaults to 90).
#' @param r \code{numeric}. The radius of the Earth in kilometres. Defaults to
#'   the the mean radius of the Earth (6371 km).
#' @param plot \code{logical}. Should a plot of the latitudinal bins be
#'   generated? If `TRUE`, a plot is generated. Defaults to `FALSE`.
#' @return A \code{data.frame} of user-defined number of latitudinal bins. The
#'   \code{data.frame} contains the following columns: bin (bin number), min
#'   (minimum latitude of the bin), mid (midpoint latitude of the bin),
#'   max (maximum latitude of the bin), area (the area of the bin in
#'   km\ifelse{html}{\out{<sup>2</sup>}}{\eqn{^2}}), area_prop (the
#'   proportional area of the bin across all bins).
#' @seealso
#' For bins with unequal area, but equal latitudinal range, see
#'   \link{lat_bins_degrees}.
#' @importFrom graphics polygon abline title
#' @section Developer(s):
#'   Lewis A. Jones & Kilian Eichenseer
#' @section Reviewer(s):
#'   Kilian Eichenseer & Bethany Allen
#' @export
#' @examples
#' # Generate 12 latitudinal bins
#' bins <- lat_bins_area(n = 12)
#' # Generate latitudinal bins for just the (sub-)tropics
#' bins <- lat_bins_area(n = 6, min = -30, max = 30)
#' # Generate latitudinal bins and a plot
#' bins <- lat_bins_area(n = 24, plot = TRUE)
lat_bins_area <- function(n = 12,
                          min = -90, max = 90, r = 6371,
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

  # Convert r to metres
  r <- r * 1000

  # sine of the min and max latitudes (using radians)
  sin_min_lat <- sin(min * pi / 180)
  sin_max_lat <- sin(max * pi / 180)

  # indices to divide the area into n parts
  indices <- n:0

  # latitudes of bin boundaries in radians
  latitudes_rad <- asin(sin_min_lat + indices / n * (sin_max_lat - sin_min_lat))

  # latitudes of bin boundaries in degrees
  latitudes <- latitudes_rad * 180 / pi

  # vertical sine span of the bands on the surface of the sphere
  sine_spans <- sin(latitudes_rad[-(n + 1)]) - sin(latitudes_rad[-1])

  # absolute surface area for each band
  band_areas <- 2 * pi * r^2 * sine_spans

  # surface area proportion for each band
  band_areas_prop <- sine_spans / (sin_max_lat - sin_min_lat)

  # midpoint for each band
  mid_points <- (latitudes[-(n + 1)] + latitudes[-1]) / 2

  # populate data frame
  bins <- data.frame(bin = 1:n,
                     min = latitudes[-1],
                     mid = mid_points,
                     max = latitudes[-(n + 1)],
                     area = band_areas,
                     area_prop = band_areas_prop)

  # plot latitudinal bins
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
