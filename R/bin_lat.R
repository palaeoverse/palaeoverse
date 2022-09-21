#' Assign fossil occurrences to time bins
#'
#' A function to assign fossil occurrences to user-specified latitudinal bins.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to bin. This dataframe should contain a column with the
#' latitudinal coordinates of occurrence data.
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#' allocate fossil occurrences to, such as that returned by
#' \code{\link[palaeoverse:lat_bins]{lat_bins()}}. This dataframe must
#' contain at least the following named columns: "bin", "max" and "min".
#' @param lat \code{character}. The name of the column you wish to be treated
#' as the input latitude (e.g., "lat" or "p_lat"). This column should contain
#' numerical values. Defaults to "lat".
#'
#' @return A dataframe of the original input `occdf` with appended
#' columns containing respective latitudinal bin information.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' XXX
#' @export
#' @examples
#' # Load occurrence data
#' occdf <- tetrapods
#' # Generate latitudinal bins
#' bins <- lat_bins(size = 10)
#' # Bin data
#' bin_lat(occdf = occdf, bins = bins, lat = "lat")
#'
bin_lat <- function(occdf, bins, lat = "lat"){
  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }
  if (is.data.frame(bins) == FALSE) {
    stop("`bins` should be a dataframe.")
  }
  if (lat %in% colnames(occdf) == FALSE) {
    stop("`lat` column name does not exist in `occdf`")
  }
  if (any(is.na(occdf[, lat]))) {
    stop("`lat` contains NA values")
  }
  if (any(occdf[, lat] > 90 | occdf[, lat] < -90)) {
    stop("Latitudes should be more than -90 and less than 90")
  }
  if (sum(c("max", "min", "bin") %in% colnames(bins)) != 3) {
    stop("`bins` does not contain bin, max and min named columns")
  }
  #=== Set up ===
  # Add mid bin
  bins$mid <- (bins$max + bins$min) / 2
  occdf$lat_bin <- NA
  occdf$lat_max <- NA
  occdf$lat_mid <- NA
  occdf$lat_min <- NA
  #=== Assign data ===
  for (i in seq_len(nrow(bins))){
    vec <- which(occdf[, lat] <= bins$max[i] & occdf[, lat] >= bins$min[i])
    occdf$lat_bin[vec] <- bins$bin[i]
    occdf$lat_max[vec] <- bins$max[i]
    occdf$lat_mid[vec] <- bins$mid[i]
    occdf$lat_min[vec] <- bins$min[i]
  }
  #=== Return data ===
  return(occdf)
}
