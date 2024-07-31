#' Assign fossil occurrences to latitudinal bins
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
#' as the input latitude (e.g. "lat" or "p_lat"). This column should contain
#' numerical values. Defaults to "lat".
#' @param boundary \code{logical}. If \code{TRUE}, occurrences
#' falling on the boundaries of latitudinal bins will be duplicated and
#' assigned to both bins.
#' If \code{FALSE}, occurrences will be binned into the upper bin
#' only (i.e. highest row number).
#'
#' @return A dataframe of the original input `occdf` with appended
#' columns containing respective latitudinal bin information.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Sofia Galvan
#' @export
#' @examples
#' # Load occurrence data
#' occdf <- tetrapods
#' # Generate latitudinal bins
#' bins <- lat_bins(size = 10)
#' # Bin data
#' occdf <- bin_lat(occdf = occdf, bins = bins, lat = "lat")
#'
bin_lat <- function(occdf, bins, lat = "lat", boundary = FALSE) {
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
  if (any(is.na(occdf[, lat, drop = TRUE]))) {
    stop("`lat` contains NA values")
  }
  if (any(occdf[, lat, drop = TRUE] > 90 | occdf[, lat, drop = TRUE] < -90)) {
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
  for (i in seq_len(nrow(bins))) {
    vec <- which(occdf[, lat, drop = TRUE] <= bins$max[i] &
                 occdf[, lat, drop = TRUE] >= bins$min[i])
    occdf$lat_bin[vec] <- bins$bin[i]
    occdf$lat_max[vec] <- bins$max[i]
    occdf$lat_mid[vec] <- bins$mid[i]
    occdf$lat_min[vec] <- bins$min[i]
  }
  #=== Boundary bins ===
  if (boundary == TRUE &&
      any(occdf[, lat, drop = TRUE] %in% c(bins$max, bins$min))) {
    # Which occurrences fall on boundaries?
    tmp <- occdf[which(occdf[, lat, drop = TRUE] %in% c(bins$max, bins$min)), ]
    # Reverse direction to ensure alternative bin is assigned
    for (i in rev(seq_len(nrow(bins)))) {
      vec <- which(tmp[, lat, drop = TRUE] <= bins$max[i] &
                     tmp[, lat, drop = TRUE] >= bins$min[i])
      tmp$lat_bin[vec] <- bins$bin[i]
      tmp$lat_max[vec] <- bins$max[i]
      tmp$lat_mid[vec] <- bins$mid[i]
      tmp$lat_min[vec] <- bins$min[i]
    }
    occdf <- rbind.data.frame(occdf, tmp)
  }
  #=== Add warning ===
  if (boundary == FALSE &&
      any(occdf[, lat, drop = TRUE] %in% c(bins$max, bins$min))) {
    message(paste("Presence of occurrences falling on boundaries detected.",
                   "\nOccurrences assigned to upper bin."))
  }
  #=== Return data ===
  return(occdf)
}
