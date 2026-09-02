#' Assign fossil occurrences to latitudinal bins
#'
#' A function to assign fossil occurrences to user-specified latitudinal bins.
#'
#' @param occdf `dataframe`. A dataframe of the fossil occurrences you
#' wish to bin. This dataframe should contain a column with the
#' latitudinal coordinates of occurrence data.
#' @param bins `dataframe`. A dataframe of the bins that you wish to
#' allocate fossil occurrences to, such as that returned by
#' [`lat_bins_degrees()`] and [`lat_bins_area()`]. This dataframe must
#' contain at least the following named columns: "bin", "max" and "min".
#' @param lat `character`. The name of the column you wish to be treated
#' as the input latitude (e.g. "lat" or "p_lat"). This column should contain
#' numerical values. Defaults to "lat".
#' @param boundary `logical`. If `TRUE`, occurrences
#' falling on the boundaries of latitudinal bins will be duplicated and
#' assigned to both bins.
#' If `FALSE`, occurrences will be binned into the upper bin
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
#' bins <- lat_bins_degrees(size = 10)
#' # Bin data
#' occdf <- bin_lat(occdf = occdf, bins = bins, lat = "lat")
#'
bin_lat <- function(occdf, bins, lat = "lat", boundary = FALSE) {
  ensure_args_are_named()

  check_data_frame(occdf)
  check_data_frame(bins)
  rlang::check_bool(boundary)
  check_column_presence(occdf, lat)
  check_column_presence(bins, "min")
  check_column_presence(bins, "max")
  check_column_presence(bins, "bin")

  lat_vals <- occdf[[lat]]
  check_na(occdf, lat)
  check_range(occdf, lat, -90, 90)

  #=== Set up ===
  # Add mid bin
  bins$mid <- (bins$max + bins$min) / 2
  occdf$lat_bin <- NA
  occdf$lat_max <- NA
  occdf$lat_mid <- NA
  occdf$lat_min <- NA
  #=== Assign data ===
  for (i in seq_len(nrow(bins))) {
    vec <- which(
      lat_vals <= bins$max[i] &
        lat_vals >= bins$min[i]
    )
    occdf$lat_bin[vec] <- bins$bin[i]
    occdf$lat_max[vec] <- bins$max[i]
    occdf$lat_mid[vec] <- bins$mid[i]
    occdf$lat_min[vec] <- bins$min[i]
  }
  #=== Boundary bins ===
  if (
    boundary &&
      any(lat_vals %in% c(bins$max, bins$min))
  ) {
    # Which occurrences fall on boundaries?
    tmp <- occdf[which(lat_vals %in% c(bins$max, bins$min)), ]
    # Reverse direction to ensure alternative bin is assigned
    for (i in rev(seq_len(nrow(bins)))) {
      vec <- which(
        tmp[, lat, drop = TRUE] <= bins$max[i] &
          tmp[, lat, drop = TRUE] >= bins$min[i]
      )
      tmp$lat_bin[vec] <- bins$bin[i]
      tmp$lat_max[vec] <- bins$max[i]
      tmp$lat_mid[vec] <- bins$mid[i]
      tmp$lat_min[vec] <- bins$min[i]
    }
    occdf <- rbind.data.frame(occdf, tmp)
  }
  #=== Add warning ===
  if (
    !boundary &&
      any(lat_vals %in% c(bins$max, bins$min))
  ) {
    cli::cli_warn(c(
      "Presence of occurrences falling on boundaries detected.",
      "i" = "Occurrences assigned to upper bin."
    ))
  }
  #=== Return data ===
  return(occdf)
}
