#' Generate pseudo-occurrences from latitudinal range data
#'
#' A function to generate pseudo-occurrences for taxa based on latitudinal
#' ranges (e.g. the output of the 'lat' method in
#' \code{\link{tax_range_space}}).
#' While the resulting pseudo-occurrences should not be treated as equivalent
#' to actual occurrence data (e.g. like that from the Paleobiology Database),
#' such pseudo-occurrences may be useful for  performing statistical analyses
#' where the row representing a taxon must be replicated for each latitudinal
#' bin through which the taxon ranges.
#'
#' @param taxdf \code{dataframe}. A dataframe of taxa (such as the
#' output of the 'lat' method in \code{\link{tax_range_space}}) with columns
#' containing latitudinal range data (maximum and minimum latitude). Column
#' names are assumed to be "max_lat" and "min_lat", but may be updated via the
#' `max_lat` and `min_lat` arguments.
#' Each row should represent a unique taxon. Additional columns may be included
#' (e.g. taxon names, additional taxonomy, etc) and will be included in the
#' returned \code{data.frame}.
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#' allocate fossil occurrences to, such as that returned by
#' \code{\link{lat_bins}}. This dataframe must contain at least the following
#' named columns: "bin", "max" and "min".
#' @param max_lat \code{character}. The name of the column you wish to be
#' treated as the maximum latitude of the latitudinal range (e.g. "max_lat").
#' @param min_lat \code{character}. The name of the column you wish to be
#' treated as the minimum latitude of the latitudinal range (e.g. "min_lat").
#'
#' @return A \code{dataframe} where each row represents a latitudinal bin which
#' a taxon ranges through. The columns are identical to those in the
#' user-supplied data with additional columns included to identify bins. Output
#' will be returned in the order of supplied bins.
#' @section Developer(s):
#' Lewis A. Jones & William Gearty
#' @section Reviewer(s):
#' Christopher D. Dean
#' @export
#' @examples
#' bins <- lat_bins_degrees()
#' taxdf <- data.frame(name = c("A", "B", "C"),
#'                     max_lat = c(60, 20, -10),
#'                     min_lat = c(20, -40, -60))
#' ex <- tax_expand_lat(taxdf = taxdf,
#'                      bins = bins,
#'                      max_lat = "max_lat",
#'                      min_lat = "min_lat")
tax_expand_lat <- function(
  taxdf,
  bins,
  max_lat = "max_lat",
  min_lat = "min_lat"
) {
  check_data_frame(taxdf)
  check_data_frame(bins)

  check_column_presence(bins, "bin")
  check_column_presence(bins, "max")
  check_column_presence(bins, "min")
  check_column_presence(taxdf, max_lat)
  check_column_presence(taxdf, min_lat)

  check_range(taxdf, min_lat, -90, 90)
  check_range(taxdf, max_lat, -90, 90)

  rows_with_max_lat_smaller_than_min_lat <- which(
    taxdf[, max_lat, drop = TRUE] < taxdf[, min_lat, drop = TRUE]
  )
  if (length(rows_with_max_lat_smaller_than_min_lat) > 0) {
    truncated <- if (length(rows_with_max_lat_smaller_than_min_lat) > 5) {
      " (first 5)"
    } else {
      ""
    }
    to_report <- cli::cli_vec(
      head(rows_with_max_lat_smaller_than_min_lat, n = 5),
      list(`vec-last` = ", ")
    )
    cli::cli_abort(
      c(
        "Maximum latitude must be larger than or equal to minimum latitude.",
        "i" = "Row(s) where {.arg max_lat} is smaller than {.arg min_lat}{truncated}: {.val {to_report}}."
      )
    )
  }

  if (anyDuplicated(taxdf) > 0) {
    cli::cli_abort("{.arg taxdf} must not have duplicated rows.")
  }

  # Replicate taxon rows for each lat bin they span
  dat_list <- lapply(seq_len(nrow(bins)), function(i) {
    int_tax <- taxdf[
      taxdf[, min_lat, drop = TRUE] < bins$max[i] &
        taxdf[, max_lat, drop = TRUE] > bins$min[i],
    ]
    if (nrow(int_tax) == 0) {
      return(NULL)
    }
    suppressWarnings(cbind(int_tax, bins[i, ]))
  })
  dat <- do.call(rbind, dat_list)
  rownames(dat) <- NULL
  dat
}
