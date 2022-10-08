#' Generate pseudo-occurrences from latitudinal range data
#'
#' A function to generate pseudo-occurrences for taxa based on latitudinal
#' ranges (e.g. the output of the 'lat' method in \code{\link{tax_range_geo}}).
#' While the resulting pseudo-occurrences should not be treated as equivalent
#' to actual occurrence data (e.g. like that from the Paleobiology Database),
#' such pseudo-occurrences may be useful for  performing statistical analyses
#' where the row representing a taxon must be replicated for each latitudinal
#' bin through which the taxon ranges.
#'
#' @param taxdf \code{dataframe}. A \code{data.frame} of taxa (such as the
#' output of the 'lat' method in \code{\link{tax_range_geo}}) with columns
#' containing latitudinal range data (maximum and minimum latitude).
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
#' user-supplied data with additional columns included to identify bins.
#' @section Developer(s):
#' Lewis A. Jones & Will Gearty
#' @section Reviewer(s):
#' XXX
#' @export
#' @examples
#' bins <- lat_bins()
#' taxdf <- data.frame(name = c("A", "B", "C"),
#'                     max_lat = c(60, 20, -10),
#'                     min_lat = c(20, -40, -60))
#' tax_expand_lat(taxdf = taxdf, bins = bins)
tax_expand_lat <- function(taxdf,
                           bins,
                           max_lat = "max_lat",
                           min_lat = "min_lat") {
  # Handle errors
  if (is.data.frame(taxdf) == FALSE) {
    stop("`taxdf` should be a dataframe")
  }

  if (is.data.frame(bins) == FALSE) {
    stop("`bins` should be a dataframe")
  }

  if (!all(c("bin", "max", "min") %in% colnames(bins))) {
    stop("Either 'bin', 'max' or 'min' is not a named column in `bins`")
  }

  if (!all(c(max_lat, min_lat) %in% colnames(taxdf))) {
    stop("Either `max_lat` or `min_lat` is not a named column in `taxdf`")
  }

  if (!is.numeric(taxdf[, max_lat])) {
    stop("The class of the max_lat column must be numeric.")
  }

  if (!is.numeric(taxdf[, min_lat])) {
    stop("The class of the min_lat column must be numeric.")
  }

  if (any(c(taxdf[, c(min_lat, max_lat)] < -90))) {
    stop("Maximum and minimum latitudes must be more than or equal to -90")
  }

  if (any(c(taxdf[, c(min_lat, max_lat)] > 90))) {
    stop("Maximum and minimum latitudes must be less than or equal to 90")
  }

  if (any(taxdf[, max_lat] < taxdf[, min_lat])) {
    stop("Maximum latitude must be larger than or equal to minimum latitude")
  }

  if (any(duplicated(taxdf))) {
    stop("Not all rows in `taxdf` are unique")
  }

  # Replicate taxon rows for each lat bin they span
  dat_list <- lapply(seq_len(nrow(bins)), function(i) {
    int_tax <- taxdf[taxdf[, min_lat] < bins$max[i] &
                       taxdf[, max_lat] > bins$min[i], ]
    if (nrow(int_tax) == 0) {
      return(NULL)
    }
    cbind(int_tax, bins[i, ])
  })
  dat <- do.call(rbind,dat_list)
  rownames(dat) <- NULL
  dat
}

