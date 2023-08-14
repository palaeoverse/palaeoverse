#' Generate pseudo-occurrences from temporal range data
#'
#' A function to generate interval-level pseudo-occurrences for taxa based on
#' temporal ranges (e.g. the output of \code{\link{tax_range_time}}). While the
#' resulting pseudo-occurrences should not be treated as equivalent to actual
#' occurrence data (e.g. like that from the Paleobiology Database), such
#' pseudo-occurrences may be useful for performing statistical analyses where
#' the row representing a taxon must be replicated for each interval through
#' which the taxon persisted.
#'
#' @param taxdf \code{dataframe}. A dataframe of taxa (such as that
#'   produced by \code{\link{tax_range_time}}) with columns for the maximum and
#'   minimum ages (FADs and LADs). Each row should represent a unique taxon.
#'   Additional columns may be included (e.g. taxon names, additional taxonomy,
#'   etc) and will be included in the returned \code{data.frame}. If required,
#'   `numeric` ages can be generated from interval names via the
#'   \code{\link{look_up}} function.
#' @param max_ma \code{character}. The name of the column you wish to be treated
#'   as the maximum limit (FADs) of the age range (e.g. "max_ma").
#' @param min_ma \code{character}. The name of the column you wish to be treated
#'   as the minimum limit (LADs) of the age range (e.g. "min_ma").
#' @param bins \code{dataframe}. A dataframe of the bins that you wish to
#'   allocate pseudo-occurrences to such as that returned by
#'   \code{\link[palaeoverse:time_bins]{time_bins()}}. This dataframe must
#'   contain at least the following named columns: "bin", "max_ma" and
#'   "min_ma". Columns "max_ma" and "min_ma" must be `numeric` values.
#' @param scale \code{character}. Specify the desired geological timescale to be
#'   used, either "GTS2020" or "GTS2012". Passed to
#'   \code{\link[palaeoverse:time_bins]{time_bins()}} if \code{bins} is not
#'   specified.
#' @param rank \code{character}. Specify the desired stratigraphic rank. Choose
#'   from: "stage", "epoch", "period", "era", and "eon". Passed to
#'   \code{\link[palaeoverse:time_bins]{time_bins()}} if \code{bins} is not
#'   specified.
#' @param ext_orig \code{logical}. Should two additional columns be added to
#'   identify the intervals in which taxa originated and went extinct?
#'
#' @return A \code{dataframe} where each row represents an interval during which
#'   a taxon in the original user-supplied data persisted. The columns are
#'   identical to those in the user-supplied data with additional columns
#'   included to identify the intervals. If \code{ext_orig} is \code{TRUE},
#'   two additional columns are added to identify in which intervals taxa
#'   originated and went extinct.
#' @section Developer(s):
#'   William Gearty & Lewis A. Jones
#' @section Reviewer(s):
#'   Lewis A. Jones
#' @export
#' @examples
#' taxdf <- data.frame(name = c("A", "B", "C"),
#'                     max_ma = c(150, 60, 30),
#'                     min_ma = c(110, 20, 0))
#' ex <- tax_expand_time(taxdf)
tax_expand_time <- function(
    taxdf,
    max_ma = "max_ma",
    min_ma = "min_ma",
    bins = NULL,
    scale = "GTS2020",
    rank = "stage",
    ext_orig = TRUE) {
  # Handle errors
  if (is.data.frame(taxdf) == FALSE) {
    stop("`taxdf` should be a dataframe")
  }

  if (!all(c(min_ma, max_ma) %in% colnames(taxdf))) {
    stop("Either `min_ma` or `max_ma` is not a named column in `taxdf`")
  }

  if (!is.numeric(taxdf[, max_ma])) {
    stop("The class of the max_ma column must be numeric.")
  }
  if (!is.numeric(taxdf[, min_ma])) {
    stop("The class of the min_ma column must be numeric.")
  }

  if (any(taxdf[, c(min_ma, max_ma)] < 0)) {
    stop("Maximum and minimum ages must be positive.")
  }

  if (any(taxdf[, max_ma] < taxdf[, min_ma])) {
    stop("Maximum ages must be larger than or equal to minimum ages.")
  }

  if (length(rank) > 1 ||
      !(rank %in% c("stage", "epoch", "period", "era", "eon"))) {
    stop("`rank` must be either: stage, epoch, period, era, or eon")
  }

  if (!is.logical(ext_orig)) {
    stop("`ext_orig` should be logical (TRUE/FALSE)")
  }

  if (is.null(bins) && (is.null(scale) || is.null(rank))) {
    stop("Either `bin` or `scale` and `rank` must be specified.")
  }
  if (!is.null(bins)) {
    if (is.data.frame(bins) == FALSE) {
      stop("`bins` should be a dataframe.")
    }
    if (!all(c("bin", "max_ma", "min_ma") %in% colnames(bins))) {
      stop("bin, max_ma and/or min_ma do not exist in `bins`.")
    }
  } else {
    # get the desired timescale at the desired rank
    bins <- time_bins(rank = rank, scale = scale)
  }

  if (any(duplicated(taxdf))) {
    stop("Not all rows in `taxdf` are unique!")
  }

  # replicate taxon rows for each interval they span
  dat_list <- lapply(seq_len(nrow(bins)), function(i) {
    int_tax <- taxdf[taxdf[, min_ma] < bins$max_ma[i] &
                       taxdf[, max_ma] > bins$min_ma[i], ]
    if (ext_orig) {
      int_tax$ext <- int_tax[, min_ma] >= bins$min_ma[i] &
                       int_tax[, min_ma] > 0
      int_tax$orig <- int_tax[, max_ma] <= bins$max_ma[i]
    }
    if (nrow(int_tax) == 0) return(NULL)
    cbind(int_tax, bins[i, ])
  })
  dat <- do.call(rbind, dat_list)
  rownames(dat) <- NULL
  dat
}
