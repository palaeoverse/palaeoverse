#' Expand taxa to interval-level pseudo-occurrences
#'
#' A function to generate interval-level pseudo-occurrences for taxa based on
#' temporal ranges (e.g., the output of \code{\link{tax_range_time}}). While the
#' resulting pseudo-occurrences should not be treated as equivalent to actual
#' occurrence data (e.g., like that from the Paleobiology Database), such
#' pseudo-occurrences may be useful for performing statistical analyses where
#' the row representing a taxon must be replicated for each interval through
#' which the taxon survived.
#'
#' @param taxdf \code{dataframe}. A \code{data.frame} of taxa (such as that
#'   produced by \code{\link{tax_range_time}}) with columns for the maximum and
#'   minimum ages (FADs and LADs). Each row should represent a unique taxon.
#'   Additional columns may be included (e.g., taxon names, additional taxonomy,
#'   etc) and will be included in the returned \code{data.frame}.
#' @param max_ma \code{character}. The name of the column you wish to be treated
#'   as the maximum limit (FADs) of the age range (e.g., "max_ma").
#' @param min_ma \code{character}. The name of the column you wish to be treated
#'   as the minimum limit (LADs) of the age range (e.g., "min_ma").
#' @param scale \code{character}. Specify the desired geological timescale to
#'   be used, either "GTS2020" or "GTS2012".
#' @param rank \code{character}. Specify the desired stratigraphic rank. Choose
#'   from: "stage", "epoch", "period", "era", and "eon".
#' @param ext_orig \code{logical}. Should two additional columns be added to
#'   identify the intervals in which taxa originated and went extinct?
#'
#' @return A \code{dataframe} where each row represents an interval during which
#'   a taxon in the original user-supplied data was alive. The columns are
#'   identical to those in the user-supplied data with additional columns
#'   included to identify the intervals. If \code{ext_orig} is \code{TRUE},
#'   two additional columns are added to identify in which intervals taxa
#'   originated and went extinct.
#' @section Developer(s):
#'   William Gearty
#' @section Reviewer(s):
#'   TBD
#' @export
#' @examples
#' taxdf <- data.frame(name = c("A", "B", "C"),
#'                     max_ma = c(150, 60, 30),
#'                     min_ma = c(110, 20, 0))
#' tax_time_expand(taxdf)
tax_time_expand <- function(
    taxdf,
    max_ma = "max_ma",
    min_ma = "min_ma",
    scale = "GTS2020",
    rank = "stage",
    ext_orig = TRUE) {
  # Handle errors
  if (is.data.frame(taxdf) == FALSE) {
    stop("`taxdf` should be a dataframe")
  }

  if (!is.numeric(taxdf[, max_ma])) {
    stop("The class of the max_ma column must be numeric.")
  }
  if (!is.numeric(taxdf[, min_ma])) {
    stop("The class of the min_ma column must be numeric.")
  }

  if (!(rank %in% c("stage", "epoch", "period", "era", "eon"))) {
    stop("`rank` must be either: stage, epoch, period, era, or eon")
  }

  if (!any(c(min_ma, max_ma) %in% colnames(taxdf))) {
    stop("Either `min_ma` or `max_ma` is not a named column in `taxdf`")
  }

  if (!is.logical(ext_orig)) {
    stop("`ext_orig` should be logical (TRUE/FALSE)")
  }

  # get the desired timescale at the desired rank
  if (scale == "GTS2020") {
    timescale <- palaeoverse::GTS2020
  }
  else if (scale == "GTS2012") {
    timescale <- palaeoverse::GTS2012
  } else {
    stop('`scale` should be "GTS2020" or "GTS2012"')
  }

  if (any(duplicated(taxdf))) {
    stop('Not all rows in `taxdf` are unique!')
  }

  intervals <- timescale[timescale$rank == rank,]

  # replicate taxon rows for each interval they span
  dat_list <- lapply(1:nrow(intervals), function(i){
    int_tax <- taxdf[taxdf[, min_ma] < intervals$max_ma[i] &
                       taxdf[, max_ma] > intervals$min_ma[i],]
    if (ext_orig) {
      int_tax$ext <- int_tax[, min_ma] >= intervals$min_ma[i] &
                       int_tax[, min_ma] > 0
      int_tax$orig <- int_tax[, max_ma] <= intervals$max_ma[i]
    }
    if (nrow(int_tax) == 0) return(NULL)
    cbind(int_tax, intervals[i,])
  })
  do.call(rbind, dat_list)
}
