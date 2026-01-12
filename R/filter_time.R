#' Filter fossil occurrences by time
#'
#' @description
#' A function to trim fossil occurrences to within specified stratigraphic
#' bounds and/or degree of precision. Filtering can be performed on
#' occurrences with point or uncertainty-bounded ages using stratigraphic
#' interval names or absolute numeric ages.
#'
#' @param occdf `data.frame`. The fossil occurrences you wish to filter. This
#' must contain minimum and maximum occurrence age columns, or a column of
#' point ages. All must be numeric and cannot contain missing values.
#' @param bins This may be left as `NULL` if filtering by numeric values only.
#' Otherwise, a `data.frame` of time bins used to look up numeric ages for
#' filtering fossil occurrences, such as that returned by [time_bins()]. This
#' must contain at least three columns as named by `max_ma`, `min_ma` and
#' `interval_name`. No column can contain missing values and columns `max_ma`
#' and `min_ma` must additionally be numeric.
#' @param min_ma `character`. The name of the column you wish to be treated as
#' minimum ages in `occdf` and `bins`. `min_ma` in `occdf` can instead be
#' treated as point ages.
#' @param max_ma `character`. The name of the column you wish to be treated as
#' maximum ages in `occdf` and `bins`. `max_ma` in `occdf` can instead be
#' treated as point ages (i.e. the same column name as for `min_ma`).
#' @param interval_name `character` The name of the column in `bins` that
#' contains the interval names.
#' @param min_age The minimum desired occurrence age. This can be either a
#' numeric value or an interval name, which then must be present in column
#' `interval_name` in `bins`.
#' @param max_age The maximum desired occurrence age. This can be either a
#' numeric value or an interval name, which then must be present in column
#' `interval_name` in `bins`.
#' @param overlap `logical`. Occurrences which partially overlap with the set
#' filtering duration are discarded by default, so that the returned data falls
#' strictly within these bounds. Set to `TRUE` to retain overlaps instead.
#' @param uncertainty A numeric value denoting the maximum tolerable temporal
#' uncertainty for retaining occurrences. Alternatively, if this argument is
#' set to "bins", then only occurrences which span no more than `n_bins`
#' intervals in `bins` will be retained.
#' @param n_bins `numeric` The maximum tolerable bin-wise uncertainty above
#' which occurrences will be discarded if `uncertainty = "bins"`.
#' @param retain.taxa `logical`. Complete loss of some taxa may occur if all
#' their occurrences exceed the temporal uncertainty criterion. If this is
#' undesirable, occurrences for these taxa will be retained as exceptions to
#' the this filtering criterion.
#' @param taxa `character`. The name of the column you wish to be treated as
#' taxon names in `occdf` if `retain.taxa = TRUE`.
#' @param verbose `logical`. Sets if the impact of time filtering is reported
#' to the console or not.
#'
#' @returns
#' The input `data.frame`, `occdf`, minus any occurrences discarded under the
#' chosen trimming and filtering parameters. If `retain.taxa = TRUE`, there
#' will be an additional column called `retained`, consisting of `0` for
#' occurrences which met all filtering criteria and `1` for occurrences which
#' were retained for taxonomic completeness. Depending on the filtering
#' parameters used, all or no occurrences may be discarded.
#'
#' @details
#' When filtering by stratigraphic bounds, minimum or maximum ages can be used
#' individually, or at the same time. By default occurrences which do not fall
#' fully within the bounds set by `min_age` and/or `max_age` are discarded.
#' This behaviour can be toggled by setting `strict = FALSE` to retain
#' occurrences which partially overlap with the bounds set. If using
#' stratigraphic interval names, `min_age` is internally set to its interval
#' upper boundary age and `max_age` to its interval lower boundary age using
#' the interval durations in `bins`. Otherwise, the numeric minimum and maximum
#' values are used directly.
#'
#' When filtering by stratigraphic precision, if `uncertainty = "bins"`, the
#' function uses [bin_time()] to assign occurrences to the stratigraphic
#' intervals present in `bins` and discards any which span more than one
#' interval, a common procedure in many computational palaeobiology workflows.
#' Alternatively, an absolute stratigraphic duration (e.g., `uncertainty = 12`)
#' can be used to discard occurrences with age uncertainties greater than this
#' duration. Filtering by temporal precision has no effect for occurrences with
#' point ages.
#'
#' It is crucial that the occurrence ages in your data and your trimming and
#' filtering parameters conform to the same stratigraphic timescale (e.g., GTS
#' 2020) to avoid incorrect exclusion of occurrences:
#'
#' * If interval boundary ages or absolute ages do not align precisely with
#' occurrence ages, this could result in substantial data loss towards the
#' stratigraphic upper and lower limits of the data during trimming, or during
#' filtering to single interval occurrences.
#'
#' * Similarly, filtering with an absolute value for temporal precision could
#' lead to loss of occurrences for entire stratigraphic intervals if that
#' value is shorter than the stratigraphic durations of those intervals.
#'
#' * Filtering to single interval occurrences may be preferable as this
#' accommodates variation in stratigraphic interval length through geological
#' time. If you choose to use an absolute duration, it may be advisable to take
#' a value no shorter than the longest interval duration present in your data
#' (e.g., the longest stage).
#'
#' Finally, the function can retain occurrences (`retain.taxa = TRUE`)
#' initially discarded when filtering by temporal uncertainty to ensure that
#' the taxonomic composition of data set within temporal bounds is unchanged,
#' whilst still reducing temporal uncertainty elsewhere in the data:
#'
#' * Taxonomic retention is achieved by checking for any taxa that are lost
#' completely when filtering by temporal uncertainty, then reintroducing all
#' occurrences for those taxa. These occurrences are marked in the "retained"
#' column appended to the returned data set with values of 1.
#'
#' * Taxa lost during trimming to a particular stratigraphic duration are
#' not reintroduced in this way, nor are occurrences with missing entries in
#' the column specified by `taxa` (i.e., those with `NA` values).
#'
#' * This procedure may be convenient for downstream analyses where the taxa
#' present in a occurrence data set must match those in a different one (e.g.,
#' a phylogeny), but a user should carefully consider whether it is necessary
#' or reasonable to use poorer quality data analytically.
#'
#' @section Developer(s): Joseph T. Flannery-Sutherland
#' @section Reviewer(s): William Gearty, Bethany Allen, Lewis Jones
#'
#' @examples
#' # get internal tetrapod data and GTS2020 stages
#' occdf <- tetrapods[1:100, ]
#' stages <- time_bins(rank = "stage")
#'
#' # trim to Kungurian occurrences
#' trimmed1 <- filter_time(occdf, min_age = 272.3, max_age = 279.3)
#'
#' # trim occurrences older than the Kungurian
#' trimmed2 <- filter_time(occdf, max_age = 279.3)
#'
#' # trim occurrences younger than the Roadian
#' trimmed3 <- filter_time(occdf, bins = stages, min_age = "Roadian")
#'
#' # trim to occurrences within the stages of the Permian
#' trimmed4 <- filter_time(occdf, bins = stages,
#'                         min_age = "Wuchiapingian", max_age = "Asselian")
#'
#' # trim to only occurrences which fall into single stages
#' trimmed5 <- filter_time(occdf, bins = stages, uncertainty = "bins", n_bins = 1)
#'
#' # trim to only Permian occurrences with age uncertainties less than or equal to 15 million years
#' trimmed6 <- filter_time(occdf, bins = stages,
#'                         min_age = "Wuchiapingian", max_age = "Asselian",
#'                         uncertainty = 15)
#'
#' # trim to occurrences within the stages of the Permian, and filter to those
#' # with age uncertainties less than or equal to 15 million years, but allow
#' # some less precise occurrences to remain to prevent loss of any taxa.
#' trimmed7 <- filter_time(occdf, bins = stages,
#'                         min_age = "Wuchiapingian", max_age = "Asselian",
#'                         uncertainty = 20, retain.taxa = TRUE, taxa = "genus")
#'
#' @export

filter_time <- function(occdf, bins = NULL,
                        min_ma = "min_ma", max_ma = "max_ma", interval_name = "interval_name",
                        min_age = NULL, max_age = NULL, overlap = FALSE,
                        uncertainty = NULL, n_bins = 1,
                        retain.taxa = FALSE, taxa = NULL, verbose = T) {

  # occdf <- data.frame(genus   = c("A", "A", "B", "B", "B", "C", "D", "E", "E", "F"),
  #                     max_ma  = c(5,   5,   5,   4,   4,   4,   4,   3,   2,   2),
  #                     min_ma  = c(4,   4,   4,   3,   3,   2,   2,   1,   0,   1),
  #                     with_na = c(5, NA,   5,   4,   4,   4,   4,   3,   2,   2),
  #                     with_ch = letters[6:15])
  # bins <- data.frame(bin = 1:5,
  #                    interval_name = letters[1:5],
  #                    max_ma = 5:1,
  #                    min_ma = 4:0,
  #                    with_ch = letters[6:10])
  # # occdf <- tetrapods[1:100, ]
  # # bins = time_bins(rank = "stage")
  # min_ma = "min_ma"
  # max_ma = "max_ma"
  # interval_name = "interval_name"
  # min_age = 0
  # max_age = 4
  # overlap = F
  # uncertainty = "bins"
  # n_bins = 2
  # retain.taxa = FALSE
  # taxa = NULL
  # verbose = T

  # check occurrence data and copy for filtering
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a data.frame.")
  }

  # check arguments
  if (any(c(!is.atomic(min_ma), length(min_ma) != 1, !min_ma %in% colnames(occdf),
            !is.atomic(max_ma), length(max_ma) != 1, !max_ma %in% colnames(occdf)))) {
    stop("Please specify `min_ma` and `max_ma` as column names in `occdf`.")
  }
  if (!is.null(max_ma)) {
    if (any(c(!is.atomic(min_ma), length(min_ma) != 1, !min_ma %in% colnames(occdf)))) {
      stop("If not NULL, please specify `max_ma` as a column name in `occdf`.")
    }
  }
  if (any(is.na(occdf[, min_ma])) || any(is.na(occdf[, max_ma]))) {
    stop(paste0("NA values detected in ", min_ma, " or ", max_ma, " in `occdf`"))
  }
  if (!is.numeric(occdf[, min_ma]) || !is.numeric(occdf[, max_ma])) {
    stop("Columns `min_ma` and `max_ma` in `occdf` should contain numeric values")
  }

  # check filtering functionality to prevent downstream errors
  if (all(is.null(min_age), is.null(max_age), is.null(uncertainty))) {
    stop("No filtering parameters have been set.")
  }
  if(!is.logical(overlap) || !length(overlap) == 1) {
    stop("`overlap` should be logical")
  }

  # check bin data, if required
  if (is.character(min_age) | is.character(max_age) | is.character(uncertainty)) {
    if (is.data.frame(bins) == FALSE) {
      stop("`bins` should be a dataframe.")
    }
    if (!all(c(interval_name, max_ma, min_ma) %in% colnames(bins))) {
      stop(paste0("Either: ", interval_name, ", ", max_ma, " or ", min_ma, " column(s) do not exist in `bins`."))
    }
    if (any(is.na(bins))) {
      stop(paste0("NA values detected in `bins`."))
    }
    if (!is.numeric(bins[, min_ma]) || !is.numeric(bins[, max_ma])) {
      stop("Columns `min_ma` and `max_ma` in `bins` should contain numeric values.")
    }
    if (!any(c(bins[, max_ma], bins[, min_ma]) %in% c(occdf[, max_ma], occdf[, min_ma]))) {
      warning("No shared ages between `bins` and `occdf`. Check the time scale and range of your data.")
    }
  }

  # apply range filtering, if specified
  nrw <- nrow(occdf)
  if (is.character(min_age)) {
    if (!min_age %in% bins[, interval_name]) {
      stop("`min_age` is not an interval name in `bins`.")
    }
    min_age <- bins[which(bins[, interval_name] == min_age), min_ma]
  }
  if (is.character(max_age)) {
    if (!max_age %in% bins[, interval_name]) {
      stop("`max_age` is not an interval name in `bins`.")
    }
    max_age <- bins[which(bins[, interval_name] == max_age), max_ma]
  }
  if (is.numeric(min_age)) {
    occdf <- occdf[which(occdf[,max_ma] > min_age),]
    if (!overlap) {
      occdf <- occdf[which(occdf[,min_ma] >= min_age),]
    }
  }
  if (is.numeric(max_age)) {
    occdf <- occdf[which(occdf[,min_ma] < max_age),]
    if (!overlap) {
      occdf <- occdf[which(occdf[,max_ma] <= max_age),]
    }
  }
  if (all(!is.null(min_age), !is.null(max_age))) {
    if (min_age >= max_age) {
      stop("The minimum filtering age is not younger than than the maximum filtering age.")
    }
  }
  if(is.numeric(min_age) | is.numeric(max_age)) {
    if (verbose) {
      cat(abs(nrow(occdf) - nrw), "occurrences outside than the filtering bounds have been dropped.\n")
    }
  }

  # apply uncertainty filtering, if specified
  nrw <- nrow(occdf)
  occdf2 <- occdf
  if (!is.null(uncertainty)) {
    if (!is.atomic(uncertainty) || length(uncertainty) != 1) {
      stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
    }
    if (uncertainty != "bins" && !is.numeric(uncertainty)) {
      stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
    }

    if(uncertainty == "bins") {
      if (!is.numeric(n_bins) | !is.atomic(n_bins) | length(n_bins) != 1) {
        stop("`n_bins` should be a single positive integer value.")
      }
      if (n_bins %% 1 != 0 | n_bins <= 0) {
        stop("`n_bins` should be a single positive integer value.")
      }
      occdf2 <- palaeoverse::bin_time(occdf = occdf2, min_ma = min_ma, max_ma = max_ma, bins = bins, method = "all")
      occdf2 <- occdf2[!duplicated(occdf2$id),]
      occdf2 <- occdf2[which(occdf2$n_bins <= n_bins), colnames(occdf)]

    # filter by a single absolute stratigraphic duration
    } else {
      if (uncertainty <= 0) {
        stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
      }
      rng <- occdf2[, max_ma] - occdf2[, min_ma]
      occdf2 <- occdf2[which(rng <= uncertainty),]
    }

    if (verbose) {
      cat(abs(nrw - nrow(occdf2)), "occurrences above uncertainty threshold have been dropped.\n")
    }
  }

  # restore taxa, if specified
  if (retain.taxa) {

    if (is.null(taxa)) {
      stop("If `retain.taxa == TRUE`, then `taxa` must be also be supplied.")
    }
    if (any(c(!is.atomic(taxa), length(taxa) != 1, !taxa %in% colnames(occdf)))) {
      stop("Please specify `taxa` as a column name in `occdf`.")
    }
    if ("retained" %in% colnames(occdf2)) {
      stop("'retained' is already a column name in `occdf`. Please change this.")
    }

    # identify taxa lost from the dataset during uncertainty trimming (but post range trimming)
    retain <- na.omit(setdiff((occdf[, taxa]), occdf2[, taxa]))

    # retrieve occurrences for those taxa
    occdf$retained <- 1
    occdf2$retained <- 0
    retain <- subset(occdf, occdf[, taxa] %in% retain)
    occdf2 <- rbind.data.frame(occdf2, retain)

    if (verbose) {
      cat(sum(occdf2$retained), "occurrences for dropped taxa have been restored.\n")
    }
  }

  if(verbose) {
    cat(nrow(occdf2), "occurrences returned under the set filtering parameters.\n")
  }

  # return filtered data
  rownames(occdf2) <- NULL
  return(occdf2)
}
