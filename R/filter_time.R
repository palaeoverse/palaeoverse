#' Filter fossil occurrences by time
#'
#' @description
#' A function to trim fossil occurrences to within a specified temporal
#' duration and/or level of precision. Filtering can be performed using
#' stratigraphic interval names or absolute ages.
#'
#' @param occdf `data.frame`. A `data.frame` of the fossil occurrences you wish
#' to filter. This `data.frame` must contain at least two columns, maximum
#' occurrence age and minimum occurrence age (see `max_ma`, `min_ma`), both of
#' which should be numeric.
#' @param min_ma `character`. The name of the column you wish to be treated as
#' the minimum ages in `occdf` and `bins` (defaults to "min_ma").
#' @param max_ma `character`. The name of the column you wish to be treated as
#' the maximum ages in `occdf` and `bins` (defaults to "max_ma").
#' @param bins `data.frame`. A `data.frame` of time bins used to look up
#' absolute stratigraphic ages for filtering fossil occurrences, such as that
#' returned by [time_bins()]. This `data.frame` must contain at least
#' three columns as named by `interval_name` (defaults to "interval_name"),
#' `max_ma` (defaults to "max_ma") and `min_ma` (defaults to "min_ma"). Columns
#' `max_ma` and `min_ma` must be numeric.
#' @param interval_name `character` The name of the column in `bins` that
#' contains the interval names.
#' @param youngest_bin `character`. The name of the youngest stratigraphic
#' interval to trim to. Only one interval name should be supplied, which should
#' be present in column `interval_name` in `bins`.
#' @param oldest_bin `character`. The name of the oldest stratigraphic interval
#' to trim to. Only one interval name should be supplied, which should be
#' present in column `interval_name` in `bins`.
#' @param min_age `numeric`. The minimum absolute occurrence age to trim to.
#' Ignored if `youngest_bin` is supplied.
#' @param max_age `numeric`. The maximum absolute occurrence age to trim to.
#' Ignored if `oldest_bin` is supplied.
#' @param uncertainty A numeric value denoting the maximum tolerable temporal
#' uncertainty for retaining occurrences. Alternatively, if this argument is
#' set to "bins", then only occurrences which span no more than `n_bins`
#' intervals in `bins` will be retained.
#' @param n_bins `numeric` The maximum tolerable bin-wise uncertainty above
#' which occurrences will be discarded if `uncertainty = "bins"` (defaults
#' to `1`).
#' @param retain.taxa `logical`. Complete loss of some taxa may occur if all
#' their occurrences exceed the temporal uncertainty criterion set by the user.
#' If this is undesirable, occurrences for these taxa will be retained as
#' exceptions to the filtering parameters (defaults to `FALSE`).
#' @param taxa `character`. The name of the column in `occdf` you wish to be
#' treated as the taxon names in `occdf` if `retain.taxa = TRUE` (defaults to
#' `NULL`).
#'
#' @returns
#' The `data.frame` containing the original input `occdf`, minus any
#' occurrences discarded under the chosen trimming and filtering parameters. If
#' `retain.taxa = TRUE`, there will be an additional column called `retained`,
#' consisting of `0` for occurrences which met the filtering criteria and `1`
#' for occurrences which were retained for taxonomic completeness. Depending on
#' the filtering parameters used, it is possible that all or no occurrences
#' will be discarded, resulting in an empty `data.frame` or an unchanged data
#' set respectively.
#'
#' @details
#' When trimming by stratigraphic interval names, the boundary ages of
#' `youngest_bin` and `oldest_bin` are taken from their entries in `bins`.
#' Occurrences with maximum ages younger than the upper boundary age of
#' `youngest_bin` or minimum ages older than the lower boundary age of
#' `oldest_bin` will be trimmed.
#'
#' When filtering by temporal precision, if `uncertainty = "bins"`, the
#' function uses [bin_time()] to assign occurrences to the
#' stratigraphic intervals present in `bins` and discards any which span more
#' than one interval, a common procedure in many computational palaeobiology
#' workflows.
#'
#' Alternatively, trimming can be performed with absolute minimum and maximum
#' age ranges specified in `min_age` and `max_age`. Occurrences are then
#' discarded as for interval-derived upper and lower boundary ages. Similarly,
#' an absolute stratigraphic duration (e.g., `uncertainty = 12`) can be used to
#' filter to occurrences with age uncertainties lower than or equal to this
#' duration.
#'
#' It is worth keeping in mind that occurrences which might overlap with your
#' specified time range but with either a minimum or maximum age outside of
#' this range will be excluded. Similarly, it is crucial that the
#' occurrence ages in your data and your trimming and filtering parameters
#' conform to the same stratigraphic timescale (e.g., GTS 2020) to avoid
#' incorrect exclusion of occurrences:
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
#' the taxonomic composition of the total data set is unchanged, whilst still
#' reducing temporal uncertainty elsewhere in the data:
#'
#' * Taxonomic retention is achieved by checking for any taxa that are lost
#' completely when filtering by temporal uncertainty, then reintroducing all
#' occurrences for those taxa. Such occurrences are marked within a column
#' appended to the returned data set.
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
#' @section Reviewer(s): William Gearty
#' @examples
#' # get internal tetrapod data and GTS2020 stages
#' occdf <- tetrapods[1:100, ]
#' stages <- time_bins(rank = "stage")
#'
#' # trim to Kungurian occurrences
#' filtered1 <- filter_time(occdf, min_age = 272.3, max_age = 279.3)
#'
#' # trim to occurrences within the stages of the Permian
#' filtered2 <- filter_time(occdf, bins = stages,
#'                          youngest_bin = "Wuchiapingian", oldest_bin = "Asselian")
#'
#' # filter to only occurrences which fall into single stages
#' filtered3 <- filter_time(occdf, bins = stages, uncertainty = "bins", n_bins = 1)
#'
#' # filter to only Permian occurrences with age uncertainties less than or equal to 15 million years
#' filtered4 <- filter_time(occdf, bins = stages,
#'                          youngest_bin = "Wuchiapingian", oldest_bin = "Asselian",
#'                          uncertainty = 15)
#'
#' # trim to occurrences within the stages of the Permian, and filter to those
#' # with age uncertainties less than or equal to 15 million years, but allow
#' # some less precise occurrences to remain to prevent loss of any taxa.
#' filtered5 <- filter_time(occdf, bins = stages,
#'                          youngest_bin = "Wuchiapingian", oldest_bin = "Asselian",
#'                          uncertainty = 20, retain.taxa = TRUE, taxa = "genus")
#'
#' @export

filter_time <- function(occdf, min_ma = "min_ma", max_ma = "max_ma",
                        bins = NULL, interval_name = "interval_name",
                        youngest_bin = NULL, oldest_bin = NULL, min_age = NULL, max_age = NULL,
                        uncertainty = NULL, n_bins = 1,
                        retain.taxa = FALSE, taxa = NULL) {

  # check occurrence data
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a data.frame.")
  }
  if (any(c(!is.atomic(min_ma), length(min_ma) != 1, !min_ma %in% colnames(occdf),
            !is.atomic(max_ma), length(max_ma) != 1, !max_ma %in% colnames(occdf)))) {
    stop("Please specify `min_ma` and `max_ma` as column names in `occdf`.")
  }
  if (any(is.na(occdf[, min_ma])) || any(is.na(occdf[, max_ma]))) {
    stop(paste0("NA values detected in ", min_ma, " or ", max_ma, " in `occdf`"))
  }
  if (!is.numeric(occdf[, min_ma]) || !is.numeric(occdf[, max_ma])) {
    stop("Columns `min_ma` and `max_ma` in `occdf` should contain numeric values")
  }

  # check filtering functionality to prevent downstream errors
  if(all(is.null(youngest_bin), is.null(oldest_bin), is.null(min_age), is.null(max_age), is.null(uncertainty))) {
    stop("No filtering parameters have been set.")
  }

  # check bin data, if required
  if(!is.null(youngest_bin) || is.character(uncertainty)) {
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
    if(!any(c(bins[, max_ma], bins[, min_ma]) %in% c(occdf[, max_ma], occdf[, min_ma]))) {
      warning("No shared ages between `bins` and `occdf`. Check the time scale and range of your data.")
    }
  }

  # shallow copy for subsequent filtering
  occdf2 <- occdf

  # apply range filtering, if specified
  if (sum(c(is.null(youngest_bin), is.null(oldest_bin))) == 1) {
    stop("If `youngest_bin` or `oldest_bin` is supplied, then the other must also be supplied.")
  }
  if (sum(c(is.null(youngest_bin), is.null(oldest_bin))) != 2) {
    if (any(c(!is.atomic(youngest_bin), length(youngest_bin) != 1, !youngest_bin %in% bins[, interval_name],
              !is.atomic(oldest_bin), length(oldest_bin) != 1, !oldest_bin %in% bins[, interval_name]))) {
      stop("One or both of `youngest_bin` and `oldest_bin` are not interval names in `bins`.")
    }
    min_age <- bins[which(bins[, interval_name] == youngest_bin), min_ma]
    max_age <- bins[which(bins[, interval_name] == oldest_bin), max_ma]
  }
  if (sum(c(is.null(min_age), is.null(max_age))) == 1) {
    stop("If `min_age` or `max_age` is supplied, then the other must also be supplied.")
  }
  if (sum(c(is.null(min_age), is.null(max_age))) == 0) {
    if (min_age >= max_age) {
      stop("The minimum filtering age/interval is older than the maximum filtering age/interval.")
    }
    occdf2 <- occdf2[which(occdf2[,max_ma] <= max_age),]
    occdf2 <- occdf2[which(occdf2[,min_ma] >= min_age),]

  }

  # apply uncertainty filtering, if specified
  if (!is.null(uncertainty)) {
    if (!is.atomic(uncertainty) || length(uncertainty) != 1) {
      stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
    }
    if (uncertainty != "bins" && !is.numeric(uncertainty)) {
      stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
    }

    if(uncertainty == "bins") {
      if(!is.numeric(n_bins) | !is.atomic(n_bins) | length(n_bins) != 1) {
        stop("`n_bins` should be a single positive integer value.")
      }
      if(n_bins %% 1 != 0 | n_bins <= 0) {
        stop("`n_bins` should be a single positive integer value.")
      }
      occdf2 <- palaeoverse::bin_time(occdf = occdf2, min_ma = min_ma, max_ma = max_ma, bins = bins, method = "all")
      occdf2 <- occdf2[which(occdf2$n_bins == n_bins), colnames(occdf)]

    # filter by a single absolute stratigraphic duration
    } else {
      if(uncertainty <= 0) {
        stop("If not NULL, then `uncertainty` should be set as 'bins' or a single positive numeric value.")
      }
      rng <- occdf2[, max_ma] - occdf2[, min_ma]
      occdf2 <- occdf2[which(rng <= uncertainty),]
    }
  }

  # restore taxa, if specified
  if (retain.taxa) {

    if(is.null(taxa)) {
      stop("If `retain.taxa == TRUE`, then `taxa` must be also be supplied.")
    }
    if (any(c(!is.atomic(taxa), length(taxa) != 1, !taxa %in% colnames(occdf)))) {
      stop("Please specify `taxa` as a column name in `occdf`.")
    }
    if("retained" %in% colnames(occdf2)) {
      stop("'retained' is already a column name in `occdf`. Please change this.")
    }

    # identify taxa lost from the dataset
    retain <- na.omit(setdiff((occdf[, taxa]), occdf2[, taxa]))

    # retrieve occurrences for those taxa
    retain <- subset(occdf, occdf[, taxa] %in% retain)

    # label these occurrences and append to the output data.frame
    retained <- c(rep(0, nrow(occdf2)), rep(1, nrow(retain)))
    occdf2 <- rbind.data.frame(occdf2, retain)
    occdf2$retained <- retained
  }

  # return filtered data
  rownames(occdf2) <- NULL
  return(occdf2)
}
