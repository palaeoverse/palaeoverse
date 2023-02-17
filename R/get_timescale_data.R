#' Get geological timescale data
#'
#' This function takes a name of a geological timescale and returns data for the
#' timescale. Valid names include those of built-in `data.frames` ([GTS2020()]
#' and [GTS2012()]) and exact matches to those hosted by Macrostrat (see full
#' list here: <https://macrostrat.org/api/defs/timescales?all>). If a built-in
#' `data.frame` is specified with `name`, a specific `rank` may also be
#' specified. Valid ranks include "period", "epoch", "stage", "era", and "eon"
#' as well as partial matches of those names (e.g., "per" or "st"). If a rank
#' is supplied, only the intervals of that rank will be returned from the
#' specified timescale.
#'
#' @param name \code{character}. The name of the desired timescale.
#' @param rank \code{character}. The name of the desired rank (if `name`
#'   specifies a built-in timescale).
#' @return A `data.frame` with at least the following columns:
#'   \item{name}{the names of the time intervals.}
#'   \item{max_age}{the oldest boundaries of the time intervals, in millions of
#'     years.}
#'   \item{min_age}{the youngest boundaries of the time intervals, in millions
#'     of years.}
#'   \item{abbr}{either traditional abbreviations of the names of the time
#'     intervals (if they exist) or custom abbreviations created with R.}
#'   \item{color}{hex color codes associated with the time intervals (if
#'     applicable).}
#' @importFrom utils read.csv
#' @importFrom curl nslookup
#' @importFrom grDevices col2rgb
#' @export
get_timescale_data <- function(name = "GTS2020", rank = NULL) {
  possible_names <- c("GTS2020", "GTS2012")
  name_match <- charmatch(name, possible_names)
  if (!is.na(name_match)) {
    if (name_match == 0) {
      stop("'name' matches both built-in scales. Please be more specific.",
           call. = FALSE
      )
    } else {
      name <- possible_names[name_match]
      if (name == "GTS2020") {
        dat <- palaeoverse::GTS2020
      } else if (name == "GTS2012") {
        dat <- palaeoverse::GTS2012
      }
    }
    if (!is.null(rank)) {
      possible_ranks <- c("period", "epoch", "stage", "era", "eon")
      rank_match <- charmatch(rank, possible_ranks)
      if (is.na(name_match)) {
        stop("Invalid `rank`. Choose either:
             'period', 'epoch', 'stage', 'era', or 'eon'.",
             call. = FALSE)
      }else if (rank_match == 0) {
        stop("'rank' matches multiple scales. Please be more specific.",
             call. = FALSE
        )
      } else {
        dat <- subset(dat, rank == possible_ranks[rank_match])
      }
    }
  } else {
    # try to get the timescale from macrostrat
    # check that we are online and macrostrat is online
    tryCatch(
      {
        nslookup("macrostrat.org")
      },
      error = function(e) {
        stop("Macrostrat is not available. Either the site is down or you are
             not connected to the internet.",
             call. = FALSE
        )
      }
    )
    URL <- url(paste0("https://macrostrat.org/api/v2/defs/intervals",
                      "?format=csv&timescale=", gsub(" ", "%20", name)))
    raw_dat <- tryCatch(
      {
        read.csv(URL, header = TRUE, stringsAsFactors = FALSE)
      },
      error = function(e) {
        stop("'name' does not match a built-in or Macrostrat timescale.",
             call. = FALSE
        )
      }
    )
    clean_dat <- raw_dat[, c("name", "b_age", "t_age", "abbrev", "color")]
    colnames(clean_dat) <- c("interval_name", "max_ma", "min_ma", "abbr", "colour")
    no_abbr <- (is.na(clean_dat$abbr) | clean_dat$abbr == "")
    clean_dat$abbr[no_abbr] <-
      abbreviate(clean_dat$interval_name, minlength = 1,
                 use.classes = FALSE, named = FALSE)[no_abbr]
    dat <- clean_dat
    # add mid_ma, duration_myr, and font columns
    dat$mid_ma <- (dat$max_ma + dat$min_ma) / 2
    dat$duration_myr <- dat$max_ma - dat$min_ma
    # based on https://stackoverflow.com/a/1855903/4660582
    rgbs <- col2rgb(dat$colour)
    dat$font <- ifelse(apply(rgbs, 2, function(x) (0.299 * x[1] + 0.587 * x[2] +
                                                     0.114 * x[3]) / 255) > .5,
                       "black", "white")
  }
  dat
}
