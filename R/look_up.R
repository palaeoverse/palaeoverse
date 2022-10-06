#' Look up geological intervals and assign geological stages
#'
#' A function that uses interval names to assign either
#' [international geological stages](
#' https://stratigraphy.org/ICSchart/ChronostratChart2022-02.pdf),
#' from the International Commission on Stratigraphy (ICS), or
#' user-defined intervals, along with associated numerical ages,
#' to fossil occurrences.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to bin, with columns of class \code{character} specifying the earliest
#' and the latest possible interval associated with each occurrence.
#' @param early_interval \code{character}. Name of the column in `occdf` that
#' contains the earliest interval from which the occurrences are from. Defaults
#'  to `early_interval`.
#' @param late_interval \code{character}. Name of the column in `occdf` that
#' contains the latest interval from which the occurrences are from. Defaults
#'  to `late_interval`.
#' @param int_key \code{dataframe}. A dataframe linking interval names to
#' international geological stage names from the ICS, or other, user-defined
#' intervals.
#' This dataframe should contain the following named columns containing
#' `character` values: \cr
#' \itemize{
#' \item `interval_name` contains the names to be matched from `occdf` \cr
#' \item `early_stage` contains the names of the earliest stages
#' corresponding to the intervals, and \cr
#' \item `late_stage` contains the latest stage corresponding to the
#' intervals \cr
#' }
#' Optionally, named columns provide maximal and minimal ages for the
#' intervals: \cr
#' \itemize{
#' \item `max_ma`
#' \item `min_ma`
#' }
#' Alternatively, this can be set to \code{FALSE} to only assign intervals using
#' one of the GTS tables (see below).
#' @param assign_with_GTS \code{character} or \code{FALSE}. Allows intervals to
#' be searched in the `GTS2020` (default) or the `GTS2012` table. Set to
#' \code{FALSE} to disable.
#' @param print_assigned \code{logical}. Should the assigned interval names be
#' printed? Defaults to \code{FALSE}.
#'
#' @return A \code{dataframe} of the original input `data` with the following
#' appended columns is returned: `early_stage` and `late_stage`, corresponding
#' to the earliest and latest international geological stage which
#' could be assigned to the occurrence based on the given interval names.
#' `interval_max_ma` and `interval_min_ma` return maximal and minimal interval
#' ages if provided in the interval key, or if they can be fetched from GTS2012
#' or GTS2020. A column `interval_mid_ma` is appended to provide the midpoint
#' age of the interval.
#'
#' @details
#' Instead of  geological stages, the user can supply any names in the
#' `early_stage` and `late_stage` column of `int_key`.
#' `assign_with_GTS` should then be set to \code{FALSE}.
#'
#' An exemplary `int_key` has been included within this package
#' (\code{\link{interval_key}}). This key works well for assigning
#' geological stages to many of the intervals from the
#' [Paleobiology Database](https://paleobiodb.org)
#' and the [Paleoreefs Database](https://www.paleo-reefs.pal.uni-erlangen.de/).
#' `palaeoverse` cannot guarantee that all of
#' the stage assignments with the exemplary key are accurate.
#' The table corresponding to this key can be loaded with
#' `palaeoverse::interval_key`.
#'
#'
#' @section Developer(s):
#' Kilian Eichenseer & William Gearty
#' @section Reviewer(s):
#' Lewis A. Jones & Christopher D. Dean
#' @examples
#' # Grab internal tetrapod data
#' occdf <- tetrapods
#' # assign stages using the exemplary interval_key
#' occdf <- look_up(occdf)
#'
#' ## Use own key to assign intervals
#' # create example data
#' occdf <- data.frame(
#'            stage = c("any Permian", "first Permian stage",
#'                      "any Permian", "Roadian"))
#' # create example key
#' interval_key <- data.frame(
#'                   interval_name = c("any Permian", "first Permian stage"),
#'                   early_stage = c("Asselian", "Asselian"),
#'                   late_stage = c("Changhsingian", "Asselian"))
#' # assign stages using the custom interval_key, use "GTS2012":
#' occdf <- look_up(occdf,
#' early_interval = "stage", late_interval = "stage",
#' int_key = interval_key, assign_with_GTS = "GTS2012",
#' print_assigned = TRUE)
#' @export
look_up <- function(occdf, early_interval = "early_interval",
                    late_interval = "late_interval",
                    int_key = palaeoverse::interval_key,
                    assign_with_GTS = "GTS2020",
                    print_assigned = FALSE) {



  #=== Handling errors ===

  if (is.data.frame(occdf) == FALSE) {
    stop('`occdf` should be a dataframe.')
  }

  if (!is.character(early_interval)) {
    stop('`early_interval` needs to be of type `character`')
  }
  if (!early_interval %in% colnames(occdf)) {
      stop('`early_interval` needs to match a column name of `occdf`')
  }

  if (!is.character(late_interval)) {
    stop('`late_interval` needs to be of type `character`')
  }
  if (!late_interval %in% colnames(occdf)) {
    stop('`late_interval` needs to match a column name of `occdf`')
  }

  # int_key checks
  if (is.data.frame(int_key) == FALSE) {
    if(int_key != FALSE) {
      stop('`int_key` should be a dataframe.')
    } else if (!(assign_with_GTS %in% c("GTS2020","GTS2012"))) stop(
      'assignment with GTS needs to be enabled if `int_key` is set to `FALSE`'
    )
  } else {

    if(!(all(c("interval_name", "early_stage", "late_stage") %in%
             colnames(int_key)))) {
      stop('`int_key` needs to contain the columns "interval_name",
           "early_stage" and "late_stage"')
    }

    if(!(is.character(int_key$interval_name) & is.character(int_key$early_stage) &
         is.character(int_key$late_stage))) {
      stop('`int_key$interval_name`, `int_key$early_stage`, and
           `int_key$late_stage` needs to be of type `character`')
    }

    if ("max_ma" %in% colnames(int_key)) {
      if (!is.numeric(int_key$max_ma)) {
        stop('`int_key$max_ma` needs to be of type `numeric`')
      }
    }

    if ("min_ma" %in% colnames(int_key)) {
      if (!is.numeric(int_key$min_ma)) {
        stop('`int_key$min_ma` needs to be of type `numeric`')
      }
    }
  }

  #=== Preparation ===

  # save early and late int columns for easier handling
  early <- occdf[, early_interval]
  late <- occdf[, late_interval]

  # if there are missing values in `late`, fill them in from `early`
  replace_pattern <- c("", " ")
  replace_ind <- which(vapply(late, function(x) {
    x %in% replace_pattern | is.na(x)
  }, logical(1)))
  late[replace_ind] <- early[replace_ind]
  # in this case, display a warning
  if(length(replace_ind) >= 1) {
    warning('`NA`, `""` or `" "` entries from `late_interval` have been
            filled in with the corresponding `early_interval` entries')
  }

  # add columns to output data frame
  occdf$early_stage <- rep(NA_character_, nrow(occdf))
  occdf$late_stage <- rep(NA_character_, nrow(occdf))
  if ("max_ma" %in% colnames(int_key) || is.character(assign_with_GTS)) {
    occdf$interval_max_ma <- rep(NA_real_, nrow(occdf))
    }
  if (("max_ma" %in% colnames(int_key) &
      "min_ma" %in% colnames(int_key)) || is.character(assign_with_GTS)) {
    occdf$interval_mid_ma <- rep(NA_real_, nrow(occdf))
    }
  if ("min_ma" %in% colnames(int_key) || is.character(assign_with_GTS)) {
    occdf$interval_min_ma <- rep(NA_real_, nrow(occdf))
    }

  ## early stages unique entries
  early_unique <- unique(early)

  ## late stages unique entries
  late_unique <- unique(late)

  #=== Assignment of stages based on look-up table ===
  if(is.data.frame(int_key)) {
    # early stage
    # find assignable intervals
    assign_ind1 <- vapply(early_unique, function(x) {
      x %in% int_key$interval_name
    }, FUN.VALUE = logical(1L))
    assign1 <- early_unique[assign_ind1]
    # loop through assignable intervals and assign early stages
    for (i in seq_len(length(assign1))) {
      # early stage
      occdf$early_stage[early == assign1[i]] <-
        int_key$early_stage[int_key$interval_name == assign1[i]]
      # max_ma
      if ("max_ma" %in% colnames(int_key)) {
        occdf$interval_max_ma[early == assign1[i]] <-
          int_key$max_ma[int_key$interval_name == assign1[i]]
      }
    }

    # late stage
    # find assignable intervals
    assign_ind2 <- vapply(late_unique, function(x) {
      x %in% int_key$interval_name
    }, FUN.VALUE = logical(1L))
    assign2 <- late_unique[assign_ind2]
    # loop through assignable intervals and assign late stages
    for (i in seq_len(length(assign2))) {
      #late stage
      occdf$late_stage[late == assign2[i]] <-
        int_key$late_stage[int_key$interval_name == assign2[i]]
      # min_ma
      if ("min_ma" %in% colnames(int_key)) {
        occdf$interval_min_ma[late == assign2[i]] <-
          int_key$min_ma[int_key$interval_name == assign2[i]]
      }
    }

  } else { # set assign indices to FALSE in case there is no int_key
    assign_ind1 <- rep(FALSE,length(early_unique))
    assign_ind2 <- rep(FALSE,length(late_unique))
  }

  #=== Assignment of stages based on GTS2020 ===
  # for intervals that could not be matched using the table, try to assign
  #   stages based on GTS2020

  # Load GTS2020 or GTS2012, or set GTS to NULL
  GTS <- switch(assign_with_GTS,
                "GTS2020" = {
                  palaeoverse::GTS2020
                },
                "GTS2012" = {
                  palaeoverse::GTS2012
                },
                {
                  if (assign_with_GTS == FALSE) {
                    NULL
                  } else {
                    stop('`assign_with_GTS` needs to be `FALSE`, `GTS2012` or
                          `GTS2020`')
                  }
                }
  )

  # correct error in GTS2012 ### REMOVE ONCE FIXED IN palaeoverse::GTS2012
  if (assign_with_GTS == "GTS2012") {
    GTS$min_ma[GTS$interval_name == "Upper Ordovician"] <- 443.4 }
  if (assign_with_GTS == "GTS2012") {
    GTS$min_ma[GTS$interval_name == "Llandovery"] <- 443.4 }

  # implement GTS assignment
  if (!is.null(GTS)) {
    # fetch GTS2020:
    # remove Pridoli once (double entry)
    GTS <- GTS[-which(GTS$interval_name == "Pridoli" & GTS$rank == "epoch"), ]

    # early stages
    early_unique_GTS <- early_unique[assign_ind1 == FALSE]
    assign_ind_GTS <- vapply(early_unique_GTS, function(x) {
      x %in% GTS$interval_name
    }, FUN.VALUE = logical(1L))
    assign_GTS1 <- early_unique_GTS[assign_ind_GTS]

    # take max_ma and assign corresponding stage
    assigned_max_ma_GTS <- vapply(assign_GTS1, function(x) {
      GTS$max_ma[x == GTS$interval_name]
    }, FUN.VALUE = numeric(1))
    # stage
    for (i in seq_len(length(assign_GTS1))) {
      occdf$early_stage[early == assign_GTS1[i] &
                          is.na(occdf$early_stage)] <-
        GTS$interval_name[GTS$max_ma == assigned_max_ma_GTS[i] &
                            GTS$rank == "stage"]

    }

    # late stages
    late_unique_GTS <- late_unique[assign_ind2 == FALSE]
    assign_ind_GTS <- vapply(late_unique_GTS, function(x) {
      x %in% GTS$interval_name
    }, FUN.VALUE = logical(1L))
    assign_GTS2 <- late_unique_GTS[assign_ind_GTS]

    # take min_ma and assign corresponding stage
    assigned_min_ma_GTS <- vapply(assign_GTS2, function(x) {
      GTS$min_ma[x == GTS$interval_name]
    }, FUN.VALUE = numeric(1))
    for (i in seq_len(length(assign_GTS2))) {
      # stage
      occdf$late_stage[late == assign_GTS2[i] &
                         is.na(occdf$late_stage)] <-
        GTS$interval_name[GTS$min_ma == assigned_min_ma_GTS[i] &
                            GTS$rank == "stage"]

    }

    # add max_ma and min_ma based on GTS
    # max_ma
    if ("interval_max_ma" %in% colnames(occdf)) {
      stage_unique <- unique(occdf$early_stage)
      # find assignable intervals
      assign_age_ind <- vapply(stage_unique, function(x) {
        x %in% GTS$interval_name
      }, FUN.VALUE = logical(1L))
      assign_age <- stage_unique[assign_age_ind]
      # get max ages again here as we excluded assigned intervals earlier
      assigned_max_ma_GTS <- vapply(assign_age, function(x) {
        GTS$max_ma[x == GTS$interval_name]
      }, FUN.VALUE = numeric(1))
      # assign max age
      for (i in seq_len(length(assign_age))) {
        occdf$interval_max_ma[occdf$early_stage == assign_age[i] &
                             is.na(occdf$interval_max_ma)] <-
          assigned_max_ma_GTS[i]
      }
    }
    # min_ma
    if ("interval_min_ma" %in% colnames(occdf)) {
      stage_unique <- unique(occdf$late_stage)
      # find assignable intervals
      assign_age_ind <- vapply(stage_unique, function(x) {
        x %in% GTS$interval_name
      }, FUN.VALUE = logical(1L))
      assign_age <- stage_unique[assign_age_ind]
      # get min ages again here as we excluded assigned intervals earlier
      assigned_min_ma_GTS <- vapply(assign_age, function(x) {
        GTS$min_ma[x == GTS$interval_name]
      }, FUN.VALUE = numeric(1))
      # assign min age
      for (i in seq_len(length(assign_age))) {
        occdf$interval_min_ma[occdf$late_stage == assign_age[i] &
                             is.na(occdf$interval_min_ma)] <-
          assigned_min_ma_GTS[i]
      }
    }
  }

  #=== add stage_mid_ma ages to the output ===


  if ("interval_max_ma" %in% colnames(occdf) &&
      "interval_min_ma" %in% colnames(occdf)) {
    occdf$interval_mid_ma <- (occdf$interval_max_ma + occdf$interval_min_ma) / 2
  }

  #=== Ouput ===

  # optional: print assigned stages
  if (print_assigned && is.data.frame(int_key)) {
    message(
      "The following intervals have been matched with stages from int_key:")
    print(unique(c(assign1, assign2)))
  }

  # return output
  occdf

}
