#' Look up geological intervals and assign ICS ages and stages
#'
#' A function to assign fossil occurrences to international geological stages
#' or user-defined intervals based on interval names.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to bin. The following named columns need to be contained:
#' `early_interval` and, optionally, `late_interval`. These columns need
#' to be `character` values. If no `late_interval` is supplied, only
#' `early_interval` is used, and it is assumed that the occurrences are from
#' that interval only.
#' @param interval_key \code{dataframe}. A dataframe linking interval names to
#' international, geological stage names, or other, user-defined intervals.
#' This dataframe should contain the following named columns containing
#' `character` values:
#' `interval_name` contains the names to be matched from `occdf`,
#' `early_stage` contains the names of the earliest or only stages corresponding
#' to the interval, and, optionally,
#' `late_stage` contains the latest stage corresponding to the
#' interval.
#' Optionally, `numeric` columns `stage_max_ma` and `min_ma` can provide maximal
#' and minimal ages for the intervals.
#' @param assign_with_GTS \code{character} or \code{FALSE}. Allows intervals to
#' be searched in the `GTS2020` (default) or the `GTS2012` table. Set to
#' \code{FALSE} to disable.
#' @param early_interval \code{character}. Alternative column name that contains
#' the earliest or only interval from which the occurrences are from.
#' @param late_interval \code{character}. Alternative column name that contains
#' the latest interval from which the occurrences are from.
#' @param print_assigned \code{logical}. Should the assigned interval names be
#' printed?
#' Defaults to \code{FALSE}.
#'
#' @return A \code{dataframe} of the original input `data` with the following
#' appended columns is returned: `early_stage` and `late_stage`, corresponding
#' to the earliest and latest international geological stage which
#' could be assigned to the occurrence based on the given interval names. If
#' provided in the `interval_key`, `stage_max_ma` and `min_ma` return maximal
#' and minimal ages
#' for the intervals, and a column `mid_ma` is appended to provide the midpoint
#' age of the interval.
#'
#' @details
#' Instead of  geological stages, the user can supply any names in the
#' `early_stage` and `late_stage` column, in which case `assign_with_GTS` should
#' be set to \code{FALSE}.
#'
#' An exemplary `interval_key` can be used by setting
#' `interval_key = "example"`. This key works well for assigning
#' geological stages to many of the
#' intervals from the Paleobiology Database and the Paleoreefs Database.
#' Palaeoverse can provide no guaranty that all of the assignments with the
#' exemplary key are accurate.
#'
#'
#' @section Developer(s):
#' Kilian Eichenseer & ...
#' @section Reviewer(s):
#' ...
#' @examples
#' \dontrun{
#' #Grab internal tetrapod data
#' occdf <- palaeoverse::tetrapods
#' occdf <- look_up(occdf,interval_key="example")
#' }
#' @export
look_up <- function(occdf, interval_key, assign_with_GTS = "GTS2020",
                    early_interval = NULL, late_interval = NULL,
                    print_assigned = FALSE) {



#=== Handling errors ===

  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }

  if (is.null(early_interval) & !("early_interval" %in% colnames(occdf))) {
    stop("`occdf` needs to have a column named `early_interval`, or an
         alternative name for the early interval column needs to be provided.")
  }

  if(!is.null(early_interval)) {
    if(!is.character(early_interval))
      stop("`early_interval` needs to be of type `character`")
   if(!early_interval %in% colnames(occdf))
     stop("`early_interval` needs to match a column name of `occdf`")
  }

  if(!is.null(late_interval)) {
    if(!is.character(late_interval))
      stop("`late_interval` needs to be of type `character`")
    if(!late_interval %in% colnames(occdf))
      stop("`late_interval` needs to match a column name of `occdf`")
  }


  #=== Fetch example interval_key if specified ===
  if(interval_key == "example") {
    #load look-up table from Palaeoverse Onedrive
    id <- "16OWHzbcUyWICDkGafZZ-pvaWDU5mzqDJ"
    interval_key <- read.csv(
      sprintf("https://docs.google.com/uc?id=%s&export=download", id))
    # rename table to follow function conventions
    colnames(interval_key)[2] <- "interval_name"
    colnames(interval_key)[5] <- "early_stage"
    colnames(interval_key)[6] <- "late_stage"
  }

  if (is.data.frame(interval_key) == FALSE) {
    stop("`interval_key` should be a dataframe.")
  }

  if(!("interval_name" %in% colnames(interval_key))) {
    stop("`interval_key` needs to contain a column `interval_name`")
  } else if(!is.character(interval_key$interval_name)) {
    stop("`interval_key$interval_name` needs to be of type `character`")
  }

  if(!("early_stage" %in% colnames(interval_key))) {
    stop("`interval_key` needs to contain a column `early_stage`")
  } else if(!is.character(interval_key$early_stage)) {
    stop("`interval_key$early_stage` needs to be of type `character`")
  }

  if("late_stage" %in% colnames(interval_key)) {
    if(!is.character(interval_key$late_stage)) {
      stop("`interval_key$late_stage` needs to be of type `character`")
    }
  }

  if("stage_max_ma" %in% colnames(interval_key)) {
    if(!is.character(interval_key$stage_max_ma)) {
      stop("`interval_key$stage_max_ma` needs to be of type `numeric`")
    }
  }

  if("min_ma" %in% colnames(interval_key)) {
    if(!is.character(interval_key$min_ma)) {
      stop("`interval_key$min_ma` needs to be of type `numeric`")
    }
  }

  #=== Preparation ===

  # save early and late int columns for easier handling
  if(is.null(early_interval)) early <- occdf$early_interval else early <-
      occdf[,early_interval]

  if(is.null(late_interval) & "late_interval" %in% colnames(occdf)) late <-
      occdf$late_interval else { if(!is.null(late_interval)) {
        late <- occdf[,late_interval]}
        else late <- early } # if there is no `late` column, insert `early`

  # if there are missing values in `late`, fill them in from `early`
  replace_pattern <- c("", " ")
  replace_ind <- which(vapply(late,function(x)
    x %in% replace_pattern | is.na(x), logical(1)))
  late[replace_ind] <- early[replace_ind]

  # add columns to output data frame
  occdf$early_stage <- rep(NA_character_,nrow(occdf))
  occdf$late_stage <- rep(NA_character_, nrow(occdf))
  if("stage_max_ma" %in% colnames(interval_key) | is.character(assign_with_GTS))
    occdf$stage_max_ma <- rep(NA_real_,nrow(occdf))
  if("stage_min_ma" %in% colnames(interval_key) | is.character(assign_with_GTS))
    occdf$stage_min_ma <- rep(NA_real_,nrow(occdf))


  #=== Assignment of stages based on look-up table ===

  ## early stage
  early_unique <- unique(early)
  # find assignable intervals
  assign_ind1 <- vapply(early_unique, function(x) x %in%
                          interval_key$interval_name,
                        FUN.VALUE = logical(1L))
  assign1 <- early_unique[assign_ind1]
  # loop through assignable intervals and assign early stages
  for(i in seq_len(length(assign1))){
    # early stage
    occdf$early_stage[early==assign1[i]] <-
      interval_key$early_stage[interval_key$interval_name==assign1[i]]
    # max_ma
    if("stage_max_ma" %in% colnames(interval_key)){
      occdf$stage_max_ma[early==assign1[i]] <-
        interval_key$stage_max_ma[interval_key$interval_name==assign1[i]]
    }
  }

  ## late stage
  late_unique <- unique(late)
  # find assignable intervals
  assign_ind2 <- vapply(late_unique, function(x) x %in%
                          interval_key$interval_name,
                        FUN.VALUE = logical(1L))
  assign2 <- late_unique[assign_ind2]
  # loop through assignable intervals and assign late stages
  for(i in seq_len(length(assign2))){
    #late stage
    occdf$late_stage[late==assign2[i]] <-
      interval_key$late_stage[interval_key$interval_name==assign2[i]]
    # min_ma
    if("stage_min_ma" %in% colnames(interval_key)){
      occdf$stage_min_ma[late==assign2[i]] <-
        interval_key$stage_min_ma[interval_key$interval_name==assign2[i]]
    }
  }

  #=== Assignment of stages based on GTS2020 ===
  # for intervals that could not be matched using the table, try to assign
  #   stages based on GTS2020

  # Load GTS2020 or GTS2012, or set GTS to NULL
  GTS <- switch(assign_with_GTS,
                "GTS2020" = {palaeoverse::GTS2020},
                "GTS2012" = {palaeoverse::GTS2012},
                { if(assign_with_GTS==FALSE) {NULL
                } else {stop(
              "`assign_with_GTS` needs to be `FALSE`, `GTS2012` or `GTS2020`")}
                }
  )

  # correct error in GTS2012 ### REMOVE ONCE FIXED IN palaeoverse::GTS2012
  if(assign_with_GTS == "GTS2012") GTS$min_ma[GTS$interval_name=="Upper Ordovician"] <- 443.4
  if(assign_with_GTS == "GTS2012") GTS$min_ma[GTS$interval_name=="Llandovery"] <- 443.4

  # implement GTS assignment
  if(!is.null(GTS)) {
    # fetch GTS2020:
    # remove Pridoli once (double entry)
    GTS <- GTS[-which(GTS$interval_name=="Pridoli" & GTS$rank=="epoch"),]

    # early stages
    early_unique_GTS <- early_unique[assign_ind1==FALSE]
    assign_ind_GTS <- vapply(early_unique_GTS, function(x) x %in%
                               GTS$interval_name,
                             FUN.VALUE = logical(1L))
    assign_GTS <- early_unique_GTS[assign_ind_GTS]

    # take max_ma and assign corresponding stage
    assigned_max_ma_GTS <- vapply(assign_GTS, function(x)
      GTS$max_ma[x==GTS$interval_name], FUN.VALUE = numeric(1))
    # stage
    for(i in seq_len(length(assign_GTS))){
      occdf$early_stage[early==assign_GTS[i] &
                          is.na(occdf$early_stage)] <-
        GTS$interval_name[GTS$max_ma==assigned_max_ma_GTS[i] &
                            GTS$rank=="stage"]

    }

    # late stages
    late_unique_GTS <- late_unique[assign_ind2==FALSE]
    assign_ind_GTS <- vapply(late_unique_GTS, function(x) x %in%
                               GTS$interval_name,
                             FUN.VALUE = logical(1L))
    assign_GTS <- late_unique_GTS[assign_ind_GTS]

    # take min_ma and assign corresponding stage
    assigned_min_ma_GTS <- vapply(assign_GTS, function(x)
      GTS$min_ma[x==GTS$interval_name], FUN.VALUE = numeric(1))
    for(i in seq_len(length(assign_GTS))){
      # stage
      occdf$late_stage[late==assign_GTS[i] &
                          is.na(occdf$late_stage)] <-
        GTS$interval_name[GTS$min_ma==assigned_min_ma_GTS[i] &
                            GTS$rank=="stage"]

    }

  # add max_ma and min_ma based on GTS
  # max_ma
  if("stage_max_ma" %in% colnames(occdf)){
    stage_unique <- unique(occdf$early_stage)
    # find assignable intervals
    assign_age_ind <- vapply(stage_unique, function(x) x %in%
                           GTS$interval_name,
                         FUN.VALUE = logical(1L))
    assign_age <- stage_unique[assign_age_ind]
    # get max ages again here as we excluded assigned intervals earlier
    assigned_max_ma_GTS <- vapply(assign_age, function(x)
      GTS$max_ma[x==GTS$interval_name], FUN.VALUE = numeric(1))
    # assign max age
    for(i in seq_len(length(assign_age))){
      occdf$stage_max_ma[occdf$early_stage==assign_age[i] &
                           is.na(occdf$stage_max_ma)] <-
        assigned_max_ma_GTS[i]
    }
  }
  # min_ma
  if("stage_min_ma" %in% colnames(occdf)){
    stage_unique <- unique(occdf$late_stage)
    # find assignable intervals
    assign_age_ind <- vapply(stage_unique, function(x) x %in%
                           GTS$interval_name,
                         FUN.VALUE = logical(1L))
    assign_age <- stage_unique[assign_age_ind]
    # get min ages again here as we excluded assigned intervals earlier
    assigned_min_ma_GTS <- vapply(assign_age, function(x)
      GTS$min_ma[x==GTS$interval_name], FUN.VALUE = numeric(1))
    # assign min age
    for(i in seq_len(length(assign_age))){
      occdf$stage_min_ma[occdf$late_stage==assign_age[i] &
                           is.na(occdf$stage_min_ma)] <-
        assigned_min_ma_GTS[i]
    }
  }
}

  #=== add stage_mid_ma ages to the output ===


    if("stage_max_ma" %in% colnames(occdf) &
       "stage_min_ma" %in% colnames(occdf)){
      occdf$stage_mid_ma <- (occdf$stage_max_ma+occdf$stage_min_ma)/2
    }

  #=== Ouput ===

  # optional: print assigned stages
  if(print_assigned) {
    message("Occurrences from the following intervals have been assigned stages:")
    print(unique(c(assign1,assign2)))
  }

  # return output
  occdf

}


#
#
occdf <- palaeoverse::tetrapods
#early_interval = "interval"
#late_interval = NULL
interval_key = "example"
assign_with_GTS = "GTS2020"
early_interval = NULL
late_interval = NULL
print_assigned = FALSE

test <- look_up(occdf, interval_key = "example", assign_with_GTS = "GTS2020",
                            early_interval = NULL, late_interval = NULL,
                            print_assigned = FALSE)


#
#
