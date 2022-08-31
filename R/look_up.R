#' Look up geological intervals and assign ICS ages and stages
#'
#' A function to assign fossil occurrences to international geological stages
#' (GTS2020) based on interval names.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to bin. This dataframe should contain  the following named columns:
#' "early_interval" and, optionally, "late_interval". These columns need
#' to be `character` values. If no "late_interval is supplied, only
#' "early_interval" is used, and it is assumed that the occurrences are from
#' that inerval only.
#' @param early_interval \code{character}. Alternative column name that contains
#' the earliest or only interval from which the occurrences are from.
#' @param late_interval \code{character}. Alternative column name that contains
#' the latest interval from which the occurrences are from.
#'
#' @return a \code{dataframe} of the original input `data` with the following
#' appended columns is returned: `early_stage` and `late_stage`, corresponding
#' to the earliest and latest international geological stage (GTS2020) which
#' could be assigned to the occurrence based on the given interval names.
#'
#' @details
#'
#' @section Developer(s):
#' Kilian Eichenseer & ...
#' @section Reviewer(s):
#' ...
#' @examples
#' \dontrun{
#' #Grab internal tetrapod data
#' occdf <- tetrapods
#' }
#' @export
look_up <- function(occdf, interval_key, assign_with_GTS = "GTS2020",
                    early_interval = NULL, late_interval = NULL,
                    print_assigned = FALSE) {

#=== Handling errors ===

  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
  }

  if (is.data.frame(interval_key) == FALSE) {
    stop("`interval_key` should be a dataframe.")
  }

  if (is.null(early_interval) & !("early_interval" %in% colnames(occdf))) {
    stop("`early_interval` needs to match a column name of `occdf`, or an
         alternative name for the early interval columns needs to be provided.")
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

  if(!("interval_name" %in% colnames(interval_key))) {
    stop("`interval_key` needs to contain a column `interval_name`")
  }

  if(!("early_stage" %in% colnames(interval_key))) {
    stop("`interval_key` needs to contain a column `early_stage`")
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
  replace_ind <- which(vapply(test,function(x)
    x %in% replace_pattern | is.na(x), logical(1)))
  late[replace_ind] <- early[replace_ind]

  # add columns to output data frame
  occdf$early_stage <- rep(NA_character_,nrow(occdf))
  occdf$late_stage <- rep(NA_character_, nrow(occdf))

  #=== Assignment of stages based on look-up table ===

  early_unique <- unique(early)
  assign_ind1 <- vapply(early_unique, function(x) x %in%
                          interval_key$interval_name,
                        FUN.VALUE = logical(1L))
  assign1 <- early_unique[assign_ind1]

  for(i in seq_len(length(assign1))){
    occdf$early_stage[early==assign1[i]] <-
      interval_key$early_stage[interval_key$interval_name==assign1[i]]
  }

  late_unique <- unique(late)
  assign_ind2 <- vapply(late_unique, function(x) x %in%
                          interval_key$interval_name,
                        FUN.VALUE = logical(1L))
  assign2 <- early_unique[assign_ind2]

  for(i in seq_len(length(assign2))){
    occdf$late_stage[early==assign2[i]] <-
      interval_key$late_stage[interval_key$interval_name==assign2[i]]
  }

  #=== Assignment of stages based on GTS2020 ===
  # for intervals that could not be matched using the table, try to assign
  #   stages based on GTS2020

  ### ###
### FIX ###
  ### ###

  switch(assign_with_GTS,
         FALSE = {},
         "GTS2020" = {GTS <- palaeoverse::GTS2020},
         "GTS2012" = {GTS <- palaeoverse::GTS2012},
         {stop("`assign_with_GTS` needs to be `FALSE`, `GTS2012` or `GTS2020`")}
  )

  ### Does not work yet

  if(assign_with_GTS %in% c("GTS2012", "GTS2020")) {
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
    for(i in seq_len(length(assign_GTS))){
      occdf$early_stage[early==assign_GTS[i] &
                          is.na(occdf$early_stage)] <-
        GTS$interval_name[GTS$max_ma==assigned_max_ma_GTS[i] &
                            GTS$rank=="stage"]
    }

    # late stages
    late_unique_GTS <- late_unique[assign_ind1==FALSE]
    assign_ind_GTS <- vapply(late_unique_GTS, function(x) x %in%
                               GTS$interval_name,
                             FUN.VALUE = logical(1L))
    assign_GTS <- late_unique_GTS[assign_ind_GTS]

    # take max_ma and assign corresponding stage
    assigned_min_ma_GTS <- vapply(assign_GTS, function(x)
      GTS$min_ma[x==GTS$interval_name], FUN.VALUE = numeric(1))
    for(i in seq_len(length(assign_GTS))){
      occdf$late_stage[late==assign_GTS[i] &
                          is.na(occdf$late_stage)] <-
        GTS$interval_name[GTS$min_ma==assigned_min_ma_GTS[i] &
                            GTS$rank=="stage"]
    }

  }



  #=== add stage ages to the output ===

  #=== Ouput ===

  # optional: print assigned stages
  if(print_assigned) {
    message("Occurrences from the following intervals have been assigned stages:")
    print(unique(c(assign1,assign2)))
  }

  # return output
  occdf

}


occdf <- palaeoverse::reefs
early_interval = "interval"
late_interval = NULL

#load look-up table from Palaeoverse Onedrive
id <- "16OWHzbcUyWICDkGafZZ-pvaWDU5mzqDJ"
interval_key <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
colnames(interval_key)[2] <- "interval_name"
colnames(interval_key)[5] <- "early_stage"
colnames(interval_key)[6] <- "late_stage"

}
