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
look_up <- function(occdf, early_interval = NULL, late_interval = NULL) {

  #=== Handling errors ===

  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe.")
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

}

# save early and late int columns for easier handling
if(is.null(early_interval)) early <- occdf$early_interval else early <-
    occdf[,early_interval]

if(is.null(late_interval) & "late_interval" %in% colnames(occdf)) late <-
    occdf$late_interval else { if(!is.null(late_interval)) {
  late <- occdf[,late_interval]}
     else late <- NULL }


# prepare output
occdf$early_stage <- rep(NA_character_,nrow(occdf))
if(!is.null(late)) occdf$late_stage <- rep(NA_character_, nrow(occdf))

#load look-up table from Palaeoverse Onedrive
id <- "16OWHzbcUyWICDkGafZZ-pvaWDU5mzqDJ"
key <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))


early_unique <- unique(early)
assign <- early_unique[early_unique %in% key$interval_name_ori]
for(i in seq_len(length(assign))){
  occdf$early_stage[early==]
}

if(!is.null(late)) late_unique <- unique(late)
assign <- early_unique[early_unique %in% key$interval_name_ori]


occdf <- palaeoverse::tetrapods
early_interval = NULL
late_interval = NULL
