#' Run other functions across subgroups of data
#'
#' A function to run other palaeoverse functions within subsets of data,
#' delineated using one or more variables.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to analyse. This dataframe must contain the necessary variables for
#' whichever palaeoverse function you wish to run on it.
#' @param FUN \code{character}. The function you wish to apply to the occdf.
#' @param variables \code{character}. A vector of column names which split the
#' occurrences into subgroups.
#'
#' @return A list of outputs from the chosen function, each corresponding to
#' an individual subgroup of data.
#'
#' @details This function is a wrapper which can be used to run the other
#' palaeoverse functions on various subgroups of data. For example,
#' this enables the separate analysis of occurrences from different time
#' intervals, spatial regions, or trait values.
#'
#' @section Developer(s):
#' Bethany Allen & William Gearty
#' @section Reviewer(s):
#' Lewis A. Jones
#' @examples
#' #Examples
#' bins <- lat_bins(size = 20)
#' occdf <- bin_lat(occdf = tetrapods, bins = bins, lat = "lat")
#'
#'
#' @export
group_apply <- function(occdf, FUN, variables, ...) {
  bin_codes <- as.formula(paste0("~ ", paste(group, collapse = " + ")))
  lst <- split(occdf, f = bin_codes, drop = TRUE)
  results <- do.call(rbind, lapply(lst, FUN()))
  return(results)
}
