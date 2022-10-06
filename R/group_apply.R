#' Run other functions across subgroups of data
#'
#' A function to run other palaeoverse functions within subsets of data,
#' delineated using one or more variables.
#'
#' @param occdf \code{dataframe}. A dataframe of the fossil occurrences you
#' wish to analyse. This dataframe must contain the necessary variables for
#' whichever palaeoverse function you wish to run on it.
#' @param function_name \code{character}. The function you wish to run on the
#' dataframe.
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
#' William Gearty and Lewis A. Jones
#' @examples
#' #Examples
#'
#' @export
group_apply <- function(occdf, function_name, variables) {
  form <- as.formula(paste0("~ ", paste(group, collapse = " + ")))
  lst <- split(tetrapods, f = form, drop = TRUE)
  results <- do.call(rbind, lapply(lst, function(item) { blah }))
  return(results)
}
