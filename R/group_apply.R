#' Apply a function over grouping(s) of data
#'
#' A function to apply `palaeoverse` functionality across subsets (groups) of
#' data, delineated using one or more variables. Functions which receive a
#' `data.frame` as input (e.g. `nrow`, `ncol`, `lengths`, `unique`) may also be
#' used.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences or taxa,
#' as relevant to the desired function.
#' This dataframe must contain the grouping variables and the necessary
#' variables for the function you wish to call (see function-specific
#' documentation for required columns).
#' @param group \code{character}. A vector of column names, specifying the
#' desired subgroups (e.g. "collection_no", "stage_bin"). Supplying more than
#' one grouping variable will produce an output containing subgroups for each
#' unique combination of values.
#' @param fun \code{function}. The function you wish to apply to
#' `occdf`. See details for compatible functions.
#' @param ... Additional arguments available in the called
#' function. These arguments may be required for function arguments
#' without default values, or if you wish to overwrite the default argument
#' value (see examples).
#'
#' @return A \code{data.frame} of the outputs from the selected function, with
#' prepended column(s) indicating the user-defined groups. If a single vector
#' is returned via the called function, it will be transformed to a
#' \code{data.frame} with the column name equal to the input function.
#'
#' @details `group_apply` applies functions to subgroups of data within a
#' supplied dataset, enabling the separate analysis of occurrences or taxa from
#' different time intervals, spatial regions, or trait values. The function
#' serves as a wrapper around `palaeoverse` functions. Other functions which
#' can be applied to a `data.frame` (e.g. `nrow`, `ncol`, `lengths`,
#' `unique`) may also be used.
#' \cr
#' \cr
#' All `palaeoverse` functions which require a dataframe input can be used in
#' conjunction with the `group_apply` function. However, this is unnecessary
#' for many functions (e.g. \code{\link{bin_time}}) as groups do not need to
#' be partitioned before binning. This list provides
#' users with `palaeoverse` functions that might be interesting to apply across
#' group(s):
#' - \code{\link{tax_unique}}: return the number of unique taxa per grouping
#' variable.
#' - \code{\link{tax_range_time}}: return the temporal range of taxa per
#' grouping variable.
#' - \code{\link{tax_range_space}}: return the geographic range of taxa per
#' grouping variable.
#' - \code{\link{tax_check}}: return potential spelling variations of the
#' same taxon per grouping variable. Note: `verbose` needs to be set to FALSE.
#'
#'
#' @section Developer(s):
#' Lewis A. Jones & William Gearty
#' @section Reviewer(s):
#' Kilian Eichenseer & Bethany Allen
#' @importFrom stats as.formula
#' @examples
#' # Examples
#' # Get tetrapods data
#' occdf <- tetrapods[1:100, ]
#' # Remove NA data
#' occdf <- subset(occdf, !is.na(genus))
#' # Count number of occurrences from each country
#' ex1 <- group_apply(occdf = occdf, group = "cc", fun = nrow)
#' # Unique genera per collection with group_apply and input arguments
#' ex2 <- group_apply(occdf = occdf,
#'                    group = c("collection_no"),
#'                    fun = tax_unique,
#'                    genus = "genus",
#'                    family = "family",
#'                    order = "order",
#'                    class = "class",
#'                    resolution = "genus")
#' # Use multiple variables (number of occurrences per collection and formation)
#' ex3 <- group_apply(occdf = occdf,
#'                    group = c("collection_no", "formation"),
#'                    fun = nrow)
#' # Compute counts of occurrences per latitudinal bin
#' # Set up lat bins
#' bins <- lat_bins_degrees()
#' # bin occurrences
#' occdf <- bin_lat(occdf = occdf, bins = bins)
#' # Calculate number of occurrences per bin
#' ex4 <- group_apply(occdf = occdf, group = "lat_bin", fun = nrow)
#' @export
group_apply <- function(occdf, group, fun, ...) {
  # Handle errors
  if (!is.data.frame(occdf)) {
    stop("`occdf` should be a dataframe")
  }

  if (!any(group %in% colnames(occdf))) {
    stop("Supplied `group` is not a named column in `occdf`")
  }

  if (!is.function(fun)) {
    stop("Supplied `fun` is not a function")
  }

  supp_args <- list(...)
  if (!("..." %in% names(formals(fun)))) {
    indx <- which(!(names(supp_args) %in% names(formals(fun))))
    if (length(indx) > 1) {
      stop(paste(paste0("`", names(supp_args)[indx], "`", collapse = "/"),
                 "are not valid arguments for the specified function"))
    } else if (length(indx) == 1) {
      stop(paste0("`", names(supp_args)[indx], "`",
                  " is not a valid argument for the specified function"))
    }
  }
  # Generate formula
  form <- as.formula(paste0("~ ", paste(group, collapse = " + ")))

  # by is a wrapper of tapply, but it ends up being MUCH faster than tapply
  # because of some data wrangling it does
  output_lst <- by(occdf, form, fun, ...)

  if (is.list(output_lst)) {
    # modified from array2DF() to handle when functions return empty dfs
    keys <- do.call(expand.grid, c(dimnames(provideDimnames(output_lst)),
                                   KEEP.OUT.ATTRS = FALSE,
                                   stringsAsFactors = FALSE))
    # filter out NULLs
    output_lst_keep <- vapply(output_lst, Negate(is.null), FALSE)
    output_lst <- output_lst[output_lst_keep]
    dfrows <- vapply(output_lst, nrow, 1L)
    keys <- keys[output_lst_keep, , drop = FALSE]
    output_df <- cbind(keys[rep(seq_along(dfrows), dfrows), , drop = FALSE],
                       do.call(rbind, output_lst))
  } else {
    fun_name <- deparse(substitute(fun))
    output_df <- array2DF(output_lst, responseName = fun_name)
    output_df <- output_df[!is.na(output_df[, fun_name]), ]
  }

  # Update output if none returned
  if (nrow(output_df) == 0) {
    output_df <- NULL
  }

  # Return output
  return(output_df)
}
