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
#' function. These additional arguments may be required for function arguments
#' without default values, or if you wish to overwrite the default argument
#' value (see examples).
#'
#' @return A \code{dataframe} of the outputs from the selected function, with an
#' appended column indicating the user-defined groups (`grouping`).
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
#' - \code{\link{tax_range_geo}}: return the geographic range of taxa per
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
#' occdf <- tetrapods
#' # Remove NA data
#' occdf <- subset(occdf, !is.na(genus))
#' # Count number of occurrences from each country
#' ex1 <- group_apply(occdf = occdf, group = "cc", fun = nrow)
#' # Temporal range of taxa per country with default arguments
#' ex2 <- group_apply(occdf = occdf, group = "cc", fun = tax_range_time)
#' # Temporal range of taxa per country with updated arguments
#' ex3 <- group_apply(occdf = occdf,
#'                    group = c("cc"),
#'                    fun = tax_range_time,
#'                    by = "LAD") # Order by LAD (default: "FAD")
#' # Use multiple variables (number of occurrences per collection and formation)
#' ex4 <- group_apply(occdf = occdf,
#'                    group = c("collection_no", "formation"),
#'                    fun = nrow)
#' # Compute counts of occurrences per latitudinal bin
#' # Set up lat bins
#' bins <- lat_bins()
#' # bin occurrences
#' occdf <- bin_lat(occdf = tetrapods, bins = bins)
#' # Calculate number of occurrences per bin
#' ex5 <- group_apply(occdf = occdf, group = "lat_bin", fun = nrow)
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

  # Update default arguments with supp arguments
  supp_args <- list(...)
  # Which arguments should be updated?
  indx <- which(names(formals(fun)) %in% names(supp_args))
  # Which arguments exist in the function?
  indy <- which(names(supp_args) %in% names(formals(fun)))
  # Subset supp arguments to those applicable
  supp_args <- supp_args[indy]
  # Update order to match function order
  supp_args <- supp_args[order(match(names(supp_args),
                                     names(formals(fun)[indx])))]
  # Update arguments with user-defined input
  formals(fun)[indx] <- supp_args
  # Generate bin codes
  bin_codes <- as.formula(paste0("~ ", paste(group, collapse = " + ")))
  # Split dataframe
  lst <- split(occdf, f = bin_codes, drop = TRUE, sep = "&1*^$0%")
  # Group names
  nme <- names(lst)
  # Split into individual columns
  nme_df <- do.call(rbind.data.frame, strsplit(nme, "&1*^$0%", fixed = TRUE))
  colnames(nme_df) <- group
  # Apply function
  output_lst <- lapply(lst, fun)
  # Add groupings to output
  output_df <- do.call(rbind.data.frame,
                       lapply(seq_along(output_lst), FUN = function(i) {
    df <- output_lst[[i]]
    if (!is.null(nrow(df)) && nrow(df) > 0) {
      cbind.data.frame(df, nme_df[i, , drop = FALSE])
    }
  }))
  # Update output if none returned
  if (nrow(output_df) == 0) {
    output_df <- NULL
  }
  # Return output
  return(output_df)
}
