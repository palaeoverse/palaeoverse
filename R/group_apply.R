#' Apply a function over grouping(s) of data
#'
#' A function to apply palaeoverse functionality across subsets (groups) of
#' data, delineated using one or more variables. See details for compatible
#' functions.
#'
#' @param df \code{dataframe}. A dataframe of the fossil data you
#' wish to analyse. This dataframe must contain the necessary variables for
#' whichever palaeoverse function you wish to call (see function specific
#' documentation for required columns).
#' @param group \code{character}. A vector of the column name(s) you wish
#' to group by (e.g. "collection_no", "stage_bin").
#' @param fun \code{character}. The function you wish to apply to `df`.
#' @param ... \code{args}. Additional arguments available in the called
#' function. These additional arguments may be required for function arguments
#' without default values, or if you wish to overwrite the default argument
#' value (see examples).
#'
#' @return A \code{dataframe} of the outputs from the selected function, with an
#' appended column indicating the user-defined grouping (`grouping`).
#'
#' @details This function is a wrapper which can be used to run other
#' palaeoverse functions on various groupings of data. For example,
#' this enables the separate analysis of occurrences from different time
#' intervals, spatial regions, or trait values.
#' \cr
#' \cr
#' Available compatible `palaeoverse` functions:
#' - \code{\link{tax_unique}}: return the number of unique taxa per grouping
#' variable.
#' - \code{\link{tax_range_time}}: return the temporal range of taxa per
#' grouping variable.
#' - \code{\link{tax_range_geo}}: return the geographic range of taxa per
#' grouping variable.
#' - \code{\link{tax_check}}: return to potential spelling variations of the
#' same taxon per grouping variable. Note: `verbose` should be set to FALSE.
#'
#'
#' @section Developer(s):
#' Lewis A. Jones, Bethany Allen & William Gearty
#' @section Reviewer(s):
#' XXX
#' @importFrom stats as.formula
#' @examples
#' # Examples
#' # Get tetrapods data
#' occdf <- tetrapods
#' # Remove NA data
#' occdf <- subset(occdf, !is.na(genus))
#' # Temporal range of data per time bin with default arguments
#' group_apply(df = occdf, group = c("cc"), fun = tax_range_time)
#' # Temporal range of data per time bin with updated arguments
#' group_apply(df = occdf,
#'             group = c("cc"),
#'             fun = tax_range_time,
#'             name = "family") # Run at family level (default: "genus")
#' # Use multiple grouping variables
#' group_apply(df = occdf,
#'             group = c("collection_no", "cc"),
#'             fun = tax_range_time)
#' @export
group_apply <- function(df, group, fun, ...) {

  # Handle errors
  if (!is.data.frame(df)) {
    stop("`df` should be a dataframe")
  }

  if (!any(group %in% colnames(df))) {
    stop("Supplied `group` is not a named column in `df`")
  }

  fun_name <- deparse(substitute(fun))
  avail <- c("tax_unique", "tax_range_time", "tax_range_geo", "tax_check")
  if (!fun_name %in% avail) {
    stop("Specified function not available or compatible with group_apply.
    Choose from:", paste(" ", avail))
  }

  # Update default arguments with supp arguments
  supp_args <- list(...)
  indx <- which(names(formals(fun)) %in% names(supp_args))
  formals(fun)[indx] <- supp_args
  # Generate bin codes
  bin_codes <- as.formula(paste0("~ ", paste(group, collapse = " + ")))
  # Split dataframe
  lst <- split(df, f = bin_codes, drop = TRUE)
  # Apply function
  results <- do.call(rbind, lapply(lst, fun))
  # Get row names (groups)
  grouping <- row.names(results)
  # Clean up group names
  grouping <- tools::file_path_sans_ext(grouping)
  # Remove funky row names
  row.names(results) <- NULL
  # Add group column
  results <- cbind.data.frame(results, grouping)
  # Return results
  return(results)
}
