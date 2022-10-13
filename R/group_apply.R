#' Apply a function over grouping(s) of data
#'
#' A function to apply `palaeoverse` functionality across subsets (groups) of
#' data, delineated using one or more variables. `data.frame` related functions
#' (e.g. `nrow`, `ncol`, `lengths`, `unique`) may also be used.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences.
#' This dataframe must contain the grouping variables and the necessary
#' variables for the function you wish to call (see function specific
#' documentation for required columns).
#' @param group \code{character}. A vector of column names, specifying the
#' grouping (e.g. "collection_no", "stage_bin").
#' @param fun \code{function}. The function you wish to apply to
#' `occdf`. See details for compatible functions.
#' @param ... Additional arguments available in the called
#' function. These additional arguments may be required for function arguments
#' without default values, or if you wish to overwrite the default argument
#' value (see examples).
#'
#' @return A \code{dataframe} of the outputs from the selected function, with an
#' appended column indicating the user-defined grouping (`grouping`).
#'
#' @details `group_apply` enables the separate analysis of occurrences from
#' different time intervals, spatial regions, or trait values. The function
#' serves as a wrapper around `palaeoverse` functions on groupings of
#' data. `data.frame` related functions (e.g. `nrow`, `ncol`, `lengths`,
#' `unique`) may also be used.
#' \cr
#' \cr
#' Relevant `palaeoverse` functions:
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
#' ex1 <- group_apply(occdf = occdf, group = c("cc"), fun = nrow)
#' # Temporal range of taxa per time bin with default arguments
#' ex2 <- group_apply(occdf = occdf, group = c("cc"), fun = tax_range_time)
#' # Temporal range of taxa per time bin with updated arguments
#' ex3 <- group_apply(occdf = occdf,
#'                    group = c("cc"),
#'                    fun = tax_range_time,
#'                    name = "family") # Run at family level (default: "genus")
#' # Use multiple variables (number of occurrences per collection and country)
#' ex4 <- group_apply(occdf = occdf,
#'                    group = c("collection_no", "cc"),
#'                    fun = nrow)
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
  lst <- split(occdf, f = bin_codes, drop = TRUE)
  # Group names
  nme <- names(lst)
  # Apply function
  output <- do.call(rbind, lapply(lst, fun))
  # String match
  # Get row names (groups)
  grouping <- row.names(output)
  for (i in nme) {
    vec <- grep(pattern = i, x = grouping, ignore.case = FALSE)
    grouping[vec] <- i
  }
  # Remove funky row names
  row.names(output) <- NULL
  # Add group column
  output <- cbind.data.frame(output, grouping)
  # Update output if none returned
  if (nrow(output) == 0) {
    output <- NULL
  }
  # Return output
  return(output)
}
