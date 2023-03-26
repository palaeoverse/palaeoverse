#' Calculate the temporal range of fossil taxa
#'
#' A function to calculate the temporal range of fossil taxa from occurrence
#' data.
#'
#' @param occdf \code{dataframe}. A dataframe of fossil occurrences containing
#' at least three columns: names of taxa, maximum age and minimum age
#' (see `name`, `min_ma`, and `max_ma` arguments).
#' These ages should constrain the age range of the fossil occurrence
#' and are assumed to be in millions of years before present.
#' @param name \code{character}. The name of the column you wish to be treated
#' as the input names, e.g. "genus" (default).
#' @param min_ma \code{character}. The name of the column you wish to be treated
#' as the minimum limit of the age range, e.g. "min_ma" (default).
#' @param max_ma \code{character}. The name of the column you wish to be treated
#' as the maximum limit of the age range, e.g. "max_ma" (default).
#' @param by \code{character}. How should the output be sorted?
#' Either: "FAD" (first-appearance date; default), "LAD" (last-appearance data),
#' or "name" (alphabetically by taxon names).
#' @param plot \code{logical}. Should a plot of the ranges be generated?
#'
#' @return A \code{dataframe} containing the following columns:
#' unique taxa (`taxon`), taxon ID (`taxon_id`), first appearance of taxon
#' (`max_ma`), last appearance of taxon (`min_ma`), duration of temporal
#' range (`range_myr`), and number of occurrences per taxon (`n_occ`) is
#' returned.
#'
#' @details The temporal range(s) of taxa are calculated by extracting all
#' unique taxa (`name` column) from the input `occdf`, and checking their first
#' and last appearance. The temporal duration of each taxon is also calculated.
#' A plot of the temporal range of each taxon is also returned if `plot = TRUE`.
#' If the input data columns contain NAs, these should be removed prior to
#' function call.
#'
#' Note: this function provides output based solely on the user input data. The
#' true duration of a taxon is likely confounded by uncertainty in dating
#' occurrences, and incomplete sampling and preservation.
#'
#' @section Developer(s):
#' Lewis A. Jones
#' @section Reviewer(s):
#' Bethany Allen & Christopher D. Dean
#' @importFrom graphics points
#' @examples
#' # Grab internal data
#' occdf <- tetrapods
#' # Remove NAs
#' occdf <- subset(occdf, !is.na(order) & order != "NO_ORDER_SPECIFIED")
#' # Temporal range
#' ex <- tax_range_time(occdf = occdf, name = "order", plot = TRUE)
#'
#' @export
tax_range_time <- function(occdf,
                           name = "genus",
                           min_ma = "min_ma",
                           max_ma = "max_ma",
                           by = "FAD",
                           plot = FALSE) {

  #=== Handling errors ===
  if (is.data.frame(occdf) == FALSE) {
    stop("`occdf` should be a dataframe")
  }

  if (is.logical(plot) == FALSE) {
    stop("`plot` should be logical (TRUE/FALSE)")
  }

  if (!is.numeric(occdf[, max_ma]) || !is.numeric(occdf[, min_ma])) {
    stop("`max_ma` and `min_ma` must be of class numeric.")
  }

  if (any(c(name, min_ma, max_ma) %in% colnames(occdf) == FALSE)) {
    stop("Either `name`, `min_ma`, or `max_ma`, is not a named column in
`occdf`")
  }

  if (any(is.na(occdf[, name]))) {
    stop("The `name` column contains NA values")
  }

  if (any(is.na(occdf[, min_ma])) || any(is.na(occdf[, max_ma]))) {
    stop("`min_ma` and/or `max_ma` columns contain NA values")
  }

  if (!by %in% c("name", "FAD", "LAD")) {
    stop('`by` must be either "FAD", "LAD", or "name"')
  }

  #=== Set-up ===
  unique_taxa <- unique(occdf[, name])
  # Order taxa
  unique_taxa <- unique_taxa[order(unique_taxa)]

  #=== Temporal range ===
  # Generate dataframe for population
  temp_df <- data.frame(taxon = unique_taxa,
                        taxon_id = seq(1, length(unique_taxa), 1),
                        max_ma = rep(NA, length(unique_taxa)),
                        min_ma = rep(NA, length(unique_taxa)),
                        range_myr = rep(NA, length(unique_taxa)),
                        n_occ = rep(NA, length(unique_taxa)))
    # Run for loop across unique taxa
    for (i in seq_along(unique_taxa)) {
      vec <- which(occdf[, name] == unique_taxa[i])
      temp_df$max_ma[i] <- max(occdf[vec, max_ma])
      temp_df$min_ma[i] <- min(occdf[vec, min_ma])
      temp_df$range_myr[i] <- temp_df$max_ma[i] - temp_df$min_ma[i]
      temp_df$n_occ[i] <- length(vec)
    }
    # Remove row names
    row.names(temp_df) <- NULL
    # Round off values
    temp_df[, c("max_ma", "min_ma", "range_myr")] <- round(
      x = temp_df[, c("max_ma", "min_ma", "range_myr")], digits = 3)

    # Should data be ordered by FAD or LAD?
    if (by == "FAD") {
      temp_df <- temp_df[order(temp_df$max_ma), ]
      temp_df$taxon_id <- seq_len(nrow(temp_df))
    }

    if (by == "LAD") {
      temp_df <- temp_df[order(temp_df$min_ma), ]
      temp_df$taxon_id <- seq_len(nrow(temp_df))
      }

    # Plot data?
    if (plot == TRUE) {
      x_range <- c(max(temp_df$max_ma), min(temp_df$min_ma))
      y_range <- c(0, nrow(temp_df))
      plot(x = NA,
           y = NA,
           xlim = x_range,
           ylim = y_range,
           axes = TRUE,
           xaxt = "n",
           xlab = NA,
           ylab = "Taxon ID",
           main = "Temporal range of taxa")
      segments(x0 = temp_df$max_ma,
               x1 = temp_df$min_ma,
               y0 = temp_df$taxon_id,
               col = "black")
      points(x = temp_df$max_ma,
             y = temp_df$taxon_id,
             pch = 20,
             col = "black")
      points(x = temp_df$min_ma,
             y = temp_df$taxon_id,
             pch = 20,
             col = "black")
      axis_geo(side = 1, intervals = "periods")
      title(xlab = "Time (Ma)", line = 4)
    }

    # Return dataframe
    return(temp_df)
  }
